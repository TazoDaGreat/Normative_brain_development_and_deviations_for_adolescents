library(tidyverse)
library(lmerTest)
library(janitor)

# ==================================== data ==========================================
cortical_thickness <- read_csv("Library/CloudStorage/Box-Box/ABCD Tabulated Data/5.1/core/imaging/mri_y_smr_thk_dsk.csv") %>% 
  filter(eventname == "baseline_year_1_arm_1" | eventname == "2_year_follow_up_y_arm_1") # filter to baseline and year two

covariates <- read_csv("Library/CloudStorage/Box-Box/2025-06-11_KAD.csv") %>% ## from data prep code
  filter(eventname == "baseline_year_1_arm_1"| eventname == "2_year_follow_up_y_arm_1") %>%
  mutate(AnyDD_ever_p_y = case_when(AnyDD_ever_p_y == "never" ~ 0,
                                    AnyDD_ever_p_y == "present" ~ 1,
                                    AnyDD_ever_p_y == "past" ~ 1),
         interview_age_scale = interview_age / 100) 

data <- left_join(cortical_thickness, covariates, by = c("src_subject_id", "eventname")) 
complete_data <- data %>% drop_na(interview_age, demo_sex_v2, AnyDD_ever_p_y, pubertal_status)
# ================================== plot ==============================================

data %>%
  ggplot(aes(x = smri_thick_cdk_banksstslh, y = smri_thick_cdk_cdacatelh, color = mri_info_manufacturer)) +
  geom_point() +
  theme_classic()


# ================================= reproducing longitudinal combat -- initial inspection of residuals pre harmonization =======================================


list_of_ct_regions <- colnames(cortical_thickness)[3:ncol(cortical_thickness)] # this is important

residuals_list <- list()

for (i in 1:71) {
  form <- as.formula(paste0(list_of_ct_regions[i], "~interview_age + demo_sex_v2 + AnyDD_ever_p_y + (1|src_subject_id)"))
  residuals_list[[i]] <- residuals(lmer(form, data = data))
}                    

residuals_df <- do.call(cbind.data.frame, residuals_list) %>% clean_names()
colnames(residuals_df) <- paste0(list_of_ct_regions, "_residuals")

complete_data <- data %>% drop_na(interview_age, demo_sex_v2, AnyDD_ever_p_y)
complete_data <- cbind(complete_data, residuals_df)

for_plot <- complete_data %>% 
  select(src_subject_id, eventname, mri_info_manufacturer, site_id_l, contains("residuals")) %>%
  pivot_longer(cols = ends_with("_residuals"), names_to = "region", values_to = "residual")

for_plot %>%
  mutate(region = str_remove(region, "smri_thick_cdk_"),
         region = str_remove(region, "_residuals")) %>%
  filter(grepl("^[a-f]", region)) %>%
  ggplot(aes(y = residual, color = mri_info_manufacturer)) +
  geom_boxplot() +
  theme_classic() +
  facet_wrap(~region, scales = "free_y")


avg_resid_by_mri_type <- for_plot %>%
  group_by(mri_info_manufacturer, region) %>%
  summarize(mean_resid = mean(residual)) %>%
  ungroup() %>%
  pivot_wider(names_from = mri_info_manufacturer, values_from = mean_resid) %>%
  clean_names() %>%
  summarise(mean(ge_medical_systems), mean(philips_medical_systems), mean(siemens)) %>% 
  pivot_longer(cols = everything(), values_to = "Average Residual", names_to = "MRI Type") %>%
  mutate(`MRI Type` = str_remove(`MRI Type`, "mean"))
  
  
top_10 <- for_plot %>%
  group_by(mri_info_manufacturer, region) %>%
  summarize(mean_resid = mean(residual)) %>%
  mutate(region = str_remove(region, "smri_thick_cdk_"),
         region = str_remove(region, "_residuals")) %>%
  ungroup() %>%
  #mutate(across(3, ~abs(.x))) %>%
  arrange(desc(mean_resid))  %>%
  head()
  
bottom_10 <- for_plot %>%
  group_by(mri_info_manufacturer, region) %>%
  summarize(mean_resid = mean(residual)) %>%
  mutate(region = str_remove(region, "smri_thick_cdk_"),
         region = str_remove(region, "_residuals")) %>%
  ungroup() %>%
  #mutate(across(3, ~abs(.x))) %>%
  arrange((mean_resid)) %>%
  head()

bad_regions <- unique(c(bottom_10$region, top_10$region))
  
bad_regions_plotted <- for_plot %>%
  mutate(region = str_remove(region, "smri_thick_cdk_"),
         region = str_remove(region, "_residuals")) %>%
  filter(region %in% bad_regions) %>%
  ggplot(aes(y = residual, color = mri_info_manufacturer)) +
  geom_boxplot(outliers = F) +
  geom_hline(yintercept = 0, linetype = 2)+
  theme_classic() +
  facet_wrap(~region, scales = "free_y")  

# ================================= reproducing longitudinal combat -- applying longCOMBAT =======================================

# NOTE: TIDYVERSE CANNOT BE LOADED AT THE SAME TIME
detach("package:tidyverse", unload=TRUE)

#library(devtools)
#devtools::install_github("jcbeer/longCombat")
library(longCombat)

# testing for batch effects before combat
boxplot_before_combat <- batchBoxplot(idvar='src_subject_id', 
             batchvar='mri_info_manufacturer', 
             feature='smri_thick_cdk_locclh', 
             formula='interview_age + AnyDD_ever_p_y*interview_age',
             ranef='(1|src_subject_id)',
             data=complete_data,
             main = "Left plot")

batchBoxplot(idvar='src_subject_id', 
             batchvar='site_id_l', 
             feature='smri_thick_cdk_tmpolerh', 
             formula='interview_age + AnyDD_ever_p_y*interview_age',
             ranef='(1|src_subject_id)',
             data=complete_data)

addTestTable <- addTest(idvar='src_subject_id', 
                        batchvar='mri_info_manufacturer', 
                        features=list_of_ct_regions, 
                        formula='interview_age + AnyDD_ever_p_y*interview_age',
                        ranef='(1|src_subject_id)',
                        data=complete_data)

multTestTable <- multTest(idvar='src_subject_id', 
                          batchvar='mri_info_manufacturer', 
                          features=list_of_ct_regions, 
                          formula='interview_age + AnyDD_ever_p_y*interview_age',
                          ranef='(1|src_subject_id)',
                          data=complete_data)

# applying combat
data_for_combat <- complete_data %>%
  select(src_subject_id, eventname, mri_info_manufacturer, interview_age, AnyDD_ever_p_y, site_id_l, starts_with("smri"))

simdata_combat <- longCombat(idvar='src_subject_id', 
                             timevar='interview_age',
                             batchvar='mri_info_manufacturer', 
                             features=list_of_ct_regions, 
                             formula='interview_age + AnyDD_ever_p_y*interview_age',
                             ranef='(1|src_subject_id)',
                             data=data_for_combat)

data_harmonized <- simdata_combat$data_combat
# save combat feature names
featurenames.combat <- names(data_harmonized)[4:74]
# merge with original dataframe
complete_data_plus_combat <- merge(complete_data, data_harmonized[,c(1,2,4:74)], by=c('src_subject_id', 'eventname'))


# testing for batch effects after combat
addTestTableCombat <- addTest(idvar='src_subject_id', 
                              batchvar='mri_info_manufacturer', 
                              features=featurenames.combat, 
                              formula='interview_age + AnyDD_ever_p_y*interview_age',
                              ranef='(1|src_subject_id)',
                              data=complete_data_plus_combat)

boxplot_after_combat <- batchBoxplot(idvar='src_subject_id', 
             batchvar='mri_info_manufacturer', 
             feature='smri_thick_cdk_locclh.combat', 
             formula='interview_age + AnyDD_ever_p_y*interview_age',
             ranef='(1|src_subject_id)',
             data=complete_data_plus_combat,
             main = "Right plot")

# boxplot_comparison <- plot_grid(boxplot_before_combat, boxplot_after_combat)
# 
# boxplot_before_combat + boxplot_after_combat

par(mfrow = c(1, 2))
boxplot_before_combat <- batchBoxplot(idvar='src_subject_id', 
                                      batchvar='mri_info_manufacturer', 
                                      feature='smri_thick_cdk_locclh', 
                                      formula='interview_age + AnyDD_ever_p_y*eventname',
                                      ranef='(1|src_subject_id)',
                                      data=complete_data,
                                      main = "Left plot")

boxplot_after_combat <- batchBoxplot(idvar='src_subject_id', 
                                     batchvar='mri_info_manufacturer', 
                                     feature='smri_thick_cdk_locclh.combat', 
                                     formula='interview_age + AnyDD_ever_p_y*eventname',
                                     ranef='(1|src_subject_id)',
                                     data=complete_data_plus_combat,
                                     main = "Right plot")

# ================================= impact of including a covariate ===========================


# applying combat
data_for_combat <- complete_data %>%
  select(src_subject_id, eventname, mri_info_manufacturer, interview_age, AnyDD_ever_p_y, demo_sex_v2, pubertal_status, site_id_l, starts_with("smri"))

simdata_combat <- longCombat(idvar='src_subject_id', 
                             timevar='interview_age',
                             batchvar='mri_info_manufacturer', 
                             features=list_of_ct_regions, 
                             formula='interview_age + AnyDD_ever_p_y*interview_age',
                             ranef='(1|src_subject_id)',
                             data=data_for_combat)

data_harmonized <- simdata_combat$data_combat
# save combat feature names
featurenames.combat <- names(data_harmonized)[4:74]
# merge with original dataframe
just_covariates <- complete_data %>% select(!contains("smri"))
complete_data_plus_combat <- merge(just_covariates, data_harmonized[,c(1,2,4:74)], by=c('src_subject_id', 'interview_age'))



# applying combat: repeat with sex as a covariate
simdata_combat_sex <- longCombat(idvar='src_subject_id', 
                             timevar='interview_age',
                             batchvar='mri_info_manufacturer', 
                             features=list_of_ct_regions, 
                             formula='interview_age + AnyDD_ever_p_y*interview_age + demo_sex_v2',
                             ranef='(1|src_subject_id)',
                             data=data_for_combat)

data_harmonized_sex <- simdata_combat_sex$data_combat
# save combat feature names
featurenames.combat <- names(data_harmonized_sex)[4:74]
# merge with original dataframe
complete_data_plus_combat_sex <- merge(just_covariates, data_harmonized_sex[,c(1,2,4:74)], by=c('src_subject_id', 'interview_age'))



trial <- complete_data_plus_combat %>%
  pivot_longer(cols = contains("combat"), names_to = "combat", values_to = "ct")

trial_sex <- complete_data_plus_combat_sex %>%
  pivot_longer(cols = contains("combat"), names_to = "combat", values_to = "ct")

trial_raw <- complete_data %>%
  pivot_longer(cols = contains("smri"), names_to = "combat", values_to = "ct")



# lmer( smri_thick_cdk_tmpolerh.combat ~ factor(demo_sex_v2) + (1|src_subject_id), data =complete_data_plus_combat_sex) 
# lmer( smri_thick_cdk_tmpolerh.combat ~ factor(demo_sex_v2) + (1|src_subject_id), data =complete_data_plus_combat) 


mods <- trial %>%
  # filter(str_detect(combat, "lh")) %>% # filter to just left hemi
  nest_by(combat) %>%
  mutate(mod = list(lmerTest::lmer(ct ~ factor(demo_sex_v2) + (1|src_subject_id), data = data))) %>%
  reframe(data.frame(coef(summary(mod))) %>% 
  rownames_to_column("term")) #%>% 
  #filter(term == "n_cpts_adj") %>% 

mods_sex <- trial_sex %>%
  # filter(str_detect(combat, "lh")) %>% # filter to just left hemi
  nest_by(combat) %>%
  mutate(mod = list(lmerTest::lmer(ct ~ factor(demo_sex_v2) + (1|src_subject_id), data = data))) %>%
  reframe(data.frame(coef(summary(mod))) %>% 
  rownames_to_column("term"))

mods_raw <- trial_raw %>%
  # filter(str_detect(combat, "lh")) %>% # filter to just left hemi
  nest_by(combat) %>%
  mutate(mod = list(lmerTest::lmer(ct ~ factor(demo_sex_v2) + (1|src_subject_id), data = data))) %>%
  reframe(data.frame(coef(summary(mod))) %>% 
            rownames_to_column("term"))

mods_clean <- mods %>% filter(term == "factor(demo_sex_v2)2") %>% mutate(type_of_ct = "combat")
mods_sex_clean <- mods_sex %>% filter(term == "factor(demo_sex_v2)2") %>% mutate(type_of_ct = "combat_sex")
mods_sex_raw <- mods_raw %>% filter(term == "factor(demo_sex_v2)2") %>% mutate(type_of_ct = "raw_ct")

all_the_mods <- rbind(mods_clean, mods_sex_clean, mods_sex_raw) %>% 
  rename(region = combat) %>%
  mutate(region = str_remove(region, "smri_thick_cdk_"),
         region = str_remove(region, ".combat"),
         hemi = if_else(str_detect(region, "lh"), "left", "right")) 
  

forest_plot <- all_the_mods %>%
  mutate(p_label = if_else(`Pr...t..` <= 0.05, "*", "")) %>%
  ggplot() +
  geom_pointrange(aes(x = region, 
                      y = Estimate, 
                      ymin = Estimate - 1.96*`Std..Error`, 
                      ymax = Estimate + 1.96*`Std..Error`,
                      color = type_of_ct,
                      shape = p_label), 
                  fill = "white", size = 1, stroke = 1.5, linewidth = 1,
                  position = position_nudge(x = if_else(all_the_mods$region == "combat", .20, if_else(all_the_mods$region == "combat_sex", -0.2, 0)))) + # fill white so you don't see the lines through the circles
  geom_hline(yintercept=0, lty=2) + 
  coord_flip() +  
  labs(x = "Region", 
       y = "Sex Coefficient Estimates") +
  theme_classic()+
  scale_shape_manual(values = c(21, 8)) + # 21 = open circle, 8 = *
  scale_color_manual(values = wes_palette("Darjeeling1")[2:4]) + ## i get error here "Error in wes_palette("Darjeeling1") : could not find function "wes_palette"
  #theme(legend.position="none")  + # remove legend
  theme(text = element_text(family = "Times New Roman"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) +
  theme(panel.spacing = unit(20, "pt"))



# ================================= demographic differences by scanner ===========================


table1_data <- complete_data %>%
  mutate(Race = case_when(Asian_race == 1 ~ "Asian",
                          Black_race == 1 ~ "Black/African American",
                          Indigenous_race == 1 ~ "Native American, Pacific Islander",
                          Other_race== 1 ~ "Other",
                          White_race == 1 ~ "White",
                          factor(demo_ethn_v2) == 1 ~ "Hispanic/Latinx")) %>%
  mutate(demo_comb_income_v2_bl = as.numeric(demo_comb_income_v2_bl),
         demo_prnt_ed_v2_bl = as.numeric(demo_prnt_ed_v2_bl)) %>%
  mutate(`Marital Status` = case_when(demo_prnt_marital_v2_bl == 1 ~ "Married",
                                      demo_prnt_marital_v2_bl == 2 ~ "Widowed",
                                      demo_prnt_marital_v2_bl == 3 ~ "Divorced",
                                      demo_prnt_marital_v2_bl== 4 ~ "Separated",
                                      demo_prnt_marital_v2_bl == 5 ~ "Never married",
                                      demo_prnt_marital_v2_bl == 6 ~ "Living with partner"),
         Gender = case_when(demo_gender_id_v2_comb == 1 ~ "Male",
                            demo_gender_id_v2_comb == 2 ~ "Female",
                            demo_gender_id_v2_comb == 3 ~ "Trans male",
                            demo_gender_id_v2_comb == 4 ~ "Trans female",
                            demo_gender_id_v2_comb == 5 ~ "Gender queer",
                            demo_gender_id_v2_comb == 6 ~ "Different gender"),
         `Pubertal Status` = case_when(pubertal_status == 1 ~ "Pre-Puberty", 
                                       pubertal_status == 2 ~ "Early Puberty",
                                       pubertal_status == 3 ~ "Mid Puberty",
                                       pubertal_status == 4 ~ "Late Puberty",
                                       pubertal_status == 5 ~"Post Puberty"),
         # income = case_when(demo_comb_income_v2_bl == 1 ~ "<$5,000",
         #                    demo_comb_income_v2_bl == 2 ~ "$5,000 - $11,999",
         #                    demo_comb_income_v2_bl == 3 ~ "$12,000 - $15,999",
         #                    demo_comb_income_v2_bl == 4 ~ "$16,000 - $24,999",
         #                    demo_comb_income_v2_bl == 5 ~ "$25,000 - $34,999",
         #                    demo_comb_income_v2_bl == 6 ~ "$35,000 - $49,999",
         #                    demo_comb_income_v2_bl == 7 ~ "$50,000 - $74,999",
         #                    demo_comb_income_v2_bl == 8 ~ "$75,000 - $99,999",
         #                    demo_comb_income_v2_bl == 9 ~ "$100,000 - $199,999",
         #                    demo_comb_income_v2_bl == 10 ~ ">$200,000"),
         Income = case_when(demo_comb_income_v2_bl >= 1 & demo_comb_income_v2_bl <= 6 ~ "$0 - $50,000",
                            demo_comb_income_v2_bl >= 7 & demo_comb_income_v2_bl <= 8 ~ "$50,000 - $99,999",
                            demo_comb_income_v2_bl == 9 ~ "$100,000 - $199,999",
                            demo_comb_income_v2_bl == 10 ~ "$200,000 + "),
         Education = case_when(demo_prnt_ed_v2_bl <= 12 ~ "Less than high school",
                               demo_prnt_ed_v2_bl == 13 | demo_prnt_ed_v2_bl == 14 ~ "High school diploma or GED",
                               demo_prnt_ed_v2_bl == 15  ~ "Some college",
                               demo_prnt_ed_v2_bl >= 16 & demo_prnt_ed_v2_bl <= 18 ~ "Finished college",
                               demo_prnt_ed_v2_bl >= 19 & demo_prnt_ed_v2_bl <= 21 ~ "Graduate Degree"),
         interview_age = interview_age/12) %>%
  rename(Depression = AnyDD_ever_p_y,
         `Suicide Attempt` = SA_ever_p_y,
         `Suicidal Ideation` = SI_ever_p_y,
         `Non-Suicidal Self-Injury` = NSSI_ever_p_y,
         Age = interview_age,
         `CBCL: Total Problems` = cbcl_scr_syn_totprob_r ,
         `CBCL: Externalizing` = cbcl_scr_syn_external_r, 
         `CBCL: Internalizing` = cbcl_scr_syn_internal_r, 
         `CBCL: Depression` = cbcl_scr_dsm5_depress_r, 
         `CBCL: Anxiety` = cbcl_scr_dsm5_anxdisord_r,
         `CBCL: ADHD` = cbcl_scr_dsm5_adhd_r) %>%
  mutate(across(c(Income, Education, `Marital Status`, ends_with("race")), ~factor(.x)))

table1_combat <- table1(~ Asian_race + Black_race + Indigenous_race + Other_race + White_race + demo_ethn_v2 + Income + Education + `Marital Status` + Gender + `Pubertal Status` + Age + 
                        `CBCL: Total Problems` + `CBCL: Externalizing` + `CBCL: Internalizing` + `CBCL: Depression` + `CBCL: Anxiety` + `CBCL: ADHD` + Depression +`Suicidal Ideation` | mri_info_manufacturer, 
                      data = table1_data)



































# ================================= pca =======================================

pca_data <- data %>%
  select(starts_with("smri")) %>%
  mutate(across(everything(), ~scale(.x))) %>%
  drop_na()

pc <- princomp(pca_data)
summary(pc)

fviz_eig(pc, addlabels = TRUE)

# ================================= lmer with pcs =======================================

the_pcs <- pc$scores

thing <- data.frame(cbind(the_pcs, mri_info_manufacturer = data$mri_info_manufacturer, src_subject_id = data$src_subject_id, eventname = data$eventname)) %>%
  mutate(across(starts_with("Comp"), ~as.numeric(.x)))

list_of_pcs <- colnames(the_pcs)

list_p <- list()

for (i in 1:71) {
  form <- as.formula(paste0(list_of_pcs[i], "~mri_info_manufacturer + (1|src_subject_id)"))
  print(form)
  list_p[[i]] <- coef(summary(lmerTest::lmer(form, data = thing)))[2,5]
}                    

# mod <- lmerTest::lmer(Comp.1 ~ mri_info_manufacturer + (1|src_subject_id), data = data.frame(thing))
# 
# coef(summary(mod))[2,5]

which.min(list_p)

thing %>%
  ggplot(aes(x = Comp.1, y = Comp.5, color = mri_info_manufacturer)) +
  geom_point(alpha = 0.5) +
  theme_classic() +
  facet_wrap(~mri_info_manufacturer)


thing %>%
  ggplot(aes(y = Comp.1, color = mri_info_manufacturer)) +
  geom_boxplot() +
  theme_classic()

thing %>%
  ggplot(aes(x = Comp.1, color = mri_info_manufacturer)) +
  geom_density() +
  theme_classic()

# ================================= lmer with smri ct vars =======================================


list_of_ct_regions <- colnames(cortical_thickness)[3:ncol(cortical_thickness)]

list_p_ct <- list()

for (i in 1:71) {
  form <- as.formula(paste0(list_of_ct_regions[i], "~mri_info_manufacturer + (1|src_subject_id)"))
  list_p_ct[[i]] <- coef(summary(lmerTest::lmer(form, data = data)))[2,5]
}                    

which(list_p_ct < 0.00005)
which.min(list_p_ct)

thing %>%
  ggplot(aes(x = Comp.1, y = Comp.5, color = mri_info_manufacturer)) +
  geom_point(alpha = 0.5) +
  theme_classic() +
  facet_wrap(~mri_info_manufacturer)


thing %>%
  ggplot(aes(y = Comp.1, color = mri_info_manufacturer)) +
  geom_boxplot() +
  theme_classic()

thing %>%
  ggplot(aes(x = Comp.1, color = mri_info_manufacturer)) +
  geom_density() +
  theme_classic()

