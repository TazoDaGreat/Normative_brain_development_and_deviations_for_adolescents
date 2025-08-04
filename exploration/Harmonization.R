library(tidyverse)
library(lmerTest)
library(janitor)

# ==================================== data ==========================================
cortical_thickness <- read_csv("~/Library/CloudStorage/Box-Box/ABCD Tabulated Data/5.1/core/imaging/mri_y_smr_thk_dsk.csv") %>% 
  filter(eventname == "baseline_year_1_arm_1" | eventname == "2_year_follow_up_y_arm_1") # filter to baseline and year two

covariates <- read_csv("~/Library/CloudStorage/Box-Box/2025-06-11_KAD.csv") %>% ## from data prep code
  filter(eventname == "baseline_year_1_arm_1"| eventname == "2_year_follow_up_y_arm_1") %>%
  mutate(AnyDD_ever_p_y = case_when(AnyDD_ever_p_y == "never" ~ 0,
                                    AnyDD_ever_p_y == "present" ~ 1,
                                    AnyDD_ever_p_y == "past" ~ 1),
         interview_age_scale = interview_age / 100)%>%
  mutate(baseline_age = interview_age_b)
cova

baseline_ages <- covariates %>%
  group_by(src_subject_id) %>%
  mutate(baseline_age = first(baseline_age)) %>% ## change "baseline_ages to something else, it could get confusing
  pull(baseline_age)



covariates <- covariates %>%
  mutate(baseline_age = baseline_ages)

data <- left_join(cortical_thickness, covariates, by = c("src_subject_id", "eventname")) 
complete_data <- data %>% drop_na(interview_age, demo_sex_v2, AnyDD_ever_p_y, pubertal_status) ## !!! you may not want drop demo_sex_v2 for your model to work!!!! Ask Ellery
# ================================== plot ==============================================

data %>%
  ggplot(aes(x = smri_thick_cdk_banksstslh, y = smri_thick_cdk_cdacatelh, color = mri_info_manufacturer)) +
  geom_point() +
  theme_classic()


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

## you have to change the longCombat parameters as explained by ellery


# <- longCombat(idvar='src_subject_id', ### stays the same
#            timevar=' ',  ### interview_age and or eventName work for this
#            batchvar=' ', ### site_id
#            features=list_of_ct_regions, ### things we want to harmonize
#            formula='demo_sex_v2 + baseline_age', ### You will have to create the baseline age on your own :(
#            ranef='(1|src_subject_id)', ### no change
#            data=data_for_combat)




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
                          formula='interview_age + AnyDD_ever_p_y*interview_age', ## change this function to match other parameters
                          ranef='(1|src_subject_id)',
                          data=complete_data)

# applying combat
data_for_combat <- complete_data %>%
  select(src_subject_id, eventname, site_id_l,interview_age, demo_sex_v2, baseline_age, site_id_l, starts_with("smri"))

# <- longCombat(idvar='src_subject_id', ### stays the same
#            timevar=' ',  ### interview_age and or eventName work for this
#            batchvar=' ', ### site_id
#            features=list_of_ct_regions, ### things we want to harmonize
#            formula='demo_sex_v2 + baseline_age', ### You will have to create the baseline age on your own :(
#            ranef='(1|src_subject_id)', ### no change
#            data=data_for_combat)

## this tries to implement Ellerys changes
simdata_combat <- longCombat(idvar='src_subject_id', 
                             timevar='interview_age',
                             batchvar='site_id_l', 
                             features=list_of_ct_regions, 
                             formula='demo_sex_v2 + baseline_age',
                             ranef='(1|src_subject_id)',
                             data=data_for_combat)


data_harmonized <- simdata_combat$data_combat

# save combat feature names
featurenames.combat <- names(data_harmonized)[4:74]
# merge with original dataframe
View(data_harmonized)
View(complete_data)
## find the data that you want and dont need and adjust the code below accordingly
## this data has rows that are the duplicates check if that matters or
complete_data_plus_combat <- merge(complete_data, data_harmonized, by=c('src_subject_id', "interview_age"))


# testing for batch effects after combat
addTestTableCombat <- addTest(idvar='src_subject_id', 
                              batchvar='site_id_l', 
                              features=featurenames.combat, 
                              formula='demo_sex_v2 + baseline_age',
                              ranef='(1|src_subject_id)',
                              data=complete_data_plus_combat)

boxplot_after_combat <- batchBoxplot(idvar='src_subject_id', 
                                     batchvar='site_id_l.x', 
                                     feature='smri_thick_cdk_locclh.combat', 
                                     formula='demo_sex_v2 + baseline_age',
                                     ranef='(1|src_subject_id)',
                                     data=complete_data_plus_combat,
                                     main = "Right plot")

# boxplot_comparison <- plot_grid(boxplot_before_combat, boxplot_after_combat)
# 
# boxplot_before_combat + boxplot_after_combat

par(mfrow = c(1, 2))
boxplot_before_combat <- batchBoxplot(idvar='src_subject_id', 
                                      batchvar='site_id_l', 
                                      feature='smri_thick_cdk_locclh', 
                                      formula='demo_sex_v2 + baseline_age',
                                      ranef='(1|src_subject_id)',
                                      data=complete_data,
                                      main = "Left plot")

boxplot_after_combat <- batchBoxplot(idvar='src_subject_id', 
                                     batchvar='site_id_l.x', 
                                     feature='smri_thick_cdk_locclh.combat', 
                                     formula='demo_sex_v2 + baseline_age',
                                     ranef='(1|src_subject_id)',
                                     data=complete_data_plus_combat,
                                     main = "Right plot")

