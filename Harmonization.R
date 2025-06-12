
#============================== Data ==============================================
library(tidyverse)
full_mri_data <- read_csv("Library/CloudStorage/Box-Box/ABCD Tabulated Data/5.1/core/imaging/mri_y_smr_thk_dsk.csv")


####do not know if I need this chunck of code ##############

# This data set is the one that contains the MRI machine and info
# Should this dataset be named covariates? ASK !!!
# The covarient will have to change based on dataset for the brain measurements that you pick
covariates <- read_csv("Library/CloudStorage/Box-Box/ABCD Tabulated Data/5.1/core/imaging/mri_y_adm_info.csv")

  
mri_brain_data <- full_mri_data %>%
  select(-src_subject_id,-eventname)


# combine both sets of data
data <- full_join(covariates,full_mri_data)


# ================================== plot ==============================================

data %>%         
  ggplot(aes(x = smri_thick_cdk_banksstslh, y = smri_thick_cdk_cdacatelh, color = mri_info_manufacturer)) +
  geom_point() +
  theme_classic()+
  facet_wrap(mri_info_manufacturer)

#=============================== code for longCombat library =======================



# you cant run tidyverse and longCombat at the same time
#detach("package:tidyverse", unload=TRUE)

#install.packages("devtools")
#devtools::install_github("jcbeer/longCombat")

# library(longCombat)
# 
# boxplot_before_combat <- batchBoxplot(idvar='src_subject_id', 
#                                       batchvar='mri_info_manufacturer', 
#                                       feature='smri_thick_cdk_locclh', 
#                                       formula='interview_age + AnyDD_ever_p_y*interview_age',
#                                       ranef='(1|src_subject_id)',
#                                       data=complete_data,
#                                       main = "Left plot")
# 
# batchBoxplot(idvar='src_subject_id', 
#              batchvar='site_id_l', 
#              feature='smri_thick_cdk_tmpolerh', 
#              formula='interview_age + AnyDD_ever_p_y*interview_age',
#              ranef='(1|src_subject_id)',
#              data=complete_data)
# 
# addTestTable <- addTest(idvar='src_subject_id', 
#                         batchvar='mri_info_manufacturer', 
#                         features=list_of_ct_regions, 
#                         formula='interview_age + AnyDD_ever_p_y*interview_age',
#                         ranef='(1|src_subject_id)',
#                         data=complete_data)
# 
# multTestTable <- multTest(idvar='src_subject_id', 
#                           batchvar='mri_info_manufacturer', 
#                           features=list_of_ct_regions, 
#                           formula='interview_age + AnyDD_ever_p_y*interview_age',
#                           ranef='(1|src_subject_id)',
#                           data=complete_data)
# 
# # applying combat
# data_for_combat <- complete_data %>%
#   select(src_subject_id, eventname, mri_info_manufacturer, interview_age, AnyDD_ever_p_y, site_id_l, starts_with("smri"))
# 
# simdata_combat <- longCombat(idvar='src_subject_id', 
#                              timevar='interview_age',
#                              batchvar='mri_info_manufacturer', 
#                              features=list_of_ct_regions, 
#                              formula='interview_age + AnyDD_ever_p_y*interview_age',
#                              ranef='(1|src_subject_id)',
#                              data=data_for_combat)
# 
# data_harmonized <- simdata_combat$data_combat
# # save combat feature names
# featurenames.combat <- names(data_harmonized)[4:74]
# # merge with original dataframe
# complete_data_plus_combat <- merge(complete_data, data_harmonized[,c(1,2,4:74)], by=c('src_subject_id', 'eventname'))
# 
# 
# # testing for batch effects after combat
# addTestTableCombat <- addTest(idvar='src_subject_id', 
#                               batchvar='mri_info_manufacturer', 
#                               features=featurenames.combat, 
#                               formula='interview_age + AnyDD_ever_p_y*interview_age',
#                               ranef='(1|src_subject_id)',
#                               data=complete_data_plus_combat)
# 
# boxplot_after_combat <- batchBoxplot(idvar='src_subject_id', 
#                                      batchvar='mri_info_manufacturer', 
#                                      feature='smri_thick_cdk_locclh.combat', 
#                                      formula='interview_age + AnyDD_ever_p_y*interview_age',
#                                      ranef='(1|src_subject_id)',
#                                      data=complete_data_plus_combat,
#                                      main = "Right plot")
# 
# # boxplot_comparison <- plot_grid(boxplot_before_combat, boxplot_after_combat)
# # 
# # boxplot_before_combat + boxplot_after_combat
# 
# par(mfrow = c(1, 2))
# boxplot_before_combat <- batchBoxplot(idvar='src_subject_id', 
#                                       batchvar='mri_info_manufacturer', 
#                                       feature='smri_thick_cdk_locclh', 
#                                       formula='interview_age + AnyDD_ever_p_y*eventname',
#                                       ranef='(1|src_subject_id)',
#                                       data=complete_data,
#                                       main = "Left plot")
# 
# boxplot_after_combat <- batchBoxplot(idvar='src_subject_id', 
#                                      batchvar='mri_info_manufacturer', 
#                                      feature='smri_thick_cdk_locclh.combat', 
#                                      formula='interview_age + AnyDD_ever_p_y*eventname',
#                                      ranef='(1|src_subject_id)',
#                                      data=complete_data_plus_combat,
#                                      main = "Right plot")
# 
# # ================================= impact of including a covariate ===========================

















