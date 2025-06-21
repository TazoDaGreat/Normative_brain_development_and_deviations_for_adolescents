library(ggseg)
library(tidyverse)

# ====================== highlight brain region on map ==================
make_plot <- function(region){
  some_data_2 <- data.frame(region = region,
                            p = region
                            )
  
  ggseg(.data = some_data_2, 
        atlas=dk, 
        colour="white", 
        linewidth = 0.25, 
        position = position_brain(hemi ~ side),
        mapping=aes(fill= factor(p))) + # p indicates what region I want to highlight
    theme(legend.position = "bottom") +
    labs(fill = "Region")
}

make_plot(c("insula", "precentral", "superior parietal"))

# make_plot(c(get_name(dk_abv_to_names, "parsopcrh") , get_name(dk_abv_to_names, "banksstslh"), get_name(dk_abv_to_names, "ptcatelh"), get_name(glasser_abv_to_names, "trvtmlh"))) # regions with largest observed discrepancies 

make_plot(c("caudal middle frontal", "superior temporal", "rostral middle frontal"))

# ============== create data set to map between real names and abcd abbreviations ==================

abcd_abv <- read_csv("~/Documents/GitHub/Normative_brain_development_and_deviations_for_adolescents/exploration/additional_files/ROI_abbreviations.csv") %>%
  mutate(label = str_replace(label, "-", "_"),
         label = if_else(var_name == "banksstslh", "lh_bankssts", label),
         label = if_else(var_name == "banksstsrh", "rh_bankssts", label))

dk_abv_to_names <- left_join(abcd_abv, dk$data, by ="label") %>%
  distinct(var_name, .keep_all = T) %>%
  dplyr::select(-geometry) %>%
  mutate(region = if_else(var_name == "mean", label, region),
         region = if_else(var_name == "meanlh", label, region),
         region = if_else(var_name == "meanrh", label, region),
         region = str_remove(region, "in mm "))

# write.csv(dk_abv_to_names, file = "~/Documents/GitHub/Normative_brain_development_and_deviations_for_adolescents/exploration/additional_files/dk_abv_to_names.csv") 

# ============== get name from abv and vice verse ============

get_name <- function(dk_abv_to_names, abv){
  (dk_abv_to_names %>% filter(var_name == abv))$region
}

get_abv <- function(dk_abv_to_names, name, left_or_right){
  (dk_abv_to_names %>% 
     filter(hemi == left_or_right) %>%
     filter(region %in% name))$var_name
}

get_abv(dk_abv_to_names, name = c("caudal middle frontal", "superior temporal", "rostral middle frontal"), left = "left")

