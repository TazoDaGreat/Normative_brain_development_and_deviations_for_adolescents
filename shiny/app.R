#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(dplyr)
library(readr)
library(gamlss)
library(ggplot2)
library(leaflet)
library(shiny)
library(sf)
library(shiny)
library(bslib)
library(palmerpenguins)
library(tidyr)
#library(shinydashboard)
#library(dashboardthemes)


# Full-size version of the image




#########

# Define UI for application that draws a histogram
ui <- page_navbar(
  title = "Adolescent Brain Development",
  # helps deal with drop down being cut off
  
  header = tags$head(tags$style(HTML("
  /* Ensure overflow content (like dropdowns or modals) isn't clipped */
  .bslib-card, .tab-content, .tab-pane, .card-body {
    overflow: visible !important;
  }

  /* Make all images responsive */
  img {
    max-width: 100%;
    height: auto;
    display: block;
    margin: 0 auto;
  }

  /* Default modal image scaling (for large screens) */
  .modal-body img {
    max-width: 100%;
    height: auto;
    display: block;
    margin: 0 auto;
  }

  /* Limit modal width on desktop and make responsive */
  .modal-dialog {
    max-width: 90vw;
    width: auto !important;
  }

  .modal-content {
    overflow: hidden;
  }

  .modal-body {
    max-height: 90vh;
    overflow: auto;
    padding: 10px;
  }

  /* Fullscreen modal behavior on mobile */
  @media (max-width: 768px) {
    .modal-dialog {
      width: 100% !important;
      height: 100% !important;
      margin: 0 !important;
    }

    .modal-content {
      height: 100% !important;
      border-radius: 0 !important;
    }

    .modal-body {
      padding: 0 !important;
      height: 100% !important;
      overflow: hidden;
      display: flex;
      justify-content: center;
      align-items: center;
    }

    .modal-body img {
      max-width: 100%;
      max-height: 100%;
      width: auto;
      height: auto;
    }
  }
"))),
  
  
  nav_panel("Home", id = "tab_1",
            layout_columns(
          
              card(tags$h3(tags$b("Welcome to our Interactive App!"), style = "text-align: center; margin-bottom: 10px;"),
                   tags$p(
                  "This project leverages data from the Adolescent Brain Cognitive Development (ABCD) Study, one of the largest ongoing
                   longitudinal study of adolescent neurodevelopment, to build normative brain development charts using MRI data from 
                  baseline, 2-year, and 4-year follow-up visits. We used Generalized Additive Models for Location, Scale, and Shape (GAMLSS) 
                  to model cortical thickness and surface area trajectories, stratified by sex. While 
                  these normative models aim to provide individualized neurodevelopmental reference points, we observed consistently higher 
                  residual errors in Black, American Indian/Alaska Native, and Native Hawaiian/Pacific Islander youth compared to the
                  White subgroup, even when race was exlcuded in model fitting. This suggests that current normative models may exhibit racial bias. We further examined the downstream
                  effects of this bias on psychopathology prediction, highlighting the need for more equitable modeling approaches in clinical research and practice"
                  , style = "font-size: 13px; margin-bottom: 10px;"),
                  tags$p(
                    "This app provides an interactive overview of our project and methods. You can explore key aspects of our data, models, and results across different combinations of race, sex, visit time points, and brain measures.",
                    style = "font-size: 13px; margin-bottom: 10px;"
                  ),
                  tags$p(
                    tags$b("Overview of Tabs:"), style = "font-size: 13px; margin-bottom: 5px;"
                  ),
                  tags$ul(
                    tags$li(tags$b("Cortical Thickness:"), " Explore our GAMLSS-based normative models for cortical thickness across all brain regions.", style = "font-size: 12px;"),
                    tags$li(tags$b("Cortical Surface Area:"), " Explore our GAMLSS-based normative models for cortical surface area across all brain regions.", style = "font-size: 12px;"),
                    tags$li(tags$b("Prediction Error:"), " Compare model prediction errors across different combinations of race, sex, and visit time.", style = "font-size: 12px;"),
                    tags$li(tags$b("Relative Difference:"), " Examine each race group's error deviation relative to the White reference group.", style = "font-size: 12px;"),
                    tags$li(tags$b("Mental Health Models:"), " Review our Year 4 psychopathology prediction models and explore error patterns across race groups.", style = "font-size: 12px;")
                  )
                  ),
              card(card_header(tags$h4("Data", style = "text-align: center; margin: 0;")),
                   tags$div(
                     tags$span("ABCD MRI Imaging data", style = "font-weight: bold; font-size: 12px; margin: 0;"),
                     style = "margin: 0; padding: 0; line-height: 0.1;"
                   ),
              
                tags$ul(style = "margin: 0; padding: 0; font-size: 12px;",
                        # first set of bullet points #tags$li("", style = "font-size: 12px; margin: 0;")
                        tags$li("MRI data from the Adolescent Brain Cognitive Development (ABCD) Study", style = " margin: 0; font-size: 12px;"),
                        tags$ul(
                          tags$li("11,000+ adolescents in the study",style = "margin: 0; font-size: 12px;"),
                          tags$li("Ages 9-10 at Baseline",style = "margin: 0; font-size: 12px;"),
                          tags$li("Repeated MRI scans of individuals across time",style = "margin: 0; font-size: 12px;"),
                          tags$li("Measures of cortical thickness and surface area at baseline, year 2, and year 4 follow-ups",style = "margin: 0; font-size: 12px;")
                          ),
                ),
                tags$div(
                  tags$span("Child Behavior Checklist Score", style = "font-weight: bold; font-size: 12px; margin: 0;"),
                  style = "margin: 0; padding: 0; line-height: 0.1;"
                ),
                tags$ul(style = "margin: 0; padding: 0; font-size: 12px;",
                        tags$li("The Child Behavior Checklist(CBCL) a measure of behavioral and emotional problems in children. For our study, we use", style = " margin: 0; font-size: 12px;"),
                        tags$ul(
                        tags$li("Internalizing Summary Score", style = "font-size: 12px; margin: 0;"), # this is the style i want
                        tags$ul(
                          tags$li("Summary of anxiety, depression, and somatic complaints",style = "margin: 0; font-size: 12px;"),
                        ),
                        tags$li("Externalizing Summary Score", style = "font-size: 12px; margin: 0;"),
                        tags$ul(
                          tags$li("Summary of social problems, thought problems, and attention problems, rule-breaking behaviour",style = "margin: 0; font-size: 12px;")
                        ),
                        tags$li("Depressive Problems Score", style = "font-size: 12px; margin: 0; "),)
                ),
                
                imageOutput("image", width = "100%"),
            
              #style = "height: 600px;" # controls height for card()
              ),
              
              
              
              card(card_header(tags$h4("Modeling", style = "text-align: center; margin: 0;")),
                tags$ul(style = "margin: 0; padding: 0; font-size: 12px;",
                           tags$li(
                             "Models fitted with the Generalized Additive Models for Location, Scale, and Shape (GAMLSS) R package.", style = "font-size: 12px; margin: 0;"),
                           tags$ul(  
                             tags$li("Fitted cortical thickness and cortical surface area for each sex and brain region (284 total models)", style = "font-size: 12px; margin: 0; "),
                             tags$li("Harmonized MRI data from scanner-related variance across sites using the Longitudinal ComBat R package", style = "font-size: 12px; margin: 0; "),
                             tags$li("Note that race was not included in the model fitting process", style = "font-size: 12px; margin: 0; "),
                           ),),
                   HTML(
                     '<p style="font-size: 12px; margin: 0;">
              For a r<sup>th</sup> region and s<sup>th</sup> sex, models for Cortical Thickness (CT) and Cortical Surface Area (CSA) were fit as following:
                </p>'
                   ),
                   layout_columns(col_widths = c(5, 5),
                                  imageOutput("image_1",width = "100%", height = "auto"),
                                  imageOutput("image_2",width = "100%", height = "auto")
                   ),
                tags$p(
                  "where", style = "margin: 0; padding: 0; font-size: 12px;"
                ),
                tags$ul(style = "margin: 0; padding: 0; font-size: 12px;",
                        tags$ul(
                          tags$li("α terms correspond to fixed effects of the intercept", style = "font-size: 12px; margin: 0;"), # this is the style i want
                          tags$li("β terms correspond to fixed effects of age", style = "font-size: 12px; margin: 0;"),
                          tags$li("γ terms correspond to subject-level random effects", style = "font-size: 12px; margin: 0; "),
                          tags$li("pb and cs correspond to smooth functions to model nonlinear relationships", style = "font-size: 12px; margin: 0; "),
                          tags$li("p corresponds to an optimal power transformation of age using GAMLSS functions", style = "font-size: 12px; margin: 0; "),
                          )
                ),
                )
            )
  ),
  
  
  
  #############
  nav_panel("Cortical Thickness", id = "tab_2",
            layout_columns(col_widths = c(12, 6, 6), 
                           card( title ="sidebar = sidebar(renderTex",
                                                         selectInput("brain_region","Select area of brain to model cortical thickness:",
                                                                     c(
                                                                       "Left Hemisphere Banks of Superior Temporal Sulcus",
                                                                       "Left Hemisphere Caudal Anterior Cingulate",
                                                                       "Left Hemisphere Caudal Middle Frontal",
                                                                       "Left Hemisphere Cuneus",
                                                                       "Left Hemisphere Entorhinal",
                                                                       "Left Hemisphere Fusiform",
                                                                       "Left Hemisphere Inferior Parietal",
                                                                       "Left Hemisphere Inferior Temporal",
                                                                       "Left Hemisphere Isthmus Cingulate",
                                                                       "Left Hemisphere Lateral Occipital",
                                                                       "Left Hemisphere Lateral Orbitofrontal",
                                                                       "Left Hemisphere Lingual",
                                                                       "Left Hemisphere Medial Orbitofrontal",
                                                                       "Left Hemisphere Middle Temporal",
                                                                       "Left Hemisphere Parahippocampal",
                                                                       "Left Hemisphere Paracentral",
                                                                       "Left Hemisphere Pars Opercularis",
                                                                       "Left Hemisphere Pars Orbitalis",
                                                                       "Left Hemisphere Pars Triangularis",
                                                                       "Left Hemisphere Pericalcarine",
                                                                       "Left Hemisphere Postcentral",
                                                                       "Left Hemisphere Posterior Cingulate",
                                                                       "Left Hemisphere Precentral",
                                                                       "Left Hemisphere Precuneus",
                                                                       "Left Hemisphere Rostral Anterior Cingulate",
                                                                       "Left Hemisphere Rostral Middle Frontal",
                                                                       "Left Hemisphere Superior Frontal",
                                                                       "Left Hemisphere Superior Parietal",
                                                                       "Left Hemisphere Superior Temporal",
                                                                       "Left Hemisphere Supramarginal",
                                                                       "Left Hemisphere Frontal Pole",
                                                                       "Left Hemisphere Temporal Pole",
                                                                       "Left Hemisphere Transverse Temporal",
                                                                       "Left Hemisphere Insula",
                                                                       "Right Hemisphere Banks of Superior Temporal Sulcus",
                                                                       "Right Hemisphere Caudal Anterior Cingulate",
                                                                       "Right Hemisphere Caudal Middle Frontal",
                                                                       "Right Hemisphere Cuneus",
                                                                       "Right Hemisphere Entorhinal",
                                                                       "Right Hemisphere Fusiform",
                                                                       "Right Hemisphere Inferior Parietal",
                                                                       "Right Hemisphere Inferior Temporal",
                                                                       "Right Hemisphere Isthmus Cingulate",
                                                                       "Right Hemisphere Lateral Occipital",
                                                                       "Right Hemisphere Lateral Orbitofrontal",
                                                                       "Right Hemisphere Lingual",
                                                                       "Right Hemisphere Medial Orbitofrontal",
                                                                       "Right Hemisphere Middle Temporal",
                                                                       "Right Hemisphere Parahippocampal",
                                                                       "Right Hemisphere Paracentral",
                                                                       "Right Hemisphere Pars Opercularis",
                                                                       "Right Hemisphere Pars Orbitalis",
                                                                       "Right Hemisphere Pars Triangularis",
                                                                       "Right Hemisphere Pericalcarine",
                                                                       "Right Hemisphere Postcentral",
                                                                       "Right Hemisphere Posterior Cingulate",
                                                                       "Right Hemisphere Precentral",
                                                                       "Right Hemisphere Precuneus",
                                                                       "Right Hemisphere Rostral Anterior Cingulate",
                                                                       "Right Hemisphere Rostral Middle Frontal",
                                                                       "Right Hemisphere Superior Frontal",
                                                                       "Right Hemisphere Superior Parietal",
                                                                       "Right Hemisphere Superior Temporal",
                                                                       "Right Hemisphere Supramarginal",
                                                                       "Right Hemisphere Frontal Pole",
                                                                       "Right Hemisphere Temporal Pole",
                                                                       "Right Hemisphere Transverse Temporal",
                                                                       "Right Hemisphere Insula",
                                                                       "Left Hemisphere",
                                                                       "Right Hemisphere",
                                                                       "Whole Brain"
                                                                     )
                                                           , selected ="Left Hemisphere Banks of Superior Temporal Sulcus", width = "100%")
            )
            ########## select race and 
            ,card(card_header(tags$h4("Males", style = "font-weight: bold; text-align: center; font-size: 16px; margin: 0; ")), title ="plot 1",imageOutput("maleImage",width = "100%", height = "400px",inline = TRUE),actionButton("expand_btn_1", "Expand Image")),

            #########
            card(card_header(tags$h4("Females", style = "font-weight: bold; text-align: center; font-size: 16px; margin: 0;")),title ="plot 2",imageOutput("femaleImage",width = "100%", height = "400px",inline = TRUE),actionButton("expand_btn_2", "Expand Image"))
            )
            ),
  

  nav_panel("Cortical Surface Area",
            id = "tab_3",
            layout_columns(col_widths = c(12, 6, 6),
                           card(title ="sidebar = sidebar(renderTex",
                                                         selectInput("type2", "Select area of brain to model cortical thickness:",
                                                                      c(
                                                                       "Left Hemisphere Banks of Superior Temporal Sulcus",
                                                                       "Left Hemisphere Caudal Anterior Cingulate",
                                                                       "Left Hemisphere Caudal Middle Frontal",
                                                                       "Left Hemisphere Cuneus",
                                                                       "Left Hemisphere Entorhinal",
                                                                       "Left Hemisphere Fusiform",
                                                                       "Left Hemisphere Inferior Parietal",
                                                                       "Left Hemisphere Inferior Temporal",
                                                                       "Left Hemisphere Isthmus Cingulate",
                                                                       "Left Hemisphere Lateral Occipital",
                                                                       "Left Hemisphere Lateral Orbitofrontal",
                                                                       "Left Hemisphere Lingual",
                                                                       "Left Hemisphere Medial Orbitofrontal",
                                                                       "Left Hemisphere Middle Temporal",
                                                                       "Left Hemisphere Parahippocampal",
                                                                       "Left Hemisphere Paracentral",
                                                                       "Left Hemisphere Pars Opercularis",
                                                                       "Left Hemisphere Pars Orbitalis",
                                                                       "Left Hemisphere Pars Triangularis",
                                                                       "Left Hemisphere Pericalcarine",
                                                                       "Left Hemisphere Postcentral",
                                                                       "Left Hemisphere Posterior Cingulate",
                                                                       "Left Hemisphere Precentral",
                                                                       "Left Hemisphere Precuneus",
                                                                       "Left Hemisphere Rostral Anterior Cingulate",
                                                                       "Left Hemisphere Rostral Middle Frontal",
                                                                       "Left Hemisphere Superior Frontal",
                                                                       "Left Hemisphere Superior Parietal",
                                                                       "Left Hemisphere Superior Temporal",
                                                                       "Left Hemisphere Supramarginal",
                                                                       "Left Hemisphere Frontal Pole",
                                                                       "Left Hemisphere Temporal Pole",
                                                                       "Left Hemisphere Transverse Temporal",
                                                                       "Left Hemisphere Insula",
                                                                       "Right Hemisphere Banks of Superior Temporal Sulcus",
                                                                       "Right Hemisphere Caudal Anterior Cingulate",
                                                                       "Right Hemisphere Caudal Middle Frontal",
                                                                       "Right Hemisphere Cuneus",
                                                                       "Right Hemisphere Entorhinal",
                                                                       "Right Hemisphere Fusiform",
                                                                       "Right Hemisphere Inferior Parietal",
                                                                       "Right Hemisphere Inferior Temporal",
                                                                       "Right Hemisphere Isthmus Cingulate",
                                                                       "Right Hemisphere Lateral Occipital",
                                                                       "Right Hemisphere Lateral Orbitofrontal",
                                                                       "Right Hemisphere Lingual",
                                                                       "Right Hemisphere Medial Orbitofrontal",
                                                                       "Right Hemisphere Middle Temporal",
                                                                       "Right Hemisphere Parahippocampal",
                                                                       "Right Hemisphere Paracentral",
                                                                       "Right Hemisphere Pars Opercularis",
                                                                       "Right Hemisphere Pars Orbitalis",
                                                                       "Right Hemisphere Pars Triangularis",
                                                                       "Right Hemisphere Pericalcarine",
                                                                       "Right Hemisphere Postcentral",
                                                                       "Right Hemisphere Posterior Cingulate",
                                                                       "Right Hemisphere Precentral",
                                                                       "Right Hemisphere Precuneus",
                                                                       "Right Hemisphere Rostral Anterior Cingulate",
                                                                       "Right Hemisphere Rostral Middle Frontal",
                                                                       "Right Hemisphere Superior Frontal",
                                                                       "Right Hemisphere Superior Parietal",
                                                                       "Right Hemisphere Superior Temporal",
                                                                       "Right Hemisphere Supramarginal",
                                                                       "Right Hemisphere Frontal Pole",
                                                                       "Right Hemisphere Temporal Pole",
                                                                       "Right Hemisphere Transverse Temporal",
                                                                       "Right Hemisphere Insula",
                                                                       "Left Hemisphere",
                                                                       "Right Hemisphere",
                                                                       "Whole Brain"
                                                                     )
                                                         , selected ="Left Hemisphere Banks of Superior Temporal Sulcus", width = "100%" )
            )
            ##########
            ,card(card_header(tags$h4("Males", style = "font-weight: bold; text-align: center; font-size: 16px; margin: 0;")),title ="plot 1",imageOutput("maleImage_area",width = "45%", height = "400px",inline = TRUE), actionButton("expand_btn_3", "Expand Image")),
            
            #########
            card(card_header(tags$h4("Females", style = "font-weight: bold; text-align: center; font-size: 16px; margin: 0;")),title ="plot 1",imageOutput("femaleImage_area",width = "45%", height = "400px",inline = TRUE), actionButton("expand_btn_4", "Expand Image"))
            ),
  ),

  nav_panel("Prediction Error",
            id = "tab_4",
            layout_columns(
              col_widths = c(12, 12),
              card(
                style = "height: 250px;",
                title = "sidebar = sidebar(renderTex",
                div(
                  tags$p(
                    tags$strong("Prediction Error"),
                    style = "margin-bottom: 10px; font-size: 28px; "
                  ),
                  tags$p(
                    "Our GAMLSS models account for individual-specific variability, allowing us to generate personalized predicted growth curves for each participant in our sample.
   By calculating the average root mean square error (RMSE) between each individual's actual brain measure and their predicted value, we quantify prediction error by race and brain region.
   This tab lets you explore those errors for cortical thickness and surface area, grouped by race and visit time point.",
                    style = "margin-bottom: 10px; font-size: 16px;"
                  ),
                  tags$p(
                    tags$strong("Key notes for interpreting this page:"),
                    style = "margin-bottom: 5px; font-size: 16px;"
                  ),
                  tags$ul(
                    tags$li("Select between brain measure, sex, race, and visit time point.", style = "margin: 0; font-size: 14px;"),
                    tags$li("Gradient color scales adjust based on your selections. The same colors in different plots may not represent the same values.", style = "margin: 0; font-size: 14px;"),
                    tags$li("Selecting 'All' includes all race groups. To focus on specific groups, remove 'All' and choose races individually.", style = "margin: 0; font-size: 14px;"),
                    tags$li("'Overall' represents the average error across all available visit time points.", style = "margin: 0; font-size: 14px;")
                  )
                ),
                layout_columns(
                  col_widths = c(2, 1, 6, 2),
                  
                  selectInput(
                    inputId = "errors_measure",
                    label = "Select Measure",
                    choices = c("Cortical Thickness", "Cortical Surface Area"),
                    selected = "Cortical Thickness"
                  ),
                  
                  selectInput(
                    inputId = "errors_sex",
                    label = "Select Sex",
                    choices = c("Male", "Female"),
                    selected = "Male"
                  ),
                  
                  selectInput(
                    inputId = "errors_races",
                    label = "Select Race(s)",
                    choices = c("All", "White", "Black", "Hispanic", "Asian", "American Indian / Alaska Native (AIAN)", "Native Hawaiian / Pacific Islander (NHPI)", "Other"),
                    selected = "All",
                    multiple = TRUE
                  ),
                  
                  selectInput(
                    inputId = "errors_timepoint",
                    label = "Select time point",
                    choices = c("Overall", "Baseline", "Year 2", "Year 4"),
                    selected = "Overall"
                  )
                )
              )
              ##########
              , card(
                card_header(tags$h4("", style = "text-align: center;")),
                title = "plot 1",
                style = "height: 5000px;",
                div(
                  style = "display: flex; justify-content: center; align-items: center;",
                  imageOutput("errors", inline = TRUE)
                )
              ),
            ),
  ),
  
  nav_panel("Relative Difference",
            id = "tab_5",
            layout_columns(
              col_widths = c(12, 12),
              card(
                style = "height: 250px;",
                title = "sidebar = sidebar(renderTex",
                div(
                  tags$p(
                    tags$strong("Relative Difference"),
                    style = "margin-bottom: 10px; font-size: 28px; "
                  ),
                  tags$p(
                    "To better compare error differences across race groups, we compute a relative deviation score using the White subgroup as the reference.
   In the plots, darker red regions indicate groups with higher average error relative to the White subgroup, while darker blue regions indicate lower average error.
   These color gradients reflect the percent difference in average error compared to the White group."
                  ),
                  tags$p(
                    tags$strong("Key notes for interpreting this page:"),
                    style = "margin-bottom: 10px; font-size: 16px;"
                  ),
                  tags$ul(
                    tags$li("Select between brain measure, sex, race, and visit time point.", style = "margin: 0; font-size: 14px;"),
                    tags$li("Gradient color scales adjust based on your selections. The same colors in different plots may not represent the same values.", style = "margin: 0; font-size: 14px;"),
                    tags$li("Selecting 'All' includes all race groups. To focus on specific groups, remove 'All' and choose races individually.", style = "margin: 0; font-size: 14px;"),
                    tags$li("'Overall' represents the average error across all available visit time points.", style = "margin: 0; font-size: 14px;")
                  )
                ),
                layout_columns(
                  col_widths = c(2, 1, 6, 2),
                  
                  selectInput(
                    inputId = "difference_measure",
                    label = "Select Measure",
                    choices = c("Cortical Thickness", "Cortical Surface Area"),
                    selected = "Cortical Thickness"
                  ),
                  
                  selectInput(
                    inputId = "difference_sex",
                    label = "Select Sex",
                    choices = c("Male", "Female"),
                    selected = "Male"
                  ),
                  
                  selectInput(
                    inputId = "difference_races",
                    label = "Select Race(s)",
                    choices = c("All", "Black", "Hispanic", "Asian", "American Indian / Alaska Native (AIAN)", "Native Hawaiian / Pacific Islander (NHPI)", "Other"),
                    selected = "All",
                    multiple = TRUE
                  ),
                  
                  selectInput(
                    inputId = "difference_timepoint",
                    label = "Select time point",
                    choices = c("Overall", "Baseline", "Year 2", "Year 4"),
                    selected = "Overall"
                  )
                )
              )
              ##########
              , card(
                card_header(tags$h4("", style = "text-align: center;")),
                title = "plot 1",
                style = "height: 5000px;",
                div(
                  style = "display: flex; justify-content: center; align-items: center;",
                  imageOutput("difference", inline = TRUE)
                )
              ),
            ),
  ),
  
  nav_panel("Mental Health Prediction",
            id = "tab_6",
            layout_columns(
              col_widths = c(12, 12),
              card(
                style = "height: 250px;",
                title = "sidebar = sidebar(renderTex",
                div(
                  tags$p(
                    "Our GAMLSS models account for individual-specific variablility, meaning we can obtain predicted growth curves specific for each individual in our sample.
          Taking the average root mean square error (RMSE) from each individual's actual measure and their predicted meassure, we can see average errors by race and region.
          This tab allows you to examine cortical thickness and surface area errors grouped by race and visit time point.
          ",style = "margin-bottom: 5px; font-size: 14px;"),
                  tags$p(
                    "Things to note for this page:", style = "margin-bottom: 5px; font-size: 14px;"),
                  tags$li("Select between what brain measure, sex, race, and visit time point", style = " margin: 0; font-size: 12px;"),
                  tags$li("The gradient labels change depending on the selection. The same color on one image may not correspond with the same value after changing selection", style = " margin: 0; font-size: 12px;"),
                  tags$li("Selecting 'All' selects all races. Remove 'All' to individually select races", style = " margin: 0; font-size: 12px;"),
                  tags$li("Overall is an average of all individual time point errors", style = " margin: 0; font-size: 12px;"),
                ),
                layout_columns(
                  col_widths = c(2, 2, 2, 2, 2, 2),
                  
                  selectInput(
                    inputId = "cbcl_measure",
                    label = "Select Measure",
                    choices = c("Cortical Thickness", "Cortical Surface Area"),
                    selected = "Cortical Thickness"
                  ),
                  
                  selectInput(
                    inputId = "cbcl_sex",
                    label = "Select Sex",
                    choices = c("Male", "Female"),
                    selected = "Male"
                  ),
                  
                  selectInput(
                    inputId = "cbcl_races",
                    label = "Select Race(s)",
                    choices = c("All", "Black", "Hispanic", "Asian", "American Indian / Alaska Native (AIAN)", "Native Hawaiian / Pacific Islander (NHPI)", "Other"),
                    selected = "All",
                    multiple = TRUE
                  ),
                  
                  selectInput(
                    inputId = "cbcl_mh",
                    label = "Select Mental Health",
                    choices = c("Internalizing Score", "Externalizing Score", "Depression Score"),
                    selected = "Internalizing Score"
                  ),
                  
                  selectInput(
                    inputId = "cbcl_covs",
                    label = "Select Covariate Model",
                    choices = c("No Covariates Model", "Race Included Model", "Race, Income, and Education Included Model"),
                    selected = "No Covariates Model"
                  ),
                  
                  selectInput(
                    inputId = "cbcl_preds",
                    label = "Select Modle Predictors",
                    choices = c("Centile Trajectory Scores", "Raw Measure Trajectory Scores", "Z-Score Trajectory Scores"),
                    selected = "Centile Trajectory Scores"
                  )
                )
              )
              ##########
              , card(
                card_header(tags$h4("", style = "text-align: center;")),
                title = "plot 1",
                style = "height: 5000px;",
                div(
                  style = "display: flex; justify-content: center; align-items: center;",
                  imageOutput("cbcl", inline = TRUE)
                )
              ),
            ),
  ),
  
  nav_panel("About Us",
            value = "tab_6",
            layout_sidebar(
              sidebar = sidebar(
                selectInput("type4", "What event do you want to visualize", choices = c("kaka"), selected = "kaka")
              ),
              leafletOutput("aboutMap")
            )
  )
  
  
)
########################## ################################################
######################### #################################################
######################## ##################################################
####################### ###################################################
###################### ####################################################
# Define server logic required to draw a histogram ########################
server <- function(session, input, output) {
  
  output$image_1 <- renderImage( 
    { 
      list(src = "model_function_1.png") 
    }, 
    deleteFile = FALSE 
  )
  
  output$image_2 <- renderImage( 
    { 
      list(src = "model_function_2.png") 
    }, 
    deleteFile = FALSE 
  ) 
  
  output$image <- renderImage( 
    { 
      list(src = "demographics.png") 
    }, 
    deleteFile = FALSE 
  ) 
  
  output$distPlot_female <- renderTable(df_one,striped = TRUE)

  output$maleImage <- renderImage({
    region <- input$brain_region
    region <- gsub(" ", "_", region)
    filename <- normalizePath(file.path('png_of_regions',
                                        paste0(region, '_male_All.png')))
    
    list(src = filename,
         alt = paste(region, "male"))
  }, deleteFile = FALSE)
  
  ####### expand button logic - 1
  observeEvent(input$expand_btn_1, {
    showModal(modalDialog(
      title = "Expanded Image",
      imageOutput("full_image", width = "100%"),
      size = "xl", easyClose = TRUE
    ))
  })
  
  # Full-size version of the image
  output$full_image <- renderImage({
    list(src = regionImage() , contentType = "image/png")
  }, deleteFile = FALSE)
  #######
  
  output$femaleImage <- renderImage({
    region <- input$brain_region
    region <- gsub(" ", "_", region)
    filename <- normalizePath(file.path('png_of_regions',
                                        paste0(region, '_female_All.png')))
    
    list(src = filename,
         alt = paste(region, "female"))
  }, deleteFile = FALSE)
  
  ####### expand button logic - 2
  observeEvent(input$expand_btn_2, {
    showModal(modalDialog(
      title = "Expanded Image",
      imageOutput("full_image", width = "100%"),
      size = "xl", easyClose = TRUE
    ))
  })
  
  # Full-size version of the image
  output$full_image <- renderImage({
    list(src = regionImage() , contentType = "image/png")
  }, deleteFile = FALSE)
  #######
  
  #### area
  
  regionImage <- reactive({
    req(input$type2)
    normalizePath(file.path("png_of_regions", paste0(input$type2, "_male_All_area.png")))
  })
  #########################################################
  #Male area plots
  #########################################################
  output$maleImage_area <- renderImage({
    region <- input$type2
    region <- gsub(" ", "_", region)
    filename <- normalizePath(file.path('png_of_regions',
                                        paste0(region, '_male_All_area.png')))
    ###
    #print("this is the file path for males")
    #print(filename)
    #print(region)
    ###
    list(src = filename,
         alt = paste(region, "male"))
  }, deleteFile = FALSE)
  
  
  ####### expand button logic - 3
  observeEvent(input$expand_btn_3, {
    showModal(modalDialog(
      title = "Expanded Image",
      imageOutput("full_image", width = "100%"),
      size = "xl", easyClose = TRUE
    ))
  })
  
  # Full-size version of the image
  output$full_image <- renderImage({
    list(src = regionImage() , contentType = "image/png")
  }, deleteFile = FALSE)
   #######
  
  #########################################################
  #Female area plots
  #########################################################
  
  output$femaleImage_area <- renderImage({
    region <- input$type2
    region <- gsub(" ", "_", region)
    filename <- normalizePath(file.path('png_of_regions',
                                        paste0(region, '_female_All_area.png')))
    ####
    #print("this is the file path for females")
    #print(filename)
    ###
    
    list(src = filename,
         alt = paste(region, "female"))
  }, deleteFile = FALSE)

  ####### expand button logic - 4
  observeEvent(input$expand_btn_4, {
    showModal(modalDialog(
      title = "Expanded Image",
      imageOutput("full_image", width = "100%"),
      size = "xl", easyClose = TRUE
    ))
  })
  
  # Full-size version of the image
  output$full_image <- renderImage({
    list(src = regionImage() , contentType = "image/png")
  }, deleteFile = FALSE)
  #######
  
  ####### Jefff stuff
  
  output$errors <- renderImage({
    races_input <- input$errors_races
    sex <- tolower(input$errors_sex) 
    timepoint <- input$errors_timepoint
    measure <- input$errors_measure
    
    visit_suffix_map <- c(
      "Overall" = "/",
      "Baseline" = "_baseline/",
      "Year 2" = "_year2/",
      "Year 4" = "_year4/"
    )
    
    measure_map <- c(
      "Cortical Thickness" = "thickness",
      "Cortical Surface Area" = "area"
    )
    
    race_map <- c(
      "White" = "White",
      "Black" = "Black",
      "Asian" = "Asian",
      "Hispanic" = "Hispanic",
      "Native Hawaiian / Pacific Islander (NHPI)" = "NHPI",
      "American Indian / Alaska Native (AIAN)" = "AIAN",
      "Other" = "Other"
    )
    ordered_race_codes <- c("White", "Black", "Asian", "Hispanic", "NHPI", "AIAN", "Other")
    
    if ("All" %in% races_input) {
      selected_races <- ordered_race_codes
    } else {
      mapped_races <- race_map[races_input]
      mapped_races <- mapped_races[!is.na(mapped_races)]
      selected_races <- ordered_race_codes[ordered_race_codes %in% mapped_races]
    }
    race_filename <- paste(selected_races, collapse = "_")
    measure_name <- measure_map[[measure]]
    
    folder_suffix <- visit_suffix_map[[timepoint]]
    folder <- paste0(sex, "_", measure_name, "_error", folder_suffix)
    
    image_path <- paste0("~/Library/CloudStorage/Box-Box/shiny_iHate/jeff_stuff/", folder, race_filename, ".png")
    print(image_path)
    list(
      src = image_path,
      contentType = "image/png"
    )
  }, deleteFile = FALSE)
  
  output$difference <- renderImage({
    races_input <- input$difference_races
    sex <- tolower(input$difference_sex) 
    timepoint <- input$difference_timepoint
    measure <- input$difference_measure
    
    visit_suffix_map <- c(
      "Overall" = "/",
      "Baseline" = "_baseline/",
      "Year 2" = "_year2/",
      "Year 4" = "_year4/"
    )
    
    measure_map <- c(
      "Cortical Thickness" = "thickness",
      "Cortical Surface Area" = "area"
    )
    
    race_map <- c(
      "White" = "White",
      "Black" = "Black",
      "Asian" = "Asian",
      "Hispanic" = "Hispanic",
      "Native Hawaiian / Pacific Islander (NHPI)" = "NHPI",
      "American Indian / Alaska Native (AIAN)" = "AIAN",
      "Other" = "Other"
    )
    ordered_race_codes <- c("Black", "Asian", "Hispanic", "NHPI", "AIAN", "Other")
    
    if ("All" %in% races_input) {
      selected_races <- ordered_race_codes
    } else {
      mapped_races <- race_map[races_input]
      mapped_races <- mapped_races[!is.na(mapped_races)]
      selected_races <- ordered_race_codes[ordered_race_codes %in% mapped_races]
    }
    race_filename <- paste(selected_races, collapse = "_")
    measure_name <- measure_map[[measure]]
    
    folder_suffix <- visit_suffix_map[[timepoint]]
    folder <- paste0(sex, "_", measure_name, "_difference", folder_suffix)
    
    image_path <- paste0("~/Library/CloudStorage/Box-Box/shiny_iHate/jeff_stuff/", folder, race_filename, ".png")
    print(image_path)
    
    list(
      src = image_path,
      contentType = "image/png"
    )
  }, deleteFile = FALSE)
  
  output$cbcl <- renderImage({
    races_input <- input$cbcl_races
    sex <- tolower(input$cbcl_sex) 
    mh <- input$cbcl_mh
    measure <- input$cbcl_measure
    covs <- input$cbcl_covs
    preds <- input$cbcl_preds
    
    measure_map <- c(
      "Cortical Thickness" = "",
      "Cortical Surface Area" = "area_"
    )
    
    covs_map <- c(
      "No Covariates Model" = "no_race",
      "Race Included Model" = "race",
      "Race, Income, and Education Included Model" = "covs"
    )
    
    preds_map <- c(
      "Centile Trajectory Scores" = "c",
      "Raw Measure Trajectory Scores" = "r",
      "Z-Score Trajectory Scores" = "z"
    )
    
    mh_map <- c(
      "Internalizing Score" = "internal",
      "Externalizing Score" = "external",
      "Depression Score" = "depression"
    )
    
    race_map <- c(
      "Black" = "Black",
      "Asian" = "Asian",
      "Hispanic" = "Hispanic",
      "Native Hawaiian / Pacific Islander (NHPI)" = "NHPI",
      "American Indian / Alaska Native (AIAN)" = "AIAN",
      "Other" = "Other"
    )
    ordered_race_codes <- c("Black", "Asian", "Hispanic", "NHPI", "AIAN", "Other")
    
    if (is.null(races_input) || length(races_input) == 0 || "All" %in% races_input) {
      selected_races <- ordered_race_codes
    } else {
      mapped_races <- race_map[races_input]
      mapped_races <- mapped_races[!is.na(mapped_races)]
      selected_races <- ordered_race_codes[ordered_race_codes %in% mapped_races]
    }
    race_filename <- paste(selected_races, collapse = "_")
    measure_name <- measure_map[[measure]]
    mh_name <- mh_map[[mh]]
    pred_name <- preds_map[[preds]]
    covs_name <- covs_map[[covs]]
    
    folder <- paste0(sex, "_", measure_name, mh_name)
    # change path below to where you have the folder saved to
    image_path <- paste0("~/Library/CloudStorage/Box-Box/shiny_iHate/jeff_stuff/", folder, "/", covs_name, "/", pred_name, "/", race_filename, ".png")
    print(image_path)
    
    list(
      src = image_path,
      contentType = "image/png",
      width = 700,
      height = 700
    )
  }, deleteFile = FALSE)
  
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    query1 <- paste(names(query), query, sep = "=", collapse=", ")
    print(query1)
    if(query1 == "ref"){
      nav_select(id = "tab_6", session = session)
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

