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
                     tags$span("ABCD MRI Imaging Data", style = "font-weight: bold; font-size: 12px; margin: 0;"),
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
            div(
              tags$p(
                tags$strong("Cortical Thickness"),
                style = "margin-bottom: 10px; font-size: 28px; "
              ),
              tags$p(
                "Normative modeling allows for the creation of centile charts,
                indicating how a patient’s cortical thickness measurement compares to
                the reference population. Just like pediatric growth charts for height or weight, the charts below show 
                where an adolescent patient lies in terms of percentiles for their age and sex, allowing for the identification of
                atypical deviations from the typical development."
              ),
              tags$p(
                "On this page, you can:"
              ),
              tags$ul(
                tags$li("Explore the growth charts of cortical thickness on various regions of the brain over age"),
                tags$li("Compare trajectories between male and female cortical thickness development")
              )
            ),
            layout_columns(col_widths = c(12, 6, 6), 
                           card( title ="sidebar = sidebar(renderTex",
                                                         selectInput("brain_region","Select brain region to model cortical thickness:",
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
            div(
              tags$p(
                tags$strong("Cortical Surface Area"),
                style = "margin-bottom: 10px; font-size: 28px; "
              ),
              tags$p(
                "Normative modeling allows for the creation of centile charts,
                indicating how a patient’s cortical surface area measurement compares to
                the reference population. Just like pediatric growth charts for height or weight, the charts below show 
                where an adolescent patient lies in terms of percentiles for their age and sex, allowing for the identification of
                atypical deviations from the typical development."
              ),
              tags$p(
                "On this page, you can:"
              ),
              tags$ul(
                tags$li("Explore the growth charts of cortical surface area on various regions of the brain over age"),
                tags$li("Compare trajectories between male and female cortical surface area development")
              )
            ),
            layout_columns(col_widths = c(12, 6, 6),
                           card(title ="sidebar = sidebar(renderTex",
                                                         selectInput("type2", "Select brain region to model cortical surface area:",
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
              card(
                style = "height: 250px;",
                title = "sidebar = sidebar(renderTex",
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
              card(
                style = "height: 250px;",
                title = "sidebar = sidebar(renderTex",
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
  
  nav_panel("Internalizing Score Prediction",
            id = "tab_6",
            layout_columns(
              col_widths = c(12, 12),
              card(
                style = "height: 250px;",
                title = "sidebar = sidebar(renderTex",
                div(
                  tags$p(
                    tags$strong("Mental Health Prediciton - Internalizing Score"),
                    style = "margin-bottom: 10px; font-size: 28px; "
                  ),
                  tags$p(
                    "In order to investigate the downstream effects of racial bias in normative modeling,
                    we fit Generalized Linear Models (Poisson family) to predict psychopathology at Year 4
                    and examined how different predictors, covariates, and outcome variables influence
                    these predictions. Here, we predict Internalizing Summary Score. Using the White 
                    subgroup as a reference, we compare the relative deviation scores across racial
                    subgroups by brain region. To assess the implications of bias in our normative models,
                    we compared multiple trajectory score predictors to observe how predictive performance
                    and racial bias may change. Furthermore, we compare the impact of including or
                    excluding race and other relevant covariates, allowing us to assess whether modeling
                    decisions alone are sufficient to mitigate racial bias.
                    ",style = "margin-bottom: 5px; font-size: 16px;"),
                  tags$p(
                    tags$strong("Key notes for interpreting this page:"),
                    style = "margin-bottom: 10px; font-size: 16px;"
                  ),
                  tags$ul(
                    tags$li("Select between brain measure, sex, race, model predictors, and model covariates.", style = "margin: 0; font-size: 14px;"),
                    tags$li("Model predictors are trajectory scores, collected from least squares regression estimates of individual change:", style = "margin: 0; font-size: 14px;"),
                    tags$ul(
                      tags$li("Centile score – reflects relative position within the normative distribution (shown to have racial bias)", style = "margin: 0; font-size: 14px;"),
                      tags$li("Raw measure – captures change in original MRI imaging measurement", style = "margin: 0; font-size: 14px;"),
                      tags$li("Z-score – standardized deviation from the population mean", style = "margin: 0; font-size: 14px;")
                    ),
                    tags$li("Model covariates are intended to account for race-related sources of variation. We compare models with:", style = "margin: 0; font-size: 14px;"),
                    tags$ul(
                      tags$li("No covariates", style = "margin: 0; font-size: 14px;"),
                      tags$li("Race as a covariate", style = "margin: 0; font-size: 14px;"),
                      tags$li("Race, household income, and parental education as covariates", style = "margin: 0; font-size: 14px;")
                    ),
                    tags$li("Gradient color scales adjust based on your selections. The same colors in different plots may not represent the same values.", style = "margin: 0; font-size: 14px;"),
                    tags$li("Selecting 'All' includes all race groups. To focus on specific groups, remove 'All' and choose races individually.", style = "margin: 0; font-size: 14px;"),
                  ),
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
    value = "tab_7",
    div(
      tags$p(
        tags$strong("About Us"),
        style = "margin-bottom: 10px; font-size: 28px; "
      )
    ),
    layout_columns(
      card(
        tags$p(
          tags$strong("Jeffrey Choi"),
          style = "margin-bottom: 10px; font-size: 20px; "
        ),
        tags$p(
          "Hello! I’m Jeffrey and I am a rising senior at New York University
          studying data science and math. I’m originally from New Jersey, but
          I’ve lived in a few different places, including Indiana when I was
          younger, so the move from New York to Minneapolis hasn’t felt like too
          much of a stretch. When I’m not working on a coding project or deep
          into data, I love spending time golfing, rock climbing, and ranking
          restaurants I’ve been to on Beli. After undergrad, I plan on pursuing
          a PhD in biostatistics or a related field, with the goal of doing more
          research in the application of statistical methods or AI. ",
          style = "margin-bottom: 5px; font-size: 14px;"
        ),
        tags$p(
          tags$strong("Email: "),
          tags$a(href = "mailto:jsc9862@nyu.edu", "jsc9862@nyu.edu"),
          style = "font-size: 14px;"
        ),
        
        tags$p(
          tags$strong("GitHub: "),
          tags$a(href = "https://github.com/yourusername", "github.com/yourusername", target = "_blank"),
          style = "font-size: 14px;"
        ),
      
      card(
        tags$p(
          tags$strong("Quique"),
          style = "margin-bottom: 10px; font-size: 20px; "
        ),
        tags$p(
          "Info Info Info Info Info Info Info Info Info Info Info Info Info Info Info 
          Info Info Info Info Info Info Info Info Info Info Info Info Info Info Info 
          Info Info Info Info Info Info Info Info Info Info Info Info Info Info Info 
          Info Info Info Info Info Info Info Info Info Info Info Info Info Info Info 
          Info Info Info Info Info Info Info Info Info Info Info Info Info Info Info 
          Info Info Info Info Info Info Info Info Info Info Info Info Info Info Info 
          Info Info Info Info Info Info Info Info Info Info Info Info Info Info Info 
          Info Info Info Info Info Info Info Info Info Info Info Info Info Info Info ",
          style = "margin-bottom: 5px; font-size: 14px;"
        ),
      ),
    )
  ),
  
  
  nav_panel("Acknowledgments",
            value = "tab_8",
            
            layout_columns(
              card(
                tags$p(
                  tags$strong("Limitations"),
                  style = "margin-bottom: 10px; font-size: 20px; "
                ),
                tags$ul(
                  tags$li("The ABCD Study is one of the most demographically representative neuroimaging datasets available", style = "margin-bottom: 5px; font-size: 14px;"),
                  tags$ul(
                    tags$li("Our results highlight the need for developing methods to account for racial bias", style = "margin-bottom: 5px; font-size: 14px;"),
                  ),
                  tags$li("Our version of the ACBD data does not include all Year 4 participants", style = "margin-bottom: 5px; font-size: 14px;"),
                  tags$ul(
                    tags$li("Our sample distribution is not strictly due to study dropout", style = "margin-bottom: 5px; font-size: 14px;"),
                    tags$li("Prior research suggests that study dropout is associated with socioeconomic status", style = "margin-bottom: 5px; font-size: 14px;"),
                    tags$li("Model performance may disproportionately affect underrepresented groups", style = "margin-bottom: 5px; font-size: 14px;"),
                  ),
                ),
                tags$p(
                  tags$strong("Notes"),
                  style = "margin-bottom: 10px; font-size: 20px; "
                ),
                tags$p(
                  "It is important to emphasize that the bias observed in this project is not a result of inherent
                  biological differences between racial groups alone. Race is a social construct, a strict
                  classification based on perceived physical characteristics, and is strongly correlated with other
                  socioeconomic and environmental factors that can also influence adolescent brain development. Without
                  comprehensive representation and genetic ancestry information in neuroimaging data, it becomes
                  challenging to identify a reference group that accurately reflects variation across different
                  demographic groups.",
                  style = "margin-bottom: 5px; font-size: 14px;"
                ),
              ),
              
              card(
                tags$p(
                  tags$strong("Acknowledgments"),
                  style = "margin-bottom: 10px; font-size: 20px; "
                ),
                tags$p(
                  "We would like to thank Mark Fiecas for his mentorship
                    in our project and during the 2025 Equitable Data Science
                    REU. We would also like to thank Kelly, Kirsten, and Ellery
                    in advising us throughout our project, and also Ellery and
                    Andres for teaching the R course over the summer.
                    Finally, we are grateful to the UMN School of Public Health
                    and the NSF for supporting and funding our summer!",
                  style = "margin-bottom: 5px; font-size: 14px;"
                ),
                
                tags$p(
                  tags$strong("References"),
                  style = "margin-bottom: 10px; font-size: 20px; "
                ),
                tags$p(
                  'Beer JC, Tustison NJ, Cook PA, Davatzikos C, Sheline YI, Shinohara RT, Linn KA. (2020) Longitudinal ComBat: A method for harmonizing longitudinal multi-scanner imaging data. NeuroImage. In press. https://doi.org/10.1016/j.neuroimage.2020.117129.',
                  style = "margin-bottom: 2px; font-size: 14px;"
                ),
                tags$p(
                  'Bethlehem, R. A. I., Seidlitz, J., White, S. R., Vogel, J. W., Anderson, K. M., Adamson, C., ... & Schaare, H. L. (2022). Brain charts for the human lifespan. Nature, 604(7906), 525-533.',
                  style = "margin-bottom: 2px; font-size: 14px;"
                ),
                tags$p(
                  'Rutherford, Saige, et al. "To which reference class do you belong? Measuring racial fairness of reference classes with normative modeling." arXiv preprint arXiv:2407.19114 (2024), https://arxiv.org/pdf/2407.19114.',
                  style = "margin-bottom: 2px; font-size: 14px;"
                ),
                tags$p(
                  'Sarah W. Feldstein Ewing, Genevieve F. Dash, Wesley K. Thompson, Chase Reuter, Vanessa G. Diaz, Andrey Anokhin, Linda Chang, Linda B. Cottler, Gayathri J. Dowling, Kimberly LeBlanc, Robert A. Zucker, Susan F. Tapert, Sandra A. Brown, Hugh Garavan, Measuring retention within the adolescent brain cognitive development (ABCD)SM study, Developmental Cognitive Neuroscience, Volume 54, 2022, https://doi.org/10.1016/j.dcn.2022.101081.',
                  style = "margin-bottom: 2px; font-size: 14px;"
                ),
                tags$p(
                  'Vincent Pandolfi, Caroline I. Magyar, Charles A. Dill, An initial psychometric evaluation of the CBCL 6–18 in a sample of youth with autism spectrum disorders, Research in Autism Spectrum Disorders, Volume 6, Issue 1, 2012, https://doi.org/10.1016/j.rasd.2011.03.009.',
                  style = "margin-bottom: 2px; font-size: 14px;"
                ),
                
              ),
        
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

