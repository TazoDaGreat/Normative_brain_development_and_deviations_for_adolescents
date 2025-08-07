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
  title = "Racial Bias in Normative Modeling",
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
  
  
  
  nav_panel("Mental Health Prediction - Externalizing Score",
            id = "tab_6",
            tags$a(
              class = "btn btn-primary nav-link",
              href = "https://hypnotoad.shinyapps.io/shiny_iHate/",
              "Back to Homepage",
              ),
            fluidRow(
              col_widths = c(12, 12),
              card(
                
                title = "sidebar = sidebar(renderTex",
                div(
                  tags$p(
                    tags$strong("Mental Health Prediciton - Externalizing Score"),
                    style = "margin-bottom: 10px; font-size: 28px; "
                  ),
                  tags$p(
                    "In order to investigate the downstream effects of racial bias in normative modeling,
                    we fit Generalized Linear Models (Poisson family) to predict psychopathology at Year 4
                    and examined how different predictors, covariates, and outcome variables influence
                    these predictions. Here, we predict Externalizing Summary Score. Using the White 
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
                      tags$li("Raw measure – captures change in original MRI imaging measurement", style = "margin: 0; font-size: 14px;")
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
                    inputId = "cbcl_covs",
                    label = "Select Covariate Model",
                    choices = c("No Covariates Model", "Race Included Model", "Race, Income, and Education Included Model"),
                    selected = "No Covariates Model"
                  ),
                  
                  selectInput(
                    inputId = "cbcl_preds",
                    label = "Select Modle Predictors",
                    choices = c("Centile Trajectory Scores", "Raw Measure Trajectory Scores"),
                    selected = "Centile Trajectory Scores"
                  )
                )
              )
              ##########
              , card(
                card_header(tags$h4("", style = "text-align: center;")),
                title = "plot 1",
                div(
                  style = "display: flex; justify-content: center; align-items: center;",
                  imageOutput("cbcl", inline = TRUE)
                )
              ),
            ),
  ),
  
  
)
########################## ################################################
######################### #################################################
######################## ##################################################
####################### ###################################################
###################### ####################################################
# Define server logic required to draw a histogram ########################
server <- function(session, input, output) {
  
  
  output$cbcl <- renderImage({
    races_input <- input$cbcl_races
    sex <- tolower(input$cbcl_sex)
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
    pred_name <- preds_map[[preds]]
    covs_name <- covs_map[[covs]]
    
    folder <- paste0(sex, "_", measure_name, "external")
    # change path below to where you have the folder saved to
    image_path <- paste0(folder, "/", covs_name, "/", pred_name, "/", race_filename, ".png")
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

