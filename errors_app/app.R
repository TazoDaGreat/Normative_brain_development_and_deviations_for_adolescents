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
  
  
  nav_panel("Prediction Error",
            id = "tab_6",
            tags$li(
              class = "nav-item",
              style = "list-style-type: none;",
              tags$a(
                class = "btn btn-primary nav-link",
                href = "https://hypnotoad.shinyapps.io/shiny_iHate/",
                "Back to Homepage",
              )
            ),
            fluidRow(
              col_widths = c(12, 12),
              card(
                
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
                    style = "margin-bottom: 10px; font-size: 16px;"),
                  tags$p(
                    tags$strong("Key notes for interpreting this page:"),
                    style = "margin-bottom: 10px; font-size: 16px;"
                  ),tags$ul(
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
                div(
                  style = "display: flex; justify-content: center; align-items: center;",
                  imageOutput("errors", inline = TRUE)
                )
              ),
            ),
  )
  
  
)
########################## ################################################
######################### #################################################
######################## ##################################################
####################### ###################################################
###################### ####################################################
# Define server logic required to draw a histogram ########################
server <- function(session, input, output) {
  
  
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
    
    image_path <- paste0(folder, race_filename, ".png")
    print(image_path)
    list(
      src = image_path,
      contentType = "image/png"
    )
  }, deleteFile = FALSE)
  
}

# Run the application 
shinyApp(ui = ui, server = server)

