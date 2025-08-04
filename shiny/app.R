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

#########

# Define UI for application that draws a histogram
ui <- page_navbar(
  title = "Adolescent Brain Development",
  ## helps deal with drop down being cut off 
  # header =tags$head(tags$style(HTML("
  #   .bslib-card, .tab-content, .tab-pane, .card-body {
  #     overflow: visible !important;
  #   }
  # "))),
  
  nav_panel("Home", id = "tab_1",
            layout_columns(
              col_widths = c(12, 12, 12),
              card(tags$b("About the Dashboard",style = "text-align: center; margin: 0;"),
                   "Distributional modeling, also known as normative modeling, allows for the creation of centile curves to visualize the variation of a developmental phenotype as a function of age. Previous studies have utilized a variety of modeling approaches, including Bayesian regression, Gaussian processes, and Generalized Additive Models for Location, Scale, and Shape (GAMLSS) to model trajectories of cortical thickness and cortical surface area over the course of the human lifespan. Such studies typically incorporate a cross-sectional design, generally discarding non-baseline data from longitudinal data sources and failing to account for within-person changes across time. Additionally, while many studies plot centile curves separately for each sex and utilize random effects to account for site- and/or study-related variability, other demographic factors, such as race, are not considered in the distributional models.

This project addresses these limitations through the creation of longitudinal distributional models for cortical thickness and surface area of adolescents ages 9 to 15, stratified by race and sex, for 70 different brain regions. Spatial and longitudinal results are visualized in this interactive dashboard – explore to learn more!",style = "font-size: 12px; margin: 0;"),
              card(card_header(tags$h4("Methods", style = "text-align: center; margin: 0;")),
                   tags$div(
                     tags$span("Data", style = "font-weight: bold; font-size: 12px; margin: 0;"),
                     style = "margin: 0; padding: 0; line-height: 0.1;"
                   ),
              
              
                tags$ul(style = "margin: 0; padding: 0; font-size: 12px;",
                        # first set of bullet points #tags$li("", style = "font-size: 12px; margin: 0;")
                  
                        tags$li("The Child Behavior Checklist(CBCL) is a family of screening tests to assess behavioral and emotional problems in children. For our study, we use", style = " margin: 0; font-size: 12px;"),
                        tags$li("MRI data from the Adolescent Brain Cognitive Development (ABCD) Study 11,000 + adolescents in the study Repeated MRI scans of individuals across time, at least one timepoint", style = " margin: 0; font-size: 12px;"),
                        tags$li("Internalizing Summary Score", style = "font-size: 12px; margin: 0;"), # this is the style i want
                        tags$li("Externalizing Summary Score", style = "font-size: 12px; margin: 0;"),
                        tags$li("Depressive Problems Scor", style = "font-size: 12px; margin: 0; "),
                ),
              tags$div(
                tags$b("Modeling ", style = "margin: 0; padding: 0; line-height: 1;")),
              tags$ul(
              "Models fitted with the Generalized Additive Models for Location, Scale, and Shape (GAMLSS) R package.", style = "font-size: 12px; margin: 0;",
              tags$li("Fitted cortical thickness and cortical surface area for each sex and brain region (284 total models)", style = "font-size: 12px; margin: 0; "),
              tags$li("Harmonized MRI data from scanner-related variance across sites using the Longitudinal ComBat R package", style = "font-size: 12px; margin: 0; "),
              tags$li("Note that race was not included in the model fitting process", style = "font-size: 12px; margin: 0; ")),
              "For a rth region and sth sex, models for Cortical Thickness (CT) and Cortical Surface Area (CSA) were fit as following: margin: 0; ", style = "font-size: 12px; margin: 0; ",
              layout_columns(col_widths = c(6, 6),
              imageOutput("image_1",width = "100%", height = "auto"),
              imageOutput("image_2",width = "100%", height = "auto")
              ),
              style = "height: 300px;" # controls height for card()
              ),
              
              
              
              card( tableOutput("table"),imageOutput("image"))
            )
  ),
  
  
  
  #############
  nav_panel("Cortical Thickness", id = "tab_2",
            layout_columns(col_widths = c(12, 6, 6),card(style = "height: 150px;", title ="sidebar = sidebar(renderTex",
          
                                                         selectInput("brain_region","Select area of brain to model cortical thickness:", choices = c(
                                                           "left Hemisphere Banks of Superior Temporal Sulcus",
                                                           "left Hemisphere caudalanteriorcingulate",
                                                           "left Hemisphere caudalmiddlefrontal",
                                                           "left Hemisphere cuneus",
                                                           "left Hemisphere entorhinal",
                                                           "left Hemisphere fusiform",
                                                           "left Hemisphere inferiorparietal",
                                                           "left Hemisphere inferiortemporal",
                                                           "left Hemisphere isthmuscingulate",
                                                           "left Hemisphere lateraloccipital",
                                                           "left Hemisphere lateralorbitofrontal",
                                                           "left Hemisphere lingual",
                                                           "left Hemisphere medialorbitofrontal",
                                                           "left Hemisphere middletemporal",
                                                           "left Hemisphere parahippocampal",
                                                           "left Hemisphere paracentral",
                                                           "left Hemisphere parsopercularis",
                                                           "left Hemisphere parsorbitalis",
                                                           "left Hemisphere parstriangularis",
                                                           "left Hemisphere pericalcarine",
                                                           "left Hemisphere postcentral",
                                                           "left Hemisphere posteriorcingulate",
                                                           "left Hemisphere precentral",
                                                           "left Hemisphere precuneus",
                                                           "left Hemisphere rostralanteriorcingulate",
                                                           "left Hemisphere rostralmiddlefrontal",
                                                           "left Hemisphere superiorfrontal",
                                                           "left Hemisphere superiorparietal",
                                                           "left Hemisphere superiortemporal",
                                                           "left Hemisphere supramarginal",
                                                           "left Hemisphere frontalpole",
                                                           "left Hemisphere temporalpole",
                                                           "left Hemisphere transversetemporal",
                                                           "left Hemisphere insula",
                                                           "right Hemisphere Banks of Superior Temporal Sulcus",
                                                           "right Hemisphere caudalanteriorcingulate",
                                                           "right Hemisphere caudalmiddlefrontal",
                                                           "right Hemisphere cuneus",
                                                           "right Hemisphere entorhinal",
                                                           "right Hemisphere fusiform",
                                                           "right Hemisphere inferiorparietal",
                                                           "right Hemisphere inferiortemporal",
                                                           "right Hemisphere isthmuscingulate",
                                                           "right Hemisphere lateraloccipital",
                                                           "right Hemisphere lateralorbitofrontal",
                                                           "right Hemisphere lingual",
                                                           "right Hemisphere medialorbitofrontal",
                                                           "right Hemisphere middletemporal",
                                                           "right Hemisphere parahippocampal",
                                                           "right Hemisphere paracentral",
                                                           "right Hemisphere parsopercularis",
                                                           "right Hemisphere parsorbitalis",
                                                           "right Hemisphere parstriangularis",
                                                           "right Hemisphere pericalcarine",
                                                           "right Hemisphere postcentral",
                                                           "right Hemisphere posteriorcingulate",
                                                           "right Hemisphere precentral",
                                                           "right Hemisphere precuneus",
                                                           "right Hemisphere rostralanteriorcingulate",
                                                           "right Hemisphere rostralmiddlefrontal",
                                                           "right Hemisphere superiorfrontal",
                                                           "right Hemisphere superiorparietal",
                                                           "right Hemisphere superiortemporal",
                                                           "right Hemisphere supramarginal",
                                                           "right Hemisphere frontalpole",
                                                           "right Hemisphere temporalpole",
                                                           "right Hemisphere transversetemporal",
                                                           "right Hemisphere insula",
                                                           "left hemisphere",
                                                           "right hemisphere",
                                                           "whole brain"
                                                           
                                                         ), selected ="left Hemisphere Banks of Superior Temporal Sulcus", width = "100%")
            )
            ########## select race and 
            ,card(card_header(tags$h4("Males", style = "font-weight: bold; text-align: center; font-size: 16px; margin: 0; ")), title ="plot 1",style = "height: 2000px;",imageOutput("maleImage",width = "100%", height = "400px",inline = TRUE)),

            #########
            card(card_header(tags$h4("Females", style = "font-weight: bold; text-align: center; font-size: 16px; margin: 0;")),title ="plot 2",style = "height: 2000px;",imageOutput("femaleImage",width = "100%", height = "400px",inline = TRUE))
            )
            ),
  

  nav_panel("Cortical Surface Area",
            id = "tab_3",
            layout_columns(col_widths = c(12, 6, 6),card(style = "height: 150px;", title ="sidebar = sidebar(renderTex", "Select area of brain to model cortical thickness:",
                                                         selectInput("type2", "", choices = c(
                                                           "left Hemisphere Banks of Superior Temporal Sulcus",
                                                           "left Hemisphere caudalanteriorcingulate",
                                                           "left Hemisphere caudalmiddlefrontal",
                                                           "left Hemisphere cuneus",
                                                           "left Hemisphere entorhinal",
                                                           "left Hemisphere fusiform",
                                                           "left Hemisphere inferiorparietal",
                                                           "left Hemisphere inferiortemporal",
                                                           "left Hemisphere isthmuscingulate",
                                                           "left Hemisphere lateraloccipital",
                                                           "left Hemisphere lateralorbitofrontal",
                                                           "left Hemisphere lingual",
                                                           "left Hemisphere medialorbitofrontal",
                                                           "left Hemisphere middletemporal",
                                                           "left Hemisphere parahippocampal",
                                                           "left Hemisphere paracentral",
                                                           "left Hemisphere parsopercularis",
                                                           "left Hemisphere parsorbitalis",
                                                           "left Hemisphere parstriangularis",
                                                           "left Hemisphere pericalcarine",
                                                           "left Hemisphere postcentral",
                                                           "left Hemisphere posteriorcingulate",
                                                           "left Hemisphere precentral",
                                                           "left Hemisphere precuneus",
                                                           "left Hemisphere rostralanteriorcingulate",
                                                           "left Hemisphere rostralmiddlefrontal",
                                                           "left Hemisphere superiorfrontal",
                                                           "left Hemisphere superiorparietal",
                                                           "left Hemisphere superiortemporal",
                                                           "left Hemisphere supramarginal",
                                                           "left Hemisphere frontalpole",
                                                           "left Hemisphere temporalpole",
                                                           "left Hemisphere transversetemporal",
                                                           "left Hemisphere insula",
                                                           "right Hemisphere Banks of Superior Temporal Sulcus",
                                                           "right Hemisphere caudalanteriorcingulate",
                                                           "right Hemisphere caudalmiddlefrontal",
                                                           "right Hemisphere cuneus",
                                                           "right Hemisphere entorhinal",
                                                           "right Hemisphere fusiform",
                                                           "right Hemisphere inferiorparietal",
                                                           "right Hemisphere inferiortemporal",
                                                           "right Hemisphere isthmuscingulate",
                                                           "right Hemisphere lateraloccipital",
                                                           "right Hemisphere lateralorbitofrontal",
                                                           "right Hemisphere lingual",
                                                           "right Hemisphere medialorbitofrontal",
                                                           "right Hemisphere middletemporal",
                                                           "right Hemisphere parahippocampal",
                                                           "right Hemisphere paracentral",
                                                           "right Hemisphere parsopercularis",
                                                           "right Hemisphere parsorbitalis",
                                                           "right Hemisphere parstriangularis",
                                                           "right Hemisphere pericalcarine",
                                                           "right Hemisphere postcentral",
                                                           "right Hemisphere posteriorcingulate",
                                                           "right Hemisphere precentral",
                                                           "right Hemisphere precuneus",
                                                           "right Hemisphere rostralanteriorcingulate",
                                                           "right Hemisphere rostralmiddlefrontal",
                                                           "right Hemisphere superiorfrontal",
                                                           "right Hemisphere superiorparietal",
                                                           "right Hemisphere superiortemporal",
                                                           "right Hemisphere supramarginal",
                                                           "right Hemisphere frontalpole",
                                                           "right Hemisphere temporalpole",
                                                           "right Hemisphere transversetemporal",
                                                           "right Hemisphere insula"
                                                           
                                                           ,
                                                           "left hemisphere",
                                                           "right hemisphere",
                                                           "whole brain"
                                                         
                                                         ), selected ="left Hemisphere Banks of Superior Temporal Sulcus", width = "100%" )
            )
            ##########
            ,card(card_header(tags$h4("Males", style = "font-weight: bold; text-align: center; font-size: 16px; margin: 0;")),title ="plot 1",style = "height: 2500px;",imageOutput("maleImage_area",width = "45%", height = "400px",inline = TRUE)),
            
            #########
            card(card_header(tags$h4("Females", style = "font-weight: bold; text-align: center; font-size: 16px; margin: 0;")),title ="plot 1",style = "height: 2500px;",imageOutput("femaleImage_area",width = "45%", height = "400px",inline = TRUE))
            ),
  ),

  nav_panel("Risdual Plotting",
            id = "tab_4",
            layout_columns(col_widths = c(12, 6, 6),card(style = "height: 150px;", title ="sidebar = sidebar(renderTex",
                                                                            layout_columns(col_widths = c(6, 6),
                                                                              selectInput("type2", "Select Race", choices = c(
                                                                              "All",
                                                                              "White",
                                                                              "Black",
                                                                              "Hispanic",
                                                                              "Asian",
                                                                              "Not listed"
                                                                            
                                                                            ), selected ="All" ),
                                                                            selectInput("type2", "select time point", choices = c(
                                                                              "All",
                                                                              "other"
                                                                            ), selected ="All" ))
                                                                            
  )
  ##########
  ,card(card_header(tags$h4("Card 2 header", style = "text-align: center;")),title ="plot 1",style = "height: 2500px;",imageOutput("residual_male",width = "45%", height = "400px",inline = TRUE)),
  
  #########
  card(card_header(tags$h4("Card 2 header", style = "text-align: center;")),title ="plot 1",style = "height: 2500px;",imageOutput("residual_female",width = "45%", height = "400px",inline = TRUE))
  ),),


  nav_panel("About Us",
            value = "tab_5",
            layout_sidebar(
              sidebar = sidebar(
                selectInput("type4", "What event do you want to visualize", choices = c("kaka"), selected = "kaka")
              ),
              leafletOutput("aboutMap")
            )
  )
  
  
)

# Define server logic required to draw a histogram
server <- function(session, input, output) {
  
  output$image_1 <- renderImage( 
    { 
      list(src = "model_function_1.png",
           width = 150,        # ← in pixels
           height = 100) 
    }, 
    deleteFile = FALSE 
  )
  
  output$image_2 <- renderImage( 
    { 
      list(src = "model_function_2.png",
           width = 150,        # ← in pixels
           height = 100) 
    }, 
    deleteFile = FALSE 
  ) 
  
  output$image <- renderImage( 
    { 
      list(src = "demographics.png",
           width = 400,        # ← in pixels
           height = 325) 
    }, 
    deleteFile = FALSE 
  ) 
  
  output$distPlot_female <- renderTable(df_one,striped = TRUE)

  output$maleImage <- renderImage({
    region <- input$brain_region
    filename <- normalizePath(file.path('png_of_regions',
                                        paste0(region, '_male_All.png')))
    
    list(src = filename,
         alt = paste(region, "male"),
         width = 600,        # ← in pixels
         height = 500)
  }, deleteFile = FALSE)
  
  output$femaleImage <- renderImage({
    region <- input$brain_region
    filename <- normalizePath(file.path('png_of_regions',
                                        paste0(region, '_female_All.png')))
    
    list(src = filename,
         alt = paste(region, "female"),
         width = 600,        # ← in pixels
         height = 500)
  }, deleteFile = FALSE)
  
  #### area
  output$maleImage_area <- renderImage({
    region <- input$type2
    filename <- normalizePath(file.path('png_of_regions',
                                        paste0(region, '_male_All_area.png')))
    
    list(src = filename,
         alt = paste(region, "male"),
         width = 600,        # ← in pixels
         height = 500)
  }, deleteFile = FALSE)
  
  output$femaleImage_area <- renderImage({
    region <- input$type2
    filename <- normalizePath(file.path('png_of_regions',
                                        paste0(region, '_female_All_area.png')))
    print(filename)
    list(src = filename,
         alt = paste(region, "female"),
         width = 600,        # ← in pixels
         height = 500)
  }, deleteFile = FALSE)
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    query1 <- paste(names(query), query, sep = "=", collapse=", ")
    print(query1)
    if(query1 == "ref"){
      nav_select(id = "tab_5", session = session)
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)



