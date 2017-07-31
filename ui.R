library(shiny)
source("helpers.R")
# DataSet
ds = read.csv('data/test-data.csv', na.strings=c(""), header=TRUE, stringsAsFactors=FALSE)
ds[is.na(ds)] <- 'None'
ds$Tags <- strsplit(ds$Tags, "/")
ds$Subject <- strsplit(ds$Subject, "/")
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  includeCSS("www/app.css"),
  # Application title
  titlePanel("Destiny Patch Notes Explorer"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(width = 3, class = "sidebar-panel",
      tags$head(
        tags$style("
          form.well {background-color: white; }
          div.quick-summary {
            padding-top: 10px; 
            font-size: 16px; 
            font-weight: bold; 
            text-align: right;
          }
          div.checkbox-container {
            font-size: 16px; 
            font-weight: bold; 
            color: goldenrod;
          }
          div.button-container {
            padding-top: 10px;
          }
          #buffs {color: green; margin-right: 10px;}
          #nerfs {color: red; margin-right: 10px;}
          #neutral {color: grey; margin-right: 10px;}
          #unknown {color: black; }
        ")
      ),
     selectInput("environment", 
                 label = "Environment",
                 choices = c("Both", "PvE", "PvP"),
                 selected = c("Both", "PvE", "PvP"),
                 multiple = TRUE),
     selectInput("results", 
                 label = "Net Result", 
                 choices = c(
                   "Buff",
                   "Nerf", 
                   "Neutral", 
                   "Unknown"
                 ),
                 multiple = TRUE,
                 selected = c("Buff", "Nerf", "Neutral", "Unknown")
    ),
    selectInput("subject", 
                label = "Affected Subject", 
                choices = getSubjectChoices(ds),
                multiple = TRUE,
                selected = getSubjectChoices(ds)
    ),
    selectInput("tags", 
                label = "Tags", 
                choices = getTagChoices(ds),
                multiple = TRUE,
                selected = getTagChoices(ds)
    )),
    # Show a plot of the generated distribution
    mainPanel(
      width = 9,
      class = "main-panel",
      fluidRow(
        column(3,
               tags$div(
                 class = "checkbox-container",
                 checkboxInput("tableIncludeExotics", "Include Exotics", value = TRUE)
               )
        ),
        column(3,
               tags$div(
                 class = "button-container",
                 downloadButton('downloadDataTable', 'Download This Subset')
               )
        ),
        column(6,
               tags$div(
                 class = "quick-summary",
                 textOutput("buffs", inline=TRUE),
                 textOutput("nerfs", inline=TRUE),
                 textOutput("neutral", inline=TRUE),
                 textOutput("unknown", inline=TRUE)
               )
        )
      ),
      tabsetPanel(
        tabPanel("Timeline",
                 plotOutput("timePlot")),
        
        tabPanel("Data Table",
                 dataTableOutput("dataTable")
        ),
                 
        tabPanel("Information", 
                 tags$div(
                   HTML("
                        <h3>
                        About this app:
                        </h3>
                        ")
                  )
        )
      ),
      fluidRow(
        column(12,
               tags$div(
                 class = "patch-version-slider-container",
                 sliderInput("versionSlider", "Patch Version:",
                             width = '100%',
                             min = 1, max = 48, value = c(1,48))
               )
        )
      ),
      textOutput("text1")
    )
  )
))