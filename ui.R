library(shiny)
source("helpers.R")
# DataSet
ds = read.csv('data/dataset.csv', na.strings=c(""), header=TRUE, stringsAsFactors=FALSE)
ds[is.na(ds)] <- 'None'
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Patch Notes Explorer"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(width = 3,
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
                 selected = c("Buff", "Nerf")
    ),
    selectInput("subclasses", 
                label = "Affected Subclass", 
                choices = getSubclassChoices(ds),
                multiple = TRUE,
                selected = getSubclassChoices(ds)
    ),
    selectInput("gear", 
                label = "Affected Gear", 
                choices = getGearChoices(ds),
                multiple = TRUE,
                selected = getGearChoices(ds)
    )),
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Histogram", 
                 plotOutput("histPlot")),
        tabPanel("Timeline",
                 plotOutput("timePlot")),
        
        tabPanel("Data Table",
                 fluidRow(
                   column(3,
                          tags$div(
                            class = "checkbox-container",
                            checkboxInput("tableIncludeExotics", "Include Exotics", value = FALSE)
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
      textOutput("text1")
    )
  )
))