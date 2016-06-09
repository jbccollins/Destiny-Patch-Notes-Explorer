# server.R

require(dplyr)
source("helpers.R")
library(ggplot2)

# DataSet
ds = read.csv('data/dataset.csv', na.strings=c(""), header=TRUE, stringsAsFactors=FALSE)
ds[is.na(ds)] <- 'None'

currentDataSet = ds

shinyServer(function(input, output) {
  
  table_data_filter <- function(){
    environment = input$environment
    results = input$results
    subclasses = input$subclasses
    gear = input$gear
    includeExotics = input$tableIncludeExotics
    x <- ds
    toSelect = c("Version", "Subclass", "Gear", "Environment", "Result", "Detail")
    if(includeExotics == TRUE) {
      toSelect <- c(toSelect, "ExoticName")
    } else {
      x <- x %>% filter(
        ExoticName == "None"
      )
    }
    colNums <- match(toSelect, names(x))
    x <- x %>% select (colNums) 
    x <- x %>% filter(
      Environment %in% environment,
      Result %in% results,
      Subclass %in% subclasses,
      Gear %in% gear
    )
    
    #if(length(x$Version) <= 1){
    #  output$text1 <- renderText({ 
    #    "No matching observations found. Try adjusting your parameters"
    #  }) 
    #} else {
    #    w <- x$Version
    #    output$text1 <- renderText({ 
    #      paste(length(w), "Matching observations found.", sep=" ")
    #  })
    #}
    currentDataSet = x
    return (x)
  }
  
  output$histPlot <- renderPlot({
      # draw the histogram with the specified number of bins
      #hist(w, breaks = bins, col = 'springgreen3', border = 'white', xlab='Income', main="Histogram of Income")
  })
  
  output$dataTable <- renderDataTable({
    x <- table_data_filter()
    choices = getResultChoices(ds)
    numBuffs = sum(x$Result == "Buff")
    numNerfs = sum(x$Result == "Nerf")
    numNeutral = sum(x$Result == "Neutral")
    numUnknown = sum(x$Result == "Unknown")
    output$buffs <- renderText({
      paste(c(numBuffs, "Buffs"), collapse = " ")
    })
    output$nerfs <- renderText({
      paste(c(numNerfs, "Nerfs"), collapse = " ")
    })
    output$neutral <- renderText({
      paste(c(numNeutral, "Neutral"), collapse = " ")
    })
    output$unknown <- renderText({
      paste(c(numUnknown, "Unknown"), collapse = " ")
    })
    return(x)
  })
  
  output$timePlot <- renderPlot({
    plot <- ggplot(ds,aes(x=Date,y=0))
    print(plot)
  })
  
  output$downloadDataTable <- downloadHandler(
    filename = function() { paste('datatable', '.csv', sep='') },
    content = function(file) {
      write.csv(table_data_filter(), file)
    }
  )
  
  output$text1 <- renderText({ 
    paste("This Text Exists")
  })
})