# server.R

require(dplyr)
source("helpers.R")
library(ggplot2)
library(data.table)


# DataSet
ds = read.csv('data/test-data.csv', na.strings=c(""), header=TRUE, stringsAsFactors=FALSE)
ds[is.na(ds)] <- 'None'
ds$Tags <- strsplit(ds$Tags, "/")

shinyServer(function(input, output) {
  
  data_filter <- function(){
    environment = input$environment
    results = input$results
    subject = input$subject
    tags = input$tags
    includeExotics = input$tableIncludeExotics
    x <- ds
    toSelect = c("Version", "Subject", "Environment", "Tags", "Result", "Detail")
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
      Subject %in% subject
    )
    idx <- which(sapply(x$Tags, function(y) length(intersect(tags, y))) > 0)
    x <- x[idx,]
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
    return (x)
  }
  
  output$dataTable <- renderDataTable({
    return(data_filter());
  })
  
  output$timePlot <- renderPlot({
    ds_filtered <- data_filter()
    dt <- data.table(ds_filtered)
    dt_buff <- dt[, sum(Result == "Buff"), by = Version]
    dt_nerf <- dt[, sum(Result == "Nerf"), by = Version]
    # Factors prevent ggplot from sorting the x axis alphabetically
    dt_buff$Version <- factor(dt_buff$Version, levels = dt_buff$Version)
    dt_nerf$Version <- factor(dt_nerf$Version, levels = dt_nerf$Version)
    # Rename columns for clarity
    setnames(dt_buff, "V1", "Buffs")
    setnames(dt_nerf, "V1", "Nerfs")
    # set the ON clause as keys of the tables:
    setkey(dt_buff,Version)
    setkey(dt_nerf,Version)
    
    # Create the result data table and merge in the buffs and nerfs
    # to ensure that all versions are displayed
    none_filter <- sapply(unique(ds$Version), function(x) x !="None")
    filtered_version  <- unique(ds$Version)[none_filter]
    dt_result <- data.table(Version=filtered_version) #TODO: This should just be unique(ds$Version) once the data is clean
    dt_result$Version = factor(dt_result$Version, levels = dt_result$Version)
    setkey(dt_result,Version)
    # Perform the join
    dt_result <- merge(merge(dt_result,dt_buff, all.x=TRUE),dt_nerf, all.x=TRUE)
    dt_result[is.na(dt_result)] <- 0 #TODO: Shouldn't be necessary post cleanup
    # Make Nerfs negative numbers
    dt_result$Nerfs <- dt_result$Nerfs * -1
    dt_result$Difference <- dt_result$Buffs + dt_result$Nerfs;
    dt_result$Trend <- cumsum(dt_result$Difference)
        
    min_val <- min(with(dt_result, pmin(Nerfs, Buffs, Difference)))
    max_val <- max(with(dt_result, pmax(Nerfs, Buffs, Difference)))
    min_val <- -25
    max_val <- 25
    #op <- par(mar = c(10,0,0,0))
    plot(dt_result$Buffs,type="h",ylim=c(min_val,max_val),col="green4",ylab="Number",lend=1,lwd=20,xlab="",xaxt="n")
    #par(op)
    lines(dt_result$Nerfs,type="h",col="red4",lend=1,lwd=20)
    grid()
    lines(dt_result$Trend,type="l",col="hotpink",lwd=4,lty=1)
    points(dt_result$Difference,col="gold",pch=18,cex=1,lwd=4)
    #legend("topright",legend=c("Buffs", "Nerfs", "Difference", "Trend"),col=c("green4", "red4", "gold", "hotpink"),bg="white",lwd=2)
    axis(1,at=c(1:nrow(dt_result)),labels=dt_result$Version,las=2)
  })
  
  output$downloadDataTable <- downloadHandler(
    filename = function() { paste('datatable', '.csv', sep='') },
    content = function(file) {
      write.csv(data_filter(), file)
    }
  )
})