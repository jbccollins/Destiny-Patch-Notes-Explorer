require(dplyr)
getSubclassChoices <- function(ds){
  unique(ds$Subclass)
}

getGearChoices <- function(ds){
  unique(ds$Gear)
}

getResultChoices <- function(ds){
  unique(ds$Result)
}