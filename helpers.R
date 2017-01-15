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

getAffectedStatChoices <- function(ds){
  choices = c()
  # TODO: There has got to be a better way to do this than
  # with three nested for loops
  for(statList in ds$Affects){
    for(s in statList){
      if (! (s %in% choices) ){
        choices <- c(choices, s)
      }
    }
  }
  choices <- sort(choices)
  return(choices)
}

getStatIntersect <- function(statEntry, statList){
  print("ENTRY")
  print(unlist(statEntry))
  print(statList)
  z <- intersect(statEntry, statList)
  print("MATCHING")
  print(z)
  return(length(z))
}