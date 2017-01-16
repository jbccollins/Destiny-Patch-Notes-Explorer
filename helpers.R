require(dplyr)

getSubjectChoices <- function(ds){
  sort(unique(ds$Subject))
}

getResultChoices <- function(ds){
  sort(unique(ds$Result))
}

getTagChoices <- function(ds){
  choices = c()
  # TODO: There has got to be a better way to do this than
  # with three nested for loops
  for(tagList in ds$Tags){
    for(s in tagList){
      if (! (s %in% choices) ){
        choices <- c(choices, s)
      }
    }
  }
  choices <- sort(choices)
  return(choices)
}
