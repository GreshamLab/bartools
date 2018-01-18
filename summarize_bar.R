summarize_bar <- function(data, sum_by="SampleNum", tags=c("UP", "DOWN")) {
  
  library(tidyr)
  library(vegan)
  
  if(is.null(tags)) {
    summary_data <- data %>%
      group_by_(sum_by) %>%
      summarize(TotalCounts = sum(Counts, na.rm=TRUE),
                NumSamples = sum(Counts > 0),
                Shannon = diversity(Counts, index="shannon"))
  } else {
    summary_data <- data %>%
      group_by_(sum_by) %>%
      summarize(UPCounts = sum(Counts[Tag == tags[1]], na.rm=TRUE),
                DNCounts = sum(Counts[Tag == tags[2]], na.rm=TRUE),
                UPNumVal = sum(Counts[Tag == tags[1]] > 0),
                DNNumVal = sum(Counts[Tag == tags[2]] > 0),
                LibrarySize = sum(Counts, na.rm=TRUE),
                Shannon = diversity(Counts, index="shannon"))
  }
  
  return(summary_data)
}