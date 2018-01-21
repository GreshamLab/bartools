summarize_bar <- function(data, sum_by="SampleNum", barcode_by="Strain", count_by="Counts", tag_by="Tag", tags=c("UP", "DOWN")) {
  # Summarizes the results of a bar experiment in simple table
  #
  # Args:
  #
  #   data:       A data.frame with the barcoding experiment data
  #   sum_by:     A string naming the data frame column containing the sample ID. Defaults to SampleNum
  #   barcode_by: A string naming the data frame column containing the barcode ID. Defaults to Strain
  #   count_by:   A string naming the data frame column containing the sequence counts. Defaults to Counts
  #   tag_by:     A string naming the data frame column containing the directional tags. Defaults to "Tag". Will skip tags if set to NULL.
  #   tags:       A vector of the two possible tag direction names. Defaults to UP and DOWN. Will skip tags if set to NULL.
  #
  # Returns:
  #   The summary data table
  
  library(tidyr)
  library(dplyr)
  library(vegan)
  library(rlang)
  
  if(is.null(tags) | is.null(tag_by)) {
    summary_data <- data %>%
      group_by(!! sym(sum_by)) %>%
      summarize(TotalCounts = sum(!! sym(count_by), na.rm=TRUE),
                NumSamples = sum(!! sym(count_by) > 0),
                Shannon = diversity(!! sym(count_by), index="shannon"))
  } else {
    summary_data <- data %>%
      group_by(!! sym(sum_by)) %>%
      summarize(UPCounts = sum((!! sym(count_by))[(!! sym(tag_by)) == tags[1]], na.rm=TRUE),
                DNCounts = sum((!! sym(count_by))[(!! sym(tag_by)) == tags[2]], na.rm=TRUE),
                UPNumVal = sum((!! sym(count_by))[(!! sym(tag_by)) == tags[1]] > 0),
                DNNumVal = sum((!! sym(count_by))[(!! sym(tag_by)) == tags[2]] > 0),
                NumSamples = n_distinct(!! sym(barcode_by)),
                LibrarySize = sum(!! sym(count_by), na.rm=TRUE),
                Shannon = diversity(!! sym(count_by), index="shannon"))
  }
  
  return(summary_data)
}