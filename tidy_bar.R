###these functions:
###1. reads in output of a barcode counting algorithm
###2. assigns sample names and metadata
###3. tidies data
###4. assigns UID to each row, either default combination of path and sample or combines columns passed to u_vector
library(tidyverse) 

gene_lookup_table <- read_csv("gene_lookup_table.csv")

##this one is for output of bartender
tidy_bar_tender <- function(sample_sheet, u_vector = NA) {
  library(tidyverse) #make sure you have tidyverse loaded
  s_sheet <- read_csv(sample_sheet) #read in samplesheet, column 1 must be File_path, column 2 must be Sample, other columns can be whatever metadata you would like
  z<-tibble(File_path=unique(s_sheet%>%pull(File_path)))%>%
    mutate(raw_data = map(File_path, function(x) { 
        read_csv(x) %>% gather(Sample,Counts,starts_with("time_point"))
      })) %>% 
    unnest()
  z <-left_join(s_sheet,z,by=c("File_path","Sample")) %>% 
    rename(Barcode = Center)
  #add UID column
  if(is.na(u_vector[1])) {
    z = z %>% mutate_(UID = quote(str_c(File_path, Sample, sep = '_')))
  } else { #if you passed it a vector it will combine those columns to make the UID
    z = z %>% unite_('UID', u_vector, remove = F)
  }
  z <- z%>%left_join(z,gene_lookup_table%>%rename(Barcode=Systematic),by="Barcode")
  return(z)
}

#this one is for output of barnone
tidy_bar_none <- function(sample_sheet, u_vector = NA) {
  library(tidyverse) #make sure you have tidyverse loaded
  s_sheet <- read_csv(sample_sheet) #read in samplesheet, column 1 must be File_path, column 2 must be Sample, other columns can be whatever metadata you would like
  z<-tibble(File_path=unique(s_sheet%>%pull(File_path)))%>%
    mutate(raw_data = map(File_path, function(x) { 
        read_tsv(x) %>% gather(Sample,Counts,starts_with("Sample")) %>% 
          separate(Sample,c("Sample","Tag"),"_")
      })) %>% 
    unnest()
  z <-left_join(s_sheet,z,by=c("File_path","Sample")) %>% 
    rename(Barcode = Strain)
  if(is.na(u_vector[1])) {
    z = z %>% mutate_(UID = quote(str_c(File_path, Sample, sep = '_')))
  } else {
    z = z %>% unite_('UID', u_vector, remove = F)
  }
  z <- z%>%left_join(z,gene_lookup_table%>%rename(Barcode=Systematic),by="Barcode")
  return(z)
}

