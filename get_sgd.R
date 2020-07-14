
library(tidyverse)

urlz <- "https://downloads.yeastgenome.org/curation/chromosomal_feature/saccharomyces_cerevisiae.gff"

z <- read_tsv(urlz,col_names=F,comment="#") %>% 
  filter(X3=="gene")%>% 
  mutate(subtibble=map(X9,function(x){ 
    z<-str_split(x,pattern=";")[[1]]; 
    setNames(str_replace(z,".+=",""),str_replace(z,"=.+",""))[c("ID","gene")] }))%>%
  select(subtibble)%>%
  mutate(Systematic=map(subtibble,~.x[[1]]),Common=map(subtibble,~.x[[2]]))%>%
  select(-subtibble)%>%unnest() 

write_csv(z,path="gene_lookup_table.csv")

