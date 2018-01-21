
#Provides barplot of counts per library colored by tags and sorted by total counts.
summarize_barnon_plot <- function(df, datadir=getwd(), format = '.pdf'){
  #df is the output file by running function = tidy_bar.R
  
  library(GGally)
  library(tidyverse)
  library(gridExtra)
  
  #Library size overview
  p1 <- df %>%
    group_by(Sample, Tag, Conditions, UID) %>%
    summarise(total=sum(Counts)) %>%
    arrange(desc(total)) %>%
    ggplot(aes(x=reorder(Sample, -total), y=total, fill = Tag))+
    geom_bar(position=position_dodge(), stat="identity") +
    ggtitle("Library size for each sample")+
    scale_y_log10()+
    theme_bw(base_size=6)+
    theme(axis.text.x=element_text(angle=90, size=4),legend.position="bottom")
  
  #Histogram of strain frequencies over all libraries
  p2 <- df %>%
    group_by(Barcode, Tag, Conditions) %>%  
    summarise(straincounts=sum(Counts)) %>%
    arrange(desc(straincounts)) %>%
    ggplot(aes(x=straincounts, col= Tag))+
    scale_x_log10()+
    ggtitle("Strain frequency over all libraries")+
    ylab("Frequency")+
    xlab("Strain Counts")+
    stat_bin(position="identity",geom="line",bins=30)
  
  #Scatter plot for UP vs DOWN tag for each library
  p3 <- df %>% 
    spread(key = Tag, value = Counts)%>% 
    ggplot(aes(x = UP, y = DOWN))+
    facet_wrap(~Sample, scale='free')+
    geom_point(alpha = .2, size=0.2)+
    theme_bw(base_size=6)+
    scale_y_log10()+
    scale_x_log10()+
    ggtitle("Scatter plot for UP vd DOWN tag")+
    theme(axis.text.x=element_text(angle=90),legend.position="bottom")
  
  #Pairwise comparison across all sample 
  p4 <- df %>% 
    unite(Sample_Tag, Sample, Tag) %>%
    unstack(form = Counts ~ Sample_Tag) %>%
    ggcorr(size = 1.5, legend.size = 4, label_size= 2) +
    theme_bw(base_size=6)+
    ggtitle("pairwise comparison for libraries (correlation coefficient)")+
    theme(axis.text.x=element_text(size=4))
 
  MyPlots = list(p1,p2,p3,p4)
  
  #save plot according to your taste
  for (i in 1:length(MyPlots)) {
    ggsave(file=paste0(datadir, "/QC_plots_", i, format), 
           plot=MyPlots[[i]],width = 8, height = 8)
  }
}




