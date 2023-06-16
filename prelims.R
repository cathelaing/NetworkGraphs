# load packages

library(tidyverse)
library(stringi)
library(stringr)
library(ggplot2)
library(tibble)
library(lmerTest)
library(papaja)
library(tidyverse)
library(dplyr)
library(feather)
library(ggthemes)
library(nlme)
library(broom)
library(kableExtra)
library(knitr)
library(janitor)
library(ggraph)
library(data.table)
library(linguisticsdown)
library(igraph)

# add this to prep files eventually

session_data_P <- read_csv("Data/comparison_data_providence.csv") %>%    # need to add ordinal session numbers for GAMMs
  group_by(Speaker, age) %>%
  tally() %>%
  filter(n > 1) %>%
  dplyr::select(Speaker, age) %>%
  group_by(Speaker, age) %>% 
  tally() %>%
  filter(!(Speaker == "Alex" & age == "16")) %>%
  mutate(session_ordinal = row_number()) %>%
  mutate(Speaker = as.factor(Speaker)) %>%
  dplyr::select(-n)

session_data_L <- read_csv("Data/comparison_data_lyon.csv") %>%    # need to add ordinal session numbers for GAMMs
  group_by(Speaker, age) %>%
  tally() %>%
  filter(n > 1) %>%
  dplyr::select(Speaker, age) %>%
  group_by(Speaker, age) %>% 
  tally() %>%
  filter(!(Speaker == "Anais" & age == "12")) %>%
  mutate(session_ordinal = row_number()) %>%
  mutate(Speaker = as.factor(Speaker)) %>%
  dplyr::select(-n)

globalsmallworlddata_L <- feather::read_feather("Data/globalsmallworlddata_comparison_lyon.feather") %>% 
  mutate(corpus = "French",
         corpus = as.factor(corpus),
         Speaker = as.factor(Speaker)) %>% 
  left_join(session_data_L)

globalsmallworlddata_P <- feather::read_feather("Data/globalsmallworlddata_comparison_providence.feather") %>% 
  mutate(corpus = "English",
         corpus = as.factor(corpus),
         Speaker = as.factor(Speaker)) %>% 
  left_join(session_data_P)

globalsmallworlddata <- rbind(globalsmallworlddata_P, globalsmallworlddata_L) %>%
  mutate(data_type= as.factor(data_type)) %>%
  dplyr::select(-lowerCI, -upperCI, -lowerQuantile, -upperQuantile) # remove variables that aren't used

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



