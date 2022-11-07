library(dplyr)
library(ggplot2)
library(ggpubr)

dep = read.csv("deposition.csv")

ggplot(data = dep, aes(x = parameter, y = deposition)) + 
  geom_point() + 
  scale_x_log10()
