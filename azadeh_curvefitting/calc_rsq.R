library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)

d = read.csv(file = "deposition_predicted.csv")

rss_s = sum((d$dep_predict - d$deposition)^2)
rss_p = sum((d$dep_bestfit - d$deposition)^2)
tss = sum(d$deposition - mean(d$deposition)^2)

rsq_s = 1-(rss_s/tss)
rsq_p = 1-(rss_p/tss)
print(rsq_s)
print(rsq_p)