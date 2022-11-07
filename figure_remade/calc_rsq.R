library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)


# General model:
# f(x) = 1 - (1/(a*x^b+1))
# Coefficients (with 95% confidence bounds):
#   a =   3.988e-08  (3.079e-09, 7.668e-08)
#   b =       1.746  (1.669, 1.823)

# d = read.csv("deposition_effiiciency.csv")

d = read.csv(file = "imp_dep.csv")

rss_s = sum((d$dep_predict - d$dep)^2)
rss_p = sum((d$dep_bestfit - d$dep)^2)
tss = sum(d$dep - mean(d$dep)^2)

rsq_s = 1-(rss_s/tss)
rsq_p = 1-(rss_p/tss)