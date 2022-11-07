library(dplyr)
library(writexl)
library(readxl)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(scales)
library(latex2exp)
library(plyr)

dep = read.csv("deposition.csv")

dep$subjects = mapvalues(dep$subjects, 
                         from = c("PD01", "PD07", "PD03", "PD02", 
                                  "PD08", "PD20", "PD21", "PD09",
                                  "PD19", "PD10", "PD13"), 
                         to = c("H1", "H4", "H3", "H2", 
                                "H5", "H6", "H7", "COPD1",
                                "COPD4", "COPD2", "COPD3"))

dep = dep %>%
  mutate(imp_cm3 = parameter/0.06) %>%
  mutate(dep_predict = (1-1/((0.000000035)*imp_cm3^1.7+1))) %>%
  mutate(dep_bestfit = (1-1/((6.733e-08)*(imp_cm3)^1.645+1))) %>%
  mutate(deposition = deposition * 100) %>%
  mutate(dep_predict = dep_predict * 100) %>%
  mutate(dep_bestfit = dep_bestfit * 100)
dep$flowrate = mapvalues(dep$flowrate, from = c(18, 45), to = c("18 L/min", "45 L/min"))

dep_prediction = tibble(
  imp_cm3 = seq(from = range(dep$imp_cm3)[1], to = range(dep$imp_cm3)[2])
) %>%
  mutate(dep_predict = (1-1/((0.000000035)*imp_cm3^1.7+1))) %>%
  mutate(dep_bestfit = (1-1/((6.733e-08)*(imp_cm3)^1.645+1))) %>%
  mutate(dep_predict = dep_predict * 100) %>%
  mutate(dep_bestfit = dep_bestfit * 100)

dep_prediction = gather(dep_prediction, key = "model", "value", -imp_cm3)
dep_prediction$model = mapvalues(dep_prediction$model, 
                                 from = c("dep_predict", "dep_bestfit"), 
                                 to = c("Stahlhofen et al. (1989)", "Best fit"))

dep$imp_cm3 = dep$imp_cm3 * (6/100)
dep_prediction$imp_cm3 = dep_prediction$imp_cm3 * (6/100)

# dep$imp_cm3 = log(dep$imp_cm3, base = 10)
# dep_prediction$imp_cm3 = log(dep_prediction$imp_cm3, base = 10)

plt = ggplot(data = dep, aes(x = imp_cm3)) + 
  geom_point(aes(y = deposition, shape = subjects, color = as.factor(flowrate)), size = 3) +
  scale_shape_manual(values = c(0:10)) + 
  geom_line(data = dep_prediction, aes(x = imp_cm3, y = value, color = model)) + 
  scale_color_manual(values = c("darkgray", "black", "blue", "red")) + 
  labs(color = "", shape = "") + 
  xlab(TeX("Impaction Parameter $d_a^2Q (\\mu m^2L/min)$")) + 
  ylab(TeX("Oral Airway Deposition (%)")) + 
  scale_x_continuous(breaks = c(seq(10, 90, 10), 
                                seq(100, 900, 100), 
                                seq(1000, 9000, 1000), 
                                seq(10000, 100000, 10000)),
                     labels = c(TeX("$10^1$"), 
                                "","","","","","","","",
                                TeX("$10^2$"), 
                                "","","","","","","","",
                                TeX("$10^3$"), 
                                "","","","","","","","",
                                TeX("$10^4$"), 
                                "","","","","","","","",
                                TeX("$10^5$")), 
                     trans = "log10",
                     limits = c(10, 100000)) +
  theme_pubr() + 
  theme(text = element_text(size = 15)); plt

ggsave(filename = "curve_fitting.png", plot = plt, dpi = 1200,
       width = 7, height = 6)

ggsave(filename = "curve_fitting_legend.png", plot = plt, dpi = 1200,
       width = 10, height = 6)
