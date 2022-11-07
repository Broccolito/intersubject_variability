library(dplyr)
library(ggplot2)
library(ggpubr)
library(latex2exp)
library(purrr)
library(readxl)
library(writexl)

d = read_excel(path = "Figure 7 data for Wanjun.xlsx", sheet = "Sheet2")

d$model = factor(d$model, levels = c("MPPD/CFD", "MPPD/Stahlhofen", "Experiments"))

d$value = d$value * 100

plt1 = ggplot(data = d, aes(x = parameter, y = value)) +
  geom_boxplot() + 
  geom_point(fill = "gray", color = "black", shape = 21, 
             size = 2, position = position_dodge2(0.2)) + 
  stat_compare_means(method = "t.test", 
                     comparisons = list(c("1 µm-slow", "1 µm-fast"),
                                          c("3 µm-slow", "3 µm-fast")),
                     symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                        symbols = c("****", "***", "**", "*", "NS"))) +
  ylab(TeX("Deposition (%)")) + 
  xlab("") + 
  ylim(c(0, 100)) + 
  facet_grid(.~model) + 
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), legend.position = "none")

plt2 = ggplot(data = d, aes(x = model, y = value)) +
  geom_boxplot() + 
  geom_point(fill = "gray", color = "black", shape = 21, 
             size = 2, position = position_dodge2(0.2)) + 
  # stat_compare_means(method = "anova", 
                     # comparisons = list(c("MPPD/CFD", "MPPD/Stahlhofen"),
                     #                    c("MPPD/Stahlhofen", "Experiments"),
                     #                    c("MPPD/CFD", "Experiments")),
                     # symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.005, 0.01, 1), 
                     #                    symbols = c("****", "***", "**", "*", "NS"))) +
  ylab(TeX("Deposition (%)")) + 
  xlab("") + 
  ylim(c(0, 100)) + 
  facet_grid(.~parameter) + 
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), legend.position = "none")

anova_result = aov(formula = value ~ model, data = filter(d, parameter == "1 µm-slow"))
TukeyHSD(anova_result)

anova_result = aov(formula = value ~ model, data = filter(d, parameter == "1 µm-fast"))
TukeyHSD(anova_result)

plt2
plt1

ggsave(filename = "figure7.1.png", plot = plt1, dpi = 1200, height = 4, width = 6)
ggsave(filename = "figure7.2.png", plot = plt2, dpi = 1200, height = 4, width = 6)
