library(dplyr)
library(writexl)
library(readxl)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(scales)
library(latex2exp)

imp = read_excel(path = "impaction_parameter.xlsx", sheet = "Impaction")
dep = read_excel(path = "impaction_parameter.xlsx", sheet = "Deposition")

subjects = c(
  "PD01-18LPM",
  "PD01-45LPM",
  "PD07-18LPM",
  "PD07-45LPM",
  "PD03-18LPM",
  "PD03-45LPM",
  "PD02-18LPM",
  "PD02-45LPM",
  "PD08-18LPM",
  "PD08-45LPM",
  "PD20-18LPM",
  "PD20-45LPM",
  "PD21-18LPM",
  "PD21-45LPM",
  "PD09-18LPM",
  "PD09-45LPM",
  "PD19-18LPM",
  "PD19-45LPM",
  "PD10-18LPM",
  "PD10-45LPM",
  "PD13-18LPM",
  "PD13-45LPM"
)

imp_dep = cbind.data.frame(
  gather(imp, "ps", "imp") %>%
    mutate(ps = gsub("ps", "", ps)),
  gather(dep, "ps", "dep") %>%
    select(-ps)
) %>%
  mutate(subject_flow = rep(subjects, 30)) %>%
  mutate(subject = unlist(lapply(strsplit(subject_flow, split = "-"),function(x){x[1]}))) %>%
  mutate(flow = unlist(lapply(strsplit(subject_flow, split = "-"),function(x){x[2]}))) %>%
  mutate(imp_cm3 = imp/0.06) %>%
  mutate(dep_predict = (1-1/((3.5e-08)*(imp_cm3)^1.7+1))) %>%
  mutate(dep_bestfit = (1-1/((3.987e-08)*(imp_cm3)^1.746+1))) %>%
  select(subject, flow, ps, imp, dep, imp_cm3, dep_predict, dep_bestfit)

"
     General model:
     f(x) = 1 - (1/(a*x^b+1))
     Coefficients (with 95% confidence bounds):
       a =   3.987e-08  (3.082e-09, 7.665e-08)
       b =       1.746  (1.669, 1.823)
"

write.csv(imp_dep, file = "imp_dep.csv", quote = FALSE, row.names = FALSE)

# plt_leg1 = ggplot(data = imp_dep, aes(x = imp, y = dep)) + 
#   geom_point(aes(fill = subject, shape = flow, color = subject), size = 2) + 
#   scale_shape_manual(values = c(21, 22)) + 
#   scale_x_log10() + 
#   theme_pubclean() + 
#   theme(text = element_text(size = 15))
# plt_leg = get_legend(plt_leg)
# ggsave(filename = "legend.png", dpi = 1200, plot = plt_leg1,
#        width = 9)

df2 = data.frame(sex = rep(c("Best Fit", "Stahlhofen et al., 1989 (Q=31.2 L/min)"), each=3),
                 time=c("breakfeast", "Lunch", "Dinner"),
                 bill=c(10, 30, 15, 13, 40, 17) )
plt_leg2 = ggplot(df2, aes(x=time, y=bill, group=sex)) +
  geom_line(aes(linetype=sex), size = 1)+
  scale_linetype_manual(values = c("dotted", "solid")) + 
  geom_point()+
  theme(legend.position="top") +
  theme(text = element_text(size = 15)); plt_leg2
plt_leg2 = get_legend(plt_leg2)
ggsave(filename = "legend2.png", dpi = 1200, plot = plt_leg2,
       width = 6)

plt = ggplot(data = imp_dep, aes(x = imp, y = dep)) + 
  geom_point(aes(fill = subject, shape = flow), color = "black", size = 2) + 
  geom_line(aes(y = dep_predict), size = 1) + 
  geom_line(aes(y = dep_bestfit), size = 1, linetype = "dotted") +
  scale_shape_manual(values = c(21, 22)) + 
  scale_x_log10() + 
  # scale_y_continuous(labels = percent) + 
  xlab(TeX("Impaction Parameter, $d_a^2Q$(\\mu$m^2L$/min)")) + 
  ylab("Upper Airway Deposition Fraction") + 
  theme_pubr() + 
  theme(text = element_text(size = 15),
        legend.position = "none"); plt

ggsave(filename = "figure.png", dpi = 1200, plot = plt,
       width = 7, height = 5)
