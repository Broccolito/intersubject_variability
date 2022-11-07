library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(plotly)

stk1 = read_excel(path = "stk_dep.xlsx", sheet = "stk1")
stk2 = read_excel(path = "stk_dep.xlsx", sheet = "stk2")
stk3 = read_excel(path = "stk_dep.xlsx", sheet = "stk3")
dep = read_excel(path = "stk_dep.xlsx", sheet = "dep")

stk_dep = cbind.data.frame(
  gather(stk1, "ps", "stk1",2:31)[,-3],
  stk1 = gather(stk1, "ps", "stk1",2:31)[,3],
  stk2 = gather(stk2, "ps", "stk2",2:31)[,3],
  stk3 = gather(stk3, "ps", "stk3",2:31)[,3],
  dep = gather(dep, "ps", "dep",2:31)[,3]
)

# write.csv(stk_dep, file = "stk_dep.csv", quote = FALSE, row.names = FALSE)
## Import into matlab and get correlation coefficients
"
f1 = 

     General model:
     f1(x) = 1 - exp(a*x)
     Coefficients (with 95% confidence bounds):
       a =      -35.81  (-37.81, -33.82)
>> f2

f2 = 

     General model:
     f2(x) = 1 - exp(a*x)
     Coefficients (with 95% confidence bounds):
       a =      -7.692  (-8.219, -7.164)
>> f3

f3 = 

     General model:
     f3(x) = 1 - exp(a*x)
     Coefficients (with 95% confidence bounds):
       a =      -31.13  (-32.89, -29.38)
"

stk_dep = stk_dep %>%
  mutate(dep_stk1 = 1-exp(stk1*(-35.81))) %>%
  mutate(dep_stk2 = 1-exp(stk2*(-7.692))) %>%
  mutate(dep_stk3 = 1-exp(stk3*(-31.13)))

plt = ggplot(data = stk_dep) +
  scale_x_log10() + 
  geom_point(aes(x = stk1, y = dep), color = "black") +
  geom_point(aes(x = stk2, y = dep), color = "blue") +
  geom_point(aes(x = stk3, y = dep), color = "red") +
  geom_line(aes(x = stk1, y = dep_stk1),color = "black", size = 0.8) + 
  geom_line(aes(x = stk2, y = dep_stk2), color = "blue", size = 0.8) + 
  geom_line(aes(x = stk3, y = dep_stk3), color = "red", size = 0.8) + 
  xlab("Stokes Number") + 
  ylab("Deposition Fraction") + 
  theme_pubr() + 
  theme(text = element_text(size = 15))

ggsave(filename = "dep_stk.png", plot = plt, device = "png", dpi = 1200)

ggplotly(plt)

### Calculating R square
rss_stk1 = sum((stk_dep$dep - stk_dep$dep_stk1)^2)
rss_stk2 =sum((stk_dep$dep - stk_dep$dep_stk2)^2)
rss_stk3 =sum((stk_dep$dep - stk_dep$dep_stk3)^2)
tss = sum(stk_dep$dep - mean(stk_dep$dep)^2)

rsq_stk1 = 1-(rss_stk1/tss)
rsq_stk2 = 1-(rss_stk2/tss)
rsq_stk3 = 1-(rss_stk3/tss)

"
> rsq_stk1
[1] 0.9048181
> rsq_stk2
[1] 0.8592848
> rsq_stk3
[1] 0.9011861
"


