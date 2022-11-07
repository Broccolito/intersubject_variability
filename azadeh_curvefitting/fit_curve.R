library(dplyr)
library(tidyr)
library(readxl)
library(writexl)

x = read_xlsx("All Data_Laminar_Turbulent.xlsx", sheet = "Sheet1")
y = read_xlsx("All Data_Laminar_Turbulent.xlsx", sheet = "Sheet2")

x = gather(data = x, key = "particle_size", value = "parameter", -SUBJECTS)
y = gather(data = y, key = "particle_size", value = "deposition", -SUBJECTS)

d = cbind.data.frame(x, select(y, deposition)) %>%
  rename("subjects" = "SUBJECTS") %>%
  mutate(flowrate = ifelse(grepl(pattern = "18LPM", subjects), 18, 45)) %>%
  mutate(subjects = strsplit(subjects, split = "-") %>%
           lapply(function(x){
             x[1]
           }) %>%
           unlist()) %>%
  mutate(particle_size = gsub("size", "", particle_size)) %>%
  mutate(particle_size = as.numeric(particle_size)) %>%
  select(subjects, particle_size, flowrate, parameter, deposition)

write.csv(d, file = "deposition.csv", quote = FALSE, row.names = FALSE)


