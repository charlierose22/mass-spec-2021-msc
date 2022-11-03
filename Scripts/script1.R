# set working directory
setwd("~/GitHub/mass-spec-2021")

# load (and/or install) packages
library(tidyverse)
library(readr)

# import peak area df
PeakAreas <- read_excel("Raw Data/NaburnPeakAreas.xlsx")

# import cv df
NaburnCVs <- read_excel("Raw Data/NaburnCVs.xlsx")

# pivot both longer 
PeakAreaLong <- pivot_longer(PeakAreas,
                       !Name,
                       names_to = "TreatmentStage", 
                       values_to = "PeakAreas")
CVLong <- pivot_longer(NaburnCVs,
                             !Name,
                             names_to = "TreatmentStage", 
                             values_to = "CV")

Rearranged_PeakAreaLong <- mutate(PeakAreaLong, TreatmentStage = case_when(
  str_detect(TreatmentStage, "Feed Pump") ~ "A",
  str_detect(TreatmentStage, "Tank 1") ~ "B",
  str_detect(TreatmentStage, "Tank 2") ~ "C",
  str_detect(TreatmentStage, "Tank 3") ~ "D",
  str_detect(TreatmentStage, "Tank 4") ~ "E",
  str_detect(TreatmentStage, "Fresh") ~ "F",
  str_detect(TreatmentStage, "Dried") ~ "G",
  str_detect(TreatmentStage, "Output") ~ "H",
  str_detect(TreatmentStage, "Control") ~ "I"))

NoNA <- drop_na(Rearranged_PeakAreaLong)

# heatmaps
mycol <- c("navy", "blue", "cyan", "lightcyan", "yellow", "red", "red4")
ggplot(NoNA, aes(y = Name, x = TreatmentStage, fill = PeakAreas)) +
  geom_tile(aes(fill = PeakAreas)) +
  scale_fill_gradientn(colours = mycol, trans = "log") +
  labs(x = "Wastewater Treatment Stage", 
       y = "Compound", 
       fill = "Mean Peak Area") +
  theme_bw()
