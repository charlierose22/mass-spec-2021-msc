# set working directory
setwd("~/GitHub/mass-spec-2021")

# load (and/or install) packages
library(tidyverse)
library(readxl)
library(readr)
library(dplyr)

# import peak area df
NaburnPeakAreas <- read_excel("Raw Data/NaburnPeakAreas.xlsx", 
                              + n_max = 25)

# import cv df
NaburnCVs <- read_excel("Raw Data/NaburnCVs.xlsx")

# drop columns to keep only biosolids
Biosolid <- select(NaburnPeakAreas, -c("AD Feed Pump":"AD Tank 4"))

# pivot both longer 
PeakAreaLong <- pivot_longer(PeakAreas,
                             !Name,
                             names_to = "TreatmentStage", 
                             values_to = "PeakAreas")

CVLong <- pivot_longer(NaburnCVs,
                       !Name,
                       names_to = "TreatmentStage", 
                       values_to = "CV")

BiosolidLong <- pivot_longer(Biosolid,
                             !Name,
                             names_to = "TreatmentStage",
                             values_to = "PeakAreas")
# rename some columns
RenamedBiosolid <- mutate(BiosolidLong, TreatmentStage = case_when(
  str_detect(TreatmentStage, "Fresh") ~ "1 Fresh Biosolid",
  str_detect(TreatmentStage, "Dried") ~ "2 Aged Biosolid",
  str_detect(TreatmentStage, "Control") ~ "3 Control"))

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

NoNA <- drop_na(RenamedBiosolid)

# heatmaps
mycol <- c("navy","blue", "cyan", "light cyan", "yellow", "red", "red4")
ggplot(NoNA, aes(y = Name, x = TreatmentStage, fill = PeakAreas)) +
  geom_tile(aes(fill = PeakAreas)) +
  scale_fill_gradientn(colours = mycol, trans = "log") +
  labs(x = "Wastewater Treatment Stage", 
       y = "Antimicrobial Compound", 
       fill = "Mean Peak Area") +
  theme_bw()

ggplot(NoNA, aes(y = TreatmentStage, x = Name, fill = PeakAreas)) +
  geom_tile(aes(fill = PeakAreas)) +
  scale_fill_gradientn(colours = mycol, trans = "log") +
  labs(x = "Antimicrobial Compound", 
       y = "Wastewater Treatment Stage", 
       fill = "Mean Peak Area") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme_light()



