# Read me ----
# Author: Hollie Booth, David Williams
# Date: June 2020
# Purpose: Cleans up the data from the GENuS data base and combines it with UN population data 
#    so that we have the per country total of game protein consumption.
# 
# EDIT: Now also takes the data from Halpern et al 2019 and converts it to wildmeat protein
#    and combines with GENuS.
#
# NOTE: I (DW) am going with GENuS if some countries have both. 
#
# Packages -----
library(tidyverse)
library(dplyr)
library(tidyr)

options(tibble.width = Inf)
rm(list=ls())

# Preparing raw data from GENuS ######
# If you put a load of hashes at the end of a line, it allows "code folding" in RStudio.
# So if you look to the left of "Preparing raw data from GENus" there is a little arrow. 
# Click on that and it hides the code until the next set of hashes.
# Similarly, those hashes (or ----- ) allow you to navigate through code. If you look at 
#    bottom of this window, you can see a little drop-down menu that allows you to 
#    navigate to different bits of the script

# Load data ----
pops <- read_csv("Data/Pops.csv")
genus <- read_csv("Data/GenusNutrientsByFood.csv")

# Select medians for analysis and combine with populations -----
meds<- select(genus, ISO3, COUNTRY, contains("Median"))
b <- select(meds, contains("Median"))
meds$allprotien_percap <- rowSums(b, na.rm = TRUE)
meatmeds<- select(meds, ISO3, COUNTRY, contains("Meat"), contains("meat"), contains ("snails"), contains ("allprotien_percap"))

# add population estimates
d1 <- left_join(pops, meatmeds, by = "ISO3")

# rename
protein<- d1 %>% 
  rename(bovinepppd = 'Bovine MeatMediang/person/day',
         shoatpppd = 'Mutton & Goat MeatMediang/person/day',
         pigpppd = 'PigmeatMediang/person/day',
         poultrypppd = 'Poultry MeatMediang/person/day',
         birdpppd = 'Bird meat; nesMediang/person/day',
         horsepppd = `Horse meatMediang/person/day`,
         assespppd = `Meat of assesMediang/person/day`,
         mulespppd = `Meat of mulesMediang/person/day`,
         camelpppd = `Camel meatMediang/person/day`,
         rabbitpppd = `Rabbit meatMediang/person/day`,
         rodentspppd = `Meat of other rodentsMediang/person/day`,
         camelidspppd = `Meat of other camelidsMediang/person/day`,
         gamepppd = `Game meatMediang/person/day`,
         driednespppd = 'Meat; dried; nesMediang/person/day',
         nespppd = 'Meat; nesMediang/person/day',
         snailpppd = `Snails; not seaMediang/person/day`)

# Calculate game and total meat per country per year -----
# Game (in kgs)
protein$gamepcpa <- protein$gamepppd*protein$Pop*365.25/1000
# FYI, another way to do this, if you like the %>% notation:
# protein <- protein %>%
#   mutate(gamepcpa = gamepppd * Pop * 365.25/1000)
# Can sometimes be a bit clearer, sometimes not!

# calculate total meat per country per year (in kgs)
a <- select(protein, contains("pppd"))
protein$allmeatpppd <- rowSums(a, na.rm = TRUE)
protein$allmeatpcpa <- protein$allmeatpppd*protein$Pop*365.25/1000
protein$percent_game_pppd <- protein$gamepppd/protein$allmeatpppd*100
protein$protiennogame_ppd <- protein$allprotien_percap - protein$gamepppd

# Save the cleaned data -----
# (in a new directory to separate out raw from processed) 
write_csv(protein,
          path = "ProcessedData/GENusData_cleaned.csv")

# HALPERN DATA ------
# Clean up, load -----
rm(list=ls())
halpern <- read_csv("Data/Halpern2019_S2.csv")

# Convert from tonnes "wet" into tonnes protein -----
# I am using the unweighted means of Poore & Nemecek's data, specifically:
# (RW + EO) / HSCW --Table S5
# The protein content from Table S1
# 
# This translates as 13% protein 
#
halpern <- halpern %>%
  select(iso_a3, 
         game_t_per_year = Bushmeat) %>%
  mutate(game_protein_kg_extra = game_t_per_year * .13 * 1000) %>%
  filter(game_protein_kg_extra > 0)

write_csv(halpern, 
          path = "ProcessedData/Halpern2019BushmeatData.csv")




