# Read me ----
# Author: Hollie Booth, David Williams
# Date: June 2020
# Purpose: Cleans up the data from the GENuS data base and combines it with UN population data 
#    so that we have the per country total of game protein consumption.
# 
# EDIT: Now also takes the data from FAO production and trade data, AND from Halpern et al 2019 
#    and converts it to wildmeat protein 
#
# NOTE: I (DW) am going with GENuS if some countries have both. 
#
# Packages -----
library(tidyverse)
library(countrycode)

options(tibble.width = Inf)
rm(list=ls())

# Preparing raw data from GENuS -----
# Load data ----
pops <- read_csv("Data/Pops.csv")
genus <- read_csv("Data/GenusNutrientsByFood.csv")

# Select medians for analysis and combine with populations -----
meds<- select(genus, ISO3, COUNTRY, contains("Median"))
b <- select(meds, contains("Median"))
# Calculate total protein consumption 
meds$allprotien_percap <- rowSums(b, na.rm = TRUE)
# Select meats and total protein
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

# Calculate game and total meat per country per year (pcpa) -----
# Game (in kgs)
protein$gamepcpa <- protein$gamepppd*protein$Pop*365.25/1000

# calculate total meat per country per year (in kgs)
a <- select(protein, contains("pppd"))
protein$allmeatpppd <- rowSums(a, na.rm = TRUE)
protein$allmeatpcpa <- protein$allmeatpppd*protein$Pop*365.25/1000
protein$percent_game_pppd <- protein$gamepppd/protein$allmeatpppd*100
protein$protiennogame_ppd <- protein$allprotien_percap - protein$gamepppd

# Save the cleaned data -----
write_csv(protein,
          path = "ProcessedData/GENusData_cleaned.csv")

# FAO Data (DW from here) -----
rm(list=ls())
fao_prod <- read_csv("Data/FAOGameProduction_2011.csv")
fao_trade <- read_csv("Data/FAOGameTrade_2011.csv")

# Calculate production plus imports minus exports -----
fao_trade <- fao_trade %>%
  mutate(ISO3 = countrycode(Area, 
                            origin = "country.name", 
                            destination = "iso3c")) %>%
  select(ISO3, Element, Value) %>%
  pivot_wider(names_from = Element,
              values_from = Value) %>%
  replace_na(list(`Import Quantity` = 0, 
                  `Export Quantity` = 0)) %>%
  mutate(balance_t = `Import Quantity` - `Export Quantity`)

fao_consumption <- fao_prod %>%
  mutate(ISO3 = countrycode(Area, 
                            origin = "country.name", 
                            destination = "iso3c")) %>%
  select(ISO3, production_t = Value) %>%
  full_join(., fao_trade %>%
              select(ISO3, balance_t)) %>%
  replace_na(list(production_t = 0,
                  balance_t = 0)) %>%
  mutate(consumption_t = production_t + balance_t,
         consumption_t = if_else(consumption_t < 0, 
                                 true = 0,
                                 false = consumption_t))
# HALPERN DATA ------
# Clean up, load -----
rm(fao_prod, fao_trade)
halpern <- read_csv("Data/Halpern2019_S2.csv")

# Any countries we don't have? ----
included <- fao_consumption %>%
  filter(consumption_t != 0)

halpern <- halpern %>%
  filter(Bushmeat >0,
         !(iso_a3 %in% included$ISO3)) # Yup

# Bind to FAO data and convert from tonnes "wet" into tonnes protein -----
# I am using the unweighted means of Poore & Nemecek's data, specifically:
# (RW + EO) / HSCW --Table S5
# The protein content from Table S1
# 
# This translates as 13% protein 
protein <- fao_consumption %>%
  select(ISO3, consumption_t) %>%
  mutate(source = "FAO trade balance sheets") %>%
  bind_rows(., halpern %>%
              mutate(source = "Halpern et al 2019") %>%
              select(ISO3 = iso_a3,
                     consumption_t = Bushmeat, 
                     source)) %>%
  mutate(game_protein_kg_extra = consumption_t * .13 * 1000) %>%
  filter(game_protein_kg_extra > 0)

write_csv(protein, 
          path = "ProcessedData/FAO_Halpern2019BushmeatData.csv")

# Combine the datasets to make things cleaner ----
rm(list=ls())
protein <- read_csv("ProcessedData/GENusData_cleaned.csv")
fao_halpern <- read_csv("ProcessedData/FAO_Halpern2019BushmeatData.csv")

protein <- protein %>%
  left_join(., select(fao_halpern,ISO3, game_protein_kg_extra, source)) %>%
  mutate(game_protein_g_extra_pppd = game_protein_kg_extra / Pop / 365.25 * 1000,
         # Take the GENuS data if present, otherwise use FAO / Halpern
         percent_game_pppd = ifelse(is.na(percent_game_pppd), 
                                    yes = game_protein_g_extra_pppd / allmeatpppd * 100, 
                                    no = percent_game_pppd),
         source = ifelse(is.na(source), 
                         yes = "GENuS database",
                         no = source))

write_csv(protein, 
          path = "ProcessedData/AllGameConsumptionData.csv")



