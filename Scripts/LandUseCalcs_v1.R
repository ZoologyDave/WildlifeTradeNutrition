# Read me ----
# Taking the LCA data from Poore & Nemecek (provided by Mike Clark) and linking it to estimates
# of wild meat consumption in each country
#
# This version uses region-specific LCAs
#
# Packages -----
library(tidyverse)
library(dplyr)
library(tidyr)
library(countrycode)

options(tibble.width = Inf)
rm(list=ls())

# Load LCA and GENus data -----
protein <- read_csv("ProcessedData/GENusData_cleaned.csv")

# First time: clean up lca data to match protein names
# lca <- read_csv("Data/Region Land Estimates.csv")
# lca <- lca %>%
#   mutate(Product = case_when(Food.Group == "Bovine Meat (Beef Herd)" ~ "Bovine Meat (beef herd)",
#                              Food.Group == "Mutton & Goat Meat" ~ "Lamb & Mutton",
#                              TRUE ~ Food.Group))
# write_csv(x = lca, path = "ProcessedData/RegionalLCA_cleaned.csv")
lca <- read_csv("ProcessedData/RegionalLCA_cleaned.csv")

# 1) Calculate proption of different meats in each country ----
types <- names(protein)
# grep searches for the string (e.g. "pppd" and returns the indices where it appears)
grep("pppd", types)
# grepl does the same but returns a logical for each position:
grepl("pppd", types)
# You then index by this (either grep or grepl should work here)
types[grep("pppd", types)] 
types <- types[grep("pppd", types)] 
types <- types[-grep("all", types)]
types <- types[-grep("game", types)]

# 1)a) Reassign weird meats to ones we have LCA data for -----
# Do this in two stages: 
# a) Reassign those we're sure of (birds --> poultry etc.)
# b) Reassign those we're not sure of proportionally

# a) Ones we're sure of:
lookup <- data.frame(MeatType = types) %>%
  arrange(MeatType) %>%
  mutate(Product = c("Bovine Meat (beef herd)", 
                     "Poultry Meat",
                     "Bovine Meat (beef herd)", 
                     "Bovine Meat (beef herd)", 
                     "Bovine Meat (beef herd)", 
                     NA, 
                     "Bovine Meat (beef herd)", 
                     "Bovine Meat (beef herd)", 
                     NA, 
                     "Pig Meat", 
                     "Poultry Meat",
                     "Pig Meat", 
                     "Pig Meat", 
                     "Lamb & Mutton", 
                     NA))
# Refs: 
# http://agritech.tnau.ac.in/animal_husbandry/animhus_rabbitbreed.html
# https://books.google.co.uk/books?id=CtD-6CPdZCgC&printsec=frontcover&source=gbs_ge_summary_r&cad=0#v=onepage&q&f=false
conventional_prot <- protein %>%
  select(ISO3, types) %>%
  pivot_longer(-ISO3,
               names_to = "MeatType",
               values_to = "PerPersonPerDay_g") %>%
  left_join(., lookup) %>%
  # Cut the ones we're NOT sure about
  filter(!is.na(Product)) %>%
  # Sum up across products
  group_by(ISO3, Product) %>%
  summarise(PerPersonPerDay_g = sum(PerPersonPerDay_g, na.rm = TRUE)) %>%
  # Summarise "undoes" one grouping --in this case 'Product'-- so we can now take the total
  mutate(Total = sum(PerPersonPerDay_g, na.rm = TRUE),
         Proportion = PerPersonPerDay_g / Total,
         check = sum(Proportion)) %>%
  ungroup()
  
# b) Ones we're not sure of. I don't think we need to worry about the fact that dried is dried
#    because everything is done in protein
unconventional_prot <- protein %>%
  select(ISO3, types) %>%
  pivot_longer(-ISO3,
               names_to = "MeatType",
               values_to = "PerPersonPerDay_g") %>%
  left_join(., lookup) %>%
  # Cut ones we ARE sure about
  filter(is.na(Product)) %>%
  # Sum up across products
  group_by(ISO3) %>%
  summarise(Unconv_pppd = sum(PerPersonPerDay_g, na.rm = TRUE)) %>%
  ungroup()

# Join to conventional and reassign
conventional_prot <- conventional_prot %>%
  left_join(., unconventional_prot) %>%
  mutate(PerPersonPerDay_g = PerPersonPerDay_g + Proportion * Unconv_pppd) %>%
  # Recalculate the total ()
  group_by(ISO3, Product) %>%
  summarise(PerPersonPerDay_g = sum(PerPersonPerDay_g, na.rm = TRUE)) %>%
  mutate(Total = sum(PerPersonPerDay_g, na.rm = TRUE),
         Proportion = PerPersonPerDay_g / Total,
         check = sum(Proportion)) %>%
  ungroup()
range(conventional_prot$check, na.rm = TRUE)

# 2) Assign game in these proportions -----
land_grab <- protein %>%
  select(ISO3, COUNTRY, gamepcpa) %>%
  left_join(conventional_prot, .) %>%
  filter(!is.na(gamepcpa)) %>%
  mutate(kg_extra = Proportion * gamepcpa)

# Get the region for each country -----
land_grab <- land_grab %>%
  mutate(Region = countrycode(ISO3, origin = "iso3c", destination = "region23"))

# # Which regions for missing data? [First time only] ----
# # Which regions are most similar?
# ggplot(data = lca %>%
#          filter(Food.Group %in% c("Bovine Meat (Beef Herd)",
#                                   "Pig Meat",
#                                   "Mutton & Goat Meat",
#                                   "Poultry Meat")),
#        aes(x = Region,
#            y = Crop_m2_kg_protein,
#            fill = Region)) +
#   facet_wrap(~Food.Group) +
#   geom_bar(stat = "identity") +
#   labs(x = NULL) +
#   theme(legend.position = c(1,1),
#         legend.justification = c(1,1),
#         axis.text.x = element_text(angle = 90)) +
#   guides(fill = guide_legend(ncol = 2))
# ggsave("Outputs/Prelim/LCAPerRegion_crop.png",
#        height = 20, width = 25, units = "cm")

# ggplot(data = lca %>%
#          filter(Food.Group %in% c("Bovine Meat (Beef Herd)",
#                                   "Pig Meat",
#                                   "Mutton & Goat Meat",
#                                   "Poultry Meat")),
#        aes(x = Region,
#            y = Pasture_m2_kg_protein, 
#            fill = Region)) + 
#   facet_wrap(~Food.Group) + 
#   geom_bar(stat = "identity") + 
#   labs(x = NULL) + 
#   theme(legend.position = c(1,0.4),
#         legend.justification = c(1,1),
#         axis.text.x = element_text(angle = 90)) + 
#   guides(fill = guide_legend(ncol = 2))
# ggsave("Outputs/Prelim/LCAPerRegion_pasture.png",
#        height = 20, width = 25, units = "cm")
# 

# Link these to the LCA data -----
land_grab <- land_grab %>%
  left_join(., select(lca,
                      Region, 
                      Product,
                      Pasture_m2_kg_protein:sd_Crop_m2_kg_protein))

# Assign global averages where data are missing
global_av <- lca %>%
  group_by(Product) %>%
  summarise(Pasture_m2_kg_protein = median(Pasture_m2_kg_protein, na.rm = TRUE),
            sd_Pasture_m2_kg_protein = median(sd_Pasture_m2_kg_protein, na.rm = TRUE),
            Crop_m2_kg_protein = median(Crop_m2_kg_protein, na.rm = TRUE),
            sd_Crop_m2_kg_protein = median(sd_Crop_m2_kg_protein, na.rm = TRUE)) %>%
  ungroup()

land_grab <- land_grab %>%
  filter(is.na(Pasture_m2_kg_protein)) %>%
  select(ISO3:Region) %>%
  left_join(., global_av) %>%
  bind_rows(., land_grab %>%
              filter(!is.na(Pasture_m2_kg_protein)))

# Get land grab into sensible units ---- 
land_grab <- land_grab %>%
  mutate(extra_pasture_m = kg_extra * Pasture_m2_kg_protein,
         extra_pasture_m_sd = kg_extra * sd_Pasture_m2_kg_protein,
         extra_crop_m = kg_extra * Crop_m2_kg_protein,
         extra_crop_m_sd = kg_extra * sd_Crop_m2_kg_protein,
         extra_total_m = extra_pasture_m + extra_crop_m,
         extra_total_m_sd = extra_pasture_m_sd + extra_crop_m_sd,
         extra_pasture_km = extra_pasture_m / 1000000,
         extra_pasture_km_sd = extra_pasture_m_sd / 1000000,
         extra_crop_km = extra_crop_m / 1000000,
         extra_crop_km_sd = extra_crop_m_sd / 1000000,
         extra_total_km = extra_total_m / 1000000,
         extra_total_km_sd = extra_total_m_sd / 1000000)

# Total up by country -----
land_grab_country <- land_grab %>%
  group_by(ISO3, COUNTRY) %>%
  summarise(country_extra_pasture_km = sum(extra_pasture_km, na.rm = TRUE),
            country_extra_crop_km = sum(extra_crop_km, na.rm = TRUE),
            country_extra_total_km = sum(extra_total_km, na.rm = TRUE)) %>%
  ungroup() 

write_csv(land_grab_country,
          "ProcessedData/LandDemandByCountry.csv")

# Make some figures ----
plot_data <- land_grab_country %>%
  arrange(-country_extra_total_km) %>%
  filter(country_extra_total_km > 0) %>%
  pivot_longer(cols = country_extra_pasture_km:country_extra_total_km,
               names_to = "land_type",
               values_to = "km_2") %>% 
  filter(land_type != "country_extra_total_km") %>%
  mutate(COUNTRY = factor(COUNTRY, levels = unique(COUNTRY), ordered = TRUE),
         land_type = gsub("(^.*extra_)(.*)(_km)", "\\2", land_type))
  
ggplot(data = plot_data,
       aes(x = COUNTRY, y = km_2, fill = land_type)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("gold", "green4")) + 
  labs(x = NULL, y = "Area required (km^2)", fill = NULL) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90),
        legend.position = c(.9, .9))

ggsave("Outputs/Prelim/LandGrab_v2.pdf",
       height = 15, width = 30, units = "cm")

ggsave("Outputs/Prelim/LandGrab_v2.png",
       height = 15, width = 30, units = "cm")
