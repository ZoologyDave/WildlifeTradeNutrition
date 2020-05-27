# Packages -----
library(tidyverse)
library(dplyr)
library(tidyr)

options(tibble.width = Inf)
rm(list=ls())

#################################
# Preparing raw data from GENuS #
#################################

pops <- read_csv("Data/Pops.csv")
genus <- read_csv("Data/GenusNutrientsByFood.csv")

# select medians only for analysis
meds<- select(genus, ISO3, COUNTRY, contains("Median"))
meatmeds<- select(meds, ISO3, COUNTRY, contains("Meat"), contains("meat"), contains ("snails"))

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

# calculate game meat per country per year (in kgs)
protein$gamepcpa <- protein$gamepppd*protein$Pop*365.25/1000

# calculate total meat per country per year (in kgs)
a <- select(protein, contains("pppd"))
protein$allmeatpppd <- rowSums(a, na.rm = TRUE)
protein$allmeatpcpa <- protein$allmeatpppd*protein$Pop*365.25/1000

protein$percent_game_pppd <- protein$gamepppd/protein$allmeatpppd*100
protein$percent_game_pcpa <- protein$gamepcpa/protein$allmeatpcpa*100

###################
# Land conversion #
###################

# Load LCA data -----
lca <- read_csv("Data/LCAdataforbushmeat.csv")

# Calculate proption of different meats in each country ----
types <- names(protein)
types <- types[grep("Meat", types)]
types <- types[-grep("All", types)]

protein$non_game_meat <- rowSums(protein[,types], na.rm = TRUE)

for(i in 1:length(types)){
  name <- paste("Prop",
                gsub("_median_g_capita_day", "", types[i]),
                sep = "_")
  protein[[name]] <- protein[[types[i]]] / protein$non_game_meat
                
}

# Check
unique(rowSums(protein[,grep("Prop", names(protein))], na.rm = TRUE))

# Assign game in these proportions -----
land_grab <- protein %>%
  select(ISO3, COUNTRY, Pop,
         Game_median_kg_country_year,
         grep("Prop", names(.))) %>%
  pivot_longer(-c(ISO3, COUNTRY, Pop, Game_median_kg_country_year),
               names_to = "MeatType",
               values_to = "Prop") %>%
  mutate(MeatType = gsub("Prop_", "", MeatType),
         kg_extra = Prop * Game_median_kg_country_year) %>%
  filter(!is.na(Game_median_kg_country_year))

# Link these to the LCA data -----
lookup <- data.frame(MeatType = unique(land_grab$MeatType)) %>%
  arrange(MeatType) %>%
  mutate(Product = c("Poultry Meat",
                     "Bovine Meat (beef herd)", 
                     "Bovine Meat (beef herd)", 
                     "Bovine Meat (beef herd)", 
                     NA, NA, 
                     "Lamb & Mutton", 
                     "Pig Meat", 
                     "Poultry Meat",
                     "Pig Meat", "Pig Meat", 
                     NA))

land_grab <- land_grab %>%
  left_join(.,lookup) %>%
  left_join(., lca) %>%
  mutate(extra_pasture_m = kg_extra * `Perm Past`,
         extra_arable_fallow_m = kg_extra * (Fallow + Arable),
         extra_total_m = kg_extra * Land_total_m_kg,
         extra_pasture_km = extra_pasture_m / 1000000,
         extra_arable_fallow_km = extra_arable_fallow_m / 1000000,
         extra_total_km = extra_total_m / 1000000)

# Total up by country -----
land_grab_country <- land_grab %>%
  group_by(ISO3, COUNTRY) %>%
  summarise(country_extra_pasture_km = sum(extra_pasture_km, na.rm = TRUE),
            country_extra_arable_fallow_km = sum(extra_arable_fallow_km, na.rm = TRUE),
            country_extra_total_km = sum(extra_total_km, na.rm = TRUE)) %>%
  ungroup() 

write_csv(land_grab_country,
          "Data/LandDemandByCountry.csv")
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

ggsave("Outputs/Prelim/LandGrab_v1.pdf",
       height = 15, width = 30, units = "cm")

