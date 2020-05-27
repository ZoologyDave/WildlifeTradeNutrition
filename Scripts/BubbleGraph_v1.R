rm(list=ls())

# Packages -----
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)

# Get data sets -----
land <- read_csv("ProcessedData/LandDemandByCountry.csv")
food <- read_csv("Data/FoodSecurity.csv")
protein <- read_csv("ProcessedData/GENusData_cleaned.csv")

# rename ISO in food
f1 <- food %>% 
  rename(ISO3 = 'ISO')

# Join data sets -----
fp <- left_join(protein, f1, by = "ISO3")
lfp <- left_join(fp, land, by = 'ISO3')

# Patronising Dave is patronising (1):
# If names match, you don't need "by", saving you whole seconds of typing
# Patronising Dave is patronising (2):
# You can avoid altering data by using "by" if names DON'T match
left_join(protein, food, by = c("ISO3" = "ISO"))

lfp$FS_rank <- as.numeric(lfp$FS_rank)
View(lfp)
# Hollie's first bubble plot -----
p1<- ggplot(data = lfp,
       aes(x = FS_rank,
           y = percent_game_pppd,
           size = country_extra_total_km)) + 
  geom_point() +
  labs(x = "Food Insecurity", y = "% of per capita animal protein from wild meat", size = "Est. land conversion (km2)") +
  geom_text(aes(label=COUNTRY.y), size=3, hjust = -0.4) +
  theme_bw()
p1
