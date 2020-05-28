rm(list=ls())

# Packages -----
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(ggrepel)

# Get data sets -----
land <- read_csv("ProcessedData/LandDemandByCountry.csv")
food <- read_csv("Data/FoodSecurity.csv")
protein <- read_csv("ProcessedData/GENusData_cleaned.csv")
biodiv <- read_csv("ProcessedData/Biodiversity Estimates 28May2020.csv")


# rename ISO in food
f1 <- food %>% 
  rename(ISO3 = 'ISO')

# Join data sets -----
fp <- left_join(protein, f1, by = "ISO3")
lfp <- left_join(fp, land, by = 'ISO3')
blfp <- left_join(lfp, biodiv %>% dplyr::select(ISO3, biodiv = Median_total_glob_trans)) %>%
  mutate(biodiv = ifelse(is.na(biodiv), 0, biodiv)) # Converting NAs to 0s

# Patronising Dave is patronising (1):
# If names match, you don't need "by", saving you whole seconds of typing
# Patronising Dave is patronising (2):
# You can avoid altering data by using "by" if names DON'T match
left_join(protein, food, by = c("ISO3" = "ISO"))

blfp$FS_rank <- as.numeric(blfp$FS_rank)
View(blfp)

# Creating colour palette
pal1 <- c('#0a6165','#5bc2af','#e6e1bc')
pal1 <- colorRampPalette(pal1,bias = 1)
my.palette1 <- pal1(100)

pal2 <- c('#db4224','#eca24d','#e6e1bc')
pal2 <- colorRampPalette(pal2,bias = 1)
my.palette2 <- pal2(100)


# Hollie's first bubble plot -----
p1<- ggplot(data = blfp,
            aes(x = FS_rank,
                y = percent_game_pppd,
                size = country_extra_total_km, 
                fill = biodiv)) + 
  geom_point(shape = 21) +
  scale_fill_gradientn(colours = rev(my.palette1)) +
  scale_y_continuous(trans = 'log10') +
  labs(x = "Food Insecurity", y = "% of per capita animal protein from wild meat", 
       size = "Est. land conversion (km2)", fill = "Species Destined for Extinction") +
  # geom_text(aes(label=COUNTRY.y), size=3, hjust = -0.4) +
  geom_text_repel(aes(label = COUNTRY.y), size = 3) +
  theme_bw()
p1


p2<- ggplot(data = blfp,
            aes(x = FS_rank,
                y = percent_game_pppd,
                size = country_extra_total_km, 
                fill = biodiv)) + 
  geom_point(shape = 21) +
  scale_fill_gradientn(colours = rev(my.palette2)) +
  scale_y_continuous(trans = 'log10') +
  labs(x = "Food Insecurity", y = "% of per capita animal protein from wild meat", 
       size = "Est. land conversion (km2)", fill = "Species Destined for Extinction") +
  # geom_text(aes(label=COUNTRY.y), size=3, hjust = -0.4) +
  geom_text_repel(aes(label = COUNTRY.y), size = 3) +
  theme_bw()
p2
