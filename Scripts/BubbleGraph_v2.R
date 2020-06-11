rm(list=ls())

# Packages -----
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(ggrepel)

setwd("/Users/macuser/Documents/GitHub/WildlifeTradeNutrition/")

# Get data sets -----
land <- read_csv("ProcessedData/LandDemandByCountry.csv")
food <- read_csv("Data/FoodSecurity.csv")
protein <- read_csv("ProcessedData/GENusData_cleaned.csv")
biodiv <- read_csv("ProcessedData/Biodiversity Estimates 5June2020.csv")
eid <- read_csv("Data/EID By Country.csv")

# Getting land and eid categories
land.eid <- 
  full_join(land, eid) %>%
  filter(country_extra_total_km > 0) %>%
  mutate(eid.cat = ifelse(eid > quantile(.$eid, .67,na.rm=TRUE),3,
                          ifelse(eid > quantile(.$eid, .33, na.rm = TRUE), 2, 1)),
         land.cat = ifelse(country_extra_total_km > quantile(.$country_extra_total_km, .67, na.rm = TRUE),3,
                           ifelse(country_extra_total_km > quantile(.$country_extra_total_km,.33, na.rm = TRUE),2,1))) %>%
  mutate(land.eid.sum = eid.cat + land.cat) %>%
  mutate(tot.cat = ifelse(land.eid.sum >= 5, 3,
                          ifelse(land.eid.sum == 4, 2, 1)))


# rename ISO in food
f1 <- food %>% 
  rename(ISO3 = 'ISO')

# Join data sets -----
fp <- left_join(protein, f1)
elfp <- left_join(fp, land.eid %>% dplyr::select(ISO3, eid.cat, land.cat, tot.cat))
belfp <- left_join(elfp, biodiv %>% dplyr::select(ISO3, biodiv = Median_total_glob_trans)) %>%
  mutate(biodiv = ifelse(is.na(biodiv), 0, biodiv)) # Converting NAs to 0s


# Patronising Dave is patronising (1):
# If names match, you don't need "by", saving you whole seconds of typing
# Patronising Dave is patronising (2):
# You can avoid altering data by using "by" if names DON'T match
left_join(protein, food, by = c("ISO3" = "ISO"))

belfp$FS_rank <- as.numeric(belfp$FS_rank)
belfp$tot.cat <- as.factor(belfp$tot.cat)
levels(belfp$tot.cat)
View(belfp)

# Creating colour palette
# Only need to use colorRampPalette with continuous variables
pal1 <- c('#0a6165','#5bc2af','#e6e1bc')
# pal1 <- colorRampPalette(pal1,bias = 1)
# my.palette1 <- pal1(100)

pal2 <- c('#db4224','#eca24d','#e6e1bc')
# pal2 <- colorRampPalette(pal2,bias = 1)
# my.palette2 <- pal2(100)


# Hollie's first bubble plot -----
p1<- ggplot(data = belfp %>% 
              filter(percent_game_pppd > 0) %>%
              filter(!is.na(tot.cat)) %>%
              filter(!is.na(biodiv)),
            aes(x = FS_rank,
                y = percent_game_pppd,
                size = biodiv, 
                fill = tot.cat)) + 
  geom_point(shape = 21) +
  scale_fill_manual(values = rev(pal1), labels = c('Low','Medium','High')) +
  scale_y_continuous(trans = 'log10') +
  labs(x = "Food Insecurity Rank", y = "% of per capita animal protein from wild meat", 
       size = "Est. biodiversity loss (species driven to extinction)", fill = "Land Cover Change and EID Risk") +
  # geom_text(aes(label=COUNTRY.y), size=3, hjust = -0.4) +
  geom_text_repel(aes(label = COUNTRY), size = 3) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  guides(fill = guide_legend(title.position="top", title.hjust = 0.5),
         size = guide_legend(title.position="top", title.hjust = 0.5))
p1
# Saving
ggsave("Figs/Bubble Plot Colour 1 11June2020.pdf", 
       width = 10, height = 6, units = 'in')


p2<- ggplot(data = belfp %>% 
              filter(percent_game_pppd > 0) %>%
              filter(!is.na(tot.cat)) %>%
              filter(!is.na(biodiv)),
            aes(x = FS_rank,
                y = percent_game_pppd,
                size = biodiv, 
                fill = tot.cat)) + 
  geom_point(shape = 21) +
  scale_fill_manual(values = rev(pal2), labels = c('Low','Medium','High')) +
  scale_y_continuous(trans = 'log10') +
  labs(x = "Food Insecurity Rank", y = "% of per capita animal protein from wild meat", 
       size = "Est. biodiversity loss (species driven to extinction)", fill = "Land Cover Change and EID Risk") +
  # geom_text(aes(label=COUNTRY.y), size=3, hjust = -0.4) +
  geom_text_repel(aes(label = COUNTRY), size = 3) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  guides(fill = guide_legend(title.position="top", title.hjust = 0.5),
         size = guide_legend(title.position="top", title.hjust = 0.5))
p2
# Saving
ggsave("Figs/Bubble Plot Colour 2 11June2020.pdf", 
       width = 10, height = 6, units = 'in')
