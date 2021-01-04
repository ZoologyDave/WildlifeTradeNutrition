# READ ME: ----
# Author: Hollie Booth
# Purpose: taking the processed data from LandUseCalcs_ForPublication.R, GenusDataPrep_ForPublication.R, and "EID country estimates.R"
# and producing Figure 2

rm(list=ls())

# Packages -----
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(ggrepel)

# Get data sets -----
rm(list=ls())
land <- read_csv("ProcessedData/LandDemandByCountry.csv")
food <- read_csv("Data/FoodSecurity.csv")
biodiv <- read_csv("ProcessedData/Biodiversity Estimates.csv")
eid <- read_csv("ProcessedData/EID By Country.csv")
protein <- read_csv("ProcessedData/AllGameConsumptionData.csv")


# Getting land and eid categories -----
land.eid <- 
  full_join(land, eid) %>%
  # Subset to only countries with extra land use
  filter(country_extra_total_km > 0) %>%
  # Bin countries into categories
  mutate(eid.cat = ifelse(eid > quantile(.$eid, .67,na.rm=TRUE),3,
                          ifelse(eid > quantile(.$eid, .33, na.rm = TRUE), 2, 1)),
         land.cat = ifelse(country_extra_total_km > quantile(.$country_extra_total_km, .67, na.rm = TRUE),3,
                           ifelse(country_extra_total_km > quantile(.$country_extra_total_km,.33, na.rm = TRUE),2,1))) %>%
  # Calculate total of EID and land
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

belfp$FS_rank <- as.numeric(belfp$FS_rank)
belfp$tot.cat <- as.factor(belfp$tot.cat)
levels(belfp$tot.cat)
# View(belfp)

# Creating colour palette ----
pal <- c('#db4224','#eca24d','#e6e1bc')

# Country labels -----
labels <- belfp %>% 
  filter(percent_game_pppd > 0,
         is.finite(percent_game_pppd),
         !is.na(tot.cat),
         !is.na(biodiv),
         !is.na(FS_rank)) %>%
  dplyr::select(COUNTRY) %>%
  dplyr::arrange(COUNTRY) %>%
  mutate(plot_country = case_when(COUNTRY == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
                                  COUNTRY == "United Republic of Tanzania" ~ "Tanzania",
                                  COUNTRY == "United States of America" ~ "USA",
                                  COUNTRY == "United Kingdom" ~ "UK",
                                  COUNTRY == "Czech Republic" ~ "Czechia",
                                  COUNTRY == "Bolivia (Plurinational State of)" ~ "Bolivia",
                                  COUNTRY == "Cote d'Ivoire" ~ "Côte d'Ivoire", # CIRCUMFLEXES MIGHT BREAK THINGS! 
                                  COUNTRY == "Sudan (former)" ~ "Sudan",
                                  COUNTRY %in% c("Norway", "Switzerland", "Finland", 
                                                 "Netherlands", "Belgium", "Denmark", 
                                                 "United Arab Emirates") ~ "", # Add any more you don't want to plot
                                  TRUE ~ COUNTRY))

belfp <- belfp %>%
  left_join(., labels)

# Plot -----
bubble_plot <- ggplot(data = belfp %>% 
                        filter(percent_game_pppd > 0,
                               is.finite(percent_game_pppd),
                               !is.na(tot.cat),
                               !is.na(biodiv)),
                      aes(x = FS_rank,
                          y = percent_game_pppd,
                          size = biodiv, 
                          fill = tot.cat)) + 
  geom_point(shape = 21) +
  scale_size_continuous(range = c(2,6)) + 
  scale_fill_manual(values = rev(pal), 
                    labels = c('Low','Medium','High')) +
  scale_y_continuous(limits = c(0,80), 
                     breaks = c(0,20,40,60,80),
                     labels = c(0,20,40,60,80)) +
  coord_cartesian(clip = 'off') +
  labs(x = "Food Insecurity Rank", 
       y = "Animal protein from wild meat (%)", 
       size = "Estimated biodiversity loss (species driven to extinction)", 
       fill = "Land Cover Change and EID Risk") +
  geom_text_repel(aes(label = plot_country), size = 3) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  guides(fill = guide_legend(title.position="top", title.hjust = 0.5, override.aes = list(size = 5)),
         size = guide_legend(title.position="top", title.hjust = 0.5))

ggsave(plot = bubble_plot,
       filename = "Figs/BubblePlot.pdf",
       width = 25, height = 15, units = 'cm')

