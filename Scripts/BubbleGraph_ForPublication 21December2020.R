# READ ME: 
# Taking the processed data from LandUseCalcs_ForPublication.R, GenusDataPrep_ForPublication.R,
# and producing Figure 2

rm(list=ls())

# Packages -----
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(ggrepel)

git.directory <- "/Users/macuser/Documents/GitHub/WildlifeTradeNutrition/"

# Get data sets -----
land <- read_csv(paste0(git.directory,"ProcessedData/LandDemandByCountry.csv"))
food <- read_csv(paste0(git.directory,"Data/FoodSecurity.csv"))
biodiv <- read_csv(paste0(git.directory,"ProcessedData/Biodiversity Estimates 21December2020.csv"))
eid <- read_csv(paste0(git.directory,"Data/EID By Country.csv"))
protein <- read_csv(paste0(git.directory,"ProcessedData/AllGameConsumptionData.csv"))


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

# If names match, you don't need "by", saving you whole seconds of typing
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
                                  COUNTRY == "Cote d'Ivoire" ~ "Côte d'Ivoire", # THIS MIGHT BREAK THINGS! 
                                  COUNTRY == "Sudan (former)" ~ "Sudan",
                                  COUNTRY %in% c("Norway", "Switzerland", "Finland", 
                                                 "Netherlands", "Belgium", "Denmark", 
                                                 "United Arab Emirates") ~ "", # Add any more you don't want to plot
                                  TRUE ~ COUNTRY))

belfp <- belfp %>%
  left_join(., labels)

# Hollie's first bubble plot -----
p1<- ggplot(data = belfp %>% 
              filter(percent_game_pppd > 0) %>%
              filter(is.finite(percent_game_pppd)) %>%
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
# ggsave("Figs/Bubble Plot Colour 1 12June2020.pdf", 
       # width = 10, height = 6, units = 'in')

# Adding a random data point to increase point size
# This will need to be deleted in editing software
belfp <-
  rbind(belfp,
        belfp[1,] %>% mutate(biodiv = -5, FS_rank = 10, percent_game_pppd = 75, COUNTRY = 'DELETE ME'))

p2<- ggplot(data = belfp %>% 
              filter(percent_game_pppd > 0) %>%
              filter(is.finite(percent_game_pppd)) %>%
              filter(!is.na(tot.cat)) %>%
              filter(!is.na(biodiv)),
            aes(x = FS_rank,
                y = percent_game_pppd,
                size = biodiv, 
                fill = tot.cat)) + 
  geom_point(shape = 21) +
  scale_fill_manual(values = rev(pal2), labels = c('Low','Medium','High')) +
  scale_y_continuous(trans = 'log10', breaks = c(.001,.1,10),labels = c('0.001%','0.1%','10%')) +
  labs(x = "Food Insecurity Rank", y = "% of per capita animal protein from wild meat", 
       size = "Est. biodiversity loss (species driven to extinction)", fill = "Land Cover Change and EID Risk") +
  # geom_text(aes(label=COUNTRY.y), size=3, hjust = -0.4) +
  geom_text_repel(aes(label = COUNTRY), size = 3) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  guides(fill = guide_legend(title.position="top", title.hjust = 0.5, override.aes = list(size = 5)),
         size = guide_legend(title.position="top", title.hjust = 0.5))
p2
# Saving
dir.create(paste0(git.directory,"Figs/"))

ggsave(paste0(git.directory,"Figs/Bubble Plot Colour 2 21December2020.pdf"),
       width = 10, height = 6, units = 'in')

p2_linear<- ggplot(data = belfp %>% 
              filter(percent_game_pppd > 0) %>%
              filter(is.finite(percent_game_pppd)) %>%
              filter(!is.na(tot.cat)) %>%
              filter(!is.na(biodiv)) %>%
                filter(COUNTRY != 'DELETE ME'),
            aes(x = FS_rank,
                y = percent_game_pppd,
                size = biodiv, 
                fill = tot.cat)) + 
  geom_point(shape = 21) +
  scale_size_continuous(range = c(2,6)) + # This is to make the blobs bigger without using the "delete me" point
  # I think this is a more accurate reflection of the changes between the values too
  scale_fill_manual(values = rev(pal2), labels = c('Low','Medium','High')) +
  scale_y_continuous(limits = c(0,80), 
                     breaks = c(0,20,40,60,80),
                     labels = c(0,20,40,60,80)) +
  coord_cartesian(clip = 'off') +
  labs(x = "Food Insecurity Rank", 
       y = "Animal protein from wild meat (%)", 
       size = "Est. biodiversity loss (species driven to extinction)", 
       fill = "Land Cover Change and EID Risk") +
  # geom_text(aes(label=COUNTRY.y), size=3, hjust = -0.4) +
  geom_text_repel(aes(label = plot_country), size = 3) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  guides(fill = guide_legend(title.position="top", title.hjust = 0.5, override.aes = list(size = 5)),
         size = guide_legend(title.position="top", title.hjust = 0.5))


p2_linear
ggsave(paste0(git.directory,"Figs/Bubble Plot Colour 2 Linear Scale 21December2020.pdf"),
       width = 10, height = 6, units = 'in')
