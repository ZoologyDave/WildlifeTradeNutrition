# Playing with figure for food security impacts

# Packages -----
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)

rm(list=ls())

# Get data -----
prot <- read_csv("ProcessedData/GENusData_cleaned.csv")
fs <- read_csv("Data/FoodSecurity.csv")
d1 <- left_join(prot, fs, by = c("ISO3" = "ISO"))
d1$animalprotien_nogame_pppd <- d1$allmeatpppd - d1$gamepppd

# filter data by countries with >1% protien pppd from game -----
mostgame <- d1 %>%
  filter(percent_game_pppd > 1) %>%
  filter(allprotien_percap > 0) %>%
  filter(protiennogame_ppd != "NA") %>%
  filter(allprotien_percap != "NA") %>%
  filter(COUNTRY != "NA")

mostg_mod1 <- mostgame %>%
  select(COUNTRY, allprotien_percap, protiennogame_ppd) %>%
  pivot_longer(cols = -COUNTRY,
               names_to = "Scenario",
               values_to = "PerCapProtein")

mostg_mod <- left_join(mostg_mod1, prot, by = "COUNTRY")
mostg_mod$Population_millions <- mostg_mod$Pop / 1000000

# make plots -----

# with uniform dot sizes
ggplot(mostg_mod, 
       aes(x = reorder(COUNTRY, -PerCapProtein), 
           y = PerCapProtein,
           colour = Scenario)) + 
  geom_point(size = 2.2) +
  theme_bw() +
  theme(legend.position="bottom",
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold")) +
  scale_colour_manual(values = c("Grey65", "tomato1"),
                      labels = c("Current protien consumption", "Protien consumption without wild meat")) +
  labs (x = "Country", y = "National estimated protien intake per person per day (g)") +
  geom_hline(yintercept=56, linetype="dashed", color = "dodgerblue3") + 
  geom_hline(yintercept=46, linetype="dashed", color = "red")  +
  coord_flip() +
  annotate(geom="text", x=2, y=44, label="Minimum intake (women)", angle = 90, colour = "red", size = 3.5, hjust = 0) +
  annotate(geom="text", x=2, y=54, label="Minimum intake (men)", angle = 90, colour = "dodgerblue3", size = 3.5, hjust = 0) +
  scale_x_discrete(labels=c("United Republic of Tanzania" = "Tanzania",
                            "United States of America" = "USA",
                            "Sudan (former)" = "Sudan"))

# with Pop size dot sizes
ggplot(mostg_mod, 
       aes(x = reorder(COUNTRY, PerCapProtein), 
           y = PerCapProtein,
           colour = Scenario,
           size = Population_millions)) + 
  geom_point() +
  scale_colour_manual(values = c("Grey65", "tomato1"),
                      labels = c("Current protien consumption", "Protien consumption without wild meat")) + 
  labs (x = "Country", y = "National estimated protien intake per person per day (g)") +
  geom_hline(yintercept=56, linetype="dashed", color = "dodgerblue3") + 
  geom_hline(yintercept=46, linetype="dashed", color = "red") +
  theme_bw() +
  coord_flip() +
  annotate(geom="text", x=37, y=47, label="Minimum intake (women)", angle = 270, colour = "red", size = 3.5, hjust = 0) +
  annotate(geom="text", x=37, y=57, label="Minimum intake (men)", angle = 270, colour = "dodgerblue3", size = 3.5, hjust = 0) +
  scale_x_discrete(labels=c("United Republic of Tanzania" = "Tanzania",
                            "United States of America" = "USA",
                            "Sudan (former)" = "Sudan"))

# some other plots I tried but decided not to use -----
lowfs <- d1 %>%
  filter(FS_rank > 80) %>%
  filter(allprotien_percap > 0) %>%
  filter(protiennogame_ppd != "NA") %>%
  filter(allprotien_percap != "NA") %>%
  filter(allprotien_percap < 100)

f1 <- ggplot(lowfs, 
             aes (x = reorder(COUNTRY, allprotien_percap),
                  y = value, # You don't actually need this because you're defining "y" in each point
                  color = variable)) + # You don't need this either, again, it's defined below
  geom_point (aes(y = allprotien_percap, 
                  col = "allprotien_percap")) + # Also unnecessary — this variable is what you're plotting, 
  # not a colour / grouping variable
  geom_point (aes(y = protiennogame_ppd, col = "protiennogame_ppd")) +
  labs (x = "COUNTRY", y = "Total estimated protien intake per person per day") +
  geom_hline(yintercept=56, linetype="dashed", color = "blue") + 
  geom_hline(yintercept=46, linetype="dashed", color = "red") +
  theme_bw() +
  coord_flip()
f1

# DRW edit: so the issue here is that you're plotting separate variables for the different points
# Two ways you can do this:
ggplot(lowfs, 
       aes(x = reorder(COUNTRY, allprotien_percap))) + 
  geom_point(aes(y = allprotien_percap),
             # Add the colour manually here - you want all this variable to be coloured the same
             colour = "red") +
  geom_point(aes(y = protiennogame_ppd),
             # Add the colour manually here
             colour = "blue") +
  labs (x = "COUNTRY", y = "Total estimated protien intake per person per day") +
  geom_hline(yintercept=56, linetype="dashed", color = "blue") + 
  geom_hline(yintercept=46, linetype="dashed", color = "red") +
  theme_bw() +
  coord_flip()

# The more "ggplot" way of doing this, would be to reshape your data so that it is in the 
# "long form". It might be easier to draw arrows with the other way though?
lowfs_mod <- lowfs %>%
  select(COUNTRY, allprotien_percap, protiennogame_ppd) %>%
  pivot_longer(cols = -COUNTRY, 
               names_to = "scenario",
               values_to = "PerCapProtein") 

ggplot(lowfs_mod, 
       aes(x = reorder(COUNTRY, PerCapProtein), 
           y = PerCapProtein,
           colour = scenario)) + 
  geom_point() +
  scale_colour_manual(values = c("Red", "Blue"),
                      labels = c("Current", "Without wild meat")) + 
  labs (x = "COUNTRY", y = "Total estimated protien intake per person per day") +
  geom_hline(yintercept=56, linetype="dashed", color = "blue") + 
  geom_hline(yintercept=46, linetype="dashed", color = "red") +
  theme_bw() +
  coord_flip()


f2<- ggplot(lowfs, aes (x = reorder(COUNTRY, allmeatpppd), y = value, color = variable)) +
  geom_point (aes(y = allmeatpppd, col = "allmeatpppd")) +
  geom_point (aes(y = animalprotien_nogame_pppd, col = "animalprotien_nogame_pppd")) +
  labs (x = "COUNTRY", y = "Total estimated animal protien intake per person per day") +
  theme_bw() +
  coord_flip()
f2


