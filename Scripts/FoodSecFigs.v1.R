# Playing with figure for food security impacts

# Packages -----
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)

rm(list=ls())

prot <- read_csv("ProcessedData/GENusData_cleaned.csv")
fs <- read_csv("Data/FoodSecurity.csv")

d1 <- left_join(prot, fs, by = "ISO3")
View(d1)
d1$animalprotien_nogame_pppd <- d1$allmeatpppd - d1$gamepppd

lowfs <- d1 %>%
  filter(FS_rank > 80) %>%
  filter(allprotien_percap > 0) %>%
  filter(protiennogame_ppd != "NA") %>%
  filter(allprotien_percap != "NA") %>%
  filter(allprotien_percap < 100)

f1 <- ggplot(lowfs, aes (x = reorder(Country, allprotien_percap), y = value, color = variable)) +
  geom_point (aes(y = allprotien_percap, col = "allprotien_percap")) +
  geom_point (aes(y = protiennogame_ppd, col = "protiennogame_ppd")) +
  labs (x = "Country", y = "Total estimated protien intake per person per day") +
  geom_hline(yintercept=56, linetype="dashed", color = "blue") + 
  geom_hline(yintercept=46, linetype="dashed", color = "red") +
  theme_bw() +
  coord_flip()
f1

f2<- ggplot(lowfs, aes (x = reorder(Country, allmeatpppd), y = value, color = variable)) +
  geom_point (aes(y = allmeatpppd, col = "allmeatpppd")) +
  geom_point (aes(y = animalprotien_nogame_pppd, col = "animalprotien_nogame_pppd")) +
  labs (x = "Country", y = "Total estimated animal protien intake per person per day") +
  theme_bw() +
  coord_flip()
f2

mostgame <- d1 %>%
  filter(percent_game_pppd > 1) %>%
  filter(allprotien_percap > 0) %>%
  filter(protiennogame_ppd != "NA") %>%
  filter(allprotien_percap != "NA") %>%
  filter(Country != "NA")


f3 <- ggplot(mostgame, aes (x = reorder(Country, allprotien_percap), y = value, color = variable)) +
  geom_point (aes(y = allprotien_percap, col = "allprotien_percap")) +
  geom_point (aes(y = protiennogame_ppd, col = "protiennogame_ppd")) +
  labs (x = "Country", y = "Total estimated protien intake per person per day") +
  geom_hline(yintercept=56, linetype="dashed", color = "blue") + 
  geom_hline(yintercept=46, linetype="dashed", color = "red") +
  theme_bw() +
  coord_flip()
f3
