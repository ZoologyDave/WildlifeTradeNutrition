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
# This didn't work for me: "Data/FoodSecurity.csv" didn't have "ISO3"
d1 <- left_join(prot, fs, by = c("ISO3" = "ISO"))

View(d1)
d1$animalprotien_nogame_pppd <- d1$allmeatpppd - d1$gamepppd

lowfs <- d1 %>%
  filter(FS_rank > 80) %>%
  filter(allprotien_percap > 0) %>%
  filter(protiennogame_ppd != "NA") %>%
  filter(allprotien_percap != "NA") %>%
  filter(allprotien_percap < 100)

f1 <- ggplot(lowfs, 
             aes (x = reorder(Country, allprotien_percap),
                  y = value, # You don't actually need this because you're defining "y" in each point
                  color = variable)) + # You don't need this either, again, it's defined below
  geom_point (aes(y = allprotien_percap, 
                  col = "allprotien_percap")) + # Also unnecessary — this variable is what you're plotting, 
  # not a colour / grouping variable
  geom_point (aes(y = protiennogame_ppd, col = "protiennogame_ppd")) +
  labs (x = "Country", y = "Total estimated protien intake per person per day") +
  geom_hline(yintercept=56, linetype="dashed", color = "blue") + 
  geom_hline(yintercept=46, linetype="dashed", color = "red") +
  theme_bw() +
  coord_flip()
f1

# DRW edit: so the issue here is that you're plotting separate variables for the different points
# Two ways you can do this:
ggplot(lowfs, 
       aes(x = reorder(Country, allprotien_percap))) + 
  geom_point(aes(y = allprotien_percap),
             # Add the colour manually here - you want all this variable to be coloured the same
             colour = "red") +
  geom_point(aes(y = protiennogame_ppd),
              # Add the colour manually here
              colour = "blue") +
  labs (x = "Country", y = "Total estimated protien intake per person per day") +
  geom_hline(yintercept=56, linetype="dashed", color = "blue") + 
  geom_hline(yintercept=46, linetype="dashed", color = "red") +
  theme_bw() +
  coord_flip()

# The more "ggplot" way of doing this, would be to reshape your data so that it is in the 
# "long form". It might be easier to draw arrows with the other way though?
lowfs_mod <- lowfs %>%
  select(Country, allprotien_percap, protiennogame_ppd) %>%
  pivot_longer(cols = -Country, 
               names_to = "scenario",
               values_to = "PerCapProtein") 

ggplot(lowfs_mod, 
       aes(x = reorder(Country, PerCapProtein), 
           y = PerCapProtein,
           colour = scenario)) + 
  geom_point() +
  scale_colour_manual(values = c("Red", "Blue"),
                      labels = c("Current", "Without wild meat")) + 
  labs (x = "Country", y = "Total estimated protien intake per person per day") +
  geom_hline(yintercept=56, linetype="dashed", color = "blue") + 
  geom_hline(yintercept=46, linetype="dashed", color = "red") +
  theme_bw() +
  coord_flip()


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

f3 <- ggplot(mostgame, aes (x = reorder(Country, allprotien_percap))) +
  # This is your issue - you're defining the colour inside the aes() - the aesthetics. 
  # So ggplot tries to define colour as something within the data called "red"
  geom_point (aes(y = allprotien_percap, colour = "red")) +
  geom_point (aes(y = protiennogame_ppd, colour  = "black")) +
  labs (x = "Country", y = "Total estimated protien intake per person per day") +
  geom_hline(yintercept=56, linetype="dashed", color = "blue") + 
  geom_hline(yintercept=46, linetype="dashed", color = "red") +
  theme_bw() +
  coord_flip() +
  scale_color_discrete(name = "Scenarios", labels = c("Current estimated intake", "Estimated intake without wild meat")) +
  annotate(geom="text", x=30, y=47, label="Minimum intake (women)", angle = 270, colour = "red", size = 3.5, hjust = 0) +
  annotate(geom="text", x=30, y=57, label="Minimum intake (men)", angle = 270, colour = "blue", size = 3.5, hjust = 0)
f3

# DRW edit:
ggplot(mostgame, 
       aes (x = reorder(Country, allprotien_percap))) +
  geom_point (aes(y = allprotien_percap, colour = "red")) 
# Look at the legend: ggplot has got confused and defined your colour variable as a variable
# called "colour" and with the value "red"
ggplot(mostgame, 
       aes (x = reorder(Country, allprotien_percap))) +
  geom_point (aes(y = allprotien_percap),
              colour = "red") 
# Outside the aesthetics arguments, you're defining all the colour as red, rather than 
# making ggplot look for a variable "colour"


  
