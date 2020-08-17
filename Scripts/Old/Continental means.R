# Quick look at regional/continental patterns

# Packages -----
library(tidyverse)
library(dplyr)
library(tidyr)
library(countrycode)
rm(list=ls())

# assign continents to countries
cont <- read_csv("Data/cont.csv")
prot <- read_csv("ProcessedData/GENusData_cleaned.csv")
d1 <- left_join(cont, prot, by = "ISO3")
d2<- filter(d1, gamepppd != "NA")

# continent averages
avs <- d2 %>%
  group_by(Continent_Name) %>%
  summarise(avg = mean(gamepppd),
            med = median(gamepppd),
            SD = sd(gamepppd)) %>%
  arrange(avg)
View(avs)

ggplot(data = d2,
       aes(x = reorder(Continent_Name, gamepppd, FUN = median), y = gamepppd, fill = Continent_Name)) +
      geom_boxplot() +
  labs( x = "Continent", y = "Mean wild meat consumption per person per day (g)")

# yes I know this is dodgey AF
lm1 <- lm(gamepppd ~ Continent_Name, data = d2)
summary(lm1)
