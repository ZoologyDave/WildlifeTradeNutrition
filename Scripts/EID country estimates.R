# Read me: -----
# Author: Michael Clark
# Purpose: Estimating the summed EID risk by country

# Packages -----
library(raster)
library(plyr)
library(dplyr)
library(rnaturalearth)
library(countrycode)

# Importing EID Data -----
rm(list = ls())
eid.dat <- raster("Data/bsm_weight_pop fig3b.tif")

# Making a raster of countries -----
country.map <- rasterize(countries110, eid.dat, field = as.numeric(countries110$iso_n3), update = TRUE)

# Creating data frame, and summing by country
out.dat <-
  data.frame(eid = getValues(eid.dat),
             iso3n = getValues(country.map)) %>%
  group_by(iso3n) %>%
  summarise(eid = sum(eid, na.rm = TRUE)) %>%
  filter(iso3n == round(iso3n, digits = 0)) %>%
  mutate(ISO3 = countrycode(iso3n, origin = 'iso3n',destination = 'iso3c')) %>%
  mutate(eid_cat = ifelse(eid > quantile(.$eid,.67),'High',
                          ifelse(eid > quantile(.$eid,.33),'Medium','Low'))) %>%
  mutate(ISO3 = ifelse(iso3n %in% 729, 'SDN', ISO3))

# And writing file
write.csv(out.dat,
          file = "ProcessedData/EID By Country.csv", 
          row.names = FALSE)
