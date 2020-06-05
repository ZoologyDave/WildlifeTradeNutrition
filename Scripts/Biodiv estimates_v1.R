###
# libraries
library(plyr)
library(dplyr)
library(countrycode)

# Adding working directory
git.directory <- "/Users/macuser/Documents/GitHub/WildlifeTradeNutrition"

###
# data
# I don't know why I used caps for the folder names
land.dat <- read.csv(paste0(git.directory,"/ProcessedData/LandDemandByCountry.csv"), stringsAsFactors = FALSE)
land.dat <- land.dat %>% dplyr::rename(country_extra_arable_fallow_km = country_extra_crop_km)

bio.occ.dat_glob <- read.csv(paste0(git.directory,"/Data/Bio_estimates_occupation_global.csv"), stringsAsFactors = FALSE)
bio.trans.dat_glob <- read.csv(paste0(git.directory,"/Data/Bio_estimates_transformation_global.csv"), stringsAsFactors = FALSE)

bio.occ.dat_reg <- read.csv(paste0(git.directory,"/Data/Bio_estimates_occupation.csv"), stringsAsFactors = FALSE)
bio.trans.dat_reg <- read.csv(paste0(git.directory,"/Data/Bio_estimates_transformation.csv"), stringsAsFactors = FALSE)

###
# Adding ISO3 codes to country names in bio data sets
# And merging in land data set
bio.occ.dat_glob <-
  bio.occ.dat_glob %>%
  mutate(ISO3 = countrycode(Country.name, origin = 'country.name', destination = 'iso3c')) %>%
  left_join(.,land.dat)
bio.trans.dat_glob <-
  bio.trans.dat_glob %>%
  mutate(ISO3 = countrycode(Country.name, origin = 'country.name', destination = 'iso3c'))  %>%
  left_join(.,land.dat)

bio.occ.dat_reg <-
  bio.occ.dat_reg %>%
  mutate(ISO3 = countrycode(Country.name, origin = 'country.name', destination = 'iso3c')) %>%
  left_join(.,land.dat)
bio.trans.dat_reg <-
  bio.trans.dat_reg %>%
  mutate(ISO3 = countrycode(Country.name, origin = 'country.name', destination = 'iso3c'))  %>%
  left_join(.,land.dat)

###
# Getting global biodiversity estimates

# Occupation estimates
# E.g. impacts of chemical use in ag production
bio.occ.dat_glob <-
  bio.occ.dat_glob %>%
  mutate(Median_Mammals = country_extra_pasture_km * MedianPastureMammals + (MedianAnnual.cropsMammals + MedianPermanent.cropsMammals) / 2 * country_extra_arable_fallow_km * 1e6, # Multiplying out
         Median_Birds = country_extra_pasture_km * MedianPastureBirds + (MedianAnnual.cropsBirds + MedianPermanent.cropsBirds) / 2 * country_extra_arable_fallow_km  * 1e6, # 1e6 converts from sq km to sq m
         Median_Amphibians = country_extra_pasture_km * MedianPastureAmphibians + (MedianAnnual.cropsAmphibians + MedianPermanent.cropsAmphibians) / 2 * country_extra_arable_fallow_km * 1e6,
         Median_Reptiles = country_extra_pasture_km * MedianPastureReptiles + (MedianAnnual.cropsReptiles + MedianPermanent.cropsReptiles) / 2 * country_extra_arable_fallow_km * 1e6,
         Lower_Mammals = country_extra_pasture_km * lower.95.PastureMammals + (lower.95.Annual.cropsMammals + lower.95.Permanent.cropsMammals) / 2 * country_extra_arable_fallow_km * 1e6,
         Lower_Birds = country_extra_pasture_km * lower.95.PastureBirds + (lower.95.Annual.cropsBirds + lower.95.Permanent.cropsBirds) / 2 * country_extra_arable_fallow_km * 1e6,
         Lower_Amphibians = country_extra_pasture_km * lower.95.PastureAmphibians + (lower.95.Annual.cropsAmphibians + lower.95.Permanent.cropsAmphibians) / 2 * country_extra_arable_fallow_km * 1e6,
         Lower_Reptiles = country_extra_pasture_km * lower.95.PastureReptiles + (lower.95.Annual.cropsReptiles + lower.95.Permanent.cropsReptiles) / 2 * country_extra_arable_fallow_km * 1e6,
         Upper_Mammals = country_extra_pasture_km * upper.95.PastureMammals + (upper.95.Annual.cropsMammals + upper.95.Permanent.cropsMammals) / 2 * country_extra_arable_fallow_km * 1e6,
         Upper_Birds = country_extra_pasture_km * upper.95.PastureBirds + (upper.95.Annual.cropsBirds + upper.95.Permanent.cropsBirds) / 2 * country_extra_arable_fallow_km * 1e6,
         Upper_Amphibians = country_extra_pasture_km * upper.95.PastureAmphibians + (upper.95.Annual.cropsAmphibians + upper.95.Permanent.cropsAmphibians) / 2 * country_extra_arable_fallow_km * 1e6,
         Upper_Reptiles = country_extra_pasture_km * upper.95.PastureReptiles + (upper.95.Annual.cropsReptiles + upper.95.Permanent.cropsReptiles) / 2 * country_extra_arable_fallow_km * 1e6) %>%
  mutate(Median_total = Median_Mammals+Median_Birds+Median_Amphibians+Median_Reptiles, # Summing to get totals
         Lower_total = Lower_Mammals+Lower_Birds+Lower_Amphibians+Lower_Reptiles,
         Upper_total = Upper_Mammals+Upper_Birds+Upper_Amphibians+Upper_Reptiles) %>%
  filter(!is.na(Median_total)) %>% # Getting rid of countries we don't have land estimates for
  dplyr::select(ISO3, COUNTRY, country_extra_pasture_km, country_extra_arable_fallow_km, # Keeping only select columns
                Median_total_glob_occ = Median_total,Lower_total_glob_occ = Lower_total,Upper_total_glob_occ = Upper_total)

# Transformation estimates
# E.g. Direct impacts of land cover change
bio.trans.dat_glob <-
  bio.trans.dat_glob %>%
  mutate(Median_Mammals = country_extra_pasture_km * MedianPastureMammals + (MedianAnnual.cropsMammals + MedianPermanent.cropsMammals) / 2 * country_extra_arable_fallow_km * 1e6, # Multiplying out
         Median_Birds = country_extra_pasture_km * MedianPastureBirds + (MedianAnnual.cropsBirds + MedianPermanent.cropsBirds) / 2 * country_extra_arable_fallow_km  * 1e6, # 1e6 converts from sq km to sq m
         Median_Amphibians = country_extra_pasture_km * MedianPastureAmphibians + (MedianAnnual.cropsAmphibians + MedianPermanent.cropsAmphibians) / 2 * country_extra_arable_fallow_km * 1e6,
         Median_Reptiles = country_extra_pasture_km * MedianPastureReptiles + (MedianAnnual.cropsReptiles + MedianPermanent.cropsReptiles) / 2 * country_extra_arable_fallow_km * 1e6,
         Lower_Mammals = country_extra_pasture_km * lower.95.PastureMammals + (lower.95.Annual.cropsMammals + lower.95.Permanent.cropsMammals) / 2 * country_extra_arable_fallow_km * 1e6,
         Lower_Birds = country_extra_pasture_km * lower.95.PastureBirds + (lower.95.Annual.cropsBirds + lower.95.Permanent.cropsBirds) / 2 * country_extra_arable_fallow_km * 1e6,
         Lower_Amphibians = country_extra_pasture_km * lower.95.PastureAmphibians + (lower.95.Annual.cropsAmphibians + lower.95.Permanent.cropsAmphibians) / 2 * country_extra_arable_fallow_km * 1e6,
         Lower_Reptiles = country_extra_pasture_km * lower.95.PastureReptiles + (lower.95.Annual.cropsReptiles + lower.95.Permanent.cropsReptiles) / 2 * country_extra_arable_fallow_km * 1e6,
         Upper_Mammals = country_extra_pasture_km * upper.95.PastureMammals + (upper.95.Annual.cropsMammals + upper.95.Permanent.cropsMammals) / 2 * country_extra_arable_fallow_km * 1e6,
         Upper_Birds = country_extra_pasture_km * upper.95.PastureBirds + (upper.95.Annual.cropsBirds + upper.95.Permanent.cropsBirds) / 2 * country_extra_arable_fallow_km * 1e6,
         Upper_Amphibians = country_extra_pasture_km * upper.95.PastureAmphibians + (upper.95.Annual.cropsAmphibians + upper.95.Permanent.cropsAmphibians) / 2 * country_extra_arable_fallow_km * 1e6,
         Upper_Reptiles = country_extra_pasture_km * upper.95.PastureReptiles + (upper.95.Annual.cropsReptiles + upper.95.Permanent.cropsReptiles) / 2 * country_extra_arable_fallow_km * 1e6) %>%
  mutate(Median_total = Median_Mammals+Median_Birds+Median_Amphibians+Median_Reptiles, # Summing to get totals
         Lower_total = Lower_Mammals+Lower_Birds+Lower_Amphibians+Lower_Reptiles,
         Upper_total = Upper_Mammals+Upper_Birds+Upper_Amphibians+Upper_Reptiles) %>%
  filter(!is.na(Median_total)) %>% # Getting rid of countries we don't have land estimates for
  dplyr::select(ISO3, COUNTRY, country_extra_pasture_km, country_extra_arable_fallow_km, # Keeping only select columns
                Median_total_glob_trans = Median_total,Lower_total_glob_trans = Lower_total,Upper_total_glob_trans = Upper_total)


###
# Getting Regional biodiversity estimates

# Occupation estimates
# E.g. impacts of chemical use in ag production
bio.occ.dat_reg <-
  bio.occ.dat_reg %>%
  mutate(Median_Mammals = country_extra_pasture_km * MedianPastureMammals + (MedianAnnual.cropsMammals + MedianPermanent.cropsMammals) / 2 * country_extra_arable_fallow_km * 1e6, # Multiplying out
         Median_Birds = country_extra_pasture_km * MedianPastureBirds + (MedianAnnual.cropsBirds + MedianPermanent.cropsBirds) / 2 * country_extra_arable_fallow_km  * 1e6, # 1e6 converts from sq km to sq m
         Median_Amphibians = country_extra_pasture_km * MedianPastureAmphibians + (MedianAnnual.cropsAmphibians + MedianPermanent.cropsAmphibians) / 2 * country_extra_arable_fallow_km * 1e6,
         Median_Reptiles = country_extra_pasture_km * MedianPastureReptiles + (MedianAnnual.cropsReptiles + MedianPermanent.cropsReptiles) / 2 * country_extra_arable_fallow_km * 1e6,
         Lower_Mammals = country_extra_pasture_km * lower.95.PastureMammals + (lower.95.Annual.cropsMammals + lower.95.Permanent.cropsMammals) / 2 * country_extra_arable_fallow_km * 1e6,
         Lower_Birds = country_extra_pasture_km * lower.95.PastureBirds + (lower.95.Annual.cropsBirds + lower.95.Permanent.cropsBirds) / 2 * country_extra_arable_fallow_km * 1e6,
         Lower_Amphibians = country_extra_pasture_km * lower.95.PastureAmphibians + (lower.95.Annual.cropsAmphibians + lower.95.Permanent.cropsAmphibians) / 2 * country_extra_arable_fallow_km * 1e6,
         Lower_Reptiles = country_extra_pasture_km * lower.95.PastureReptiles + (lower.95.Annual.cropsReptiles + lower.95.Permanent.cropsReptiles) / 2 * country_extra_arable_fallow_km * 1e6,
         Upper_Mammals = country_extra_pasture_km * upper.95.PastureMammals + (upper.95.Annual.cropsMammals + upper.95.Permanent.cropsMammals) / 2 * country_extra_arable_fallow_km * 1e6,
         Upper_Birds = country_extra_pasture_km * upper.95.PastureBirds + (upper.95.Annual.cropsBirds + upper.95.Permanent.cropsBirds) / 2 * country_extra_arable_fallow_km * 1e6,
         Upper_Amphibians = country_extra_pasture_km * upper.95.PastureAmphibians + (upper.95.Annual.cropsAmphibians + upper.95.Permanent.cropsAmphibians) / 2 * country_extra_arable_fallow_km * 1e6,
         Upper_Reptiles = country_extra_pasture_km * upper.95.PastureReptiles + (upper.95.Annual.cropsReptiles + upper.95.Permanent.cropsReptiles) / 2 * country_extra_arable_fallow_km * 1e6) %>%
  mutate(Median_total = Median_Mammals+Median_Birds+Median_Amphibians+Median_Reptiles, # Summing to get totals
         Lower_total = Lower_Mammals+Lower_Birds+Lower_Amphibians+Lower_Reptiles,
         Upper_total = Upper_Mammals+Upper_Birds+Upper_Amphibians+Upper_Reptiles) %>%
  filter(!is.na(Median_total)) %>% # Getting rid of countries we don't have land estimates for
  dplyr::select(ISO3, COUNTRY, country_extra_pasture_km, country_extra_arable_fallow_km, # Keeping only select columns
                Median_total_reg_occ = Median_total,Lower_total_reg_occ = Lower_total,Upper_total_reg_occ = Upper_total)

# Transformation estimates
# E.g. Direct impacts of land cover change
bio.trans.dat_reg <-
  bio.trans.dat_reg %>%
  mutate(Median_Mammals = country_extra_pasture_km * MedianPastureMammals + (MedianAnnual.cropsMammals + MedianPermanent.cropsMammals) / 2 * country_extra_arable_fallow_km * 1e6, # Multiplying out
         Median_Birds = country_extra_pasture_km * MedianPastureBirds + (MedianAnnual.cropsBirds + MedianPermanent.cropsBirds) / 2 * country_extra_arable_fallow_km  * 1e6, # 1e6 converts from sq km to sq m
         Median_Amphibians = country_extra_pasture_km * MedianPastureAmphibians + (MedianAnnual.cropsAmphibians + MedianPermanent.cropsAmphibians) / 2 * country_extra_arable_fallow_km * 1e6,
         Median_Reptiles = country_extra_pasture_km * MedianPastureReptiles + (MedianAnnual.cropsReptiles + MedianPermanent.cropsReptiles) / 2 * country_extra_arable_fallow_km * 1e6,
         Lower_Mammals = country_extra_pasture_km * lower.95.PastureMammals + (lower.95.Annual.cropsMammals + lower.95.Permanent.cropsMammals) / 2 * country_extra_arable_fallow_km * 1e6,
         Lower_Birds = country_extra_pasture_km * lower.95.PastureBirds + (lower.95.Annual.cropsBirds + lower.95.Permanent.cropsBirds) / 2 * country_extra_arable_fallow_km * 1e6,
         Lower_Amphibians = country_extra_pasture_km * lower.95.PastureAmphibians + (lower.95.Annual.cropsAmphibians + lower.95.Permanent.cropsAmphibians) / 2 * country_extra_arable_fallow_km * 1e6,
         Lower_Reptiles = country_extra_pasture_km * lower.95.PastureReptiles + (lower.95.Annual.cropsReptiles + lower.95.Permanent.cropsReptiles) / 2 * country_extra_arable_fallow_km * 1e6,
         Upper_Mammals = country_extra_pasture_km * upper.95.PastureMammals + (upper.95.Annual.cropsMammals + upper.95.Permanent.cropsMammals) / 2 * country_extra_arable_fallow_km * 1e6,
         Upper_Birds = country_extra_pasture_km * upper.95.PastureBirds + (upper.95.Annual.cropsBirds + upper.95.Permanent.cropsBirds) / 2 * country_extra_arable_fallow_km * 1e6,
         Upper_Amphibians = country_extra_pasture_km * upper.95.PastureAmphibians + (upper.95.Annual.cropsAmphibians + upper.95.Permanent.cropsAmphibians) / 2 * country_extra_arable_fallow_km * 1e6,
         Upper_Reptiles = country_extra_pasture_km * upper.95.PastureReptiles + (upper.95.Annual.cropsReptiles + upper.95.Permanent.cropsReptiles) / 2 * country_extra_arable_fallow_km * 1e6) %>%
  mutate(Median_total = Median_Mammals+Median_Birds+Median_Amphibians+Median_Reptiles, # Summing to get totals
         Lower_total = Lower_Mammals+Lower_Birds+Lower_Amphibians+Lower_Reptiles,
         Upper_total = Upper_Mammals+Upper_Birds+Upper_Amphibians+Upper_Reptiles) %>%
  filter(!is.na(Median_total)) %>% # Getting rid of countries we don't have land estimates for
  dplyr::select(ISO3, COUNTRY, country_extra_pasture_km, country_extra_arable_fallow_km, # Keeping only select columns
                Median_total_reg_trans = Median_total,Lower_total_reg_trans = Lower_total,Upper_total_reg_trans = Upper_total)

###
# Merging results
out.dat <-
  left_join(bio.trans.dat_glob, bio.occ.dat_glob) %>%
  left_join(.,bio.trans.dat_reg) %>%
  left_join(.,bio.occ.dat_reg)

# Can't have negative biodiversity loss...
# Not sure why the authors of the biodiversity data set didn't do something about this?
out.dat[out.dat<0] <- 0

###
# Writing file
write.csv(out.dat,
          paste0(git.directory,"/ProcessedData/Biodiversity Estimates 5June2020.csv"),
          row.names = FALSE)

  
         
         
         
         
         
         
         
         
         
         
         
         
         