###
# Installing libraries
# Probably don't need this many
library(raster)
library(rgdal)
library(gdalUtils)
library(RColorBrewer)
library(plyr)
library(dplyr)
library(countrycode)
library(ggplot2)
library(ggspatial)
library(scales)
library(cartogram)
library(cowplot)
library(rnaturalearth)
library(rnaturalearthdata)


# Importing Data
#
biodiv <- read_csv("ProcessedData/Biodiversity Estimates 28May2020.csv")
food <- read_csv("Data/FoodSecurity.csv")

# Data management
#
bio.food <-
  full_join(biodiv, # Merging data sets
            food %>% dplyr::rename(ISO3 = ISO)) %>%
  dplyr::select(COUNTRY, ISO3, Median_total_glob_trans, FS_score, FS_rank) %>% # Dropping columns
  mutate(ISO3N = countrycode(ISO3, origin = 'iso3c', destination = 'iso3n')) %>% # Adding iso3 numeric values for mapping
  mutate(biodiv = Median_total_glob_trans) %>% # Simplifying column name
  mutate(ISO3N = ifelse(ISO3N %in% 736, 729, ISO3N)) # Updating sudan... doing south sudan later

# Making four categories:
# 1 = low biodiv and protein impact
# 2 = high biodiv and low protein impact
# 3 = low biodiv and high protein
# 4 = high biodiv and high protein
# For now, basing these on median values
# But can update these later
bio.food <-
  bio.food %>%
  mutate(biodiv_score = ifelse(biodiv > quantile(.$biodiv, .67, na.rm=TRUE),3,
                               ifelse(biodiv > 0, 2, 1)), # 33rd percentile is 0. Setting medium risk countries have food insecurity risk > 0
         food_score = ifelse(FS_score < quantile(.$FS_score, .33, na.rm=TRUE),3,
                             ifelse(FS_score < quantile(.$FS_score, .67, na.rm=TRUE),2,1))) %>%
  mutate(bio.food_score = ifelse(biodiv_score %in% 1 & food_score %in% 1, 1,
                                 ifelse(biodiv_score %in% 1 & food_score %in% 2, 2,
                                        ifelse(biodiv_score%in% 1 & food_score %in% 3, 3,
                                               ifelse(biodiv_score %in% 2 & food_score %in% 1, 4,
                                                      ifelse(biodiv_score %in% 2 & food_score %in% 2, 5,
                                                             ifelse(biodiv_score %in% 2 & food_score %in% 3, 6,
                                                                    ifelse(biodiv_score %in% 3 & food_score %in% 1, 7,
                                                                           ifelse(biodiv_score %in% 3 & food_score %in% 2, 8, 9))))))))) %>%
  mutate(bio.food_score = ifelse(is.na(biodiv_score) | is.na(food_score), 0, bio.food_score))




# Making a function for mapping
# This will probably save me a bit of time
# DW - This is handy if you have a raster map that contains iso3n values
# Note that it does not work with read_csv objects
# These need to be converted to data frames using as.data.frame(dat)
# Before this function will work with them

map.fun <-
  function(dat,
           col.fill,
           ison,
           template.map) {
    
    # Saving map
    map.out <- template.map
    
    # Saving vectors
    fill.vect <- 
      dat[,col.fill]
    
    ison.vect <-
      dat[,ison]
    
    for(i in 1:length(ison.vect)) {
      map.out[country.map == ison.vect[i]] <-
        fill.vect[i]
    }
    
    return(map.out)
  }

# Importing map for making plots
# And managing it
#
country.map <-
  raster("Data/CountryMap.tif")

# Blank maps
# Doing this to avoid having to 0s to NAs for every plot
template.map <-
  country.map

template.map[!is.na(template.map)] <-
  NA

# Creating maps
#
biodiv.food.map <-
  map.fun(dat = bio.food %>% as.data.frame(.) %>% filter(!is.na(bio.food_score)),
          col.fill = 'bio.food_score',
          ison = 'ISO3N',
          template.map = template.map)

# Updating south sudan
biodiv.food.map[country.map %in% 728] <- 
  bio.food$bio.food_score[bio.food$ISO3N %in% 729]

# And updating other countries not in our data set
biodiv.food.map[!is.na(country.map) & is.na(biodiv.food.map)] <- 0


# Creating colour template
# These values will need to be changed
# But should give general idea
pal = c("#DDDDDD", # Grey. Countries not in data set
        "#e6e1bc", # Low for both
        "#eca24d", # Medium food insecurity, low biodiversity
        '#db4224', # High food insecurity, low biodiversity
        "#5bc2af", # Low food insecurity, medium biodiversity
        "#AD9A81", # Medium food insecurity, medium biodiversity
        "#A74A35", # High food insecurity, medium biodiversity
        '#0a6165', # Low food insecurity, high biodiversity
        "#3F5A55", # Medium food insecurity, high biodiversity
        "#735245") # High food insecurity, high biodiversity



# Getting world borders
#
world <- ne_countries(scale = "medium", returnclass = "sf")

###
# And plotting
bio.food.plot = ggplot() +
  layer_spatial(biodiv.food.map) +
  geom_sf(data = world, fill = NA) + # Adding country borders
  coord_sf(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs") + # Transforming borders to correct coordinate projection
  scale_fill_gradientn(colours = pal,
                       # limits = c(0,2.5),
                       # breaks = c(-20,-10,0,10,20),
                       # labels = c('-20','-10','0','10','20'),
                       na.value = 'white',
                       guide = guide_colourbar(direction = 'vertical', 
                                               title.position = 'top',
                                               title.hjust = 00),
                       breaks = c(1,2,3,4),
                       labels = c('Low Biodiversity, Food Secure',
                                  'High Biodiversity, Food Secure',
                                  'Low Biodiversity, Food Insecure',
                                  'High Biodiversity, Food Insecure')) +
  labs(fill = 'Biodiversity and Food Security Index') +
  theme_minimal() +
  geom_rect(aes(xmin = -14000000, xmax = -12333333,ymin = -7000000, ymax = -5333333), fill = pal[2]) + # These rectangles are for the legend
  geom_rect(aes(xmin = -12333333, xmax = -10666667,ymin = -7000000, ymax = -5333333), fill = pal[5]) +
  geom_rect(aes(xmin = -10666667, xmax = -9000000,ymin = -7000000, ymax = -5333333), fill = pal[8]) +
  geom_rect(aes(xmin = -14000000, xmax = -12333333,ymin = -5333333, ymax = -3666667), fill = pal[3]) +
  geom_rect(aes(xmin = -12333333, xmax = -10666667,ymin = -5333333, ymax = -3666667), fill = pal[6]) +
  geom_rect(aes(xmin = -10666667, xmax = -9000000,ymin = -5333333, ymax = -3666667), fill = pal[9]) +
  geom_rect(aes(xmin = -14000000, xmax = -12333333,ymin = -3666667, ymax = -2000000), fill = pal[4]) +
  geom_rect(aes(xmin = -12333333, xmax = -10666667,ymin = -3666667, ymax = -2000000), fill = pal[7]) +
  geom_rect(aes(xmin = -10666667, xmax = -9000000,ymin = -3666667, ymax = -2000000), fill = pal[10]) +
  # annotate(geom = 'text', label = 'Legend', x = -12500000, y = -1500000, hjust = .5, vjust = .5) + # Legend title
  annotate(geom = 'text', label = 'Biodiversity', x = -11500000, y = -9000000, hjust = .5, vjust = .5) + # biodiversity axis label
  annotate(geom = 'text', label = 'Food Insecurity', x = -17040096, y = -4500000, hjust = .5, vjust = .5, angle = 90) + # food security axis label
  annotate(geom = 'text', label = 'Low', x = -15500000, y = -5750000, hjust = .5, vjust = .5) + # "Low" on food security axis
  annotate(geom = 'text', label = 'Low', x = -12750000, y = -7750000, hjust = .5, vjust = .5) + # "Low" on biodiversity axis
  annotate(geom = 'text', label = 'High', x = -15500000, y = -3250000, hjust = .5, vjust = .5) + # "High" on food security axis
  annotate(geom = 'text', label = 'High', x = -10250000, y = -7750000, hjust = .5, vjust = .5) + # "High" on biodiversity axis
  theme(legend.position = 'none') +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title = element_text(size = 10, hjust = .5)) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

bio.food.plot

ggsave("Figs/Biodiv Food Map 3.pdf",
       units = 'in', width = 10, height = 10)