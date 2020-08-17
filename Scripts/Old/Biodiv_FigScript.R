# Read me -----
# DRW edits to MC's script. This time we're plotting disease risk (EID) vs. land-use change (LUC)

# Loading packages ----
library(tidyverse)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(magick)

setwd("/Users/macuser/Documents/GitHub/WildlifeTradeNutrition")

# Importing Data ----
land_use_change <- read_csv("ProcessedData/LandDemandByCountry.csv")
biodiv <- read_csv("ProcessedData/Biodiversity Estimates 5June2020.csv")
world <- ne_countries(scale = "medium", returnclass = "sf")

# Bind the data together ----
world <- world %>%
  select(sov_a3, adm0_a3, name, continent, region_un, subregion, region_wb) %>%
  left_join(., land_use_change, by = c("adm0_a3" = "ISO3")) %>%
  left_join(., biodiv, by = c("adm0_a3" = "ISO3")) %>%
  mutate(biodiv_10year = Median_total_glob_trans + Median_total_glob_occ * 10) %>%
  mutate(biodiv_10year = as.numeric(biodiv_10year))

# Quick plot to see what's going on ----
# Option 1 for map
# ggplot(data = world,
#        aes(fill = biodiv_10year)) + 
#   geom_sf() + 
#   scale_fill_gradient(low = "#FF6699", high = "#FF33CC") +
#   coord_sf(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")  # Transforming borders to correct coordinate projection
# 
# ggsave(paste0(git.directory, "/Figs/Biodiv Fig 5June2020 Opt1.png"),
#        width = 10, height = 6)
# 
# # Reimporting and adding a unicorn
# biodiv.fig <- image_read(paste0(git.directory,'/Figs/Biodiv Fig 5June2020 Opt1.png'))
# unicorn.fig <- image_read(paste0(git.directory,'/Figs/unicorn_cc.png'))
# unicorn.fig <- image_trim(unicorn.fig) # Getting rid of margins
# unicorn.fig <- image_flop(unicorn.fig)
# 
# # And saving
# image_write(composite, paste0(git.directory,"/Figs/biodivfig_5June2020_opt1.png"))

  
# Option 2
# ggplot(data = world,
#        aes(fill = biodiv_10year)) + 
#   geom_sf() + 
#   scale_fill_gradientn(colours = c("#2B211F","#F1CFA3","#2B211F","#F1CFA3","#2B211F","#F1CFA3","#F0E6DE","#080506","#F0E6DE","#080506","#F0E6DE","#080506")) +
#   coord_sf(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs") +  # Transforming borders to correct coordinate projection
#   labs(fill = 'Biodiversity Loss, Using \nthe Coat Colour of\nGiraffes and Zebras\nas Inspiration')
# 
# ggsave(paste0(git.directory, "/Figs/Biodiv Fig 5June2020 Opt2.png"),
#        width = 10, height = 6)
# 
# # Appending giraffe
# # Reimporting and adding a unicorn
# biodiv.fig <- image_read(paste0(git.directory,'/Figs/Biodiv Fig 5June2020 Opt2.png'))
# giraffe.fig <- image_read(paste0(git.directory,'/Figs/funny-giraffe-pic.jpg'))
# giraffe.fig <- image_trim(giraffe.fig) # Getting rid of margins
# # giraffe.fig <- image_strip(giraffe.fig)
# 
# # Superimposing
# composite = image_composite(biodiv.fig, image_scale(giraffe.fig,"x700"), offset = "+75+800")
# # composite = image_strip(composite)
# # And saving
# image_write(composite, paste0(git.directory,"/Figs/biodivfig_5June2020_opt2.png"))


# Option 3
ggplot(data = world,
       aes(fill = biodiv_10year)) + 
  geom_sf() + 
  scale_fill_gradientn(colours = c("#e6e1bc","#eca24d",'#db4224'), na.value = "#DDDDDD") +
  guides(fill = guide_colourbar(title.position = 'top')) +
  coord_sf(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs") +  # Transforming borders to correct coordinate projection
  labs(fill = 'Biodiversity Loss\n(Species Destined for Extinction)') +
  theme(legend.position = 'bottom') +
  theme(legend.title = element_text(hjust = .5))

ggsave(paste0(git.directory, "/Figs/Biodiv Fig 5June2020 Opt3 Linear Colour Scale.png"),
       width = 10, height = 6)

# Option 4 - binning into low medium and high countries
world <-
  world %>%
  mutate(biodiv_cat = 
           ifelse(biodiv_10year %in% 0, 1,
                  ifelse(biodiv_10year > quantile(.$biodiv_10year,.66,na.rm=TRUE),3,2))) %>%
  mutate(biodiv_cat = ifelse(is.na(biodiv_cat),0,biodiv_cat)) %>%
  mutate(biodiv_cat = as.factor(biodiv_cat))

ggplot(data = world,
       aes(fill = biodiv_cat)) + 
  geom_sf() + 
  scale_fill_manual(values = c("#DDDDDD","#e6e1bc","#eca24d",'#db4224'),
                               labels = c('No Data','Low','Medium','High')) +
  guides(fill = guide_legend(title.position = 'top')) +
  coord_sf(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs") +  # Transforming borders to correct coordinate projection
  labs(fill = 'Biodiversity Loss') +
  theme(legend.position = 'bottom') +
  theme(legend.title = element_text(hjust = .5))

ggsave(paste0(git.directory, "/Figs/Biodiv Fig 5June2020 Opt4 Categories.png"),
       width = 10, height = 6)


###
# Scatterplot of biodiv vs land use
ggplot(dat = world,aes(x = country_extra_total_km, y = biodiv_10year, colour = factor(region_wb))) +
  geom_point() +
  scale_y_continuous(trans = 'log10', breaks = c(.001,.1,10), labels = c("0.001","1","10")) +
  scale_x_continuous(trans = 'log10') +
  labs(colour = 'World Region', x = 'Estimated Agricultural Land Expansion\n(Square km)',y = 'Estimated Biodiversity Impact\n(Species Destined for Extinction)') +
  theme_minimal()

ggsave(paste0(git.directory, "/Figs/Biodiv Fig 5June2020 Opt5.png"),
       width = 10, height = 6)
