# Read me -----
# DRW edits to MC's script. This time we're plotting disease risk (EID) vs. land-use change (LUC)

# Loading packages ----
library(tidyverse)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

# Importing Data ----
land_use_change <- read_csv("ProcessedData/LandDemandByCountry.csv")
disease <- read_csv("ProcessedData/WIldlifeEIDs.csv")
world <- ne_countries(scale = "medium", returnclass = "sf")

# Bind the data together ----
world <- world %>%
  select(sov_a3, adm0_a3, name, continent, region_un, subregion, region_wb) %>%
  left_join(., land_use_change, by = c("adm0_a3" = "ISO3")) %>%
  left_join(., disease, by = c("adm0_a3" = "ISO")) 

# Quick plot to see what's going on ----
ggplot(data = world,
       aes(fill = country_extra_total_km)) + 
  geom_sf() + 
  coord_sf(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")  # Transforming borders to correct coordinate projection
  
ggplot(data = world,
       aes(fill = EID_effort)) + 
  geom_sf() + 
  coord_sf(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")  # Transforming borders to correct coordinate projection

# Categorise countries based on LUC and EID -----
world <- world %>%
  mutate(eid_class = ntile(EID_effort, n = 3),
         luc_class = ntile(country_extra_total_km, n = 3),
         classification = paste(eid_class, luc_class, sep = "_"),
         classification = factor(classification,
                                 levels = paste(rep(1:3, each = 3), 
                                                rep(1:3, 3), 
                                                sep = "_"),
                                 ordered = TRUE))

# Plot -----
luc_eid_plot <- ggplot(data = world,
       aes(fill = classification)) + 
  geom_sf() + 
  scale_fill_manual(values = c("#d1ece7", "#f097ca", "#e4237a",
                               "#8bd9c9", "#bf88db", "#bd6b97",
                               "#4dc5ae", "#5e9e93", "#550a78"), 
                    na.value = "grey90") + 
  coord_sf(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs") +
  theme_classic() + 
  labs(fill = "Current EID risk") + 
  theme(legend.position = "bottom",
        panel.grid.major = element_line(colour = "black")) + 
  guides(fill = guide_legend(ncol = 4, label = FALSE,
                             keywidth = 2, keyheight = 2,
                             title.position = "top", title.hjust = 0))

ggsave(luc_eid_plot, 
       filename = "Outputs/Prelim/LUCvsEID_grid_v1.pdf",
       height = 20, width = 30, units = "cm")

# Without grid lines
luc_eid_plot + 
  theme(panel.grid.major = element_blank(),
        legend.position = "none")
ggsave("Outputs/Prelim/LUCvsEID_nogrid_v1.pdf",
       height = 20, width = 30, units = "cm")
