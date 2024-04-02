## Making a map of tisler reef 

packages <- c("devtools", "ggOceanMapsData", "ggOceanMaps")
## Install packages not yet installed
inst_packages <- packages %in% rownames(installed.packages())
if (any(!inst_packages)) {
  install.packages(packages[!inst_packages],
                   repos = c("https://cloud.r-project.org",
                             "https://mikkovihtakari.github.io/drat")
  )
}
## Load the packages to namespace
invisible(lapply(packages, library, character.only = TRUE))
library("ggspatial")
library("stringr")

basemap(60)

# Create a data frame for the marker

marker <- data.frame(Site = "Tisler Reef", lon = 10.96855, lat = 58.994583)

# Plotting - Norway

# start longitude, end longitude, minimum latitude, maximum latitude 
(norway <- basemap(limits = c(-7, 23, 70, 50), bathymetry = TRUE) +
  geom_spatial_point(data = marker,
                     aes(x = lon, y = lat, colour = Site, size = I(3))) +
  ggspatial::annotation_scale(location = "br") + 
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true", 
                                    height = unit(0.8, "cm"), width = unit(0.8, "cm")) +
  ylab(expression(Latitude~"("^degree*C*")")) +
  xlab(expression(Longitude~"("^degree*C*")")) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  theme(legend.title = element_blank(), 
        legend.position="right")
)

ggsave("/Users/ruby/Library/Mobile Documents/com~apple~CloudDocs/University/Dissertation/CWC-Dissolution /IMAGES/Norway.png", norway)

# Tisler Island
basemap(limits = c(58, 63, 12, 10))

# Tisler Reef

(tisler <- basemap(c(9,12,57,60), bathymetry = TRUE, 
        land.border.col = NA, grid.size = 0.05) +
  geom_spatial_point(data = marker,
                     aes(x = lon, y = lat, colour = Site, size = I(3))) +
  ggspatial::annotation_scale(location = "br") + 
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true", 
                                    height = unit(0.8, "cm"), width = unit(0.8, "cm")) +
  ylab(expression(Latitude~"("^degree*C*")")) +
  xlab(expression(Longitude~"("^degree*C*")")) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  theme(legend.title = element_blank(), 
       legend.position="top")
  
)

# Bathymetry Tisler Reef

install.packages("marmap")
library("marmap")

test <- marmap::getNOAA.bathy(lon1 = 9.5, lon2 = 11.5, 
                      lat1 = 58.5, lat2 = 59.5, resolution = 0)
# Arrange Plots 
library("cowplot")

plot_list <- list(norway, tisler)
plot_panel <- plot_grid(plotlist = plot_list, nrow = 1, ncol = 2)
print(plot_panel)

