##-------------STAT 394------------------##
#author: Laura Sudupe 
#email: laura.medinilla@kaust.edu.sa
#### This script is for the class matherial of 04.09.22

# install.packages("remotes")
library(remotes)
#remotes::install_github("wmgeolab/rgeoboundaries")

#https://rspatialdata.github.io/admin_boundaries.html
library(rgeoboundaries)
nigeria_boundary <- geoboundaries("Nigeria")

library(ggplot2)
ggplot(data = nigeria_boundary) +
  geom_sf()


#https://rspatialdata.github.io/osm.html
#install.packages("osmdata")
library(osmdata)
available_features()

lagos_bb <- getbb("Lagos")
lagos_bb

library(ggmap)
#install.packages("ggmap")
lagos_map <- get_map(lagos_bb, maptype = "roadmap")

lagos_hospitals <- lagos_bb %>%
  opq() %>%
  add_osm_feature(key = "amenity", value = "hospital") %>%
  osmdata_sf()

ggmap(lagos_map) +
  geom_sf(
    data = lagos_hospitals$osm_polygons,
    inherit.aes = FALSE,
    colour = "#08519c",
    fill = "#08306b",
    alpha = .5,
    size = 1
  ) +
  labs(x = "", y = "")

#####EXERCISE
#13.1 Download map Saudi Arabia
library(rgeoboundaries)
library(ggplot2)
library(viridis)

#install.packages("mapview")
devtools::install_github("ropenscilabs/rnaturalearth")
devtools::install_github("ropenscilabs/rnaturalearthdata", force = TRUE)
install.packages("rnaturalearthhires",
                 repos = "http://packages.ropensci.org",
                 type = "source")
library(mapview)
library(rnaturalearth)

# map <- rgeoboundaries::geoboundaries(country = "SAU")
map <- ne_countries(type = "countries", country = "Saudi Arabia", scale = "medium", returnclass = "sf")
plot(map)

 map1 <- geoboundaries("Saudi Arabia", "adm1")
map1 <- rnaturalearth::ne_states("Saudi Arabia", returnclass = "sf")
names(map1)



##########https://r-spatial.github.io/sf/articles/sf1.html

library(sf)
nc <- st_read(system.file("shape/nc.shp", package="sf"))
class(nc)
attr(nc, "sf_column")
class(nc$geometry)
print(nc[9:15], n = 3)
methods(class = "sf") 
