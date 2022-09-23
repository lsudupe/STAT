##-------------STAT 394------------------##
#author: Laura Sudupe 
#email: laura.medinilla@kaust.edu.sa
#### This script is for the first project code

###Download the data and check it
library(sf)
library(spData)
data("nz")
class(nz)
names(nz)
head(nz)
?nz

head(nz$Land_area)
rownames(nz) <- nz$Name

###Data preparation

# convert to SpatialPolygonsDataFrame
map_spdf <- as_Spatial(nz, IDs = TRUE) ##EL BUENO
class(map_spdf)
plot(map_spdf)

#https://blog.exploratory.io/making-maps-for-new-zealand-regions-and-territories-in-r-4400333df644
wgs84 = '+proj=longlat +datum=WGS84'
nz_region <- spTransform(map_spdf, CRS(wgs84))

####
# convert to SpatialPolygons #https://r-spatial.github.io/sf/reference/coerce-methods.html
#class(nz)
#nz_sp <- as(st_geometry(nz), "Spatial",  )  ##I can not plot
#nz_sp <- SpatialPolygonsDataFrame(nz, match.ID = TRUE)
#class(nz_sp)

###Mapping median income
library(leaflet)
le <- leaflet(nz_region) %>% addTiles()

pale <- colorNumeric(palette = "YlOrRd", domain = nz_region$Median_income)

le %>% addPolygons(color = "grey", weight = 1, fillColor = ~pale(Median_income), fillOpacity = 0.5) %>%
  addLegend(pal = pale, values = ~Median_income, opacity = 0.5, title = "Median_income", position = "bottomright")


#Let's get the neighbors of each county. FUnction poly2nb() spdep library
library(spdep)
nb <- poly2nb(map)
head(nb)
nb[[1]] #the first county has 2 neighbours

d <- data.frame(county = names(map), neigh = rep(0, length(map)))
rownames(d) <- names(map)
map <- SpatialPolygonsDataFrame(map, d, match.ID = TRUE)
map$neigh[nb[[2]]] <- 1
map$neigh[nb[[6]]] <- 1
map$neigh[nb[[13]]] <- 1

##add different vars to my data
coord <- coordinates(map)
map$long <- coord[, 1]
map$lat <- coord[, 2]
map$ID <- 1:dim(map@data)[1]

##create the map
library(sf)
mapsf <- st_as_sf(map)
class(mapsf)
library(ggplot2)
ggplot(mapsf) + geom_sf(aes(fill = as.factor(neigh))) +
  geom_text(aes(long, lat, label = ID), color = "white") +
  theme_bw() + guides(fill = FALSE)



###############MODELIING###############
library(INLA)
library(spdep)


nb2INLA("map.adj", nb)
g <- inla.read.graph(filename = "map.adj")

##Inference using INLA

map$re_u <- 1:nrow(map@data)
map$re_v <- 1:nrow(map@data)

#formula
formula <- Median_income ~ Population + f(re_u, model = "besag", graph = g, scale.model = TRUE) + f(re_v, model = "iid")

nz$Median_income
res <- inla(formula, family = "poisson", data = map@data, E = E, control.predictor = list(compute = TRUE))

