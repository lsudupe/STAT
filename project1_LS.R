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

le %>% addPolygons(color = "grey", weight = 1, fillColor = ~pale(Median_income), fillOpacity = 0.5,
                  highlightOptions = highlightOptions(weight = 4),
                  label = labels,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                              textsize = "15px", direction = "auto")) %>%
  addLegend(pal = pale, values = ~Median_income, opacity = 0.5, title = "SMR", position = "bottomright")

###Modelling
library(spdep)
library(INLA)
nb_nz <- poly2nb(nz_region)
head(nb_nz)
nb_nz[[1]] #the first county has 2 neighbours

nb2INLA("nz_region.adj", nb_nz)
ge <- inla.read.graph(filename = "nz_region.adj")

###inference using inla
#create variables
nz_region$re_u <- 1:nrow(nz_region@data)
nz_region$re_v <- 1:nrow(nz_region@data)
#formula
formula <- Sex_ratio ~ Median_income + f(re_u, model = "besag", graph = ge, scale.model = TRUE) + f(re_v, model = "iid")
#https://groups.google.com/g/r-inla-discussion-group/c/EPTiPRE7jAM?pli=1 ERROR
rese <- inla(formula, family = "xpoisson", data = nz_region@data, control.predictor = list(compute = TRUE))

###results
summary(rese)

library(ggplot2)
marginale <- inla.smarginal(rese$marginals.fixed$Median_income)
marginale <- data.frame(marginale)
ggplot(marginale, aes(x = x, y = y)) + geom_line() + labs(x = expression(beta[1]), y = "Density") +
  geom_vline(xintercept = 0, col = "blue") + theme_bw()
##IT IS NOT OKEY

###add results to map
head(rese$summary.fitted.values)

nz_region$RR <- rese$summary.fitted.values[, "mean"]
nz_region$LL <- rese$summary.fitted.values[, "0.025quant"]
nz_region$UL <- rese$summary.fitted.values[, "0.975quant"]

###mappring median income
pale <- colorNumeric(palette = "YlOrRd", domain = nz_region$RR)

labels <- sprintf("<strong> %s </strong> <br/> Observed: %s <br/> Expected: %s <br/>
                  Smokers proportion: %s <br/>SMR: %s <br/>RR: %s (%s, %s)",
                  nz_region$Name, nz_region$Sex_ratio,  #round(nz_region$E, 2),  
                  nz_region$Median_income, round(nz_region$Population, 2),
                  round(nz_region$RR, 2), round(nz_region$LL, 2), round(nz_region$UL, 2)) %>%
  lapply(htmltools::HTML)

leaflet(nz_region) %>% addTiles() %>%
  addPolygons(color = "grey", weight = 1, fillColor = ~pale(RR),  fillOpacity = 0.5,
              highlightOptions = highlightOptions(weight = 4),
              label = labels,
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "15px", direction = "auto")) %>%
  addLegend(pal = pal, values = ~RR, opacity = 0.5, title = "RR", position = "bottomright")






