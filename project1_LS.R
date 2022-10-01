##-------------STAT 394------------------##
#author: Laura Sudupe 
#email: laura.medinilla@kaust.edu.sa
#### This script is for the first project code

###Download the data and check it
library(sf)
library(epitools)
library(SpatialEpi)
library(spData)
library(ggplot2)
library(leaflet)
library(spdep)
library(INLA)

data("nz")
class(nz)
names(nz)
head(nz)
?nz

head(nz$Land_area)
rownames(nz) <- nz$Name
summary(nz)

###Add data

nz$Island <- as.factor(nz$Island)
nz_region@data[["Population"]] <- as.integer(nz_region@data[["Population"]])
##https://rdrr.io/cran/SpatialEpi/src/R/expected.R
nz$side <- as.integer(ifelse(nz$Island == "North", 0, ifelse(nz$Island == "South", 1, "no data")))
####sex ratio transform 0 woman, 1 man
nz$Sex_ratio
#female:male
nz$sex <- ifelse(nz$Sex_ratio == 1, 'same', ifelse(nz$Sex_ratio > 1, 'female', "male"))

###expected cases
##area: name of the area
##Y: observed number of cases, in my case, median income
##E: expected number of cases in my case, expected median income
##quiero que dependa de
##population in each 
##SMR in each area
nz$e <- expected(population = nz$Population, cases = nz$Median_income, n.strata = 2)
nz$SMR <- nz$Median_income / nz$e

if (requireNamespace("sf", quietly = TRUE)) {
  library(sf)
  summary(nz)
  plot(nz)
}

###Data preparation

# convert to SpatialPolygonsDataFrame
map_spdf <- as_Spatial(nz, IDs = TRUE) ##EL BUENO
class(map_spdf)
#plot(map_spdf)

#https://blog.exploratory.io/making-maps-for-new-zealand-regions-and-territories-in-r-4400333df644
wgs84 = '+proj=longlat +datum=WGS84'
nz_region <- spTransform(map_spdf, CRS(wgs84))

###Mapping median income
le <- leaflet(nz_region) %>% addTiles()

pale <- colorNumeric(palette = "YlOrRd", domain = nz_region$SMR)

le %>% addPolygons(color = "grey", weight = 1, fillColor = ~pale(SMR), fillOpacity = 0.5) %>%
  addLegend(pal = pale, values = ~SMR, opacity = 0.5, title = "SMR", position = "bottomright")
######MAYBE ADD ANOTHER VARIABLE MAP

###Modelling
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
###expected cases
##area: name of the area
##Y: observed number of cases, in my case, median income
##E: expected number of cases in my case, expected median income
##quiero que dependa de 
##population in each 
##SMR in each area
formula <- Median_income ~ Sex_ratio + f(re_u, model = "besag", graph = ge, scale.model = TRUE) + f(re_v, model = "iid")

#https://groups.google.com/g/r-inla-discussion-group/c/EPTiPRE7jAM?pli=1 ERROR
rese <- inla(formula, family = "poisson", data = nz_region@data , E = e,
             control.predictor = list(compute = TRUE))

#names(inla.models()$likelihood)
###results
summary(rese)

marginale <- inla.smarginal(rese$marginals.fixed$Sex_ratio)
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
                  nz_region$Name, nz_region$Median_income,  round(nz_region$e, 2),  
                  nz_region$Sex_ratio, round(nz_region$SMR, 2),
                  round(nz_region$RR, 2), round(nz_region$LL, 2), round(nz_region$UL, 2)) %>%
  lapply(htmltools::HTML)

leaflet(nz_region) %>% addTiles() %>%
  addPolygons(color = "grey", weight = 1, fillColor = ~pale(RR),  fillOpacity = 0.5,
              highlightOptions = highlightOptions(weight = 4),
              label = labels,
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "15px", direction = "auto")) %>%
  addLegend(pal = pale, values = ~RR, opacity = 0.5, title = "RR", position = "bottomright")





