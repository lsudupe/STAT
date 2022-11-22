#mio primer intento

#library
library('spocc')
library("dplyr")
library(sp)
library(tmap)
library(raster)
library(rnaturalearth)
library(rmapshaper)
library(rgeos)
library(INLA)
library(ggplot2)



aa <- occ(query = 'Alcedo atthis', from = 'gbif',
          date = c("2000-01-01", "2020-12-31"),
          gbifopts = list(country = "ES"),
          has_coords = TRUE, limit = 1000)
names(aa)
a <- occ2df(aa)
summary(a)

#We can visualize the locations of sloths retrieved in Costa Rica using several mapping package

dpts_a <- SpatialPoints(a[, c("longitude", "latitude")])

#Then we create the map plotting the locations of dpts

tmap_mode("view")
tm_basemap() + tm_shape(dpts_a) + tm_dots()

#4 Spatial climatic covariates

rmonth_a <- getData(name = "worldclim", var = "tmin", res = 10)
#plot(rmonth_a)

rcov_a <- mean(rmonth_a)
#plot(rcov_a)

#5 Implementing and fitting the spatial point process model
#5.1 Log-Gaussian Cox process model
#5.2 Computational grid

map_a <- ne_countries(type = "countries", country = "Spain", scale = "medium")

###take out canary islands
map_a <- ms_filter_islands(map_a, min_area = 12391399903)
#plot(map_a)

##############3

resolution_a <- 0.20
r_a <- raster(map_a, resolution = resolution_a)
print(r_a)

(nrow_a <- nrow(r_a))
(ncol_a <- ncol(r_a))
nrow_a*ncol_a

#set to 0 the values of all the grid cells
r_a[] <- 0
tab_a <- table(cellFromXY(r_a, dpts_a))
tab_a

r_a[as.numeric(names(tab_a))] <- tab_a
#plot(r_a)

#Finally, we convert the raster r to a SpatialPolygonsDataFrame
grid_a <- rasterToPolygons(r_a)


#5.3 Data
#Now, we include in the SpatialPolygonsDataFrame grid the data needed for modeling
grid_a <- grid_a[as.vector(t(matrix(1:nrow(grid_a), nrow = ncol_a, ncol = nrow_a))), ]

grid_a$id <- 1:nrow(grid_a)
grid_a$Y <- grid_a$layer
grid_a$cellarea <- resolution_a*resolution_a

grid_a$cov <- extract(rcov_a, coordinates(grid_a))

#Finally, we delete the cells of grid that fall outside Costa Rica
#plot(grid)
gridmap_a <- raster::intersect(grid_a, map_a)
grid_a <- grid_a[grid_a$id %in% gridmap_a$id, ]
#plot(grid_a)
summary(grid_a)
#missing data take out
indNA_a <- which(is.na(grid_a$cov))
indNA_a
grid_a$cov[indNA_a] <- grid_a$cov[indNA_a+1]

#We use tmap to create maps of the number of sloths and 
#the covariate values in each of the cells. 

gridborder_a <- gUnaryUnion(grid_a)

tmap_mode("plot")
tm_shape(grid_a) +
  tm_polygons(col = c("Y", "cov"), border.col = "transparent") +
  tm_shape(gridborder_a) + tm_borders() +
  tm_facets(ncol = 2) + tm_legend(legend.position = c("left", "bottom"))

#5.4 Fitting the model using INLA

grid_a$id2 <- grid_a$id

formula <- Y ~ 1 + cov +
  f(id, model="rw2d", nrow = nrow_a, ncol = ncol_a) +
  f(id2, model="iid")

res_a <- inla(formula, family = "poisson", data = grid_a@data,
            E = cellarea_a, control.predictor = list(compute = TRUE))

#5.5 Results
summary(res_a)

marginal_a <- inla.smarginal(res_a$marginals.fixed$cov)
marginal_a <- data.frame(marginal_a)
ggplot(marginal_a, aes(x = x, y = y)) + geom_line() +
  labs(x = expression(beta[1]), y = "Density") +
  geom_vline(xintercept = 0, col = "black") + theme_bw()
nrow(res_a$summary.random$id)
grid_a$respa <- res_a$summary.random$id[grid_a$id, "mean"]

#We can also obtain the posterior mean of the unstructured random effect.
grid_a$reiid <- res_a$summary.random$id2[, "mean"]

#Then we can create maps of the random effects with tmap
tm_shape(grid_a) +
  tm_polygons(col = c("respa", "reiid"), style = "cont", border.col = "transparent")  +
  tm_shape(gridborder_a) + tm_borders() +
  tm_facets(ncol = 2) + tm_legend(legend.position = c("left", "bottom"))


cellarea_a <- resolution_a*resolution_a
grid_a$NE <- res_a$summary.fitted.values[, "mean"] * cellarea_a
grid_a$LL <- res_a$summary.fitted.values[, "0.025quant"] * cellarea_a
grid_a$UL <- res_a$summary.fitted.values[, "0.975quant"] * cellarea_a
summary(grid_a)

#We use tmap to create maps with the mean and lower and upper limits of 95% credible

tm_shape(grid_a) +
  tm_polygons(col = c("NE", "LL", "UL"),
              style = 'fixed', border.col = "transparent",
              breaks = c(0, 1, 2, ceiling(max(grid_a$UL)))) +
  tm_shape(gridborder_a) + tm_borders() +
  tm_facets(ncol = 3) + tm_legend(legend.position = c("left", "bottom")) 




