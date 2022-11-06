############Packages
library(raster)
library(sf)
library(dplyr)
library(ggplot2)
library(INLA)
library(viridis)
library(dplyr)
library(ggplot2)
library(sf)


############Data
m <- getData(name = "GADM", country = "Poland", level = 0)
#plot(m1)
m <- m %>%
  st_as_sf() %>%
  st_cast("POLYGON") %>%
  mutate(area = st_area(.)) %>%
  arrange(desc(area)) %>%
  slice(1)
m <- m %>% st_transform(25830)
#ggplot(mi) + geom_sf() + theme_bw() + coord_sf(datum = st_crs(mi))

#####

d <- read.csv("./L.S.geo/data_p.csv")
d <- d[, c(
  "ReportingYear", "StationLocalId",
  "SamplingPoint_Longitude",
  "SamplingPoint_Latitude",
  "AQValue"
)]
names(d) <- c("year", "id", "long", "lat", "value")
d <- d[d$value >= 10, ]
d <- d[d$value <= 40,]
d <- d[!grepl("2013", d[,1]),]
d <- d[!grepl("2014", d[,1]),]
d <- d[!grepl("2015", d[,1]),]
d <- d[!grepl("2016", d[,1]),]
d <- d[!grepl("2017", d[,1]),]
d <- d[!grepl("2018", d[,1]),]


p <- st_as_sf(data.frame(long = d$long, lat = d$lat),
               coords = c("long", "lat"))

st_crs(p) <- st_crs(4326)
p <- p %>% st_transform(2180)

d[, c("x", "y")] <- st_coordinates(p)

ind <- st_intersects(m, p)
d <- d[ind[[1]], ]

ggplot(m) + geom_sf() + coord_sf(datum = st_crs(m)) +
  geom_point(data = d, aes(x = x, y = y)) + theme_bw()

ggplot(d) +
  geom_histogram(mapping = aes(x = value)) +
  facet_wrap(~year, ncol = 1) +
  theme_bw()

###
coo <- cbind(d$x, d$y)
bnd <- inla.nonconvex.hull(st_coordinates(m)[, 1:2])
mesh <- inla.mesh.2d(
  loc = coo, boundary = bnd,
  max.edge = c(100000, 200000), cutoff = 500
)
mesh$n
#plot(mesh)
#points(coo, col = "red")

############Building the SPDE model on the mesh
spde <- inla.spde2.pcmatern(
  mesh = mesh, alpha = 2, constr = TRUE,
  prior.range = c(10000, 0.01), # P(range < 10000) = 0.01
  prior.sigma = c(3, 0.01) # P(sigma > 3) = 0.01
)

#10.3.4 Index set
timesn <- length(unique(d$year))
indexs <- inla.spde.make.index("s",
                               n.spde = spde$n.spde
                               # ,n.group = timesn
)
lengths(indexs)


############Projection matrix
# group <- d$year - min(d$year) + 1
A <- inla.spde.make.A(mesh = mesh, loc = coo)

############Prediction data
bb <- st_bbox(m)
x <- seq(bb$xmin - 1, bb$xmax + 1, length.out = 50)
y <- seq(bb$ymin - 1, bb$ymax + 1, length.out = 50)
dp <- as.matrix(expand.grid(x, y))
#plot(dp, asp = 1)

p <- st_as_sf(data.frame(x = dp[, 1], y = dp[, 2]),
              coords = c("x", "y")
)
st_crs(p) <- st_crs(2180)
ind <- st_intersects(m, p)
dp <- dp[ind[[1]], ]
#plot(dp, asp = 1)

#dp <- rbind(cbind(dp, 1), cbind(dp, 2), cbind(dp, 3))
head(dp)

coop <- dp[, 1:2]
#groupp <- dp[, 1]
Ap <- inla.spde.make.A(mesh = mesh, loc = coop)

############Stack with data for estimation and prediction

stk.e <- inla.stack(
  tag = "est",
  data = list(y = d$value),
  A = list(1, A),
  effects = list(data.frame(b0 = rep(1, nrow(d))), s = indexs$s )
)

stk.p <- inla.stack(
  tag = "pred",
  data = list(y = NA),
  A = list(1, Ap),
  effects = list(data.frame(b0 = rep(1, nrow(dp))), s = indexs$s )
)

stk.full <- inla.stack(stk.e, stk.p)

############Model formula
rprior <- list(theta = list(prior = "pccor1", param = c(0, 0.9)))
formula <- y ~ 0 + b0 + f(s,
                          model = spde
                          
)

###########inla() call
#https://groups.google.com/g/r-inla-discussion-group/c/C8Ueyl1NaaE
res <- inla(formula,
            data = inla.stack.data(stk.full),
            control.inla=list(cmin=0),
            control.predictor = list(
              compute = TRUE,
              A = inla.stack.A(stk.full),
              link = 1), verbose=TRUE)



############Results
summary(res)
index <- inla.stack.index(stack = stk.full, tag = "pred")$data
pred_mean <- res$summary.fitted.values[index, "mean"]
pred_ll <- res$summary.fitted.values[index, "0.025quant"]
pred_ul <- res$summary.fitted.values[index, "0.975quant"]

dpm <- rbind(
  data.frame(
    east = coop[, 1], north = coop[, 2],
    value = pred_mean, variable = "pred_mean"
  ),
  data.frame(
    east = coop[, 1], north = coop[, 2],
    value = pred_ll, variable = "pred_ll"
  ),
  data.frame(
    east = coop[, 1], north = coop[, 2],
    value = pred_ul, variable = "pred_ul"
  )
)

dpm$variable <- as.factor(dpm$variable)

ggplot(dpm) + geom_tile(aes(east, north, fill = value)) +
  facet_wrap(~variable, nrow = 1) +
  coord_fixed(ratio = 1) +
  scale_fill_gradient(
    name = "Rainfall",
    low = "blue", high = "orange"
  ) +
  theme_bw()




