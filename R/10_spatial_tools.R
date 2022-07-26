#Packages-----------------------------------------------------------------------
library("sf")
library(tmap)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthhires)
library("raster")
#Examing an sf code-------------------------------------------------------------
data(World)
tm_shape(World) +
  tm_borders()


#simple examination
names(World)
class(World)
dplyr::glimpse(World)

#plotting World
plot(World[1])
plot(World[,1])
plot(World[1,])
plot(World["pop_est"])

#geometries object - A key difference between data frames and sf objects is the presence of geometry, that looks like a column when printing the summary of any sf object
head(World[, 1:4])
class(World$geometry)
head(sf::st_coordinates(World))
no_geom <- sf::st_drop_geometry(World)
class(no_geom)
#bounding boxes
st_bbox(World)

#Manipulating sf objects--------------------------------------------------------
names(World)
unique(World$continent)
World %>%
  filter(continent == "South America") %>%
  tm_shape() +
  tm_borders()
World %>%
  mutate(our_countries = if_else(iso_a3 %in% c("COL","BRA", "MEX"), "red", "grey")) %>%
  tm_shape() +
  tm_borders() +
  tm_fill(col = "our_countries") +
  tm_add_legend("fill",
                "Countries",
                col = "red")

#Loading, ploting, and saving a shapefile---------------------------------------
bra <- ne_states(country = "brazil", returnclass = "sf")
plot(bra)
dir.create("data/shapefiles", recursive = TRUE)
st_write(obj = bra, dsn = "data/shapefiles/bra.shp", delete_layer = TRUE)

#Loading, ploting, and saving a raster------------------------------------------
dir.create(path = "data/raster/", recursive = TRUE)
tmax_data <- getData(name = "worldclim", var = "tmax", res = 10, path = "data/raster/")
plot(tmax_data)
dim(tmax_data)
