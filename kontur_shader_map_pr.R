# Loading Libraries
library(sf)
library(tidyverse)
library(elevatr)
library(rayshader)
library(glue)
library(colorspace)
library(tigris)
library(stars)
library(MetBrewer)
library(NatParksPalettes)
library(rayrender)

# Loading data
data <- st_read("kontur_data/kontur_population_XK_20231101.gpkg")
kosovo <- st_read("KO-_adm/KO__adm2.shp") |> 
  st_transform(crs = st_crs(data))

# Map check
kosovo |> 
  ggplot() +
  geom_sf()

# Intersecting map with kontur data
st_kosovo <- st_intersection(data, kosovo)
st_prishtina <- filter(st_kosovo, str_detect(VARNAME_2, "Prishtina"))

st_kosovo |> 
  group_by(VARNAME_2) |> 
  slice_max(order_by = population, n=1) |> 
  arrange(desc(population))
# Define aspect ratio based on country bounding box
bb <- st_bbox(st_prishtina)

bottom_left <- st_point(c(bb[["xmin"]], bb[["ymin"]])) |> 
  st_sfc(crs = st_crs(data))

bottom_right <- st_point(c(bb[["xmax"]], bb[["ymin"]])) |> 
  st_sfc(crs = st_crs(data))

top_left <- st_point(c(bb[["xmin"]], bb[["ymax"]])) |> 
  st_sfc(crs = st_crs(data))

top_right <- st_point(c(bb[["xmax"]], bb[["ymax"]])) |> 
  st_sfc(crs = st_crs(data))

width <- st_distance(bottom_left, bottom_right)
height <- st_distance(bottom_left, top_left)

if (width > height){
  w_ratio <- 1
  h_ratio <- height/width
} else {
  h_ratio <- 1
  w_ratio <- width/height
}

h_ratio <- 0.79

# Rasterizing so that we can convert to matrix
size <- 5000
size_small <- 1000
size_jumboc <- 5500
size_jumbo <- 6000
prishtina_rast <- st_rasterize(st_prishtina,
                            nx = floor(size_jumbo*w_ratio),
                            ny = floor(size_jumbo*h_ratio))

mat <- matrix(prishtina_rast$population,
              nrow = floor(size_jumbo*w_ratio),
              ncol = floor(size_jumbo*h_ratio))

# Color palette 1
pal <- "hiroshige"
c1 <- met.brewer("Hiroshige")
colors <- c1[c(5:1, 10:6)] |> rev()
swatchplot(colors)

texture <- grDevices::colorRampPalette(colors, bias = 2)(256)

swatchplot(texture)

# Plotting 3D map
mat |> 
  height_shade(texture = texture) |> 
  plot_3d(heightmap = mat,
          #zscale = 60/(size_jumbo / 1000),
          zscale = 1,
          solid = F,
          shadowdepth = 0)

render_camera(
  theta = 0,
  phi = 70,
  zoom = 0.75
)

{
  start_time <- Sys.time()
  cat(glue("Start Time: {start_time}"), "\n")
  
  render_highquality(
    filename = "images/prishtina_plot6.png",
    interactive = F,
    light = TRUE,
    lightdirection = rev(c(70, 70, 80, 80)),
    lightcolor = c(colors[3], "white", colors[7], "white"),
    lightintensity = c(750, 50, 1000, 50),
    lightaltitude = c(10, 80, 10, 80),
    samples = 450,
    width = size_jumbo,
    height = size_jumbo*h_ratio,
    ground_material = rayrender::diffuse(color = colors[1])
  )
  end_time <- Sys.time()
  cat(glue("End time: {end_time}"), "\n")
}

rgl::close3d()


# Color palette 2
pal <- "paquin"

c1 <- met.brewer("Paquin")
colors <- c1 |> rev()
swatchplot(colors)

texture <- grDevices::colorRampPalette(colors, bias = 4)(256)

swatchplot(texture)

# Plotting 3D map
mat |> 
  height_shade(texture = texture) |> 
  plot_3d(heightmap = mat,
          #zscale = 60/(size_jumbo / 1000),
          zscale = 2.5,
          solid = F,
          shadowdepth = 0)

render_camera(
  theta = 0,
  phi = 50,
  zoom = 0.75
)

{
  start_time <- Sys.time()
  cat(glue("Start Time: {start_time}"), "\n")
  
  render_highquality(
    filename = "images/prishtina_plot6.png",
    interactive = F,
    light = TRUE,
    lightdirection = rev(c(70, 70, 80, 80)),
    lightcolor = c(colors[3], "white", colors[7], "white"),
    lightintensity = c(750, 50, 1000, 50),
    lightaltitude = c(10, 80, 10, 80),
    samples = 450,
    width = size_jumbo,
    height = size_jumbo*h_ratio,
    ground_material = rayrender::diffuse(color = colors[5])
  )
  end_time <- Sys.time()
  cat(glue("End time: {end_time}"), "\n")
}

rgl::close3d()
