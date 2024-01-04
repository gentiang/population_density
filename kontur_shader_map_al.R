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

# Loading data
data <- st_read("kontur_data/kontur_population_AL_20231101.gpkg")
albania <- st_read("ALB_adm/ALB_adm3.shp") |> 
  st_transform(crs = st_crs(data))

# Map check
albania |> 
  ggplot() +
  geom_sf()

# Intersecting map with kontur data
st_albania <- st_intersection(data, albania)

# Define aspect ratio based on country bounding box
bb <- st_bbox(st_albania)

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

w_ratio <- 0.447

# Rasterizing so that we can convert to matrix
size <- 5000
size_small <- 1000
size_jumbo <- 6000
albania_rast <- st_rasterize(st_albania,
                            nx = floor(size_jumbo*w_ratio),
                            ny = floor(size_jumbo*h_ratio))

mat <- matrix(albania_rast$population,
              nrow = floor(size_jumbo*w_ratio),
              ncol = floor(size_jumbo*h_ratio))

# Color palette 1
c1 <- met.brewer("OKeeffe2")
swatchplot(c1)

texture <- grDevices::colorRampPalette(c1,
                                       bias = 2)(256)
swatchplot(texture)

# Plotting 3D map
mat |> 
  height_shade(texture = texture) |> 
  plot_3d(heightmap = mat,
          zscale = 60/(size_jumbo / 1000),
          #zscale = 100,
          solid = F,
          shadowdepth = 0)

render_camera(
  theta = -40,
  phi = 45,
  zoom = 0.75
)

#rgl::close3d()


{
  start_time <- Sys.time()
  cat(glue("Start Time: {start_time}"), "\n")
  
  render_highquality(
  filename = "images/albania_plot4.png",
  interactive = F,
  lightdirection = 280,
  lightaltitude = c(20, 80),
  lightcolor = c(c1[4], "white"),
  lightintensity = c(750, 50),
  samples = 450,
  width = size_jumbo,
  height = size_jumbo
)
  end_time <- Sys.time()
  cat(glue("Total time: {end_time - start_time}"), "\n")
  }


# Color palette 2
pal <- "miami"

c1 <- mixcolor(alpha = seq(from = 0, to = 1, by = .25), color1 =  hex2RGB("#00efff"), 
               color2 = hex2RGB("#ffffff")) |> 
  hex()
c2 <- mixcolor(alpha = seq(from = 0, to = 1, by = .25), color1 =  hex2RGB("#ff4992"), 
               color2 = hex2RGB("#ffffff")) |> 
  hex()

colors <- c(c1[1:4], rev(c2[1:4]))
swatchplot(colors)

texture <- grDevices::colorRampPalette(colors, 4)(256)

swatchplot(texture)

# Plotting 3D map
mat |> 
  height_shade(texture = texture) |> 
  plot_3d(heightmap = mat,
          zscale = 60/(size_jumbo / 1000),
          #zscale = 100,
          solid = F,
          shadowdepth = 0)

render_camera(
  theta = -40,
  phi = 45,
  zoom = 0.75
)

{
  start_time <- Sys.time()
  cat(glue("Start Time: {start_time}"), "\n")
  
  render_highquality(
    filename = "images/albania_plot5.png",
    interactive = F,
    light = TRUE,
    lightdirection = rev(c(140, 140, 150, 150)),
    lightcolor = c(colors[7], "white", colors[2], "white"),
    lightintensity = c(750, 50, 1000, 50),
    lightaltitude = c(10, 80, 10, 80),
    samples = 450,
    width = size_jumbo,
    height = size_jumbo,
    ground_material = rayrender::diffuse(color = colors[3])
  )
  end_time <- Sys.time()
  cat(glue("Total time: {end_time - start_time}"), "\n")
}

# Color palette 3
pal <- "golden_brown"

c1 <- natparks.pals("Acadia", n = 10)
c2 <- natparks.pals("Redwood", n = 10)

colors <- c(lighten(c2[1], .75),
            lighten(c2[1], .5),
            lighten(c2[1], .25), 
            c2[1], c1[10:6])

swatchplot(colors)



texture <- grDevices::colorRampPalette(colors, bias = 3)(256)

swatchplot(texture)


# Plotting 3D map
mat |> 
  height_shade(texture = texture) |> 
  plot_3d(heightmap = mat,
          zscale = 60/(size_jumbo / 1000),
          #zscale = 100,
          solid = F,
          shadowdepth = 0)

render_camera(
  theta = -40,
  phi = 45,
  zoom = 0.75
)

{
  start_time <- Sys.time()
  cat(glue("Start Time: {start_time}"), "\n")
  
  render_highquality(
    filename = "images/albania_plot6.png",
    interactive = F,
    lightdirection = rev(c(140, 140, 150, 150)),
    lightcolor = c(colors[4], "white", colors[7], "white"),
    lightintensity = c(750, 50, 1000, 50),
    lightaltitude = c(10, 80, 10, 80),
    samples = 450,
    width = size_jumbo,
    height = size_jumbo
  )
  end_time <- Sys.time()
  cat(glue("Total time: {end_time - start_time}"), "\n")
}