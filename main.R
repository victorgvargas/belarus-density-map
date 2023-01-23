library(sf)
library(tidyverse)
library(stars)
library(rayshader)
library(MetBrewer)
library(colorspace)

data <- st_read("data/kontur_population_BY_20220630.gpkg")

bb <- st_bbox(data)

bottom_left <- st_point(c(bb[["xmin"]], bb[["ymin"]])) |>
    st_sfc(crs = st_crs(data))

bottom_right <- st_point(c(bb[["xmax"]], bb[["ymin"]])) |>
  st_sfc(crs = st_crs(data))

width <- st_distance(bottom_left, bottom_right)

top_left <- st_point(c(bb[["xmin"]], bb[["ymax"]])) |>
  st_sfc(crs = st_crs(data))

height <- st_distance(bottom_left, top_left)

if (width > height) {
  w_ratio <- 1
  h_ratio <- height / width
} else {
  h_ratio <- 1
  w_ratio <- width / height
}

c1 <- met.brewer("Homer2")

texture <- grDevices::colorRampPalette(c1, bias = 2)(256)
swatchplot(texture)

size <- 1000

belarus_rast <- st_rasterize(data, nx = floor(size * w_ratio), ny = floor(size * h_ratio))

mat <- matrix(belarus_rast$population, nrow = floor(size * w_ratio), ncol = floor(size * h_ratio))

mat |>
  height_shade(texture) |>
  plot_3d(heightmap = mat, zscale = 100, solid = FALSE, shadowdepth = 0)


render_camera(zoom = .8)

{
  outifle <- "images/final_plot.png"
  start_time <- Sys.time()
  cat(crayon::cyan(start_time), "\n")
  if(!file.exists(outfile)) {
    png::writePNG(matrix(1), target = outifle)
  }
  render_highquality(
    filename = outifle,
    interactive = FALSE,
    lightdirection = 280,
    lightaltitude = c(20,80),
    lightcolor = c(c1[2], "white"),
    lightintensity = c(600, 100),
    samples = 450,
    width = 6000,
    height = 6000
  )
  end_time <- Sys.time()
  diff <- end_time - start_time
  cat(crayon::cyan(diff), "\n")
}