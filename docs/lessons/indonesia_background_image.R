
library(rnaturalearth)
library(sf)
library(ggplot2)

indonesia <- ne_countries(scale = "large",
                          country = "Indonesia",
                          returnclass = "sf")

indonesia <- st_transform(indonesia, 3857)

bb8 <- st_bbox(indonesia)

xmid <- (bb8[["xmin"]] + bb8[["xmax"]])/2

dx <- 1.05 * (xmid - bb8[["xmin"]])

bb8["xmin"] <- xmid - dx
bb8["xmax"] <- xmid + dx

ymid <- (bb8[["ymin"]] + bb8[["ymax"]])/2

wide <- bb8[["xmax"]] - bb8[["xmin"]]

tall <- wide * (9/16)

bb8["ymin"] <- ymid - 1.4 * (ymid - bb8[["ymin"]])
bb8["ymax"] <- bb8[["ymin"]] + tall

gs_topo <- function(x, crs, width = 8000){

  # gs_check_crs(crs)

  # ArcGIS REST API
  map.service <- "http://services.arcgisonline.com/arcgis/rest/services/World_Physical_Map/MapServer/export?"

  # re-project bbox to make sure crs is right
  # bb8 <- gs_transform_bbox(x, crs)
  bb8 <- as.list(x)


  ### DEFINE EXPORT PARAMETERS ###
  # bbox
  xmin <- bb8$xmin
  ymin <- bb8$ymin
  xmax <- bb8$xmax
  ymax <- bb8$ymax

  bbox <- paste(xmin, ymin, xmax, ymax, sep = ",")
  bbox <- paste0("bbox=", bbox)

  crs <- st_crs(crs)

  # bbox spatial reference
  bboxSR <- paste0("bboxSR=", crs$epsg)

  # image spatial reference
  imageSR <- paste0("imageSR=", crs$epsg)

  # image format
  format <- "format=jpg"

  # size
  ratio <- (xmax - xmin) / (ymax - ymin)
  height <- width/ratio
  size <- paste0("size=", width, ",", height)

  # pixel
  pixel <- paste("&pixelType=U8",
                 "noDataInterpretation=esriNoDataMatchAny",
                 "interpolation=+RSP_BilinearInterpolation",
                 sep = "&")

  # output format
  output <- "f=image"

  # collect all parameters
  parameters <- paste(bbox, bboxSR, imageSR, format, size, pixel, output, sep = "&")


  ### WRITE URL ###
  url <- paste0(map.service, parameters)


  ### DOWNLOAD IMAGE ###
  path <- tempfile(fileext = ".jpg")

  httr::GET(url, httr::write_disk(path, overwrite = TRUE))


  ### RETURN ###
  topo <- jpeg::readJPEG(path, native = TRUE)

  return(topo)

}

topo <- gs_topo(bb8, crs = 3857)

cover <- st_difference(st_as_sfc(bb8), indonesia)

bob <-
  ggplot() +
  annotation_raster(topo,
                    bb8["xmin"],
                    bb8["xmax"],
                    bb8["ymin"],
                    bb8["ymax"]) +
  geom_sf(data = cover, fill = "white", alpha = 0.5) +
  geom_sf(data = indonesia, fill = "transparent", color = "black") +
  coord_sf(expand = FALSE) +
  theme_void()

bob

ggsave(plot = bob,
       filename = "lessons/figures/indonesia_background_image.png",
       width = 16,
       height = 9)
