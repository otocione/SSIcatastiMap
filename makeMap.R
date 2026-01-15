library(tidyverse)
library(readxl)
library(sf)
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(geodata)

source("utils.R")

# tabella catasti
xlsxFile <- "data/localRegistries.xlsx"
tbCatasti <- read_xlsx(xlsxFile)

# Crea Mappa
sfMap <-
  # Geometrie delle regioni (GADM level=1)
  geodata::gadm(country = "ITA", level = 1, path = tempdir()) |>
  st_as_sf() |>
  st_transform(4326) |>
  # join tabella catasti
  left_join(tbCatasti, by=join_by(NAME_1)) |>
  # label
  mutate (labelHtml = paste(
    osr_region, ": <b>", osr_name, "</b>"
    )) |>
  # popup
  mutate (popupHtml = paste (
    sep = "<br/>",
    paste0( "<b>", osr_name, "</b>"),
    link_html("Sito web:\n", osr_webHome),
    link_html("Consultazione: ", osr_webGis)
    )) |>
  # colore poligoni
  mutate(color=case_when(
    !is.na(hasExport) & !is.na(hasExportGeoRef) ~ "green",
    !is.na(hasExport) ~ "yellow",
    .default = "red",
  ))

lf1 <-
  leaflet(
    sfMap,
    options = leafletOptions(
    preferCanvas = TRUE,
    minZoom = 6,
    maxZoom = 6)
    ) |>
  addProviderTiles(providers$CartoDB.Voyager)|>
  setView(lng = 12.5, lat = 42.0, zoom = 6) |>
  addPolygons(
    weight = 1,
    opacity = 1,
    fillOpacity = 0.5,
    label=lapply(as.list(sfMap$labelHtml), HTML),
    popup=lapply(as.list(sfMap$popupHtml), HTML),
    highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
    fillColor = ~color,
    labelOptions = labelOptions(
      direction = "auto",
      textsize = "12px",
      style = list("padding" = "2px 4px")
    )
  ) 

saveWidget(
  lf1,
  file = "catastiMap.html",
  selfcontained = TRUE
  )
