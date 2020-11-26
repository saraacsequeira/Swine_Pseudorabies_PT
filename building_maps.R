# Libraries
library(tidyverse)
library(colorspace)
library(ggplot2)
library(sf)
library(dplyr)
library(data.table)
library(DBI)
library(RMySQL)
library(lwgeom)

# Connection with MySQL database
connection <- dbConnect(RMariaDB :: MariaDB(),
                        dbname = 'Swine',
                        host = "localhost",
                        user = "root",
                        password = "projetoporcos")

# Read table from MYSQL
freguesias <- dbReadTable(connection, "st_tabela_freguesias")

# Table with dicofre, concelho, svl and dsavr
concelhos <- freguesias %>% select(dicofre, concelho, svl, dsavr)

## Remove the last 2 digits from dicofre
concelhos$dicofre <- substr(concelhos$dicofre, 1, nchar(concelhos$dicofre) - 2)
concelhos <- unique(concelhos)

# Different tables for the continent and the islands
continente <- concelhos %>% filter(svl != "DRADR AÇORES" & svl != "DRADR MADEIRA")
ilhas <- concelhos %>% filter(svl == "DRADR AÇORES" | svl == "DRADR MADEIRA")


# Download continent map with different freguesias (https://www.dgterritorio.gov.pt/cartografia/cartografia-tematica/caop)
temp_dir <- tempdir()
temp <- tempfile(tmpdir = temp_dir, fileext = ".zip")
download.file("http://mapas.dgterritorio.pt/ATOM-download/CAOP-Cont/Cont_AAD_CAOP2019.zip", destfile = temp)
unzip(temp, exdir = temp_dir)

## Table
cont_map <- read_sf(temp_dir)

## Convert SRS to WGS84
cont_geo <- st_as_sf(cont_map, 4326)
cont_geo <- st_transform(cont_geo, "+proj=longlat +datum=WGS84")

ggplot(cont_geo) + geom_sf()

## Add column with area
cont_geo$area <- st_area(cont_geo)

## Remove last 2 digits from DICOFRE
cont_geo$Dicofre <- substr(cont_geo$Dicofre, 1, nchar(cont_geo$Dicofre) - 2)

## Merge with concelho's table
cont_geo_concelhos <- merge(cont_geo, concelhos, by.x = "Dicofre", by.y = "dicofre", all = TRUE)

## Aggregate by LVS
cont_geo_lvs <- cont_geo_concelhos %>%
  group_by(svl) %>%
  summarise(area = sum(area))

### View map
ggplot(cont_geo_lvs) + geom_sf()


## Agregate by FVRD
cont_geo_fvrd <- cont_geo_concelhos %>%
  group_by(dsavr) %>%
  summarise(area = sum(area))

### View geo 
ggplot(cont_geo_fvrd) + geom_sf()


## Export as SHP files
st_write(cont_geo_fvrd, dsn = "continent_dsavr_map", driver = "ESRI Shapefile")
st_write(cont_geo_lvs, dsn = "continent_svl_map", driver = "ESRI Shapefile")
