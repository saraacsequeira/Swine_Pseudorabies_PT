# Libraries
library(tidyverse)
library(colorspace)
library(ggplot2)
library(sf)
library(dplyr)
library(data.table)
library(DBI)
library(RMySQL)

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


# Download continent and islands maps with different freguesias
## Continent map
tmp_dir_c <- tempdir()
tmp_c <- tempfile(tmpdir = tmp_dir_c, fileext = ".zip")
download.file("http://mapas.dgterritorio.pt/ATOM-download/CAOP-Cont/Cont_AAD_CAOP2019.zip", destfile = tmp_c)
unzip(tmp_c, exdir = tmp_dir_c)

### Table
cont_map <- read_sf(tmp_dir_c)
ggplot(cont_map) + geom_sf()


## Madeira map
tmp_dir_m <- tempdir()
tmp_m <- tempfile(tmpdir = tmp_dir_m, fileext = ".zip")
download.file("http://mapas.dgterritorio.pt/ATOM-download/CAOP-RAM/ArqMadeira_AAD_CAOP2019.zip", destfile = tmp_m)
unzip(tmp_m, exdir = tmp_dir_m)

### Table
mad_map <- read_sf(tmp_dir_m)
ggplot(mad_map) + geom_sf()


## Azores map
### Western group
tmp_dir_azw <- tempdir()
tmp_azw <- tempfile(tmpdir = tmp_dir_azw, fileext = ".zip")
download.file("http://mapas.dgterritorio.pt/ATOM-download/CAOP-RAA/ArqAcores_GOcidental_AAd_CAOP2019.zip", destfile = tmp_azw)
unzip(tmp_azw, exdir = tmp_dir_azw)

### Table
azw_map <- read_sf(tmp_dir_azw)

### Eastern group
tmp_dir_aze <- tempdir()
tmp_aze <- tempfile(tmpdir = tmp_dir_aze, fileext = ".zip")
download.file("http://mapas.dgterritorio.pt/ATOM-download/CAOP-RAA/ArqAcores_GOriental_AAd_CAOP2019.zip", destfile = tmp_aze)
unzip(tmp_aze, exdir = tmp_dir_aze)

### Table
aze_map <- read_sf(tmp_dir_aze)

### Central group
tmp_dir_azc <- tempdir()
tmp_azc <- tempfile(tmpdir = tmp_dir_azc, fileext = ".zip")
download.file("http://mapas.dgterritorio.pt/ATOM-download/CAOP-RAA/ArqAcores_GCentral_AAd_CAOP2019.zip", destfile = tmp_azc)
unzip(tmp_azc, exdir = tmp_dir_azc)

### Table
azc_map <- read_sf(tmp_dir_azc)

## Gather Azores tables in one
azores_map <- rbind(azc_map, aze_map, azw_map)


# Gather Azores and Madeira tables
isl_map <- rbind(azores_map, mad_map)






