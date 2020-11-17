# Libraries
library(DBI)
library(RMySQL)

library(dplyr)
library(data.table)
library(ggplot2)
library(ggthemes)
library(grid)
library(tibble)
library(reshape2)
library(ggpubr)
library(maps)
library(ggiraph)
library(leaflet)
library(geojsonio)
library(gganimate)
library(plotly)
library(RColorBrewer)
library(sf)
library(grid)
library(gridExtra)
library(htmltools)
library(zoo)
library(testthat)
library(rlang)
library(devtools)
library(tidyr)
library(lubridate)
library(scales)
library(highcharter)
library(here)
library(purrr)
library(magrittr)

# Connection with MySQL database
connection <- dbConnect(RMariaDB :: MariaDB(),
                        dbname = 'Swine',
                        host = "localhost",
                        user = "root",
                        password = "projetoporcos")

## Read tables (PARA QUANDO TIVERMOS AS TABELAS)
exploracoes <- dbReadTable(connection, "st_tabela_exploracoes")
contagens <- dbReadTable(connection, "st_tabela_contagens")
vacinacoes <- dbReadTable(connection, "st_tabela_vacinacoes")
