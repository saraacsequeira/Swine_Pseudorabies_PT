# Libraries
library(DBI)
library(RMySQL)

library(dplyr)
library(data.table)
library(tidyr)
library(devtools)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(plotly)
library(RColorBrewer)
library(gganimate)
library(ggpubr)
library(tibble)
library(reshape2)
library(mapdeck)
library(colourvalues)
library(RColorBrewer)

# Connection with MySQL database
connection <- dbConnect(RMariaDB :: MariaDB(),
                        dbname = 'Swine',
                        host = "localhost",
                        user = "root",
                        password = "projetoporcos")
#Mapdeck token
token <- "pk.eyJ1IjoidGVyZXNhcGNvdXRpbmhvIiwiYSI6ImNraG9tbGRvZTBiNW8yc3A1cHgwMTM3engifQ.IZkYiF2VaRnuW9lm6h3SgQ"

#Tables (a aplicar + à frente em cada tópico)
vacinacoes <- dbReadTable(connection, "st_tabela_vacinacoes")
classificacoes <- dbReadTable(connection, "st_tabela_classificacoes")
controlos <- dbReadTable(connection, "st_tabela_controlos")
localizacoes <- dbReadTable(connection, "st_tabela_localizacoes")
abates <- dbReadTable(connection, "st_tabela_abates")
animais_abatidos <- dbReadTable(connection, "st_tabela_animais_abatidos")
matadouros <- dbReadTable(connection, "st_tabela_matadouros")
contingencias <- dbReadTable(connection, "st_tabela_contingencias")

# Pig farm's distribution by LVS (local veterinary service)
## Read tables from MySQLWorkbench
exploracoes <- dbReadTable(connection, "st_tabela_exploracoes")
contagens <- dbReadTable(connection, "st_tabela_contagens")

# Number of animals by farm 
## Table with total animals
count <- as.data.frame(aggregate(contagens$contagem, by = list(contagens$declaracao_existencias), FUN = sum))
count <- as.data.frame(merge(contagens, count, by.x = "declaracao_existencias", by.y = "Group.1"))
names(count)[11] <- "total"
count$total <- as.numeric(count$total)

## Remove duplicated rows based on declaracao_existencias
count <- count %>% 
  distinct(declaracao_existencias, .keep_all = TRUE)

## Add column with label 
count$info <- paste0("<br>", count$exploracao, "<br>", count$svl, " - ", count$dsavr, "<br>", count$contagem, " ", "animais", "<br>")

## Select only declaracao_existencias from 2020
count <- count %>%
  filter(count$data > as.Date("2020-01-01"))

## Remove farms missing any information
count <- na.omit(count)

## Define categories based on total animals
count$categoria <- cut(count$total, c(0,50,100,250,500,750,1000,2500,5000,10000,25000,50000))
levels(count$categoria) <- c("0-50", "50-100", "100-250", "250-500", "500-750", "750-1000", "1000-2500", "2500-5000", "5000-10000", "10000-25000", "25000-50000")
count$categoria <- as.character(count$categoria)



## Map
mapdeck(token = token, style = mapdeck_style("light")) %>%
  add_scatterplot(data = count, 
                  lat = "latitude", 
                  lon = "longitude",
                  radius = 2000,
                  fill_colour = "categoria",
                  legend = TRUE, 
                  tooltip = "info",
                  layer_id = "scatter_layer",
                  legend_options = list(fill_colour = list(title = "Number of animals by farm")),
                  palette = "inferno")
