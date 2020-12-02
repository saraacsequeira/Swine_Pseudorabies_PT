# Libraries
library(DBI)
library(RMySQL)

library(dplyr)
library(data.table)
library(tidyr)
library(tidyverse)
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
library(scales)
library(rgdal)
library(hrbrthemes)
library(sf)



# Connection with MySQL database
connection <- dbConnect(RMariaDB :: MariaDB(),
                        dbname = 'Swine',
                        host = "localhost",
                        user = "root",
                        password = "projetoporcos")
#Mapdeck token
token <- "pk.eyJ1IjoidGVyZXNhcGNvdXRpbmhvIiwiYSI6ImNraG9tbGRvZTBiNW8yc3A1cHgwMTM3engifQ.IZkYiF2VaRnuW9lm6h3SgQ"

# Read tables from MySQLWorkbench
exploracoes <- dbReadTable(connection, "st_tabela_exploracoes")
contagens <- dbReadTable(connection, "st_tabela_contagens")
vacinacoes <- dbReadTable(connection, "st_tabela_vacinacoes")
classificacoes <- dbReadTable(connection, "st_tabela_classificacoes")
controlos <- dbReadTable(connection, "st_tabela_controlos")
localizacoes <- dbReadTable(connection, "st_tabela_localizacoes")
abates <- dbReadTable(connection, "st_tabela_abates")
animais_abatidos <- dbReadTable(connection, "st_tabela_animais_abatidos")
matadouros <- dbReadTable(connection, "st_tabela_matadouros")
contingencias <- dbReadTable(connection, "st_tabela_contingencias")



# 1 - PIG FARM'S DISTRIBUTION

# 1.1 - Pig farm's distribution by LVS (local veterinary service)

## Table with total of animals by LVS
count_svl <- as.data.frame(aggregate(contagens$contagem, by = list(contagens$classe_produtiva, contagens$svl), FUN = sum))
count_svl <- count_svl %>% arrange(Group.2, Group.1)
names(count_svl) <- c("class", "svl", "count")

count_svl_total <- as.data.frame(aggregate(count_svl$count, by = list(count_svl$svl), FUN = sum))
names(count_svl_total) <- c("svl", "total")
count_svl_total$total <- as.numeric(count_svl_total$total)

### Remove scientific notation
options(scipen=999)

## 1.1.1 Plot with total number of animals by LVS
count_svl_total_graph <- ggplot(count_svl_total, aes(x = svl, y = total, fill = svl)) + 
  geom_bar(stat = "identity", aes(text = paste0(svl, "<br>", total, " animals"))) + 
  coord_flip() + 
  theme_light() +
  theme(legend.position = "none") +
  labs( title = "Number of animals by LVS", size = 15,
        y = "Number of animals",
        x = "Local Veterinary Service")

## Interactive Graph
ggplotly(count_svl_total_graph, tooltip = "text") %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 30),
                                       "Local Veterinary Service",
                                       rep("&nbsp;", 30),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")),
         legend = list(x = 1, y = 0))

## 1.1.2 Map with animals' distribution by LVS
### Read map
setwd("C:/Users/teres/Desktop/EPIVET/DGAV - SISS/Swine_Pseudorabies_PT/maps")
setwd("~/Desktop/Treino Estágio 2020-2021/Swine_Pseudorabies_PT/maps")
pt_lvs_map <- read_sf("pt_svl_map")

### Azores all in one
pt_lvs_map <- pt_lvs_map %>%
  group_by(svl) %>%
  summarise(area = sum(area))

### Merge map with count by LVS
count_lvs_map <- merge(count_svl_total, pt_lvs_map, by.x = "svl", by.y = "svl", all.x = TRUE, all.y = TRUE)

### Add column with label
count_lvs_map$info <- paste0(count_lvs_map$svl, "<br>", count_lvs_map$total, " animals")

### Replace NA de Foz Côa com valor
count_lvs_map$total[is.na(count_lvs_map$total)] <- 0

### Total as numeric
count_lvs_map$total <- as.numeric(count_lvs_map$total)

### Define categories based on total animals
count_lvs_map$categoria <- cut(count_lvs_map$total, c(0,10000,25000,50000,100000,200000,300000,400000,500000))
levels(count_lvs_map$categoria) <- c("0;10000", "10000;25000", "25000;50000", "50000;100000", "100000;200000", "200000;300000", "300000;400000", "400000;500000")

### Convert to sf
count_lvs_map <- st_as_sf(count_lvs_map)


## Mapdeck
mapdeck(token = token, style = mapdeck_style("dark")) %>%
  add_polygon(data = count_lvs_map,
              layer_id = "polygon_layer", 
              fill_colour = "categoria",
              legend = TRUE,
              tooltip = "info",
              legend_options = list(fill_colour = list(title = "Number of animals by Local Veterinary Service")),
              palette = "inferno", 
              auto_highlight = TRUE)


# 1.2 - Pig farm's distribution by FVRD (food and veterinary regional directorate)
## Table with total of animals by FVRD
count_fvrd <- as.data.frame(aggregate(contagens$contagem, by = list(contagens$classe_produtiva, contagens$dsavr), FUN = sum))
count_fvrd <- count_fvrd %>% arrange(Group.2, Group.1)
names(count_fvrd) <- c("class", "fvrd", "count")

count_fvrd_total <- as.data.frame(aggregate(count_fvrd$count, by = list(count_fvrd$fvrd), FUN = sum))
names(count_fvrd_total) <- c("fvrd", "total")
count_fvrd_total$total <- as.numeric(count_fvrd_total$total)

### Remove scientific notation
options(scipen=999)

## 1.2.1 Plot with total number of animals by FVRD
count_fvrd_total_graph <- ggplot(count_fvrd_total, aes(x = fvrd, y = total, fill = fvrd)) + 
  geom_bar(stat = "identity", aes(text = paste0(fvrd, "<br>", total, " animals"))) + 
  coord_flip() + 
  theme_light() +
  theme(legend.position = "none") +
  labs( title = "Number of animals by FVRD", size = 15,
        y = "Number of animals",
        x = "Food and Veterinary Regional Directorate")

## Interactive Graph
ggplotly(count_fvrd_total_graph, tooltip = "text") %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 30),
                                       "Food and Veterinary Regional Directorate",
                                       rep("&nbsp;", 30),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")),
         legend = list(x = 1, y = 0))

## 1.2.2 Map with animals' distribution by LVS
### Read map
setwd("C:/Users/teres/Desktop/EPIVET/DGAV - SISS/Swine_Pseudorabies_PT/maps")
setwd("~/Desktop/Treino Estágio 2020-2021/Swine_Pseudorabies_PT/maps")
pt_fvrd_map <- read_sf("pt_fvrd_map")

### Azores all in one
pt_fvrd_map <- pt_fvrd_map %>%
  group_by(dsavr) %>%
  summarise(area = sum(area))

### Merge map with count by LVS
count_fvrd_map <- merge(count_fvrd_total, pt_fvrd_map, by.x = "fvrd", by.y = "dsavr", all.x = TRUE, all.y = TRUE)

### Add column with label
count_fvrd_map$info <- paste0(count_fvrd_map$fvrd, "<br>", count_fvrd_map$total, " animals")

### Total as numeric
count_fvrd_map$total <- as.numeric(count_fvrd_map$total)

### Convert to sf
count_fvrd_map <- st_as_sf(count_fvrd_map)


## Mapdeck
mapdeck(token = token, style = mapdeck_style("dark")) %>%
  add_polygon(data = count_fvrd_map,
              layer_id = "polygon_layer", 
              fill_colour = "fvrd",
              legend = TRUE,
              tooltip = "info",
              legend_options = list(fill_colour = list(title = "Number of animals by Local Veterinary Service")),
              palette = "rainbow_hcl", 
              auto_highlight = TRUE)


# 1.3 - Number of animals by farm 
## Table with total animals
contagens$contagem <- as.numeric(contagens$contagem)
count <- as.data.frame(aggregate(contagens$contagem, by = list(contagens$declaracao_existencias), FUN = sum))
count <- as.data.frame(merge(contagens, count, by.x = "declaracao_existencias", by.y = "Group.1", all = TRUE))
names(count)[11] <- "total"
count$total <- as.numeric(count$total)

### Remove duplicated rows based on declaracao_existencias
count <- count %>% 
  distinct(declaracao_existencias, .keep_all = TRUE)

## Map
### Add column with label 
count$info <- paste0(count$exploracao, "<br>", count$svl, " - ", count$dsavr, "<br>", count$total, " ", "animals", "<br>")

### Select only declaracao_existencias from 2020
count <- count %>%
  filter(count$data > as.Date("2020-01-01")) 

### Select only columns useful for the map
count <- count %>% select(longitude, latitude, exploracao, dsavr, svl, total, info)

### Remove NA
count <- na.omit(count)

### Define categories based on total animals
count$categoria <- cut(count$total, c(0,50,100,250,500,750,1000,2500,5000,10000,25000,50000))
levels(count$categoria) <- c("0;50", "50;100", "100;250", "250;500", "500;750", "750;1000", "1000;2500", "2500;5000", "5000;10000", "10000;25000", "25000;50000")


### Mapdeck
mapdeck(token = token, style = mapdeck_style("dark")) %>%
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



# 1.3.1 - Percentage of animals by class by LVS
## Table with percentage of animals by class by LVS
count_svl <- as.data.frame(merge(count_svl, count_svl_total, by.x = "svl", by.y = "svl"))
names(count_svl)[4] <- "total"

count_svl$count <- as.numeric(count_svl$count)
count_svl$total <- as.numeric(count_svl$total)
count_svl$percentage <- (count_svl$count / count_svl$total * 100)
count_svl$percentage <- round(count_svl$percentage, digits = 2)

## Plot with percentage of animals by class in each LVS
count_svl_graph <- ggplot(count_svl, aes(fill = class, y = percentage, x = svl)) + 
  geom_bar(position = "fill", stat = "identity", aes(text = paste0(percentage, "%"))) +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_brewer(palette = "Set2", labels = c("Weaners", "Piglets", "Sows", "Pigs", "Boars")) +
  labs( title = "Percentage of animals in each class by LVS", size = 15,
        y = "Percentage",
        x = "Local Veterinary Service", 
        caption = "Fonte: DGAV",
        fill = "")

##Interactive graph
ggplotly(count_svl_graph, tooltip = "text") %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 30),
                                       "Percentage",
                                       rep("&nbsp;", 30),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")),
         legend = list(x = 1, y = 0))


# 1.4 Percentage of pig farms currently classified (in general and farm specified)
## Selecting specific columns
class <- as.data.frame(merge(contagens, classificacoes, by.x = "exploracao", by.y = "exploracao_id")) %>% 
  select(data.y, exploracao, longitude, latitude, svl, classificacao_sanitaria)

### Remove rows with NA's
class <- na.omit(class)


## Select the last status from each farm
class_last <- class %>%
  arrange(data.y, exploracao) %>% 
  group_by(exploracao) %>% 
  summarise_all(last)

## Clean the wrong data formats
class_last <- class_last %>%
  filter(class_last$data.y < as.Date("2105-11-10"))

## Classification percentage for each SVL (in 2020)
### Give each row a number
classification_count <- class %>%
  mutate(count = 1)

## Add the number of animals by farm
### Merge tables
class_last <- merge(class_last, count, by.x = "exploracao", by.y = "exploracao", all.x = TRUE, all.y = FALSE)
class_last <- class_last %>% select(exploracao, longitude.x, latitude.x, svl.x, classificacao_sanitaria, total)
names(class_last)[2:3] <- c("longitude", "latitude")

### Add the word "animals" to the total and replace the NA with a blank space
class_last$total <- paste0(class_last$total, " ", "animals")
class_last$total <- replace(class_last$total, class_last$total == "NA animals", " ")

## Map with the last classification for each farm
### Add label
class_last$info1 <- paste0(class_last$exploracao, "<br>", class_last$svl, "<br>", class_last$classificacao_sanitaria, " ", "(2020)", "<br>", class_last$total)

mapdeck(token = token, style = mapdeck_style("dark"), pitch = 20) %>%
  add_scatterplot(data = class_last, 
                  lat = "latitude", 
                  lon = "longitude",
                  radius = 500,
                  fill_colour = "classificacao_sanitaria",
                  legend = TRUE, 
                  tooltip = "info1",
                  layer_id = "point",
                  legend_options = list(fill_colour = list(title = "Sanitary Classification")),
                  palette = "spectral")




# 2. Farms' status overview;
# 2.1 Farms' status by production type over the years
## Table with the farms' status and production type by year
status <- as.data.frame(merge(classificacoes, exploracoes, by.x = "exploracao_id", by.y = "exploracao")) 
status <- status %>% 
  filter(status$estado == "CONCLUIDO") %>%
  select(exploracao_id, data, classificacao_sanitaria, tipo_producao) %>%
  arrange(data, exploracao_id)

### Only with results between 2016 and 2020
status <- status %>% filter(status$data > "2016-01-01" & status$data < "2020-12-31")

### Remove duplicate rows
status <- unique(status)

### Add column with year
status$year <- format(as.Date(status$data, format="%d/%m/%Y"),"%Y")

### Give each row a number
status <- status %>%
  mutate(count = 1)

### Find the number of farms for each status
status_by_year <- as.data.frame(aggregate(status$count, by = list(status$year, status$classificacao_sanitaria), FUN = sum))
names(status_by_year) <- c("year", "status", "count")

## Remove A0 and SC status
status_by_year <- as.data.frame(status_by_year[!status_by_year$status == "A0" & !status_by_year$status == "SC",])

## Barplot
status_by_year_graph <- ggplot(status_by_year, aes(x = year, y = count, fill = status)) + 
  geom_bar(stat = "identity", position = "dodge", aes(text = paste('Year: ', year,
                                                                   '<br>Status: ', status,
                                                                   '<br>Nr. of Farms: ', count))) + 
  theme_ipsum() +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, max(status_by_year$count + 500))) +
  scale_fill_brewer(palette = "Set3") + 
  labs( title = "Number of farms per status over the years", size = 15,
        y = "Number of farms",
        x = "Year", 
        caption = "Fonte: DGAV") +
  theme(axis.title = element_text(size = 12),
        legend.title = element_blank(),
        axis.title.x = element_text(size = 9, hjust = 1),
        axis.title.y = element_text(size = 9, vjust = 1))

#Fazer gráfico interativo
ggplotly(status_by_year_graph, tooltip = "text") %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 30),
                                       "Number of farms",
                                       rep("&nbsp;", 30),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")),
         legend = list(x = 1, y = 0))


## Number of farms by type of production and by status in each year
### Remove A0 and SC status
status <- as.data.frame(status[!status$classificacao_sanitaria == "A0" & !status$classificacao_sanitaria == "SC",])

### Select the last status of each production class by year
status_last_production <- aggregate(status$classificacao_sanitaria, by=list(status$exploracao_id, status$year, status$tipo_producao), FUN=last)
names(status_last_production) <- c("exploracao_id", "year", "production", "status")

### Give each row a number
status_last_production <- status_last_production %>%
  mutate(count = 1)

### Find the number of farms per type of production
farms_production_status <- as.data.frame(aggregate(status_last_production$count, by = list(status_last_production$status, status_last_production$year, status_last_production$production), FUN = sum))
farms_production_status <- farms_production_status %>%
  arrange(Group.1, Group.3, Group.2)

names(farms_production_status) <- c("status", "year", "production", "count")

### Year as Date
farms_production_status$year <- as.Date(farms_production_status$year, format = "%Y")
farms_production_status$year <- format(as.Date(farms_production_status$year, format = "%Y-%m-%d"), "%Y")

### Chane to english
farms_production_status$production <- replace(farms_production_status$production, farms_production_status$production == "Centro de Colheita de sémen", "Semen Collection Center")
farms_production_status$production <- replace(farms_production_status$production, farms_production_status$production == "Montanheira", "Mountain")
farms_production_status$production <- replace(farms_production_status$production, farms_production_status$production == "Outros", "Others")
farms_production_status$production <- replace(farms_production_status$production, farms_production_status$production == "Produção", "Production of Pigs")
farms_production_status$production <- replace(farms_production_status$production, farms_production_status$production == "Produção de Leitões", "Production of Piglets")
farms_production_status$production <- replace(farms_production_status$production, farms_production_status$production == "Quarentena", "Quarentine")
farms_production_status$production <- replace(farms_production_status$production, farms_production_status$production == "Recria e/ou acabamento", "Rearing and/or Finisher")
farms_production_status$production <- replace(farms_production_status$production, farms_production_status$production == "Seleção e/ou multiplicação", "Selection and/or Breeding")

## Stacked bar plot
farms_production_graph <- ggplot(farms_production_status, aes(fill = production, y = count, x = status)) +
  geom_bar(position = "stack", stat = "identity", aes(text = paste0(production, " - ", count, " ", "farms"))) + 
  facet_wrap(~year) + 
  theme_pubclean() + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("Number of farms by type of production in each status over the years") + 
  scale_fill_brewer(palette = "Accent") +
  labs(y = " ",
       x = " ", 
       caption = "Fonte: DGAV",
       fill = " ")

ggplotly(farms_production_graph, tooltip = "text") %>%
  layout(yaxis = list(title =paste0(c(rep("&nbsp;", 20),
                                      "Number of farms",
                                      rep("&nbsp;", 20),
                                      rep("\n&nbsp;", 2)),
                                    collapse = "")),
         legend = list(x = 1, y = 0))


# Line chart for each status
## all in one
graph <- ggplot(farms_production_status, aes(color = production, group = production, y = count, x = year)) +
  geom_line(size = 1) +
  geom_point(size = 2, aes(text = paste0(production, "<br>", count, " farms"))) + 
  facet_wrap(~status) +
  scale_y_continuous(breaks=(seq(0, 70, 10)), limits = c(0, 70)) +
  scale_color_brewer(palette = "Accent") +
  theme_ipsum() + 
  theme() + 
  ggtitle("Number of farms by type of production and status over the years") + 
  labs(y = "Number of farms",
       x = "Year",
       color = " ")

### A1 Interactive graph
ggplotly(graph, tooltip = "text") %>%
  layout(yaxis = list(title =paste0(c(rep("&nbsp;", 30),
                                      "Number of farms",
                                      rep("&nbsp;", 30),
                                      rep("\n&nbsp;", 2)),
                                    collapse = "")),
         legend = list(x = 1, y = 0))

## A1 ggplot
a1 <- farms_production_status %>% filter(farms_production_status$status == "A1")

a1_graph <- ggplot(a1, aes(color = production, group = production, y = count, x = year)) +
  geom_line(size = 1) +
  geom_point(size = 2, aes(text = paste0(production, "<br>", count, " farms"))) + 
  scale_y_continuous(breaks=(seq(0, 70, 10)), limits = c(0, 70)) +
  scale_color_brewer(palette = "Accent") +
  theme_ipsum() + 
  theme() + 
  ggtitle("A1 Status - Number of farms by type of production over the years") + 
  labs(y = "Number of farms",
       x = "Year",
       color = " ")

### A1 Interactive graph
ggplotly(a1_graph, tooltip = "text") %>%
  layout(yaxis = list(title =paste0(c(rep("&nbsp;", 30),
                                      "Number of farms",
                                      rep("&nbsp;", 30),
                                      rep("\n&nbsp;", 2)),
                                    collapse = "")),
         legend = list(x = 1, y = 0))


## A2 ggplot
a2 <- farms_production_status %>% filter(farms_production_status$status == "A2")

a2_graph <- ggplot(a2, aes(color = production, group = production, y = count, x = year)) +
  geom_line(size = 1) +
  scale_y_continuous(breaks=(seq(0, 70, 10)), limits = c(0, 70)) +
  geom_point(size = 2, aes(text = paste0(production, "<br>", count, " farms"))) + 
  scale_color_brewer(palette = "Accent") +
  theme_ipsum() + 
  theme(legend.position = "bottom") + 
  ggtitle("A2 Status - Number of farms by type of production over the years") + 
  labs(y = "Number of farms",
       x = "Year",
       color = " ")

### A2 Interactive graph
ggplotly(a2_graph, tooltip = "text") %>%
  layout(yaxis = list(title =paste0(c(rep("&nbsp;", 30),
                                      "Number of farms",
                                      rep("&nbsp;", 30),
                                      rep("\n&nbsp;", 2)),
                                    collapse = "")),
         legend = list(x = 1, y = 0))


## A2A ggplot
a2a <- farms_production_status %>% filter(farms_production_status$status == "A2A")

a2a_graph <- ggplot(a2a, aes(color = production, group = production, y = count, x = year)) +
  geom_line(size = 1) +
  scale_y_continuous(breaks=(seq(0, 70, 10)), limits = c(0, 70)) +
  geom_point(size = 2, aes(text = paste0(production, "<br>", count, " farms"))) + 
  scale_color_brewer(palette = "Accent") +
  theme_ipsum() + 
  theme(legend.position = "bottom") + 
  ggtitle("A2A Status - Number of farms by type of production over the years") + 
  labs(y = "Number of farms",
       x = "Year",
       color = " ")

### A2A Interactive graph
ggplotly(a2a_graph, tooltip = "text") %>%
  layout(yaxis = list(title =paste0(c(rep("&nbsp;", 30),
                                      "Number of farms",
                                      rep("&nbsp;", 30),
                                      rep("\n&nbsp;", 2)),
                                    collapse = "")),
         legend = list(x = 1, y = 0))

## A2NA ggplot
a2na <- farms_production_status %>% filter(farms_production_status$status == "A2NA")

a2na_graph <- ggplot(a2na, aes(color = production, group = production, y = count, x = year)) +
  geom_line(size = 1) +
  geom_point(size = 2, aes(text = paste0(production, "<br>", count, " farms"))) + 
  scale_y_continuous(breaks=(seq(0, 70, 10)), limits = c(0, 70)) +
  scale_color_brewer(palette = "Accent") +
  theme_ipsum() + 
  theme(legend.position = "bottom") + 
  ggtitle("A2NA Status - Number of farms by type of production over the years") + 
  labs(y = "Number of farms",
       x = "Year",
       color = " ")

### A2NA Interactive graph
ggplotly(a2na_graph, tooltip = "text") %>%
  layout(yaxis = list(title =paste0(c(rep("&nbsp;", 30),
                                      "Number of farms",
                                      rep("&nbsp;", 30),
                                      rep("\n&nbsp;", 2)),
                                    collapse = "")),
         legend = list(x = 1, y = 0))


## A3 ggplot
a3 <- farms_production_status %>% filter(farms_production_status$status == "A3")

a3_graph <- ggplot(a3, aes(color = production, group = production, y = count, x = year)) +
  geom_line(size = 1) +
  scale_y_continuous(breaks=(seq(0, 70, 10)), limits = c(0, 70)) +
  geom_point(size = 2, aes(text = paste0(production, "<br>", count, " farms"))) + 
  scale_color_brewer(palette = "Accent") +
  theme_ipsum() + 
  theme(legend.position = "bottom") + 
  ggtitle("A3 Status - Number of farms by type of production over the years") + 
  labs(y = "Number of farms",
       x = "Year",
       color = " ")

### A3 Interactive graph
ggplotly(a3_graph, tooltip = "text") %>%
  layout(yaxis = list(title =paste0(c(rep("&nbsp;", 30),
                                      "Number of farms",
                                      rep("&nbsp;", 30),
                                      rep("\n&nbsp;", 2)),
                                    collapse = "")),
         legend = list(x = 1, y = 0))

## A4 ggplot
a4 <- farms_production_status %>% filter(farms_production_status$status == "A4")

a4_graph <- ggplot(a4, aes(color = production, group = production, y = count, x = year)) +
  geom_line(size = 1) +
  scale_y_continuous(breaks=(seq(0, 70, 10)), limits = c(0, 70)) +
  geom_point(size = 2, aes(text = paste0(production, "<br>", count, " farms"))) + 
  scale_color_brewer(palette = "Accent") +
  theme_ipsum() + 
  theme(legend.position = "bottom") + 
  ggtitle("A4 Status - Number of farms by type of production over the years") + 
  labs(y = "Number of farms",
       x = "Year",
       color = " ")

### A4 Interactive graph
ggplotly(a4_graph, tooltip = "text") %>%
  layout(yaxis = list(title =paste0(c(rep("&nbsp;", 30),
                                      "Number of farms",
                                      rep("&nbsp;", 30),
                                      rep("\n&nbsp;", 2)),
                                    collapse = "")),
         legend = list(x = 1, y = 0))

## A5 ggplot
a5 <- farms_production_status %>% filter(farms_production_status$status == "A5")

a5_graph <- ggplot(a5, aes(color = production, group = production, y = count, x = year)) +
  geom_line(size = 1) +
  scale_y_continuous(breaks=(seq(0, 70, 10)), limits = c(0, 70)) +
  geom_point(size = 2, aes(text = paste0(production, "<br>", count, " farms"))) + 
  scale_color_brewer(palette = "Accent") +
  theme_ipsum() + 
  theme(legend.position = "bottom") + 
  ggtitle("A5 Status - Number of farms by type of production over the years") + 
  labs(y = "Number of farms",
       x = "Year",
       color = " ")

### A5 Interactive graph
ggplotly(a5_graph, tooltip = "text") %>%
  layout(yaxis = list(title =paste0(c(rep("&nbsp;", 30),
                                      "Number of farms",
                                      rep("&nbsp;", 30),
                                      rep("\n&nbsp;", 2)),
                                    collapse = "")),
         legend = list(x = 1, y = 0))



## Percentage of farms by status
### Table with number of farms by status 
status_percentage <- as.data.frame(aggregate(x = class_last, list(status = class_last$classificacao_sanitaria), FUN = length))
status_percentage <- status_percentage %>% select(status, total)
status_percentage <- status_percentage[-10, ]

### Add column with total number of farms
status_percentage$sum <- sum(status_percentage$total)

### Add column with percentage
status_percentage$percentage <- (status_percentage$total / status_percentage$sum) * 100
status_percentage$percentage <- round(status_percentage$percentage, digits = 2)

### Add column with label
status_percentage$label <- paste0(status_percentage$status, " ", "(", status_percentage$percentage, "%", ")")

## Lollipop chart
status_percent_graph <- ggplot(status_percentage, aes(x = status, y = percentage, color = status)) +
  geom_point(size = 5, aes(text = label)) + 
  geom_segment(aes(x = status,xend = status,  y = 0, yend = percentage), linetype = "dotted", color = "grey60") +
  scale_color_brewer(palette = "Set3") +
  theme_ipsum() + 
  theme(legend.position = "right") + 
  ggtitle("Percentage of farms by status") + 
  labs(caption = "Fonte: DGAV",
       color = " ",
       x = "Status") 

## Interactive graph
ggplotly(status_percent_graph, tooltip = "text") %>%
  layout(yaxis = list(title =paste0(c(rep("&nbsp;", 30),
                                      "Percentage (%)",
                                      rep("&nbsp;", 30),
                                      rep("\n&nbsp;", 2)),
                                    collapse = "")),
         legend = list(x = 1, y = 0))



# 3. Slaughters
# 3.1 Number of animals slaughtered by farm in 2019
## Table with animals slaughtered by farm in 2019
### Change date format
animais_abatidos$data_de_entrada <- strftime(animais_abatidos$data_de_entrada, format = "%Y-%m-%d")
animais_abatidos$data_de_entrada <- as.Date(animais_abatidos$data_de_entrada)

### Select 2019 slaghters
slaughter_2019 <- animais_abatidos %>% filter(data_de_entrada <= "2019-12-31" & data_de_entrada >= "2019-01-01")

### Remove farms without location
slaughter_2019 <- slaughter_2019 %>% filter(long_exploracao != "NA")

### Total number of animals slaUghtered by farm
slaughter_2019_total <- aggregate(slaughter_2019$confirmados, by = list(slaughter_2019$exploracao, slaughter_2019$long_exploracao, slaughter_2019$lat_exploracao), FUN = sum)
names(slaughter_2019_total) <- c("exploracao", "longitude", "latitude", "count")
slaughter_2019_total$count <- as.numeric(slaughter_2019_total$count)

### Define categories
slaughter_2019_total$categ <- cut(slaughter_2019_total$count, c(0,50,500,1000,2500,5000,10000,50000,100000,250000))
levels(slaughter_2019_total$categ) <- c("0;50", "50;500", "500;1000", "1000;2500", "2500;5000", "5000;10000", "10000;50000", "50000;100000", "100000;250000")

## Add info for label
slaughter_2019_total$info <- paste0(slaughter_2019_total$exploracao, "<br>", slaughter_2019_total$count, " animals slaughtered")


## Map with number of animals slaughtered by farm
mapdeck(token = token, style = mapdeck_style("dark")) %>%
  add_scatterplot(data = slaughter_2019_total,
                  lat = "latitude",
                  lon = "longitude", 
                  radius = 2000,
                  fill_colour = "categ", 
                  legend = TRUE,
                  tooltip = "info",
                  layer_id = "scatter_layer",
                  legend_options = list(fill_colour = list(title = "Number of animals slaughtered by farm in 2019")),
                  palette = "heat_hcl")


# 3.2 Principal itineraries to the slaughterhouses in 2019
## Table with all slaughter itineraries in 2019
### Remove slaughterhouses without location
itineraries_2019 <- slaughter_2019 %>% filter(long_matadouro != "NA")

### Select only columns that matter
itineraries_2019 <- itineraries_2019 %>% select(exploracao, long_exploracao, lat_exploracao, matadouro, long_matadouro, lat_matadouro)

### Give each row a number
itineraries_2019 <- itineraries_2019 %>% mutate(count = 1)

### Count frequency of each itinerarie
itineraries_2019 <- as.data.frame(aggregate(itineraries_2019$count, by = list(itineraries_2019$exploracao, itineraries_2019$long_exploracao, itineraries_2019$lat_exploracao, itineraries_2019$matadouro, itineraries_2019$long_matadouro, itineraries_2019$lat_matadouro), FUN = sum))
names(itineraries_2019) <- c("exploracao", "long_exploracao", "lat_exploracao", "matadouro", "long_matadouro", "lat_matadouro", "freq")

### Select only itineraries with freq > 10
itineraries_2019 <- itineraries_2019 %>% filter(freq >= 50)

### Add label column 
itineraries_2019$info <- paste0(itineraries_2019$exploracao, " to ", itineraries_2019$matadouro, "<br>", itineraries_2019$freq, " trips")

## Map with itineraries by frequency in 2019
mapdeck(token = token, style = mapdeck_style("dark")) %>%
  add_animated_arc(data = itineraries_2019, 
          layer_id = "arc_layer",
          origin = c("long_exploracao", "lat_exploracao"),
          destination = c("long_matadouro", "lat_matadouro"),
          stroke_from = "exploracao",
          stroke_to = "matadouro",
          stroke_width = "stroke",
          tooltip = "info",
          palette = "viridis")


# 3.3 Geomline with the number of animals slaughtered over the year of 2019
## Table with number of animals slaughtered by month in 2019
slaughter_2019_month <- slaughter_2019 %>% select(id, data_de_entrada, exploracao, confirmados)
slaughter_2019_month <- unique(slaughter_2019_month)

### Add column with the month
slaughter_2019_month$month <- as.Date(slaughter_2019_month$data_de_entrada, format = "%m")
slaughter_2019_month$m <- as.Date(slaughter_2019_month$data_de_entrada, format = "%m")
slaughter_2019_month$month <- format(as.Date(slaughter_2019_month$month, format = "%Y-%m-%d"), "%m")
slaughter_2019_month$m <- format(as.Date(slaughter_2019_month$m, format = "%Y-%m-%d"), "%B")

### Number of animals slaughtered by month
slaughter_2019_month <- aggregate(slaughter_2019_month$confirmados, by = list(slaughter_2019_month$month, slaughter_2019_month$m), FUN = sum)
names(slaughter_2019_month) <- c("month", "m", "count")
slaughter_2019_month$count <- as.numeric(slaughter_2019_month$count)
slaughter_2019_month$month <- as.numeric(slaughter_2019_month$month)

## Geomline 
month_graph <- ggplot(slaughter_2019_month, aes(x = month, y = count, color = m)) +
  geom_line(size = 0.5, color = "gray60") + 
  geom_point(size = 2, aes(text = paste0(m, " - ", count, " animals slaughtered"))) + 
  scale_color_brewer(palette = "Paired") + 
  theme_light() + 
  theme() + 
  ggtitle("Animals slaughtered over the year of 2019") + 
  labs(y = "Number of animals slaughtered",
       x = "Month", 
       color = " ") + 
  scale_x_continuous(breaks = seq(1,12, by = 1))

## Interactive graph
ggplotly(month_graph, tooltip = "text") %>%
  layout(yaxis = list(title =paste0(c(rep("&nbsp;", 30),
                                      "Number of animals slaughtered",
                                      rep("&nbsp;", 30),
                                      rep("\n&nbsp;", 2)),
                                    collapse = "")))


# 3.4 Animals slaughtered by FVRD in 2019
## Table with animals slaughtered in each FVRD in 2019
slaughter_dsavr <- merge(slaughter_2019, exploracoes, by.x = "exploracao", by.y = "exploracao", all.x = TRUE, all.y = FALSE)

### Remove farms without FVRD info
slaughter_dsavr <- slaughter_dsavr %>% filter(dsavr != "NA")
slaughter_dsavr <- slaughter_dsavr %>% select(exploracao, data_de_entrada, long_exploracao, lat_exploracao, confirmados, svl, dsavr)

slaughter_dsavr <- as.data.frame(aggregate(slaughter_dsavr$confirmados, by = list(slaughter_dsavr$dsavr), FUN = sum))
names(slaughter_dsavr) <- c("dsavr", "count")
slaughter_dsavr$count <- as.numeric(slaughter_dsavr$count)


## 3.4.1 Plot with number of animals slaughtered by FVRD
dsavr_graph <- ggplot(slaughter_dsavr, aes(x = dsavr, y = count, color = dsavr)) + 
  theme_light() + 
  theme(axis.text.x = element_blank(), plot.title = element_text(hjust = 0.5)) + 
  geom_point(size = 5, aes(text = paste0(dsavr, "<br>", count, " animals slaughtered")), show.legend = TRUE) + 
  geom_segment(aes(x = dsavr, xend = dsavr,  y = 0, yend = count), linetype = "dotted", color = "black") +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Number of animals slaughtered in each \n Food and Veterinary Regional Directorate during 2019",
       x = " ", 
       y = " ", 
       color = " ") +
  scale_y_continuous(breaks = seq(0, 1150000, by = 150000), limits = c(0, 1150000))
  
## Interactive graph
ggplotly(dsavr_graph, tooltip = "text") %>%
  layout(yaxis = list(title =paste0(c(rep("&nbsp;", 30),
                                      "Number of animals slaughtered",
                                      rep("&nbsp;", 30),
                                      rep("\n&nbsp;", 2)),
                                    collapse = "")))


## 3.4.2 Map with number of animals slaughtered by FVRD
### Merge table with map
slaughter_fvrd_map <- merge(slaughter_dsavr, pt_fvrd_map, by.x = "dsavr", by.y = "dsavr", all = TRUE)

### Add column with label
slaughter_fvrd_map$info <- paste0(slaughter_fvrd_map$dsavr, "<br>", slaughter_fvrd_map$count, " animals slaughtered")

### Total as numeric
slaughter_fvrd_map$count <- as.numeric(slaughter_fvrd_map$count)

### Convert to sf
slaughter_fvrd_map <- st_as_sf(slaughter_fvrd_map)


## Mapdeck
mapdeck(token = token, style = mapdeck_style("dark")) %>%
  add_polygon(data = slaughter_fvrd_map,
              layer_id = "polygon_layer", 
              fill_colour = "dsavr",
              legend = TRUE,
              tooltip = "info",
              legend_options = list(fill_colour = list(title = "Number of animals slaughtered by Local Veterinary Service")),
              palette = "rainbow_hcl", 
              auto_highlight = TRUE)



# 3.5 Animals slaughtered by LVS in 2019
## Table with animals slaughtered in each LVS in 2019
slaughter_lvs <- merge(slaughter_2019, exploracoes, by.x = "exploracao", by.y = "exploracao", all.x = TRUE, all.y = FALSE)

### Remove farms without FVRD info
slaughter_lvs <- slaughter_lvs %>% filter(svl != "NA")
slaughter_lvs <- slaughter_lvs %>% select(exploracao, data_de_entrada, long_exploracao, lat_exploracao, confirmados, svl, dsavr)

slaughter_lvs <- slaughter_lvs %>%
  group_by(svl) %>%
  summarise(confirmados = sum(confirmados))
names(slaughter_lvs) <- c("svl", "count")
slaughter_lvs$count <- as.numeric(slaughter_lvs$count)

## 3.5.1 Plot with number of animals slaughtered by LVS
lvs_graph <- ggplot(slaughter_lvs, aes(x = svl, y = count, color = svl)) + 
  theme_light() + 
  theme(axis.text.x = element_blank(), plot.title = element_text(hjust = 0.5)) + 
  geom_point(size = 2, aes(text = paste0(svl, "<br>", count, " animals slaughtered")), show.legend = TRUE) + 
  geom_segment(aes(x = svl, xend = svl,  y = 0, yend = count), linetype = "dotted", color = "black") +
  labs(title = "Number of animals slaughtered in each \n Local Veterinary Service during 2019",
       x = " ", 
       y = " ", 
       color = " ")

## Interactive graph
ggplotly(lvs_graph, tooltip = "text") %>%
  layout(yaxis = list(title =paste0(c(rep("&nbsp;", 30),
                                      "Number of animals slaughtered",
                                      rep("&nbsp;", 30),
                                      rep("\n&nbsp;", 2)),
                                    collapse = "")))


## 3.5.2 Map with number of animals slaughtered by LVS
### Merge with map
slaughter_lvs_map <- merge(slaughter_lvs, pt_lvs_map, by.x = "svl", by.y = "svl", all = TRUE)

### Add column with label
slaughter_lvs_map$info <- paste0(slaughter_lvs_map$svl, "<br>", slaughter_lvs_map$count, " animals slaughtered")

### Total as numeric
slaughter_lvs_map$count <- as.numeric(slaughter_lvs_map$count)

### Define categories based on total animals
slaughter_lvs_map$categoria <- cut(slaughter_lvs_map$count, c(0,10000,25000,50000,100000,200000,300000,400000,500000))
levels(slaughter_lvs_map$categoria) <- c("0;10000", "10000;25000", "25000;50000", "50000;100000", "100000;200000", "200000;300000", "300000;400000", "400000;500000")

### Convert to sf
slaughter_lvs_map <- st_as_sf(slaughter_lvs_map)


## Mapdeck
mapdeck(token = token, style = mapdeck_style("dark")) %>%
  add_polygon(data = slaughter_lvs_map,
              layer_id = "polygon_layer", 
              fill_colour = "categoria",
              legend = TRUE,
              tooltip = "info",
              legend_options = list(fill_colour = list(title = "Number of animals slaughtered by Local Veterinary Service")),
              palette = "heat_hcl", 
              auto_highlight = TRUE)


# 3.6 Mean of daily slaughters in each month between 2016 and 2020
## Table with mean of daily slaughters in each month between 2016 and 2020
### Filter by date
mean_slaughter <- animais_abatidos %>% filter(data_de_entrada >= "2016-01-01" & data_de_entrada <= "2020-12-31")

### Aggregate by day
mean_slaughter <- as.data.frame(aggregate(mean_slaughter$confirmados, by = list(mean_slaughter$data_de_entrada), FUN = sum))
names(mean_slaughter) <- c("data_de_entrada", "count")

### Add column with year and month
mean_slaughter$month <- as.Date(mean_slaughter$data_de_entrada, format = "%m")
mean_slaughter$month <- format(as.Date(mean_slaughter$month, format = "%Y-%m-%d"), "%m")
mean_slaughter$year <- as.Date(mean_slaughter$data_de_entrada, format = "%Y")
mean_slaughter$year <- format(as.Date(mean_slaughter$year, format = "%Y-%m-%d"), "%Y")

### Confirmed, year and  month as numeric
mean_slaughter$count <- as.numeric(mean_slaughter$count)
mean_slaughter$month <- as.numeric(mean_slaughter$month)
mean_slaughter$year <- as.factor(mean_slaughter$year)

### Mean of daily slaughters in each month
mean_slaughter <- aggregate(mean_slaughter$count, by = list(mean_slaughter$month, mean_slaughter$year), FUN = mean)
names(mean_slaughter) <- c("month", "year", "mean")
mean_slaughter$mean <- round(mean_slaughter$mean, digits = 1)

### Add column with written month
mean_slaughter$m <- mean_slaughter$month
mean_slaughter$m[mean_slaughter$m == c("1","2","3","4","5","6","7","8","9","10","11","12")] <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

## Geomline with evolution
mean_slaughter_graph <- ggplot(mean_slaughter, aes(x = month, y = mean, color = year)) + 
  geom_line(size = 0.7) + 
  geom_point(size = 1.5, aes(text = paste0(year, "/", m, " - ", "mean of ", mean, " animals slaughtered"))) +
  scale_color_brewer(palette = "Dark2") + 
  theme_light() + 
  theme() + 
  ggtitle("Mean of daily slaughters in each month between 2016 and 2020") + 
  labs(y = "Number of animals slaughtered",
       x = "Month", 
       color = " ") + 
  scale_x_continuous(breaks = seq(1,12, by = 1))

## Interactive graph
ggplotly(mean_slaughter_graph, tooltip = "text") %>%
  layout(yaxis = list(title =paste0(c(rep("&nbsp;", 30),
                                      "Mean of daily slaughters",
                                      rep("&nbsp;", 30),
                                      rep("\n&nbsp;", 2)),
                                    collapse = "")))



# 4. Laboratory Tests
## 4.1. Number of animals sampled over time

### Remove NA values and other formats to numeric
controlos_laboratoriais <- na.omit(controlos)

controlos_laboratoriais$resultados_positivos <- as.numeric(as.character(unlist(controlos_laboratoriais$resultados_positivos)))

controlos_laboratoriais$resultados_positivos <- as.numeric(controlos_laboratoriais$resultados_positivos)
controlos_laboratoriais$animais_amostrados <- as.numeric(controlos_laboratoriais$animais_amostrados)

### Select data of interest
controlos_laboratoriais2 <- controlos_laboratoriais %>%
  select(data_rececao_laboratorio, exploracoes_marca, resultados_positivos, animais_amostrados, classe) 

## Geom_area with the number of sampled animals over time (since they started sampling)
samples_graph <- ggplot(controlos_laboratoriais2, aes(x=data_rececao_laboratorio, y=animais_amostrados, fill=classe)) + 
  geom_area(alpha=.5, size=.9, aes(text = paste('Date: ', data_rececao_laboratorio,
                                                '<br>Nº Sampled Animals: ', animais_amostrados,
                                                '<br>Production Class: ', classe))) + 
  scale_fill_brewer(palette = "Dark2") +

  theme_ipsum() + 
  ggtitle("Number of sampled animals over time") +
  theme(axis.title = element_text(size = 15),
        legend.title = element_blank(),
        axis.title.x = element_text(size = 9, hjust = 1),
        axis.title.y = element_text(size = 9, vjust = 1)) +
  labs(x = "Laboratory reception date", 
       y = "Nº sampled animals") +
  geom_vline(aes(xintercept=mean(animais_amostrados)),
             color="blue", linetype="dashed", size=2)

#Tornar gráfico interativo
ggplotly(samples_graph, tooltip = "text") %>% 
  layout(legend = list(x = 1, y = 0))


## 4.2. Evaluate % of positive animals among total sampled animals by SVL / laboratory

controlos_laboratoriais <- controlos_laboratoriais %>%
  select(data_rececao_laboratorio, longitude, latitude, svl, exploracoes_marca, classe, resultados_positivos, animais_amostrados) %>%
  # New variable with positive animals among total sampled
  mutate(ratio_positive = resultados_positivos / animais_amostrados) %>%
  # New variable with % positive animals among total sampled
  mutate(percent_positive = resultados_positivos / animais_amostrados * 100)

### Remove NaN values (0 positive / 0 sampled)
controlos_laboratoriais <- na.omit(controlos_laboratoriais)

### Remove infinite values (1 positive or more / 0 sampled)
controlos_laboratoriais <- controlos_laboratoriais %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))

### Change values (only 2 digits)
controlos_laboratoriais[,9:10] <- round(controlos_laboratoriais[,9:10], digits = 2)

## Plot with percentage of animals by class in each LVS
percent_samples_graph <- ggplot(controlos_laboratoriais, aes(fill = classe, y = percent_positive, x = svl)) + 
  geom_bar(position = "fill", stat = "identity", aes(text = paste0(percent_positive, "%"))) +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_brewer(palette = "Set2", labels = c("Fattening", "Breeding", "Replacement")) +
  labs( title = "Percentage of vaccinated animals in each production class by LVS", size = 15,
        y = "Percentage",
        x = "Local Veterinary Service", 
        caption = "Fonte: DGAV",
        fill = "")

##Interactive graph
ggplotly(percent_samples_graph, tooltip = "text") %>% 
  layout(legend = list(x = 1, y = 0))






## 4.3. Positive samples per status over time (geom_line/point)









# 5. Vaccination
## Vaccinated animals by production class and status over the years
### Select rows of interest
vaccination_data <- vacinacoes %>% 
  filter(vacinacoes$estado == "CONCLUIDO") %>%
  select(data, exploracao, classe_controlo, vacinados_classe) %>%
  arrange(data, exploracao)

colnames(vaccination_data)[2] <- "exploracao_id"

### Only with results between 2016 and 2020
vaccination_data <- vaccination_data %>% filter(vaccination_data$data > "2016-01-01" & vaccination_data$data < "2020-12-31")

### Remove NA values
vaccination_data <- na.omit(vaccination_data)

##### HELP
## Number of vaccinated animals per production class by year
### Add status column to our data
status_last <- status %>%
  group_by(exploracao_id) %>%
  slice(which.max(as.Date(data, "%Y-%m-%d")))

vaccination_status <- merge(vaccination_data, status_last, by.x = "exploracao_id", by.y = "exploracao_id", all.x = TRUE, all.y = FALSE)

vaccination_production_status <- vaccination_status %>% 
  select(classificacao_sanitaria, data.x, exploracao_id, classe_controlo, vacinados_classe)

## Add year column
vaccination_production_status$year <- as.Date(vaccination_production_status$data.x, format = "%Y")
vaccination_production_status$year <- format(as.Date(vaccination_production_status$year, format = "%Y-%m-%d"), "%Y")


### Group by status, year and production classes
vaccination_production_year <- as.data.frame(aggregate(vaccination_production_status$vacinados_classe, by = list(vaccination_production_status$classificacao_sanitaria, vaccination_production_status$year, vaccination_production_status$classe_controlo), FUN = sum))
names(vaccination_production_year) <- c("status", "year", "production", "count")

### Remove A0 and SC status
vaccination_production_year <- as.data.frame(vaccination_production_year[!vaccination_production_year$status == "A0" & !vaccination_production_year$status == "SC",])

### Count as numeric
vaccination_production_year$count <- as.numeric(vaccination_production_year$count)

### Chane to english
vaccination_production_year$production <- replace(vaccination_production_year$production, vaccination_production_year$production == "Engorda", "Fattening")
vaccination_production_year$production <- replace(vaccination_production_year$production, vaccination_production_year$production == "Reprodutores", "Breeding")
vaccination_production_year$production <- replace(vaccination_production_year$production, vaccination_production_year$production == "Substituição", "Replacement")

### Year as Date
vaccination_production_year$year <- as.Date(vaccination_production_year$year, format = "%Y")
vaccination_production_year$year <- format(as.Date(vaccination_production_year$year, format = "%Y-%m-%d"), "%Y")

### Prepare for graph
vaccination_production_year <- vaccination_production_year %>% 
  group_by(year) %>%
  mutate(position = rank(count))
  # Order by vaccinated counts
         
## Stacked bar plot
vaccination_graph <- ggplot(vaccination_production_year, 
                            aes(x = status, y = count, fill = reorder(production, count), group = position)) + 
  geom_bar(position = "stack", stat = "identity", aes(text = paste0(count, "<br>", " animals"))) + 
  facet_wrap(~year) +
  scale_y_continuous(trans = "log10") +
  coord_flip() + 
  theme_bw() +
  labs( title = "Number of animals vaccinated by production type in each status over the years", size = 20,
        y = "Number of animals",
        x = "", 
        caption = "Fonte: DGAV") +
  scale_fill_brewer(palette = "Accent") +
  theme(legend.title = element_blank(),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10, hjust = 1),
        axis.text.x = element_text(size = 5, angle = 80, hjust = 1, 
                                   color = "black"),
        axis.text.y = element_text(size=8,
                                   color = "black"))

## Interactive Graph
ggplotly(vaccination_graph, tooltip = "text") %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 15),
                                       "Status",
                                       rep("&nbsp;", 15),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")),
         legend = list(x = 1, y = 0))
      ## tentar resolver a apresentação de texto do eixo x e interactividade





## Percentage of vaccinated animals by status and production class in 2019
#### Select only 2019 data
vaccination_2019 <- vaccination_production_status %>% 
  filter(year == "2019") %>%
  na.omit(vaccination_2019)

## Remove A0 and SC status
vaccination_2019 <- as.data.frame(vaccination_2019[!vaccination_2019$classificacao_sanitaria == "A0" & !vaccination_2019$classificacao_sanitaria == "SC",])

vaccination_2019$vacinados_classe <- as.numeric(vaccination_2019$vacinados_classe)

## Group vaccinated animals per status
vaccination_2019_status <- aggregate(vaccination_2019, by=list(vaccination_2019$classificacao_sanitaria, vaccination_2019$vacinados_classe), FUN = sum)
vaccination_2019_status <- as.data.frame(aggregate(vaccination_2019$vacinados_classe , by = list(vaccination_2019$classificacao_sanitaria), FUN = sum))

names(status_last_production) <- c("exploracao_id", "year", "production", "status")



#### Select only 2020 count and status counts
count_19 <- count %>%
  filter(data > "2018-12-31" & data < "2020-01-01")

status_19 <- status %>%
  filter(data > "2018-12-31" & data < "2020-01-01")

#### Number of animals by production class and status in 2020
count_status <- merge(x = count_19, y = status_19, by.x = "exploracao", by.y = "exploracao_id", all.x = TRUE, all.y = TRUE)

#### Remove NA values, SC and A0
count_status <- na.omit(count_status)
count_status <- as.data.frame(count_status[!count_status$classificacao_sanitaria == "A0" & !count_status$classificacao_sanitaria == "SC",])

#### Change to date format to arrange by data and farm id's --- ARRANGE NÃO RESULTA
count_status$data.y <- as.Date(count_status$data.y, format = "%Y-%m-%d")

count_status <- count_status %>% 
  arrange(data.y)

#### Select the last status of each farm -- NÃO ESTÁ A FUNCIONAR ESCOLHER O ÚLTIMO STATUS DE CADA EXPLORAÇÃO
count_last_status <- aggregate(count_status$classificacao_sanitaria, by=list(count_status$exploracao, count_status$contagem), FUN=last)
names(status_last_production) <- c("exploracao_id", "year", "production", "status")


#### Arrange counts per status 
count_status <- count_status %>%
  select(data)


### Percentage of vaccinated animals
vaccination_2019$percentage <- vaccination_2019$count / sum(vaccination_2019$count) * 100
vaccination_2019$percentage <- round(vaccination_2019$percentage, digits = 5)


vaccination_2020_graph <- ggplot(vaccination_2019, aes(x = status, y = percentage * 100, fill = production)) +
  geom_col(position = "dodge", size = 10, width = 1, aes(text = paste('Status: ', status,
                                                '<br>Production class: ', production,
                                                '<br>Vaccinated Animals: ', count,
                                                '<br>Percentage (%): ', percentage *100))) +
  scale_y_continuous(limits=c(0,1)) +
  theme_classic() +
  theme(legend.title = element_blank(),
        axis.title.x = element_text( size = 12),
        axis.text.x = element_text(size=8, 
                                   color = "black"),
        axis.text.y = element_text(size=10,
                                   color = "black")) +
  guides(fill=guide_legend(title="Género")) +
  scale_fill_brewer(palette = "Accent")
  
  
#Fazer gráfico interativo
ggplotly(vaccination_2019_graph, tooltip = "text") %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 15),
                                       "Vaccinated animals",
                                       rep("&nbsp;", 15),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")),
         legend = list(x = 1, y = 0))





## Vacinnated animals by production class over the months (2019)
vaccination_production_status$data.x <- as.Date(vaccination_production_status$data.x)
vaccination_production_status <- na.omit(vaccination_production_status)


### Add column with the month
vaccination_2019$month <- as.Date(vaccination_2019$data_de_entrada, format = "%m")
vaccination_2019$m <- as.Date(vaccination_2019)

slaughter_2019_month$month <- format(as.Date(slaughter_2019_month$month, format = "%Y-%m-%d"), "%m")
slaughter_2019_month$m <- format(as.Date(slaughter_2019_month$m, format = "%Y-%m-%d"), "%B")


### Geom_density plot of 2020
vaccination_production_status %>%
  filter(year == "2020") %>%
  ggplot(aes(data.x, fill = classe_controlo)) + 
  geom_density(alpha = 0.3, bw=0.5, position = "stack") + 
  scale_fill_brewer(palette="Dark2") +
  scale_x_continuous(trans = "log2")


## Geomline 
ggplot(vaccination_2020, aes(x = month, y = count, color = production)) +
  geom_line(size = 0.5, color = "gray60") + 
  geom_point(size = 2, aes(text = paste0(m, " - ", count, " animals vaccinated"))) + 
  scale_color_brewer(palette = "Paired") + 
  theme_light() + 
  theme() + 
  ggtitle("Animals vaccinated over the year's") + 
  labs(y = "Number of animals vaccinated",
       x = "Month", 
       color = " ") + 
  scale_x_continuous(breaks = seq(1,12, by = 1))

## Interactive graph
ggplotly(month_graph, tooltip = "text") %>%
  layout(yaxis = list(title =paste0(c(rep("&nbsp;", 30),
                                      "Number of animals vaccinated",
                                      rep("&nbsp;", 30),
                                      rep("\n&nbsp;", 2)),
                                    collapse = "")))




## box plot, % vaccinated animals by status (x_axis) and farm (points)
murders %>% mutate(rate = total/population*100000) %>%
  mutate(region=reorder(region, rate, FUN=median)) %>%
  ggplot(aes(region, rate)) +
  geom_boxplot() +
  geom_point()



## Vaccinated animals by LVS (mapa)


## See if vaccination intervals are accourdingly to the plan for each status/ production class (pie chart?)
###Verificação de cumprimento de prazos. 



# Map with contingency areas
## Table
contingency <- unique(contingencias)

### Convert geometry to sfc
contingency$geometria <- st_as_sfc(contingency$geometria, crs = NA_integer_, GeoJSON = FALSE)

### Convert table to sf dataframe
contingency <- st_as_sf(contingency)

### Add label column
contingency$info <- paste0(contingency$exploracao, " - ", contingency$fase, "<br>", contingency$designacao)

### Add colour column
colourvalues::colour_values(c(1:100),palette = "rainbow")
contingency$colour <- c("#FF7C00FF", "#00BCFFFF", "#FF0072FF")

### Manual legend
leg <- legend_element(variables = c("PTRG47B", "PTWX90H", "PTWX93C"),
                      colours = c("#FF7C00FF", "#00BCFFFF", "#FF0072FF"),
                      colour_type = "fill", 
                      variable_type = "category", 
                      title = "Farms under surveillance")
leg <- mapdeck_legend(leg)

## Mapdeck
mapdeck(token = token, style = mapdeck_style("dark")) %>%
  add_polygon(data = contingency,
              layer_id = "polygon_layer",
              fill_colour = "colour",
              legend = leg,
              tooltip = "info", 
              auto_highlight = TRUE)




##NEXT POSSIBLE QUESTIONS : Geom_point with vaccinated animals vs slaughtered animals , or other any other possible correlations?





