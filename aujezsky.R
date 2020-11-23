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
library(scales)

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

## 1.1 - Pig farm's distribution by LVS (local veterinary service) - HOW TO HAVE A LAYER WITH THAT?







# 1.2 - Number of animals by farm 
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


# 1.2.1 - Number of animals by LVS
## Table with total of animals by LVS
count_svl <- as.data.frame(aggregate(contagens$contagem, by = list(contagens$classe_produtiva, contagens$svl), FUN = sum))
count_svl <- count_svl %>% arrange(Group.2, Group.1)
names(count_svl) <- c("class", "svl", "count")

count_svl_total <- as.data.frame(aggregate(count_svl$count, by = list(count_svl$svl), FUN = sum))
names(count_svl_total) <- c("svl", "total")
count_svl_total$total <- as.numeric(count_svl_total$total)

## Plot with total number of animals by LVS
ggplot(count_svl_total, aes(x = svl, y = total, fill = svl)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  theme_light() +
  theme(legend.position = "none") +
  labs( title = "Number of animals by LVS", size = 15,
        y = "Number of animals",
        x = "Local Veterinary Service", 
        caption = "Fonte: DGAV") + 
  geom_text(aes(label=total), vjust = 0.3, hjust = 0, size = 2)

# 1.2.2 - Percentage of animals by class by LVS
## Table with percentage of animals by class by LVS
count_svl <- as.data.frame(merge(count_svl, count_svl_total, by.x = "svl", by.y = "svl"))
names(count_svl)[4] <- "total"

count_svl$count <- as.numeric(count_svl$count)
count_svl$total <- as.numeric(count_svl$total)
count_svl$percentage <- (count_svl$count / count_svl$total * 100)
names(count_svl)[5] <- "percentage"

## Plot with percentage of animals by class in each LVS
ggplot(count_svl, aes(fill = class, y = percentage, x = svl)) + 
  geom_bar(position = "fill", stat = "identity") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_brewer(palette = "Set2", labels = c("Weaners", "Piglets", "Sows", "Pigs", "Boars")) +
  labs( title = "Percentage of animals in each class by LVS", size = 15,
        y = "Percentage",
        x = "Local Veterinary Service", 
        caption = "Fonte: DGAV",
        fill = "")


# 1.3 Percentage of pig farms currently classified (in general and farm specified)

class <- as.data.frame(merge(contagens, classificacoes, by.x = "exploracao", by.y = "exploracao_id"))

class %>% select(exploracao, data, longitude, latitude, svl, classificacao_sanitaria)

### Select only declaracao_existencias from 2020
count <- count %>%
  filter(count$data > as.Date("2020-01-01"))






# → Tabela da prevalência, usar de alguma forma!!

# 2. Farms' status overview;
# 2.1 Farms' status by production type over the years
## Table with the farms' status and production type by year
status <- as.data.frame(merge(classificacoes, exploracoes, by.x = "exploracao_id", by.y = "exploracao")) 
status <- status %>% 
  filter(status$estado == "CONCLUIDO") %>%
  select(exploracao_id, data, classificacao_sanitaria, tipo_producao) %>%
  arrange(data, exploracao_id)

### Only with results between 2016 and 2020
status <- status %>% filter(status$data > "2016-01-01")
status <- status %>% filter(status$data < "2020-12-31")

### Remove duplicate rows
status <- unique(status)

### Add column with year
status$year <- format(as.Date(status$data, format="%d/%m/%Y"),"%Y")

### Different tables for each year
status_2016 <- status[status$year == '2016', ]
status_2017 <- status[status$year == '2017', ]
status_2018 <- status[status$year == '2018', ]
status_2019 <- status[status$year == '2019', ]
status_2020 <- status[status$year == '2020', ]

### Number of farms with each status by year
n_status_2016 <- as.data.frame(aggregate(x = status_2016, list(status = status_2016$classificacao_sanitaria), FUN = length))
n_status_2016 <- n_status_2016 %>% select(status, data)
names(n_status_2016) <- c("status", "count")

n_status_2017 <- as.data.frame(aggregate(x = status_2017, list(status = status_2017$classificacao_sanitaria), FUN = length))
n_status_2017 <- n_status_2017 %>% select(status, data)
names(n_status_2017) <- c("status", "count")

n_status_2018 <- as.data.frame(aggregate(x = status_2018, list(status = status_2018$classificacao_sanitaria), FUN = length))
n_status_2018 <- n_status_2018 %>% select(status, data)
names(n_status_2018) <- c("status", "count")

n_status_2019 <- as.data.frame(aggregate(x = status_2019, list(status = status_2019$classificacao_sanitaria), FUN = length))
n_status_2019 <- n_status_2019 %>% select(status, data)
names(n_status_2019) <- c("status", "count")

n_status_2020 <- as.data.frame(aggregate(x = status_2020, list(status = status_2020$classificacao_sanitaria), FUN = length))
n_status_2020 <- n_status_2020 %>% select(status, data)
names(n_status_2020) <- c("status", "count")

### Gather all tables in one
status_by_year <- as.data.frame(merge(n_status_2016, n_status_2017, by.x = "status", by.y = "status"))
names(status_by_year) <- c("status", "2016", "2017")
status_by_year <- as.data.frame(merge(status_by_year, n_status_2018, by.x = "status", by.y = "status"))
names(status_by_year) <- c("status", "2016", "2017", "2018")
status_by_year <- as.data.frame(merge(status_by_year, n_status_2019, by.x = "status", by.y = "status"))
names(status_by_year) <- c("status", "2016", "2017", "2018", "2019")
status_by_year <- as.data.frame(merge(status_by_year, n_status_2020, by.x = "status", by.y = "status"))
names(status_by_year) <- c("status", "2016", "2017", "2018", "2019", "2020")
status_by_year <- status_by_year[-9, ]

### Melt table
status_by_year <- as.data.frame(melt(status_by_year, id.vars = "status", measure.vars = c("2016", "2017", "2018", "2019", "2020")))
names(status_by_year) <- c("status", "year", "count")
status_by_year$count <- as.numeric(status_by_year$count)

## Barplot
ggplot(status_by_year, aes(x = year, y = count, fill = status)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme_light() +
  theme() +
  scale_fill_brewer(palette = "Set3") + 
  labs( title = "Number of farms in each status over the years", size = 15,
        y = "Number of farms",
        x = "Year", 
        caption = "Fonte: DGAV",
        fill = "Status")


### Number of farms by type of production and by status in each year
### Different tables for each year
rearing_2016 <- status_2016[status_2016$tipo_producao == "Recria e/ou acabamento", ]
production_2016 <- status_2016[status_2016$tipo_producao == "Produção", ]
piglets_2016 <- status_2016[status_2016$tipo_producao == "Produção de Leitões", ]
selection_2016 <- status_2016[status_2016$tipo_producao == "Seleção e/ou multiplicação", ]
others_2016 <- status_2016[status_2016$tipo_producao == "Outros", ]
collection_2016 <- status_2016[status_2016$tipo_producao == "Centro de Colheita de sémen", ]

rearing_2017 <- status_2017[status_2017$tipo_producao == "Recria e/ou acabamento", ]
production_2017 <- status_2017[status_2017$tipo_producao == "Produção", ]
piglets_2017 <- status_2017[status_2017$tipo_producao == "Produção de Leitões", ]
selection_2017 <- status_2017[status_2017$tipo_producao == "Seleção e/ou multiplicação", ]
others_2017 <- status_2017[status_2017$tipo_producao == "Outros", ]
collection_2017 <- status_2017[status_2017$tipo_producao == "Centro de Colheita de sémen", ]

rearing_2018 <- status_2018[status_2018$tipo_producao == "Recria e/ou acabamento", ]
production_2018 <- status_2018[status_2018$tipo_producao == "Produção", ]
piglets_2018 <- status_2018[status_2018$tipo_producao == "Produção de Leitões", ]
selection_2018 <- status_2018[status_2018$tipo_producao == "Seleção e/ou multiplicação", ]
others_2018 <- status_2018[status_2018$tipo_producao == "Outros", ]
collection_2018 <- status_2018[status_2018$tipo_producao == "Centro de Colheita de sémen", ]

rearing_2019 <- status_2019[status_2019$tipo_producao == "Recria e/ou acabamento", ]
production_2019 <- status_2019[status_2019$tipo_producao == "Produção", ]
piglets_2019 <- status_2019[status_2019$tipo_producao == "Produção de Leitões", ]
selection_2019 <- status_2019[status_2019$tipo_producao == "Seleção e/ou multiplicação", ]
others_2019 <- status_2019[status_2019$tipo_producao == "Outros", ]
collection_2019 <- status_2019[status_2019$tipo_producao == "Centro de Colheita de sémen", ]

rearing_2020 <- status_2020[status_2020$tipo_producao == "Recria e/ou acabamento", ]
production_2020 <- status_2020[status_2020$tipo_producao == "Produção", ]
piglets_2020 <- status_2020[status_2020$tipo_producao == "Produção de Leitões", ]
selection_2020 <- status_2020[status_2020$tipo_producao == "Seleção e/ou multiplicação", ]
others_2020 <- status_2020[status_2020$tipo_producao == "Outros", ]
collection_2020 <- status_2020[status_2020$tipo_producao == "Centro de Colheita de sémen", ]

### Keep only the last change in the each farm status
rearing_2016 <- setDT(rearing_2016)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
production_2016 <- setDT(production_2016)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
piglets_2016 <- setDT(piglets_2016)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
selection_2016 <- setDT(selection_2016)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
others_2016 <- setDT(others_2016)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
collection_2016 <- setDT(collection_2016)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]

rearing_2017 <- setDT(rearing_2017)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
production_2017 <- setDT(production_2017)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
piglets_2017 <- setDT(piglets_2017)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
selection_2017 <- setDT(selection_2017)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
others_2017 <- setDT(others_2017)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
collection_2017 <- setDT(collection_2017)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]

rearing_2018 <- setDT(rearing_2018)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
production_2018 <- setDT(production_2018)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
piglets_2018 <- setDT(piglets_2018)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
selection_2018 <- setDT(selection_2018)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
outhers_2018 <- setDT(others_2018)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
collection_2018 <- setDT(collection_2018)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]

rearing_2019 <- setDT(rearing_2019)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
production_2019 <- setDT(production_2019)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
piglets_2019 <- setDT(piglets_2019)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
selection_2019 <- setDT(selection_2019)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
others_2019 <- setDT(others_2019)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
collection_2019 <- setDT(collection_2019)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]

rearing_2020 <- setDT(rearing_2020)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
production_2020 <- setDT(production_2020)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
piglets_2020 <- setDT(piglets_2020)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
selection_2020 <- setDT(selection_2020)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
others_2020 <- setDT(others_2020)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
collection_2020 <- setDT(collection_2020)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]

### Count number of farms by status in each type of production and gather each year in one table
  #2016#
rearing_2016 <- as.data.frame(aggregate(x = rearing_2016, list(status = rearing_2016$classificacao_sanitaria), FUN = length))
rearing_2016 <- rearing_2016 %>% select(status, exploracao_id)
names(rearing_2016) <- c("status", "count")

production_2016 <- as.data.frame(aggregate(x = production_2016, list(status = production_2016$classificacao_sanitaria), FUN = length))
production_2016 <- production_2016 %>% select(status, exploracao_id)
names(production_2016) <- c("status", "count")

piglets_2016 <- as.data.frame(aggregate(x = piglets_2016, list(status = piglets_2016$classificacao_sanitaria), FUN = length))
piglets_2016 <- piglets_2016 %>% select(status, exploracao_id)
names(piglets_2016) <- c("status", "count")

selection_2016 <- as.data.frame(aggregate(x = selection_2016, list(status = selection_2016$classificacao_sanitaria), FUN = length))
selection_2016 <- selection_2016 %>% select(status, exploracao_id)
names(selection_2016) <- c("status", "count")

others_2016 <- as.data.frame(aggregate(x = others_2016, list(status = others_2016$classificacao_sanitaria), FUN = length))
others_2016 <- others_2016 %>% select(status, exploracao_id)
names(others_2016) <- c("status", "count")

collection_2016 <- as.data.frame(aggregate(x = collection_2016, list(status = collection_2016$classificacao_sanitaria), FUN = length))
collection_2016 <- collection_2016 %>% select(status, exploracao_id)
names(collection_2016) <- c("status", "count")

prod_2016 <- as.data.frame(merge(rearing_2016, production_2016, by.x = "status", by.y = "status", all = TRUE))
names(prod_2016) <- c("status", "recria", "production")
prod_2016 <- as.data.frame(merge(prod_2016, piglets_2016, by.x = "status", by.y = "status", all = TRUE))
names(prod_2016) <- c("status", "recria", "production", "piglets")
prod_2016 <- as.data.frame(merge(prod_2016, selection_2016, by.x = "status", by.y = "status", all = TRUE))
names(prod_2016) <- c("status", "recria", "production", "piglets", "selection")
prod_2016 <- as.data.frame(merge(prod_2016, collection_2016, by.x = "status", by.y = "status", all = TRUE))
names(prod_2016) <- c("status", "rearing", "production", "piglets", "selection", "collection")
prod_2016 <- as.data.frame(merge(prod_2016, others_2016, by.x = "status", by.y = "status", all = TRUE))
names(prod_2016) <- c("status", "rearing", "production", "piglets", "selection", "collection", "others")
prod_2016 <- prod_2016[-9, ]

prod_2016$year <- "2016"

  #2017#
rearing_2017 <- as.data.frame(aggregate(x = rearing_2017, list(status = rearing_2017$classificacao_sanitaria), FUN = length))
rearing_2017 <- rearing_2017 %>% select(status, exploracao_id)
names(rearing_2017) <- c("status", "count")

production_2017 <- as.data.frame(aggregate(x = production_2017, list(status = production_2017$classificacao_sanitaria), FUN = length))
production_2017 <- production_2017 %>% select(status, exploracao_id)
names(production_2017) <- c("status", "count")

piglets_2017 <- as.data.frame(aggregate(x = piglets_2017, list(status = piglets_2017$classificacao_sanitaria), FUN = length))
piglets_2017 <- piglets_2017 %>% select(status, exploracao_id)
names(piglets_2017) <- c("status", "count")

selection_2017 <- as.data.frame(aggregate(x = selection_2017, list(status = selection_2017$classificacao_sanitaria), FUN = length))
selection_2017 <- selection_2017 %>% select(status, exploracao_id)
names(selection_2017) <- c("status", "count")

others_2017 <- as.data.frame(aggregate(x = others_2017, list(status = others_2017$classificacao_sanitaria), FUN = length))
others_2017 <- others_2017 %>% select(status, exploracao_id)
names(others_2017) <- c("status", "count")

collection_2017 <- as.data.frame(aggregate(x = collection_2017, list(status = collection_2017$classificacao_sanitaria), FUN = length))
collection_2017 <- collection_2017 %>% select(status, exploracao_id)
names(collection_2017) <- c("status", "count")

prod_2017 <- as.data.frame(merge(rearing_2017, production_2017, by.x = "status", by.y = "status", all = TRUE))
names(prod_2017) <- c("status", "rearing", "production")
prod_2017 <- as.data.frame(merge(prod_2017, piglets_2017, by.x = "status", by.y = "status", all = TRUE))
names(prod_2017) <- c("status", "rearing", "production", "piglets")
prod_2017 <- as.data.frame(merge(prod_2017, selection_2017, by.x = "status", by.y = "status", all = TRUE))
names(prod_2017) <- c("status", "rearing", "production", "piglets", "selection")
prod_2017 <- as.data.frame(merge(prod_2017, collection_2017, by.x = "status", by.y = "status", all = TRUE))
names(prod_2017) <- c("status", "rearing", "production", "piglets", "selection", "collection")
prod_2017 <- as.data.frame(merge(prod_2017, others_2017, by.x = "status", by.y = "status", all = TRUE))
names(prod_2017) <- c("status", "rearing", "production", "piglets", "selection", "collection", "others")
prod_2017 <- prod_2017[-8, ]

prod_2017$year <- "2017"

  #2018#
rearing_2018 <- as.data.frame(aggregate(x = rearing_2018, list(status = rearing_2018$classificacao_sanitaria), FUN = length))
rearing_2018 <- rearing_2018 %>% select(status, exploracao_id)
names(rearing_2018) <- c("status", "count")

production_2018 <- as.data.frame(aggregate(x = production_2018, list(status = production_2018$classificacao_sanitaria), FUN = length))
production_2018 <- production_2018 %>% select(status, exploracao_id)
names(production_2018) <- c("status", "count")

piglets_2018 <- as.data.frame(aggregate(x = piglets_2018, list(status = piglets_2018$classificacao_sanitaria), FUN = length))
piglets_2018 <- piglets_2018 %>% select(status, exploracao_id)
names(piglets_2018) <- c("status", "count")

selection_2018 <- as.data.frame(aggregate(x = selection_2018, list(status = selection_2018$classificacao_sanitaria), FUN = length))
selection_2018 <- selection_2018 %>% select(status, exploracao_id)
names(selection_2018) <- c("status", "count")

others_2018 <- as.data.frame(aggregate(x = others_2018, list(status = others_2018$classificacao_sanitaria), FUN = length))
others_2018 <- others_2018 %>% select(status, exploracao_id)
names(others_2018) <- c("status", "count")

collection_2018 <- as.data.frame(aggregate(x = collection_2018, list(status = collection_2018$classificacao_sanitaria), FUN = length))
collection_2018 <- collection_2018 %>% select(status, exploracao_id)
names(collection_2018) <- c("status", "count")

prod_2018 <- as.data.frame(merge(rearing_2018, production_2018, by.x = "status", by.y = "status", all = TRUE))
names(prod_2018) <- c("status", "rearing", "production")
prod_2018 <- as.data.frame(merge(prod_2018, piglets_2018, by.x = "status", by.y = "status", all = TRUE))
names(prod_2018) <- c("status", "rearing", "production", "piglets")
prod_2018 <- as.data.frame(merge(prod_2018, selection_2018, by.x = "status", by.y = "status", all = TRUE))
names(prod_2018) <- c("status", "rearing", "production", "piglets", "selection")
prod_2018 <- as.data.frame(merge(prod_2018, collection_2018, by.x = "status", by.y = "status", all = TRUE))
names(prod_2018) <- c("status", "rearing", "production", "piglets", "selection", "collection")
prod_2018 <- as.data.frame(merge(prod_2018, others_2018, by.x = "status", by.y = "status", all = TRUE))
names(prod_2018) <- c("status", "rearing", "production", "piglets", "selection", "collection", "others")
prod_2018 <- prod_2018[-9, ]

prod_2018$year <- "2018"

  #2019#
rearing_2019 <- as.data.frame(aggregate(x = rearing_2019, list(status = rearing_2019$classificacao_sanitaria), FUN = length))
rearing_2019 <- rearing_2019 %>% select(status, exploracao_id)
names(rearing_2019) <- c("status", "count")

production_2019 <- as.data.frame(aggregate(x = production_2019, list(status = production_2019$classificacao_sanitaria), FUN = length))
production_2019 <- production_2019 %>% select(status, exploracao_id)
names(production_2019) <- c("status", "count")

piglets_2019 <- as.data.frame(aggregate(x = piglets_2019, list(status = piglets_2019$classificacao_sanitaria), FUN = length))
piglets_2019 <- piglets_2019 %>% select(status, exploracao_id)
names(piglets_2019) <- c("status", "count")

selection_2019 <- as.data.frame(aggregate(x = selection_2019, list(status = selection_2019$classificacao_sanitaria), FUN = length))
selection_2019 <- selection_2019 %>% select(status, exploracao_id)
names(selection_2019) <- c("status", "count")

others_2019 <- as.data.frame(aggregate(x = others_2019, list(status = others_2019$classificacao_sanitaria), FUN = length))
others_2019 <- others_2019 %>% select(status, exploracao_id)
others_2019 <- others_2019[-1, ]
names(others_2019) <- c("status", "count")

collection_2019 <- as.data.frame(aggregate(x = collection_2019, list(status = collection_2019$classificacao_sanitaria), FUN = length))
collection_2019 <- collection_2019 %>% select(status, exploracao_id)
names(collection_2019) <- c("status", "count")

prod_2019 <- as.data.frame(merge(rearing_2019, production_2019, by.x = "status", by.y = "status", all = TRUE))
names(prod_2019) <- c("status", "rearing", "production")
prod_2019 <- as.data.frame(merge(prod_2019, piglets_2019, by.x = "status", by.y = "status", all = TRUE))
names(prod_2019) <- c("status", "rearing", "production", "piglets")
prod_2019 <- as.data.frame(merge(prod_2019, selection_2019, by.x = "status", by.y = "status", all = TRUE))
names(prod_2019) <- c("status", "rearing", "production", "piglets", "selection")
prod_2019 <- as.data.frame(merge(prod_2019, collection_2019, by.x = "status", by.y = "status", all = TRUE))
names(prod_2019) <- c("status", "rearing", "production", "piglets", "selection", "collection")
prod_2019 <- as.data.frame(merge(prod_2019, others_2019, by.x = "status", by.y = "status", all = TRUE))
names(prod_2019) <- c("status", "rearing", "production", "piglets", "selection", "collection", "others")
prod_2019 <- prod_2019[-9, ]

prod_2019$year <- "2019"

  #2020#
rearing_2020 <- as.data.frame(aggregate(x = rearing_2020, list(status = rearing_2020$classificacao_sanitaria), FUN = length))
rearing_2020 <- rearing_2020 %>% select(status, exploracao_id)
names(rearing_2020) <- c("status", "count")

production_2020 <- as.data.frame(aggregate(x = production_2020, list(status = production_2020$classificacao_sanitaria), FUN = length))
production_2020 <- production_2020 %>% select(status, exploracao_id)
names(production_2020) <- c("status", "count")

piglets_2020 <- as.data.frame(aggregate(x = piglets_2020, list(status = piglets_2020$classificacao_sanitaria), FUN = length))
piglets_2020 <- piglets_2020 %>% select(status, exploracao_id)
names(piglets_2020) <- c("status", "count")

selection_2020 <- as.data.frame(aggregate(x = selection_2020, list(status = selection_2020$classificacao_sanitaria), FUN = length))
selection_2020 <- selection_2020 %>% select(status, exploracao_id)
names(selection_2020) <- c("status", "count")

others_2020 <- as.data.frame(aggregate(x = others_2020, list(status = others_2020$classificacao_sanitaria), FUN = length))
others_2020 <- others_2020 %>% select(status, exploracao_id)
names(others_2020) <- c("status", "count")

collection_2020 <- as.data.frame(aggregate(x = collection_2020, list(status = collection_2020$classificacao_sanitaria), FUN = length))
collection_2020 <- collection_2020 %>% select(status, exploracao_id)
names(collection_2020) <- c("status", "count")

prod_2020 <- as.data.frame(merge(rearing_2020, production_2020, by.x = "status", by.y = "status", all = TRUE))
names(prod_2020) <- c("status", "rearing", "production")
prod_2020 <- as.data.frame(merge(prod_2020, piglets_2020, by.x = "status", by.y = "status", all = TRUE))
names(prod_2020) <- c("status", "rearing", "production", "piglets")
prod_2020 <- as.data.frame(merge(prod_2020, selection_2020, by.x = "status", by.y = "status", all = TRUE))
names(prod_2020) <- c("status", "rearing", "production", "piglets", "selection")
prod_2020 <- as.data.frame(merge(prod_2020, collection_2020, by.x = "status", by.y = "status", all = TRUE))
names(prod_2020) <- c("status", "rearing", "production", "piglets", "selection", "collection")
prod_2020 <- as.data.frame(merge(prod_2020, others_2020, by.x = "status", by.y = "status", all = TRUE))
names(prod_2020) <- c("status", "rearing", "production", "piglets", "selection", "collection", "others")
prod_2020 <- prod_2020[-9, ]

prod_2020$year <- "2020"

### Merge all in one table
prod_year <- rbind(prod_2016, prod_2017, prod_2018, prod_2019, prod_2020)

### Substitue all NA with 0
prod_year[is.na(prod_year)] = 0

### Reshape table to plot
prod_year <- as.data.frame(melt(prod_year, id.vars = c("status", "year"), measure.vars = c("rearing", "production", "piglets", "selection", "collection", "others")))
names(prod_year)[3:4] <- c("production", "count")

### Year as Date
prod_year$year <- as.Date(prod_year$year, format = "%Y")
prod_year$year <- format(as.Date(prod_year$year, format = "%Y-%m-%d"), "%Y")

## Stacked bar plot
ggplot(prod_year, aes(fill = production, y = count, x = status)) +
  geom_bar(position = "stack", stat = "identity") + 
  scale_fill_brewer(palette = "Accent", labels = c("Rearing and finishing farms", "Production farms", "Piglet production farms", "Selection and breeding farms", "Semen collection centers", "Others")) +
  theme_pubclean() + 
  theme(legend.position = "bottom") + 
  ggtitle("Number of farms by type of production in each status over the years") + 
  facet_wrap(~year) + 
  labs(y = "Number of farms",
       x = "Status", 
       caption = "Fonte: DGAV",
       fill = "Production type")

## Line chart for each status
a1 <- prod_year %>% filter(prod_year$status == "A1")

ggplot(a1, aes(color = production, group = production, y = count, x = year)) +
  geom_line(size = 1) +
  geom_point(size = 2) + 
  scale_color_brewer(palette = "Accent", labels = c("Rearing and finishing farms", "Production farms", "Piglet production farms", "Selection and breeding farms", "Semen collection centers", "Others")) +
  theme_pubclean() + 
  theme(legend.position = "bottom") + 
  ggtitle("A1 Status - Number of farms by type of production over the years") + 
  labs(y = "Number of farms",
       x = "Year", 
       caption = "Fonte: DGAV",
       color = "Production type")


a2 <- prod_year %>% filter(prod_year$status == "A2")

ggplot(a2, aes(color = production, group = production, y = count, x = year)) +
  geom_line(size = 1) +
  geom_point(size = 2) + 
  scale_color_brewer(palette = "Accent", labels = c("Rearing and finishing farms", "Production farms", "Piglet production farms", "Selection and breeding farms", "Semen collection centers", "Others")) +
  theme_pubclean() + 
  theme(legend.position = "bottom") + 
  ggtitle("A2 Status - Number of farms by type of production over the years") + 
  labs(y = "Number of farms",
       x = "Year", 
       caption = "Fonte: DGAV",
       color = "Production type")


a2a <- prod_year %>% filter(prod_year$status == "A2A")

ggplot(a2a, aes(color = production, group = production, y = count, x = year)) +
  geom_line(size = 1) +
  geom_point(size = 2) + 
  scale_color_brewer(palette = "Accent", labels = c("Rearing and finishing farms", "Production farms", "Piglet production farms", "Selection and breeding farms", "Semen collection centers", "Others")) +
  theme_pubclean() + 
  theme(legend.position = "bottom") + 
  ggtitle("A2A Status - Number of farms by type of production over the years") + 
  labs(y = "Number of farms",
       x = "Year", 
       caption = "Fonte: DGAV",
       color = "Production type")


a2na <- prod_year %>% filter(prod_year$status == "A2NA")

ggplot(a2na, aes(color = production, group = production, y = count, x = year)) +
  geom_line(size = 1) +
  geom_point(size = 2) + 
  scale_color_brewer(palette = "Accent", labels = c("Rearing and finishing farms", "Production farms", "Piglet production farms", "Selection and breeding farms", "Semen collection centers", "Others")) +
  theme_pubclean() + 
  theme(legend.position = "bottom") + 
  ggtitle("A2NA Status - Number of farms by type of production over the years") + 
  labs(y = "Number of farms",
       x = "Year", 
       caption = "Fonte: DGAV",
       color = "Production type")


a3 <- prod_year %>% filter(prod_year$status == "A3")

ggplot(a3, aes(color = production, group = production, y = count, x = year)) +
  geom_line(size = 1) +
  geom_point(size = 2) + 
  scale_color_brewer(palette = "Accent", labels = c("Rearing and finishing farms", "Production farms", "Piglet production farms", "Selection and breeding farms", "Semen collection centers", "Others")) +
  theme_pubclean() + 
  theme(legend.position = "bottom") + 
  ggtitle("A3 Status - Number of farms by type of production over the years") + 
  labs(y = "Number of farms",
       x = "Year", 
       caption = "Fonte: DGAV",
       color = "Production type")


a4 <- prod_year %>% filter(prod_year$status == "A4")

ggplot(a4, aes(color = production, group = production, y = count, x = year)) +
  geom_line(size = 1) +
  geom_point(size = 2) + 
  scale_color_brewer(palette = "Accent", labels = c("Rearing and finishing farms", "Production farms", "Piglet production farms", "Selection and breeding farms", "Semen collection centers", "Others")) +
  theme_pubclean() + 
  theme(legend.position = "bottom") + 
  ggtitle("A4 Status - Number of farms by type of production over the years") + 
  labs(y = "Number of farms",
       x = "Year", 
       caption = "Fonte: DGAV",
       color = "Production type")


a5 <- prod_year %>% filter(prod_year$status == "A5")

ggplot(a5, aes(color = production, group = production, y = count, x = year)) +
  geom_line(size = 1) +
  geom_point(size = 2) + 
  scale_color_brewer(palette = "Accent", labels = c("Rearing and finishing farms", "Production farms", "Piglet production farms", "Selection and breeding farms", "Semen collection centers", "Others")) +
  theme_pubclean() + 
  theme(legend.position = "bottom") + 
  ggtitle("A5 Status - Number of farms by type of production over the years") + 
  labs(y = "Number of farms",
       x = "Year", 
       caption = "Fonte: DGAV",
       color = "Production type")

# 2.2 Current farms by status
## Table with current status by farm 
now_status <- as.data.frame(merge(exploracoes, classificacoes, by.x = "exploracao", by.y = "exploracao_id", all = TRUE))

now_status <- unique(now_status)

now_status <- now_status %>% group_by(exploracao)
now_status <- now_status %>% select(exploracao, longitude, latitude, data, tipo_instalacao, classificacao_sanitaria)

### Remove SC
now_status <- now_status %>% filter(classificacao_sanitaria != "SC")

### Select the last change os status in each farm
now_status <- subset(now_status, data <= "2020-12-31")
now_status <- setDT(now_status)[order(exploracao, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao)]

### Remove NA
now_status <- na.omit(now_status)

### Merge with Count table to get the number of animals by farm
now_status <- merge(now_status, count, by.x = "exploracao", by.y = "exploracao", all.x = TRUE, all.y = FALSE)
now_status <- now_status %>% select(exploracao, longitude.x, latitude.x, total, tipo_instalacao, classificacao_sanitaria )
names(now_status)[2:3] <- c("longitude", "latitude")

### Add the word "animals" to the total and replace the NA with a blank space
now_status$total <- paste0(now_status$total, " ", "animals")
now_status$total <- replace(now_status$total, now_status$total == "NA animals", " ")


### Add column with info for map
now_status$info <- paste0(now_status$exploracao, "<br>", now_status$tipo_instalacao, "<br>", now_status$classificacao_sanitaria, "<br>", now_status$total)

## Map with farms colored by status
mapdeck(token = token, style = mapdeck_style("dark")) %>%
  add_scatterplot(data = now_status, 
                  lat = "latitude",
                  lon = "longitude",
                  radius = 1500,
                  fill_colour = "classificacao_sanitaria",
                  layer_id = "scatter_layer",
                  legend = TRUE,
                  tooltip = "info", 
                  legend_options = list(fill_colour = list(title = "Farm's status")),
                  palette = "spectral")


## Percentage of farms by status
### Table with number of farms by status 
status_percentage <- as.data.frame(aggregate(x = now_status, list(status = now_status$classificacao_sanitaria), FUN = length))
status_percentage <- status_percentage %>% select(status, total)
status_percentage <- status_percentage[-1, ]

### Add column with total number of farms
status_percentage$sum <- sum(status_percentage$total)

### Add column with percentage
status_percentage$percentage <- (status_percentage$total / status_percentage$sum) * 100
status_percentage$percentage <- round(status_percentage$percentage, digits = 1)

### Add column with label
status_percentage$label <- paste0(status_percentage$status, " ", "(", status_percentage$percentage, "%", ")")

## Piechart
ggplot(status_percentage, aes(x = "", y = percentage, fill = status)) +
  geom_bar(stat = "identity", width = 1) + 
  coord_polar(theta = "y", start = 0) + 
  scale_fill_brewer(palette = "Set3", labels = status_percentage$label ) +
  theme_void() + 
  theme(legend.position = "right") + 
  ggtitle("Percentage of farms by status") + 
  labs(caption = "Fonte: DGAV",
       fill = "Status") 




# 3. Abates
##Mapa com nº abates de 2019 por exploração e/ou por SVL;
##Geom_line para avaliar o nº abates ao longo do tempo (mensal) // DSAVR ou SVL


# 4. Ensaios Laboratoriais
##Avaliar nº positivos a DA / total de animais amostrados por SVL ou por laboratório;
##Evolução do nº de resultados positivos / nº animais amostrados ao longo do tempo (geom_line);


# 5. Vacinações
##Avaliar nº vacinados nos diferentes anos por classe (small multiple geom bar, 1 para cada ano, cada barra para a classe)
##Nº vacinados por SVL (mapa)
##Nº e Percentagem de animais vacinados por classificação sanitária (lolipop chart com 2 eixos, 1 para nº e 1 para %)
##Verificação de cumprimento de prazos, intervalo entre vacinações. Se está de acordo com o estipulado para cada classificação sanitária e/ou classe (pie chart)



