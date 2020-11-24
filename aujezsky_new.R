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
count_svl_total$total <- as.numeric(count_total$total)

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
count_svl <- as.data.frame(merge(count_svl, count_total, by.x = "svl", by.y = "svl"))
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

## Map with the last classification for each farm
### Add label
class_last$info1 <- paste0(class_last$exploracao, "<br>", class_last$svl, "<br>", class_last$classificacao_sanitaria, " ", "(2020)", "<br>")

mapdeck(token = token, style = mapdeck_style("light"), pitch = 20) %>%
  add_scatterplot(data = class_last, 
                  lat = "latitude", 
                  lon = "longitude",
                  radius = 200,
                  fill_colour = "classificacao_sanitaria",
                  legend = TRUE, 
                  tooltip = "info1",
                  layer_id = "point",
                  legend_options = list(fill_colour = list(title = "Sanitary Classification")),
                  palette = "viridis")

## Map - percentage of status by SVL (in 2020) ??????





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
status <- status %>% filter(status$data > "2016-01-01" & status$data < "2020-12-31")

### Remove duplicate rows
status <- unique(status)

### Add column with year
status$year <- format(as.Date(status$data, format="%d/%m/%Y"),"%Y")

########## SARA
### Give each row a number
status <- status %>%
  mutate(count = 1)

### Find the number of farms for each status
status_by_year2 <- as.data.frame(aggregate(status$count, by = list(status$year, status$classificacao_sanitaria), FUN = sum))
names(status_by_year2) <- c("year", "status", "count")

## Remove A0 and SC status
status_by_year2 <- as.data.frame(status_by_year2[!status_by_year2$status == "A0" & !status_by_year2$status == "SC",])
############### SARA

## Barplot
status_by_year_graph <- ggplot(status_by_year2, aes(x = year, y = count, fill = status)) + 
  geom_bar(stat = "identity", position = "dodge", aes(text = paste('Year: ', year,
                                                                   '<br>Status: ', status,
                                                                   '<br>Nº of Farms: ', count))) + 
  theme_light() +
  theme() +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, max(status_by_year2$count + 500))) +
  
  scale_fill_brewer(palette = "Set3") + 
  labs( title = "Number of farms per status over the years", size = 15,
        y = "Number of farms",
        x = "Year", 
        caption = "Fonte: DGAV") +
  theme(legend.title = element_blank(),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size=8, 
                                   color = "black"),
        axis.text.y = element_text(size=8,
                                   color = "black")) +
  guides(fill=guide_legend(title="Status"))
  

#Fazer gráfico interativo
ggplotly(status_by_year_graph, tooltip = "text") %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Number of farms",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")),
         legend = list(x = 1, y = 0))

### Não deviamos ver a proporção para comparar entre anos??? nº A4/total, A3/total exploracoes, etc, por ano.

## Number of farms by type of production and by status in each year

############ SARA
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
############# SARA





### Year as Date
farms_production_status$year <- as.Date(farms_production_status$year, format = "%Y")
farms_production_status$year <- format(as.Date(farms_production_status$year, format = "%Y-%m-%d"), "%Y")

farms_production_status <- as.Date((farms_production_status$year), format = "%Y")
farms_production_status <- format(as.Date(farms_production_status$year, format = "%Y-%m-%d"), "%Y")


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

### Merge with Count table to get the numer of animals by farm
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



