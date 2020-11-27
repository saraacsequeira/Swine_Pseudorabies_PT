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
library(hrbrthemes)

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

### Remove scientific notation
options(scipen=999)

## Plot with total number of animals by LVS
count_svl_total_graph <- ggplot(count_svl_total, aes(x = svl, y = total, fill = svl)) + 
  geom_bar(stat = "identity", aes(text = paste0(svl, "<br>", total, " animals"))) + 
  coord_flip() + 
  theme_light() +
  theme(legend.position = "none") +
  labs( title = "Number of animals by LVS", size = 15,
        y = "Number of animals",
        x = "Local Veterinary Service", 
        caption = "Fonte: DGAV")

## Interactive Graph
ggplotly(count_svl_total_graph, tooltip = "text") %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 30),
                                       "Local Veterinary Service",
                                       rep("&nbsp;", 30),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")),
         legend = list(x = 1, y = 0))


# 1.2.2 - Percentage of animals by class by LVS
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
  theme_light() +
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

## Add the numer of animals by farm
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
  theme_light() +
  theme() +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, max(status_by_year$count + 500))) +
  
  scale_fill_brewer(palette = "Set3") + 
  labs( title = "Number of farms per status over the years", size = 15,
        y = "Number of farms",
        x = "Year", 
        caption = "Fonte: DGAV") +
  theme(legend.title = element_blank(),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size=8, 
                                   color = "black"),
        axis.text.y = element_text(size=10,
                                   color = "black")) +
  guides(fill=guide_legend(title="Status"))


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
## A1 ggplot
a1 <- farms_production_status %>% filter(farms_production_status$status == "A1")

a1_graph <- ggplot(a1, aes(color = production, group = production, y = count, x = year)) +
  geom_line(size = 1) +
  geom_point(size = 2, aes(text = paste0(production, "<br>", count, " farms"))) + 
  scale_y_continuous(breaks=(seq(0, 70, 10)), limits = c(0, 70)) +
  scale_color_brewer(palette = "Accent") +
  theme_pubclean() + 
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
  theme_pubclean() + 
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
  theme_pubclean() + 
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
  theme_pubclean() + 
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
  theme_pubclean() + 
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
  theme_pubclean() + 
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
  theme_pubclean() + 
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
  theme_minimal() + 
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

### Total number of animals slaghtered by farm
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


## Plot with number of animals slaughtered by FVRD
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

##Mapa com nº abates de 2019 por exploração e/ou por SVL;

# 3.5 Mean of daily slaughters in each month between 2016 and 2020
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



# 4. Ensaios Laboratoriais
##Avaliar nº positivos a DA / total de animais amostrados por SVL ou por laboratório;
##Evolução do nº de resultados positivos / nº animais amostrados ao longo do tempo (geom_line);








# 5. Vaccination
### Nº e Percentagem de animais vacinados por classificação sanitária (lolipop chart com 2 eixos, 1 para nº e 1 para %)

### Select rows of interest
vaccination_data <- vacinacoes %>% 
  filter(vacinacoes$estado == "CONCLUIDO") %>%
  select(data, exploracao, classe_controlo, vacinados_classe) %>%
  arrange(data, exploracao)

colnames(vaccination_data)[2] <- "exploracao_id"

### Only with results between 2016 and 2020
vaccination_data <- vaccination_data %>% filter(vaccination_data$data > "2016-01-01" & vaccination_data$data < "2020-12-31")

## Number of vaccinated animals per production class by year
### Add status column to our data
vaccination_status <- merge(x = vaccination_data, y = status, by = "exploracao_id", all.x = TRUE)

vaccination_production_status <- vaccination_status %>% 
  select(classificacao_sanitaria, data.x, year, exploracao_id, classe_controlo, vacinados_classe) %>%
  arrange(year, exploracao_id)

### Group by status, year and production classes
vaccination_production_year <- as.data.frame(aggregate(vaccination_production_status$vacinados_classe, by = list(vaccination_production_status$classificacao_sanitaria, vaccination_production_status$year, vaccination_production_status$classe_controlo), FUN = sum))
names(vaccination_production_year) <- c("status", "year", "production", "count")

### Remove A0 and SC status
vaccination_production_year <- as.data.frame(vaccinaction_production_year[!vaccinaction_production_year$status == "A0" & !vaccinaction_production_year$status == "SC",])

### Count as numeric
vaccination_production_year$count <- as.numeric(vaccinaction_production_year$count)

### Chane to english
vaccination_production_year$production <- replace(vaccinaction_production_year$production, vaccinaction_production_year$production == "Engorda", "Fattening")
vaccination_production_year$production <- replace(vaccinaction_production_year$production, vaccinaction_production_year$production == "Reprodutores", "Breeding")
vaccination_production_year$production <- replace(vaccinaction_production_year$production, vaccinaction_production_year$production == "Substituição", "Replacement")

### Year as Date
vaccination_production_year$year <- as.Date(vaccinaction_production_year$year, format = "%Y")
vaccination_production_year$year <- format(as.Date(vaccinaction_production_year$year, format = "%Y-%m-%d"), "%Y")

### Remove NA values
vaccination_production_year <- na.omit(vaccinaction_production_year)

### Prepare for graph
vaccination_production_year <- vaccination_production_year %>% 
  arrange(year, status, production)

## Stacked bar plot
vaccination_graph <- ggplot(vaccinaction_production_year, 
                            aes(x = status, y = count, fill = production)) + 
  geom_bar(stat = "identity", aes(text = paste0(count, "<br>", " animals"))) + 
  ## to order colors
  geom_col() +
  facet_wrap(~year) +
  coord_flip() + 
  theme_bw() +
  labs( title = "Number of animals vaccinated by production type in each status over the years", size = 20,
        y = "Number of animals",
        x = "", 
        caption = "Fonte: DGAV") +
  scale_fill_brewer(palette = "Accent") +
  scale_y_continuous(trans = "log2") +
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
      ## tentar resolver a apresentação de texto do eixo x e cores na mesma ordem





## Percentage of vaccinated animals by status in 2020
#### Select only 2020 data
vaccination_2020 <- vaccinaction_production_year %>% filter(vaccinaction_production_year$year == "2020")


vaccination_2020 <- vaccination_2020 %>%
  mutate(total = sum(count))

vaccination_2020_graph <- ggplot(vaccinaction_2020, aes(x = status, y = count / total * 100, fill = production)) +
  geom_col(width = 3, aes(text = paste('Status: ', status,
                                                '<br>Production class: ', production,
                                                '<br>Vaccinated Animals: ', count,
                                                '<br>Percentage (%): ', count / total * 100))) +
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
ggplotly(vaccination_2020_graph, tooltip = "text") %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 15),
                                       "Vaccinated animals",
                                       rep("&nbsp;", 15),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")),
         legend = list(x = 1, y = 0))











### Add counts of each farm to the data
vaccination_counts <- merge(vaccination_production_status, count, by.x = "exploracao_id", by.y = "exploracao", all.x = TRUE, all.y = FALSE)

### Select data
vaccination_counts <- vaccination_counts %>%
  select(exploracao_id, data.x, year, classificacao_sanitaria, classe_controlo, vacinados_classe, total) %>%
  arrange(classe_controlo, exploracao_id, classificacao_sanitaria)

### Remove NA values
vaccination_counts <- na.omit(vaccination_counts)

library(gganimate)
# Make a ggplot, but add frame=year: one image per year
ggplot(vaccination_counts, aes(x = classificacao_sanitaria, y = vacinados_classe, size = total, colour = exploracao_id)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~classe_controlo) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'Status', y = 'Vaccinated animals') +
  transition_time(~year) +
  ease_aes('linear')

# Save at gif:
anim_save("271-ggplot2-animated-gif-chart-with-gganimate2.gif")


## Vaccinated animals by LVS (mapa)


## See if vaccination intervals are accourdingly to the plan for each status/ production class (pie chart?)
###Verificação de cumprimento de prazos. 



