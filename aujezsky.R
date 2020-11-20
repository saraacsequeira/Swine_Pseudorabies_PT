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
count <- as.data.frame(aggregate(contagens$contagem, by = list(contagens$declaracao_existencias), FUN = sum))
count <- as.data.frame(merge(contagens, count, by.x = "declaracao_existencias", by.y = "Group.1"))
names(count)[11] <- "total"
count$total <- as.numeric(count$total)

### Remove duplicated rows based on declaracao_existencias
count <- count %>% 
  distinct(declaracao_existencias, .keep_all = TRUE)


## Map
### Add column with label 
count$info <- paste0(count$exploracao, "<br>", count$svl, " - ", count$dsavr, "<br>", count$contagem, " ", "animais", "<br>")

### Select only declaracao_existencias from 2020
count <- count %>%
  filter(count$data > as.Date("2020-01-01"))

### Remove farms missing any information
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


count_total <- as.data.frame(aggregate(count_svl$count, by = list(count_svl$svl), FUN = sum))
names(count_total) <- c("svl", "total")
count_total$total <- as.numeric(count_total$total)

<<<<<<< HEAD

## Plot with total number of animals by LVS
=======
## Plot with total number of animals by SVL
>>>>>>> c764c53c6f42cbaf4a6ed1258a53e7a73a261374
ggplot(count_total, aes(x = svl, y = total, fill = svl)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  theme_light() +
  theme(legend.position = "none") +
  labs( title = "Number of animals by LVS", size = 15,
        y = "Number of animals",
        x = "Local Veterinary Service", 
        caption = "Fonte: DGAV") + 
  geom_text(aes(label=total), vjust = 0.3, hjust = 0, size = 2)

<<<<<<< HEAD
# 1.2.2 - Percentage of animals by class by LVS
## Table with percentage of animals by class by LVS
count_svl <- as.data.frame(merge(count_svl, count_total, by.x = "svl", by.y = "svl"))
=======
# 1.2.2 - Percentage of animals by class by SVL
## Table with percentage of animals by class by SVL
count_svl1 <- as.data.frame(merge(count_svl, count_total, by.x = "svl", by.y = "svl"))
>>>>>>> c764c53c6f42cbaf4a6ed1258a53e7a73a261374
names(count_svl)[4] <- "total"

count_svl$contagem <- as.numeric(count_svl$contagem)
count_svl$total <- as.numeric(count_svl$total)
count_svl$percentagem <- (count_svl$contagem / count_svl$total * 100)
names(count_svl)[5] <- "percentagem"

<<<<<<< HEAD
## Plot with percentage of animals by class in each LVS
ggplot(count_svl, aes(fill = classe, y = percentagem, x = svl)) + 
  geom_bar(position = "fill", stat = "identity") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_brewer(palette = "Set2") +
  labs( title = "Percentage of animals in each class by LVS", size = 15,
        y = "Percentage",
        x = "Local Veterinary Service", 
        caption = "Fonte: DGAV",
        fill = "")
=======
## Plot with percentage of animals by class in each SVL



>>>>>>> c764c53c6f42cbaf4a6ed1258a53e7a73a261374


# 1.3 Percentage of pig farms currently classified (in general and farm specified)

class <- as.data.frame(merge(contagens, classificacoes, by.x = "exploracao", by.y = "exploracao_id"))

class %>% select(exploracao, data, longitude, latitude, svl, classificacao_sanitaria)

class %>%
  dplyr::filter(DATE1 >= as.POSIXct('2018-01-01'))






# → Tabela da prevalência, usar de alguma forma!!

# 2. Farms' status overview;
# 2.1 Farms' status by production type over the years
## Table with the farms' status and production type by year
status <- as.data.frame(merge(classificacoes, exploracoes, by.x = "exploracao_id", by.y = "exploracao")) %>%
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
n_status_2016 <- as.data.frame(aggregate(x = status_2016, list(estatuto = status_2016$classificacao_sanitaria), FUN = length))
n_status_2016 <- n_status_2016 %>% select(estatuto, data)
names(n_status_2016) <- c("estatuto", "contagem")

n_status_2017 <- as.data.frame(aggregate(x = status_2017, list(estatuto = status_2017$classificacao_sanitaria), FUN = length))
n_status_2017 <- n_status_2017 %>% select(estatuto, data)
names(n_status_2017) <- c("estatuto", "contagem")

n_status_2018 <- as.data.frame(aggregate(x = status_2018, list(estatuto = status_2018$classificacao_sanitaria), FUN = length))
n_status_2018 <- n_status_2018 %>% select(estatuto, data)
names(n_status_2018) <- c("estatuto", "contagem")

n_status_2019 <- as.data.frame(aggregate(x = status_2019, list(estatuto = status_2019$classificacao_sanitaria), FUN = length))
n_status_2019 <- n_status_2019 %>% select(estatuto, data)
names(n_status_2019) <- c("estatuto", "contagem")

n_status_2020 <- as.data.frame(aggregate(x = status_2020, list(estatuto = status_2020$classificacao_sanitaria), FUN = length))
n_status_2020 <- n_status_2020 %>% select(estatuto, data)
names(n_status_2020) <- c("estatuto", "contagem")

### Gather all tables in one
status_by_year <- as.data.frame(merge(n_status_2016, n_status_2017, by.x = "estatuto", by.y = "estatuto"))
names(status_by_year) <- c("estatuto", "2016", "2017")
status_by_year <- as.data.frame(merge(status_by_year, n_status_2018, by.x = "estatuto", by.y = "estatuto"))
names(status_by_year) <- c("estatuto", "2016", "2017", "2018")
status_by_year <- as.data.frame(merge(status_by_year, n_status_2019, by.x = "estatuto", by.y = "estatuto"))
names(status_by_year) <- c("estatuto", "2016", "2017", "2018", "2019")
status_by_year <- as.data.frame(merge(status_by_year, n_status_2020, by.x = "estatuto", by.y = "estatuto"))
names(status_by_year) <- c("estatuto", "2016", "2017", "2018", "2019", "2020")
status_by_year <- status_by_year[-9, ]


### Number of farms by type of production and by status in each year
### Different tables for each year
recria_2016 <- status_2016[status_2016$tipo_producao == "Recria e/ou acabamento", ]
producao_2016 <- status_2016[status_2016$tipo_producao == "Produção", ]
leitoes_2016 <- status_2016[status_2016$tipo_producao == "Produção de Leitões", ]
selecao_2016 <- status_2016[status_2016$tipo_producao == "Seleção e/ou multiplicação", ]
outros_2016 <- status_2016[status_2016$tipo_producao == "Outros", ]
colheita_2016 <- status_2016[status_2016$tipo_producao == "Centro de Colheita de sémen", ]

recria_2017 <- status_2017[status_2017$tipo_producao == "Recria e/ou acabamento", ]
producao_2017 <- status_2017[status_2017$tipo_producao == "Produção", ]
leitoes_2017 <- status_2017[status_2017$tipo_producao == "Produção de Leitões", ]
selecao_2017 <- status_2017[status_2017$tipo_producao == "Seleção e/ou multiplicação", ]
outros_2017 <- status_2017[status_2017$tipo_producao == "Outros", ]
colheita_2017 <- status_2017[status_2017$tipo_producao == "Centro de Colheita de sémen", ]

recria_2018 <- status_2018[status_2018$tipo_producao == "Recria e/ou acabamento", ]
producao_2018 <- status_2018[status_2018$tipo_producao == "Produção", ]
leitoes_2018 <- status_2018[status_2018$tipo_producao == "Produção de Leitões", ]
selecao_2018 <- status_2018[status_2018$tipo_producao == "Seleção e/ou multiplicação", ]
outros_2018 <- status_2018[status_2018$tipo_producao == "Outros", ]
colheita_2018 <- status_2018[status_2018$tipo_producao == "Centro de Colheita de sémen", ]

recria_2019 <- status_2019[status_2019$tipo_producao == "Recria e/ou acabamento", ]
producao_2019 <- status_2019[status_2019$tipo_producao == "Produção", ]
leitoes_2019 <- status_2019[status_2019$tipo_producao == "Produção de Leitões", ]
selecao_2019 <- status_2019[status_2019$tipo_producao == "Seleção e/ou multiplicação", ]
outros_2019 <- status_2019[status_2019$tipo_producao == "Outros", ]
colheita_2019 <- status_2019[status_2019$tipo_producao == "Centro de Colheita de sémen", ]

recria_2020 <- status_2020[status_2020$tipo_producao == "Recria e/ou acabamento", ]
producao_2020 <- status_2020[status_2020$tipo_producao == "Produção", ]
leitoes_2020 <- status_2020[status_2020$tipo_producao == "Produção de Leitões", ]
selecao_2020 <- status_2020[status_2020$tipo_producao == "Seleção e/ou multiplicação", ]
outros_2020 <- status_2020[status_2020$tipo_producao == "Outros", ]
colheita_2020 <- status_2020[status_2020$tipo_producao == "Centro de Colheita de sémen", ]

### Keep only the last change in the each farm status
recria_2016 <- setDT(recria_2016)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
producao_2016 <- setDT(producao_2016)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
leitoes_2016 <- setDT(leitoes_2016)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
selecao_2016 <- setDT(selecao_2016)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
outros_2016 <- setDT(outros_2016)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
colheita_2016 <- setDT(colheita_2016)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]

recria_2017 <- setDT(recria_2017)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
producao_2017 <- setDT(producao_2017)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
leitoes_2017 <- setDT(leitoes_2017)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
selecao_2017 <- setDT(selecao_2017)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
outros_2017 <- setDT(outros_2017)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
colheita_2017 <- setDT(colheita_2017)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]

recria_2018 <- setDT(recria_2018)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
producao_2018 <- setDT(producao_2018)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
leitoes_2018 <- setDT(leitoes_2018)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
selecao_2018 <- setDT(selecao_2018)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
outros_2018 <- setDT(outros_2018)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
colheita_2018 <- setDT(colheita_2018)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]

recria_2019 <- setDT(recria_2019)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
producao_2019 <- setDT(producao_2019)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
leitoes_2019 <- setDT(leitoes_2019)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
selecao_2019 <- setDT(selecao_2019)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
outros_2019 <- setDT(outros_2019)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
colheita_2019 <- setDT(colheita_2019)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]

recria_2020 <- setDT(recria_2020)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
producao_2020 <- setDT(producao_2020)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
leitoes_2020 <- setDT(leitoes_2020)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
selecao_2020 <- setDT(selecao_2020)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
outros_2020 <- setDT(outros_2020)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]
colheita_2020 <- setDT(colheita_2020)[order(exploracao_id, -as.IDate(data, "%Y-%m-%d"))][!duplicated(exploracao_id)]

### Count number of farms by status in each type of production and gather each year in one table
---- #2016# ----
recria_2016 <- as.data.frame(aggregate(x = recria_2016, list(estatuto = recria_2016$classificacao_sanitaria), FUN = length))
recria_2016 <- recria_2016 %>% select(estatuto, exploracao_id)
names(recria_2016) <- c("estatuto", "contagem")

producao_2016 <- as.data.frame(aggregate(x = producao_2016, list(estatuto = producao_2016$classificacao_sanitaria), FUN = length))
producao_2016 <- producao_2016 %>% select(estatuto, exploracao_id)
names(producao_2016) <- c("estatuto", "contagem")

leitoes_2016 <- as.data.frame(aggregate(x = leitoes_2016, list(estatuto = leitoes_2016$classificacao_sanitaria), FUN = length))
leitoes_2016 <- leitoes_2016 %>% select(estatuto, exploracao_id)
names(leitoes_2016) <- c("estatuto", "contagem")

selecao_2016 <- as.data.frame(aggregate(x = selecao_2016, list(estatuto = selecao_2016$classificacao_sanitaria), FUN = length))
selecao_2016 <- selecao_2016 %>% select(estatuto, exploracao_id)
names(selecao_2016) <- c("estatuto", "contagem")

outros_2016 <- as.data.frame(aggregate(x = outros_2016, list(estatuto = outros_2016$classificacao_sanitaria), FUN = length))
outros_2016 <- outros_2016 %>% select(estatuto, exploracao_id)
names(outros_2016) <- c("estatuto", "contagem")

colheita_2016 <- as.data.frame(aggregate(x = colheita_2016, list(estatuto = colheita_2016$classificacao_sanitaria), FUN = length))
colheita_2016 <- colheita_2016 %>% select(estatuto, exploracao_id)
names(colheita_2016) <- c("estatuto", "contagem")

prod_2016 <- as.data.frame(merge(recria_2016, producao_2016, by.x = "estatuto", by.y = "estatuto", all = TRUE))
names(prod_2016) <- c("estatuto", "recria", "producao")
prod_2016 <- as.data.frame(merge(prod_2016, leitoes_2016, by.x = "estatuto", by.y = "estatuto", all = TRUE))
names(prod_2016) <- c("estatuto", "recria", "producao", "leitoes")
prod_2016 <- as.data.frame(merge(prod_2016, selecao_2016, by.x = "estatuto", by.y = "estatuto", all = TRUE))
names(prod_2016) <- c("estatuto", "recria", "producao", "leitoes", "selecao")
prod_2016 <- as.data.frame(merge(prod_2016, colheita_2016, by.x = "estatuto", by.y = "estatuto", all = TRUE))
names(prod_2016) <- c("estatuto", "recria", "producao", "leitoes", "colheita")
prod_2016 <- as.data.frame(merge(prod_2016, outros_2016, by.x = "estatuto", by.y = "estatuto", all = TRUE))
names(prod_2016) <- c("estatuto", "recria", "producao", "leitoes", "colheita", "outros")

---- #2017# ----
recria_2017 <- as.data.frame(aggregate(x = recria_2017, list(estatuto = recria_2017$classificacao_sanitaria), FUN = length))
recria_2017 <- recria_2017 %>% select(estatuto, exploracao_id)
names(recria_2017) <- c("estatuto", "contagem")

producao_2017 <- as.data.frame(aggregate(x = producao_2017, list(estatuto = producao_2017$classificacao_sanitaria), FUN = length))
producao_2017 <- producao_2017 %>% select(estatuto, exploracao_id)
names(producao_2017) <- c("estatuto", "contagem")

leitoes_2017 <- as.data.frame(aggregate(x = leitoes_2017, list(estatuto = leitoes_2017$classificacao_sanitaria), FUN = length))
leitoes_2017 <- leitoes_2017 %>% select(estatuto, exploracao_id)
names(leitoes_2017) <- c("estatuto", "contagem")

selecao_2017 <- as.data.frame(aggregate(x = selecao_2017, list(estatuto = selecao_2017$classificacao_sanitaria), FUN = length))
selecao_2017 <- selecao_2017 %>% select(estatuto, exploracao_id)
names(selecao_2017) <- c("estatuto", "contagem")

outros_2017 <- as.data.frame(aggregate(x = outros_2017, list(estatuto = outros_2017$classificacao_sanitaria), FUN = length))
outros_2017 <- outros_2017 %>% select(estatuto, exploracao_id)
names(outros_2017) <- c("estatuto", "contagem")

colheita_2017 <- as.data.frame(aggregate(x = colheita_2017, list(estatuto = colheita_2017$classificacao_sanitaria), FUN = length))
colheita_2017 <- colheita_2017 %>% select(estatuto, exploracao_id)
names(colheita_2017) <- c("estatuto", "contagem")

prod_2017 <- as.data.frame(merge(recria_2017, producao_2017, by.x = "estatuto", by.y = "estatuto", all = TRUE))
names(prod_2017) <- c("estatuto", "recria", "producao")
prod_2017 <- as.data.frame(merge(prod_2017, leitoes_2017, by.x = "estatuto", by.y = "estatuto", all = TRUE))
names(prod_2017) <- c("estatuto", "recria", "producao", "leitoes")
prod_2017 <- as.data.frame(merge(prod_2017, selecao_2017, by.x = "estatuto", by.y = "estatuto", all = TRUE))
names(prod_2017) <- c("estatuto", "recria", "producao", "leitoes", "selecao")
prod_2017 <- as.data.frame(merge(prod_2017, colheita_2017, by.x = "estatuto", by.y = "estatuto", all = TRUE))
names(prod_2017) <- c("estatuto", "recria", "producao", "leitoes", "colheita")
prod_2017 <- as.data.frame(merge(prod_2017, outros_2017, by.x = "estatuto", by.y = "estatuto", all = TRUE))
names(prod_2017) <- c("estatuto", "recria", "producao", "leitoes", "colheita", "outros")

---- #2018# ----



##Gráfico de barras (grouped barplot) por anos e classificações por barra (A1, A2, A3, A4, A5), divididas por tipo de exploração (stacked);
##Mapa com várias explorações e respetiva informação (PLOTLY): nº animais, tipo exploração, classificação ...;
##Pie chart ou semelhante com % explorações por classificação sanitária;






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



