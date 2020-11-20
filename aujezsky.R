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
count$info <- paste0("<br>", count$exploracao, "<br>", count$svl, " - ", count$dsavr, "<br>", count$contagem, " ", "animais", "<br>")

### Select only declaracao_existencias from 2020
count <- count %>%
  filter(count$data > as.Date("2020-01-01"))

### Remove farms missing any information
count <- na.omit(count)

### Define categories based on total animals
count$categoria <- cut(count$total, c(0,50,100,250,500,750,1000,2500,5000,10000,25000,50000))
levels(count$categoria) <- c("0-50", "50-100", "100-250", "250-500", "500-750", "750-1000", "1000-2500", "2500-5000", "5000-10000", "10000-25000", "25000-50000")
count$categoria <- as.character(count$categoria)


### Map
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



# 1.3 Percentage of pig farms currently classified (in general and farm specified)

class <- as.data.frame(merge(contagens, classificacoes, by.x = "exploracao", by.y = "exploracao_id"))

class %>% select(exploracao, data, longitude, latitude, svl, classificacao_sanitaria)

class %>%
  dplyr::filter(DATE1 >= as.POSIXct('2018-01-01'))






# → Tabela da prevalência, usar de alguma forma!!

# 2. Estado geral das classificações sanitárias;
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



