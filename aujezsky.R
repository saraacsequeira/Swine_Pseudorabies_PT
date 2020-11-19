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

# Connection with MySQL database
connection <- dbConnect(RMariaDB :: MariaDB(),
                        dbname = 'Swine',
                        host = "localhost",
                        user = "root",
                        password = "projetoporcos")

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


