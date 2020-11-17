# Libraries
library(DBI)
library(RMySQL)
library(dplyr)

# Connection with MySQL database
drv <- dbDriver("MySQL")
con <- dbConnect(drv, username = 'root', 
                 password = 'Pinoquio1997', 
                 dbname = 'siss', 
                 host = 'localhost')

## Export tables
exploracoes <- dbReadTable(con, "st_tabela_exploracoes")
contagens <- dbReadTable(con, "st_tabela_contagens")
vacinacoes <- dbReadTable(con, "st_tabela_vacinacoes")
