# Assessment of Aujesky's disease status in Portugal

setwd("~/Desktop/Treino Estágio 2020-2021/Swine_Pseudorabies_PT")

# Connect R to MySQL by RMariaDB
library(DBI)
library(RMariaDB)

connection <- dbConnect(RMariaDB :: MariaDB(),
                 dbname = 'Swine',
                 host = "localhost",
                 user = "root",
                 password = "projetoporcos")


library(tidyverse)