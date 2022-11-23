# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require(ggplot2)
require(dplyr)

base_dir <- "~/buckets/b1/"

# Levantar dataset C4
# Leo el dataset para observar la cantidad de baja que se producen por mes
arch_dataset <- paste0(base_dir, "datasets/competenciaFINAL_2022.csv.gz")
dataset <- fread(arch_dataset)


mes_bajamas2 <- dataset %>%
  filter(clase_ternaria=="BAJA+2") %>%
  group_by(as.character(foto_mes),clase_ternaria) %>%
  tally()
names(mes_bajamas2)[1] <- 'foto_mes'
mes_bajamas2

mes_clientes <- dataset %>%
  group_by(as.character(foto_mes)) %>%
  tally()
names(mes_clientes)[1] <- 'foto_mes'
mes_clientes

clientes_bajas = merge(x = mes_bajamas2, y = mes_clientes, by = "foto_mes", all.x = TRUE)
names(clientes_bajas)[3] <- 'cantidad_bajamas2'
names(clientes_bajas)[4] <- 'cantidad_clientes'

setDT(clientes_bajas)
clientes_bajas[ , porcentaje_baja := (cantidad_bajamas2 * 100 ) / cantidad_clientes ]

clientes_bajas


dataset_septiembre <- dataset[foto_mes == 202109]

count(dataset_septiembre)

ggplot(data=clientes_bajas, aes(x=foto_mes, y=cantidad_bajamas2 ), geom="histogram")+
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Año-Mes", y = "Cantidad de BAJA+2 por mes", 
                     title = "BAJA+2 Histórico")

ggplot(data=clientes_bajas, aes(x=foto_mes, y=porcentaje_baja ), geom="histogram")+
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Año-Mes", y = "Porcentaje de BAJA+2 por mes", 
       title = "BAJA+2 Histórico")

ggplot(data=clientes_bajas, aes(x=foto_mes, y=n.x, group=1 ), geom="histogram")+
  geom_point() +
  geom_line() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Año-Mes", y = "Cantidad de BAJA+2 por mes", 
       title = "BAJA+2 Histórico")

head(clientes_bajas)
summary(clientes_bajas)
sapply (clientes_bajas, class)
