library(here)
library(sf)
library(tidyverse)
library(mapSpain)

#Cargamos las funciones de GitHub
source(here("02_scripts","funciones","UpdateGitHub.R"), encoding = "UTF-8")

#Cargamos las funciones para los gráficos
source(here("02_scripts","funciones","funciones_basicas.R"), encoding = "UTF-8")

setwd("D:/JAVIER/Perosnal/Mapas")

provi=esp_get_prov() %>%
  st_transform(crs = st_crs(25830))
canaries_line <- data.frame(long = c(-50e4, 8e4, 8e4,- 50e4,- 50e4),
                            lat = c(405e4, 405e4, 385e4,385e4,405e4))
paises_lim=read_sf("Paises_Limitrofes_España_ETRS89.gpkg")

  

ggplot()+
  geom_sf(data=provi)+
  geom_path(data = canaries_line, aes(x=long, y = lat, group = NULL), color = "grey40")+
  theme_void()+
  theme(
    axis.title = element_blank(), 
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    #Fondo azul
    panel.background = element_rect(fill = rgb(228/255,248/255,255/255)),
    panel.border = element_rect(colour = "black", fill="NA"))


  