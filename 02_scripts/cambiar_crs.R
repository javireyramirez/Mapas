library(here)
library(sf)
library(tidyverse)
library(mapSpain)

#Cargamos las funciones de GitHub
source(here("02_scripts","funciones","UpdateGitHub.R"), encoding = "UTF-8")

#Cargamos las funciones para los gráficos
source(here("02_scripts","funciones","funciones_basicas.R"), encoding = "UTF-8")

setwd(here())

provi=esp_get_prov() %>%
  st_transform(crs = st_crs(25830))
canaries_line <- data.frame(long = c(-50e4, 8e4, 8e4,- 50e4,- 50e4),
                            lat = c(405e4, 405e4, 385e4,385e4,405e4))
paises_lim=read_sf(here("01_datos","01_espaciales","Paises_Limitrofes_España_ETRS89.gpkg")) %>%
  filter(PAIS %in% c("FRANCIA","PORTUGAL","ANDORRA","MARRUECOS","ARGELIA")) %>%
  mutate(across(where(is.character),factor)) %>%
  arrange(PAIS)

  

covid_provi=read_csv2(here("01_datos","02_administrativos","01.0_datos_covid.csv")) %>% 
  filter(!is.na(cpro)) %>%
  filter(sexo %in% c("Hombres","Mujeres") & grupo_edad %in% c("60-69","70-79","80+")) %>%
  mutate(across(where(is.character),factor)) %>%
  group_by(cpro) %>%
  summarise(poblacion_total=sum(unique(poblacion_total)),
            across(where(is.numeric),~sum(.x,na.rm = T))) %>%
  mutate(tasa_def=num_def/poblacion_total*1000,
         tasa_mort=num_def/num_casos*1000,
         tasa_casos=num_casos/poblacion_total*1000)

colores_provi=c("#429bf3","#f3c842","#cf0000")


num_def=ggplot(data=provi %>% 
                 left_join(covid_provi,by="cpro"))+
  geom_sf(data=paises_lim)+
  geom_sf(aes(fill=num_def))+
  geom_sf_label(aes(label=coma(num_def)),size=1)+
  geom_sf_text(data=paises_lim,mapping = aes(label=PAIS),
               nudge_x = c(-5, 5, 5, 5, 50000),
               nudge_y = c(-5, -5, 5, -5, -5))+
  scale_fill_gradientn(colours =colores_provi,guide = guide_colourbar(),name="Defunciones",labels=coma)+
  geom_sf(data = esp_get_can_provinces()%>%
            st_transform(crs = st_crs(25830)))+
  coord_sf(xlim = c(-436720.6,1126309),
           ylim = c(3876597,4859002),
           datum = st_crs(25830))+
  geom_path(data = canaries_line, aes(x=long, y = lat, group = NULL), color = "grey40")+
  theme_void()+
  theme(
    axis.title = element_blank(), 
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    #Fondo azul
    panel.background = element_rect(fill = rgb(228/255,248/255,255/255)),
    panel.border = element_rect(colour = "black", fill="NA"),
    text = element_text(size=20),
    legend.key.size = unit(2.5,"line"),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20))
num_def %>% ggsave(filename = here("num_def.png"),height = 40,width = 60,units = "cm")
