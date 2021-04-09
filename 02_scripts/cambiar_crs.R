library(here)
library(sf)
library(tidyverse)
library(mapSpain)
library(ggspatial)
library(ggsn)
library(cowplot)
library(magick)
#Cargamos las funciones de GitHub
source(here("02_scripts","funciones","UpdateGitHub.R"), encoding = "UTF-8")

#Cargamos las funciones para los gráficos
source(here("02_scripts","funciones","funciones_basicas.R"), encoding = "UTF-8")

setwd(here())
logo1=image_read(here("01_datos","03_logo","JR.png"))
logo2=image_read(here("01_datos","03_logo","JR_2.png"))

provi=esp_get_prov() %>%
  st_transform(crs = 25830)
canaries_line = tibble(x = c(-44.5e4, 1.75e4, 1.75e4,- 44.5e4,- 44.5e4),
                            y = c(405e4, 405e4, 385e4,385e4,405e4))
legend_line=tibble(xmin=-47e4,xmax=-6.5e4,
                   ymin=420e4,ymax=485e4)
titulo=tibble(x=-45e4,y=480e4,label="Número de defunciones de personas\n
              mayores de España por SARS-CoV-2")
sub_titulo=tibble(x=-45e4,y=480e4,label="Número de defunciones de personas\nmayores (mayores de 60 años) de España\npor SARS-CoV-2 en el periodo 2020-2021")

paises_lim=read_sf(here("01_datos","01_espaciales","Paises_Limitrofes_España_ETRS89.gpkg")) %>%
  filter(PAIS %in% c("FRANCIA","PORTUGAL","ANDORRA","MARRUECOS","ARGELIA")) %>%
  mutate(across(where(is.character),factor)) %>%
  arrange(PAIS)

etiquetas_paises=read_sf(here("01_datos","01_espaciales","etiquetas_paise_cercanos_coordenadas.gpkg")) %>%
  st_transform(crs = st_crs(25830))
  

covid_provi=read_csv2(here("01_datos","02_administrativos","01.0_datos_covid.csv")) %>% 
  filter(!is.na(cpro)) %>%
  filter(sexo %in% c("Hombres","Mujeres") & grupo_edad %in% c("60-69","70-79","80+")) %>%
  mutate(across(where(is.character),factor)) %>%
  group_by(cpro) %>%
  summarise(poblacion_total=sum(unique(poblacion_total)),
            across(where(is.numeric),~sum(.x,na.rm = T))) %>%
  mutate(tasa_def=num_def/poblacion_total*1000,
         tasa_mort=num_def/num_casos*1000,
         tasa_casos=num_casos/poblacion_total*1000) %>%
  mutate(clase_def=factor(
           case_when(
           num_def<200~"0-200",
           num_def<400~"201-400",
           num_def<600~"401-600",
           num_def<800~"601-800",
           num_def<1000~"801-1000",
           num_def<2000~"1001-2000",
           num_def>2000~"2001-13691"
         ),levels = c("0-200","201-400","401-600","601-800","801-1000","1001-2000","2001-13691"))) 

colores_provi=c("#145a32","#1e8449","#52be80","#f7dc6f","#f1c40f","#f1948a","#b03a2e")


num_def=ggplot(data=provi %>% 
                 left_join(covid_provi,by="cpro"))+
  geom_sf(data=paises_lim)+
  geom_sf(aes(fill=clase_def))+
  #geom_sf_label(aes(label=coma(num_def)),size=4)+
  geom_sf_text(data=etiquetas_paises,aes(label=etiqueta),size=8,hjust=-0.2)+
  # geom_sf_text(data=paises_lim,mapping = aes(label=PAIS),
  #              nudge_x = c(-5, 5, 5, 5, 50000),
  #              nudge_y = c(-5, -5, 5, -5, -5))+
  scale_fill_manual(values = colores_provi)+
  geom_sf(data = esp_get_can_provinces()%>%
            st_transform(crs = st_crs(25830)))+
  coord_sf(xlim = c(-4.2e5,1.075e6),
           ylim = c(3876597,4859002),
           datum = st_crs(25830))+
  geom_path(data = canaries_line, aes(x=x, y = y, group = NULL), color = "grey40")+
  geom_rect(data=legend_line,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill="white",color="grey40")+
  geom_text(data=titulo,aes(x=x,y=y,label=label),size=8,hjust=0)+
  geom_text(data=sub_titulo,aes(x=x,y=y,label=label),size=6,hjust=0,vjust=2)+
  annotation_north_arrow(location = "tr")+
  annotation_scale()+
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
    legend.title = element_blank(),
    legend.position = c(0.05, 0.7), 
    legend.justification = c(0.05, 0.7))

gg_num_def=ggdraw() +
  draw_plot(num_def) +
  draw_image(logo1,scale =0.15,x=-0.36,y=-0.07)

gg_num_def %>% ggsave(filename = here("num_def.png"),height = 40,width = 60,units = "cm")
