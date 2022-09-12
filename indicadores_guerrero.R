## Indicadores Guerrero
# David A. Ortega - 12/07/22

# Preparar Entorno ----
pacman::p_load(tidyverse,janitor,sf)
dev.off()
rm(list=ls())
# Cargar Bases ----
resultados_ayuntamiento_21 <- read_csv("input/guerrero/elecciones_ayuntamiento_21_gro.csv") %>% clean_names()
dip_loc_mr_21 <- read_csv("input/guerrero/elecciones_diputadoslocales_mr_21_gro.csv") %>% clean_names()
dip_loc_rp_21 <- read_csv("input/guerrero/elecciones_diputadoslocales_rp_21_gro.csv") %>% clean_names()
gobernatura_21 <- read_csv("input/guerrero/elecciones_gobernatura_21_gro.csv") %>% clean_names()

# Mapas ----
dtto_local <- read_sf("/Users/NSMJ01TM8X/Desktop/mapas_mx/Guerrero/DISTRITO_LOCAL.shp")
municipios <- read_sf("/Users/NSMJ01TM8X/Desktop/mapas_mx/Guerrero/MUNICIPIO.shp")

# Diputados locales y rp

dip_loc_mr_21 %>% glimpse()
dip_loc_rp_21 %>% glimpse()

diputados_locales <- dip_loc_mr_21 %>% pivot_longer(!c(distrito,cabecera),names_to = "partido_mr",values_to = "votos_mr") %>% 
  filter(partido_mr!="total")%>% group_by(distrito) %>% mutate(porcentaje_mr = round(votos_mr/sum(votos_mr)*100,2)) %>% filter(votos_mr==max(votos_mr,na.rm = T))

diputados_locales <- left_join(dip_loc_rp_21%>% pivot_longer(!c(distrito,cabecera),names_to = "partido_rp",values_to = "votos_rp") %>% 
  filter(!partido_rp %in% c("total","votos_validos"))%>% group_by(distrito)%>% mutate(porcentaje_rp = round(votos_rp/sum(votos_rp)*100,2)) %>%
    filter(votos_rp==max(votos_rp,na.rm = T)),
  diputados_locales,by=c("distrito","cabecera"))

diputados_locales <- diputados_locales %>% rename(DISTRITO_L = distrito) %>% mutate(DISTRITO_L=as.numeric(DISTRITO_L)) 
diputados_locales%>% View()

diputados_locales_gro_21 <- dtto_local %>% left_join(diputados_locales,by="DISTRITO_L")

write_sf(diputados_locales_gro_21,"output/mapa_diputados_locales_gro21.shp")


## Elecciones para la Gobernatura ----

gobernador_21 <- gobernatura_21 %>% pivot_longer(!c(distrito,cabecera),names_to = "partido", values_to = "votos") %>% 
  filter(partido!="total") %>%
  group_by(distrito) %>% mutate(porcentaje = round(votos/sum(votos)*100,2)) %>% filter(votos == max(votos))

gobernador_21 <- left_join( gobernatura_21 %>% pivot_longer(!c(distrito,cabecera),names_to = "partido_2", values_to = "votos_2") %>% 
         filter(partido_2 !="total") %>%
         group_by(distrito) %>% mutate(porcentaje_2 = round(votos_2/sum(votos_2)*100,2))%>%
           mutate(ganador = ifelse(votos_2 == max(votos_2),1,0)) %>% 
           filter(ganador==0) %>%
           filter(votos_2 == max(votos_2))%>% select(!ganador),gobernador_21,by=c("distrito","cabecera")) %>% rename(DISTRITO_L= distrito) %>% 
  mutate(DISTRITO_L = as.numeric(DISTRITO_L)) 



gobernador_gro_21 <- dtto_local %>% left_join(gobernador_21,by="DISTRITO_L")

write_sf(gobernador_gro_21,"output/mapa_gobernatura_gro21.shp")

gobernador_gro_21 %>% count(partido)
























