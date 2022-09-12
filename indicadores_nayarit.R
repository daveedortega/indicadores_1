## Indicadores Nayarit
# David A. Ortega - 12/07/22

# Preparar Entorno ----
pacman::p_load(tidyverse,janitor,sf)
dev.off()
rm(list=ls())
# Cargar Bases ----
# resultados_ayuntamiento_21 <- read_csv("input/hidalgo/municipios_hgo_21.csv") %>% clean_names()
dip_loc_mr_21 <- read_csv("input/nayarit/dip_loc_nay_mr_21.csv") %>% clean_names()
# dip_loc_rp_21 <- read_csv("input/guerrero/elecciones_diputadoslocales_rp_21_gro.csv") %>% clean_names()
gobernatura_22 <- read_csv("input/nayarit/gobernatura_nay_21.csv") %>% clean_names()

# Mapas ----
dtto_local <- read_sf("/Users/NSMJ01TM8X/Desktop/mapas_mx/Nayarit/DISTRITO_LOCAL.shp")
municipios <- read_sf("/Users/NSMJ01TM8X/Desktop/mapas_mx/Nayarit/MUNICIPIO.shp")
st_is_valid(municipios)
st_make_valid(municipios)
plot(municipios)
# Gobernatura 2022 por Municipio ----
# Ordenamos la base
gobernatura_22_nay<- gobernatura_22 %>% mutate(NOMBRE  = toupper(municipio)) %>% mutate(NOMBRE = str_replace(NOMBRE,"Á","A")) %>% 
  mutate(NOMBRE = str_replace(NOMBRE,"É","E")) %>% 
  mutate(NOMBRE = str_replace(NOMBRE,"Í","I")) %>% 
  mutate(NOMBRE = str_replace(NOMBRE,"Ó","O")) %>% 
  mutate(NOMBRE = str_replace(NOMBRE,"Ú","U")) %>%
  select(!c(casillas_instaladas,municipio)) %>% 
  pivot_longer(!NOMBRE,names_to = "partido",values_to = "votos") %>% group_by(NOMBRE) %>% 
  mutate(porcentaje = round(votos/sum(votos)*100,2))%>% slice_max(order_by = votos,n=3) %>% 
  pivot_wider(names_from = partido,values_from = c(votos,porcentaje)) 
  
#Ok
gobernatura_22_nay$NOMBRE[!gobernatura_22_nay$NOMBRE %in% municipios$NOMBRE]

# Escribimos y tratamos de unir en qgis
write_csv(gobernatura_22_nay,"output/resultados_nay_gober_21.csv")
#Pegamos
mapa_gob_nay_21 <- left_join(municipios,gobernatura_22_nay,by="NOMBRE")
# escribimos
names(mapa_gob_nay_21) <- c("id","ent","mun","control","nom","Geometry1_","geometry","vmor","vmc","vpri","pormor","pormc","porpri")
st_make_valid(mapa_gob_nay_21)
write_sf(mapa_gob_nay_21,"output/mapa_gob_nay_21.shp")

# Diputados MR Nay 21 -----

dip_loc_mr_21_f <- dip_loc_mr_21 %>% filter(municipio!="Total Distrito") %>% select(!c(casillas_instaladas,municipio)) %>%  
  pivot_longer(!c(distrito),names_to = "partido",values_to = "votos") %>% group_by(distrito) %>% na.omit() %>%
  top_n(votos,n=3) %>% group_by(distrito,partido) %>% summarise(votos = sum(votos)) %>%  mutate(resultado = rank(desc(votos))) %>% 
  mutate(partido_2 = lag(partido,n=1,order_by = votos)) %>% 
  mutate(partido_2= ifelse(partido_2 == partido, lag(partido,n=2,order_by = votos),partido_2)) %>% 
  mutate(votos_2 = lag(votos,n=1,order_by = votos)) %>% filter(resultado==1) %>% 
  mutate(diferencia = votos - votos_2)


# Escribimos y tratamos de unir en qgis
write_csv(dip_loc_mr_21_f,"output/resultados_nay_dipmr_21.csv")


















































