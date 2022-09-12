## Indicadores Jalisco
# David A. Ortega - 28/07/22

# Preparar Entorno ----
pacman::p_load(tidyverse,janitor,sf,scales,readxl)
rm(list=ls())
dev.off()

## Cargar Bases ----
# Diputados
diputados_mr_jalisco <- read_csv("input/jalisco/elecciones_diputadosmr_jalisco.csv") %>% clean_names()
distritos_locales_jalisco <- read_sf("/Users/NSMJ01TM8X/Desktop/mapas_mx/Jalisco/DISTRITO_LOCAL.shp")
# Presidentes Municipales
ayuntamientos_jalisco <- read_xlsx("input/jalisco/resultados_ayuntamientos_casillas2021.xlsx",skip = 1) %>% clean_names()
mapa_municipioes_jalisco <- distritos_locales_jalisco <- read_sf("/Users/NSMJ01TM8X/Desktop/mapas_mx/Jalisco/MUNICIPIO.shp")
# diputados 

diputados_mr_jalisco <- diputados_mr_jalisco[1:18,]


diputados_mr_jalisco %>% select(!ends_with("_percent")) %>% 
  pivot_longer(!partido, names_to = "seccion", values_to = "votos") %>% filter(!partido %in% c("VOTACION TOTAL EMITIDA","VOTACION VALIDA")) %>% 
  mutate(DISTRITO_L = as.numeric(str_remove(seccion,"x"))) %>% 
  group_by(DISTRITO_L) %>% slice_max(order_by = votos,n=1)

## Ayuntamientos

ayuntamientos_jalisco %>% glimpse()

ayuntamientos_jalisco %>% count(id_distrito_local,cabecera_distrital_local)

ayuntamientos_jalisco <- ayuntamientos_jalisco %>% select(!c(circunscripcion,id_estado,nombre_estado,id_distrito_local,cabecera_distrital_local,
                                    seccion,tipo_casilla,id_casilla,ext_contigua,casilla,num_votos_validos,num_votos_can_nreg, 
                                    estatus_acta,tribunal,observaciones,ruta_acta,lista_nominal,total_votos)) %>% 
  group_by(id_municipio,municipio) %>% summarise_all(sum,na.rm = T) %>% 
  pivot_longer(!c(id_municipio,municipio),names_to = "partido",values_to = "votos") %>% slice_max(order_by = votos,n=1)
ayuntamientos_jalisco <- ayuntamientos_jalisco %>% rename(NOMBRE = municipio) %>% rename(MUNICIPIO = id_municipio)

mapa_municipioes_jalisco <- mapa_municipioes_jalisco %>% left_join(ayuntamientos_jalisco,by = "MUNICIPIO")

write_sf(mapa_municipioes_jalisco,"output/mapa_municipios_jal.shp")


ayuntamientos_jalisco %>% ungroup() %>%  count(partido) %>% arrange(desc(n))



