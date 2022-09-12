## Indicadores Sinaloa
# David A. Ortega - 28/07/22

# Preparar Entorno ----
pacman::p_load(tidyverse,janitor,sf)
rm(list=ls())
dev.off()
# Cargar Bases ----
resultados_ayuntamiento_21 <- read_csv("input/sinaloa/ayunt_sin_21.csv") %>% clean_names()
dip_loc_mr_21 <- read_csv("input/sinaloa/sin_dip_mr_21.csv") %>% clean_names()
dip_loc_rp_21 <- read_csv("input/sinaloa/sin_dip_rp_21.csv") %>% clean_names()
gobernatura_21 <- read_csv("input/sinaloa/gob_sin_21.csv") %>% clean_names()

## Mapa Sinaloa

mapa_mun_sinaloa <- read_sf("/Users/NSMJ01TM8X/Desktop/mapas_mx/Sinaloa/MUNICIPIO.shp")
mapa_diploc_sinaloa <- read_sf("/Users/NSMJ01TM8X/Desktop/mapas_mx/Sinaloa/DISTRITO_LOCAL.shp")

# Gobernatura 21 ----
gobernatura_21 %>% glimpse()

gobernatura_21 <- gobernatura_21 %>% select(!c(id_estado,nombre_estado,id_distrito_local,cabecera_distrital_local,id_municipio_local,seccion,
                                               tipo_casilla))  %>%
  group_by(municipio_local) %>% summarise_all(sum) %>% pivot_longer(!municipio_local,names_to = "partido",values_to = "votos") %>% 
  filter(!partido %in% c("lista_nominal_casilla","no_registrados","nulos","total_votos")) %>% group_by(municipio_local) %>% 
  mutate(votos_2 = case_when(partido %in% c("pan","pri","prd", "c_pan_prd","c_pan_pri","c_pan_pri_prd","c_pri_prd","fx_m")~"FxM",
                           partido %in% c("cc_morena_pas","morena","pas")~"Morena",
                           T~partido)) %>% 
  group_by(municipio_local,votos_2) %>% summarise(votos = sum(votos)) %>%  pivot_wider(names_from = votos_2,values_from = votos) %>% 
  ungroup() %>% group_by(municipio_local) %>% mutate(coso = names(max.col(m=c(FxM,mc,Morena,pes,pt,pvem,rsp)))) %>% 
  rename(NOMBRE = municipio_local)
# obtiene maximo de columnas 
gobernatura_21$ganador <- colnames(gobernatura_21[2:8])[max.col(gobernatura_21[2:8], ties.method = "first")]
# segundo lugar
gobernatura_21$segundo <- colnames(gobernatura_21[c(2,3,5:8)])[max.col(gobernatura_21[c(2,3,5:8)], ties.method = "first")]

gobernatura_21$NOMBRE %in% mapa_mun_sinaloa$NOMBRE

# Pegar y Escribir -
mapa_gobernatura_sin_21 <- left_join(mapa_mun_sinaloa,gobernatura_21,by="NOMBRE")
# escribir
write_sf(mapa_gobernatura_sin_21,"output/mapa_gobernatura_sin_21.shp")

# Diputados MR 21 ----
dip_loc_mr_21 %>% glimpse()

dip_loc_mr_21_sin <- dip_loc_mr_21 %>% select(!c(id_estado,nombre_estado,municipio_local,id_municipio_local,seccion,
                            tipo_casilla,cabecera_distrital_local)) %>% mutate(id_distrito_local = as.character(id_distrito_local)) %>% 
  group_by(id_distrito_local) %>% 
  summarise_all(sum) %>% pivot_longer(!id_distrito_local,names_to = "partido",values_to = "votos") %>% 
  filter(!partido %in%c("total_votos","lista_nominal_casilla")) %>% 
  pivot_wider(names_from = partido,values_from = votos) %>% 
  mutate(jhh = morena+pas+cc_morena_pas,
         vxm=pan+pri+prd+c_pan_pri_prd+c_pan_pri+c_pri_prd) %>% 
  select(id_distrito_local,jhh,vxm) %>% mutate(DISTRITO_L = as.integer(id_distrito_local))

# obtiene maximo de columnas 
dip_loc_mr_21_sin$ganador <- colnames(dip_loc_mr_21_sin[2:3])[max.col(dip_loc_mr_21_sin[2:3])]

# pegar
dip_loc_mr_21_sin %>% glimpse()

mapa_diploc_mr_sinaloa <- mapa_diploc_sinaloa %>% left_join(dip_loc_mr_21_sin,by="DISTRITO_L")

write_sf(mapa_diploc_mr_sinaloa,"output/mapa_diplocmr_sin21.shp")

# Diputados RP 21 ----

dip_loc_rp_21 %>% glimpse()

dip_loc_rp_21 %>% select(!c(id_estado,nombre_estado,id_distrito_local,cabecera_distrital_local,id_municipio_local,seccion,
                           tipo_casilla,municipio_local))  %>% summarise_all(sum)
  
  
  group_by(municipio_local) %>% summarise_all(sum) %>% pivot_longer(!municipio_local,names_to = "partido",values_to = "votos") %>% 
  filter(!partido %in% c("lista_nominal_casilla","no_registrados","nulos","total_votos")) %>% group_by(municipio_local) %>% 
  mutate(votos_2 = case_when(partido %in% c("pan","pri","prd", "c_pan_prd","c_pan_pri","c_pan_pri_prd","c_pri_prd","fx_m")~"FxM",
                             partido %in% c("cc_morena_pas","morena","pas")~"Morena",
                             T~partido)) %>% 
  group_by(municipio_local,votos_2) %>% summarise(votos = sum(votos)) %>%  pivot_wider(names_from = votos_2,values_from = votos) %>% 
  ungroup() %>% group_by(municipio_local) %>% mutate(coso = names(max.col(m=c(FxM,mc,Morena,pes,pt,pvem,rsp)))) %>% 
  rename(NOMBRE = municipio_local)





















