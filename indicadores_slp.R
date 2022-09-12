## Indicadores SLP
# David A. Ortega - 28/07/22

# Preparar Entorno ----
pacman::p_load(tidyverse,janitor,sf,scales,readxl)
rm(list=ls())
dev.off()

## Cargar Bases ----
# Diputados
diputados_mr_slp <- read_csv("input/slp/SLP_DIP_LOC_2021.csv",skip = 6) %>% clean_names()
distritos_locales_slp <- read_sf("/Users/NSMJ01TM8X/Desktop/mapas_mx/San Luis/DISTRITO_LOCAL.shp")

# Presidentes Municipales
ayuntamientos_slp <- read_csv("input/slp/SLP_AYUN_2021.csv",skip = 6) %>% clean_names()
mapa_municipioes_slp <- read_sf("/Users/NSMJ01TM8X/Desktop/mapas_mx/San Luis/MUNICIPIO.shp")

# Mapa Diputados ----

diputados_mr_slp %>% glimpse()
diputados_mr_slp <- diputados_mr_slp %>% select(id_distrito_local,distrito_local, pan,pri,prd,pt,pvem,pcp,pmc,pmc,morena,
                                                pna,pes,rsp,fm, pan_pri_prd_pcp_alianza, 
                            pan_pri_prd_pcp,pt_pvem,pan_pri_prd,pan_pri_pcp,pan_prd_pcp,pri_prd_pcp,pan_pri,pan_prd,pan_pcp,pri_prd,pri_pcp, prd_pcp,
                            gass,cnr,nulos) %>% group_by(id_distrito_local,distrito_local) %>% 
  pivot_longer(!c(id_distrito_local,distrito_local),names_to = "partido", values_to = "votos") %>% 
  mutate(votos = as.numeric(votos)) %>% group_by(id_distrito_local,distrito_local,partido) %>% 
  summarise(votos = sum(votos,na.rm = T)) %>% slice_max(order_by = votos, n = 1) %>% rename(DISTRITO_L = id_distrito_local) %>% 
  mutate(DISTRITO_L = as.numeric(DISTRITO_L))

distritos_locales_slp <- distritos_locales_slp %>% left_join(diputados_mr_slp,by = "DISTRITO_L")
write_sf(distritos_locales_slp,"output/mapa_diploc_slp_21.shp")

# Mapa Presidencias Municipales ----


ayuntamientos_slp %>% glimpse()
ayuntamientos_slp <- ayuntamientos_slp %>% select(id_municipio,municipio, pan,pri,prd,pt,pvem,pcp,pmc,morena,pna,pes,rsp,fm,pan_pcp_alianza,pri_prd_pcp_alianza, 
                             pan_prd_alianza,pan_pri_alianza, pan_pri_pcp_alianza,pri_pcp_alianza, pan_pri_prd_pcp, pt_pvem, pan_pri_prd, 
                             pan_pri_pcp,pan_prd_pcp,pri_prd_pcp,pan_pri,pan_prd,pan_pcp,pri_pcp,pri_prd,prd_pcp,rot, lvv, ngg, cnr, nulos) %>% 
  pivot_longer(!c(id_municipio,municipio),names_to = "partido", values_to = "votos") %>% mutate(votos = as.numeric(votos)) %>% 
  group_by(id_municipio,municipio,partido) %>% summarise(votos = sum(votos,na.rm = T)) %>% ungroup(partido) %>% 
  slice_max(order_by = votos,n = 1)










































