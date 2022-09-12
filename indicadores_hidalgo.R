## Indicadores Guerrero
# David A. Ortega - 12/07/22

# Preparar Entorno ----
pacman::p_load(tidyverse,janitor,sf)
dev.off()
rm(list=ls())
# Cargar Bases ----
resultados_ayuntamiento_21 <- read_csv("input/hidalgo/municipios_hgo_21.csv") %>% clean_names()
dip_loc_mr_21 <- read_csv("input/hidalgo/diputaciones_mr_hgo21.csv") %>% clean_names()
# dip_loc_rp_21 <- read_csv("input/guerrero/elecciones_diputadoslocales_rp_21_gro.csv") %>% clean_names()
gobernatura_22 <- read_csv("input/hidalgo/elecciones_hgo_22.csv") %>% clean_names()

# Mapas ----
dtto_local <- read_sf("/Users/NSMJ01TM8X/Desktop/mapas_mx/Hidalgo/DISTRITO_LOCAL.shp")
municipios <- read_sf("/Users/NSMJ01TM8X/Desktop/mapas_mx/Hidalgo/MUNICIPIO.shp")

# Gobernatura 2022 por Distrito Local ----

gobernatura_22 %>% glimpse()
gobernatura_22 %>% count(distrito_local)

gobernatura_22_distrito <- gobernatura_22 %>% select(!c(entidad,seccion,id_casilla,tipo_casilla,ext_contigua,ubicacion_casilla,tipo_acta,sobres_con_votos, representantes_pp_ci)) %>% 
  mutate(lista_nominal = as.character(lista_nominal)) %>% 
  pivot_longer(!c(id_distrito_local,distrito_local),names_to = "partido",values_to = "votos") %>% 
  mutate(votos = as.numeric(votos)) %>% 
  mutate(coalicion = case_when(str_detect(partido,"morena|pt|nah")~"Julio Menchaca",
                               str_detect(partido,"pan|pri|prd")~"Alma Carolina Viggiano",
                               str_detect(partido,"pvem")~"José Luis Lima",
                               str_detect(partido,"mc")~"Francisco Xavier Berganza")) %>% na.omit() %>% 
  group_by(id_distrito_local,distrito_local,coalicion) %>% summarise(votos = sum(votos,na.rm = T)) %>% 
  pivot_wider(names_from = coalicion,values_from = votos) %>% mutate(id_distrito_local  = as.numeric(id_distrito_local)) %>% 
  rename(DISTRITO_L = id_distrito_local)

# pegar
mapa_gobernatura_dttoloc_hgo <- left_join(dtto_local,gobernatura_22_distrito,by="DISTRITO_L")
# escribir
write_sf(mapa_gobernatura_dttoloc_hgo,"output/mapa_gobernatura_dttoloc_hgo.shp")


# Gobernatura 2022 por Municipio ----
gobernatura_22_municip <- read_csv("input/hidalgo/municipios_gobernatura_hgo.csv") %>% clean_names()

gobernatura_22_municip <- gobernatura_22_municip %>% separate(municipio, into = c("num","municipio"),sep = "\\.") %>% mutate(municipio = str_squish(municipio)) %>% 
  group_by(num,municipio) %>% 
  transmute(morena = sum(morena,nah,pt,pt_mor_nah,pt_mor,mor_nah,na.rm = T),oposicion = sum(pan,pri,prd,pvem,mc,pan_prd,pan_pri,pri_prd,na.rm = T)) %>% 
  rename(NOMBRE  = municipio)


# pegar
municipios_hgo_22 <- left_join(municipios,gobernatura_22_municip,by = "NOMBRE") %>% select(!num)
# escribir
write_sf(municipios_hgo_22,"output/mapa_gobernatura_municip_hgo.shp")

gobernatura_22_municip %>% View()


## Diputaciones mr ----

dip_loc_hgo_mr_21 <- dip_loc_mr_21[-(19:20),] %>% mutate_all( as.numeric) %>% pivot_longer(!distrito,names_to = "partido",values_to = "votos") %>% 
  filter(partido!="total") %>% mutate(coalicion = case_when(partido %in% c("morena","pt","pvem","nuevaalianza")~"Morena",
                                                            partido %in% c("pan","pri","prd","pes")~"Oposicion",
                                                            T~"Otros")) %>% 
  group_by(coalicion,distrito) %>% summarise(votos = sum(votos,na.rm = T)) %>% 
  group_by(distrito) %>% mutate(ganador = rank(desc(votos))) %>% filter(ganador %in% c(1,2,3)) %>% select(!ganador) %>% 
  pivot_wider(names_from = coalicion,values_from =votos )

titular <- data.frame(distrito = 1:18,p_coalicion = c("PVEM", "NAH","NAH","MOR","PT","PVEM","MOR","MOR","NAH","MOR","MOR","PT",
                                           "PVEM","PVEM","PT","MOR","PT","NAH"),
           p_op = c("PRD","","","PES","PAN","PAN","PES","PRI","PRD","PRI","PRI", "PRI", "PRI","PRD","PAN","PES","PRI","PRI"))

dip_loc_hgo_mr_21 <- left_join(dip_loc_hgo_mr_21,titular,by = "distrito") %>% rename(DISTRITO_L = distrito)

# Pegar
mapa_dip_hgo_mr_21 <- left_join(dtto_local,dip_loc_hgo_mr_21,by="DISTRITO_L")
# escribir
write_sf(mapa_dip_hgo_mr_21,"output/mapa_dtto_loc_hgo.shp")


## Gobiernos Municipales ----

municipios

resultados_ayuntamiento_21 <- resultados_ayuntamiento_21 %>% rename(MUNICIPIO = municipio)

municipios$NOMBRE %in% resultados_ayuntamiento_21$MUNICIPIO











