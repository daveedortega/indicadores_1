## Indicadores Oaxaca
# David A. Ortega - 28/07/22

# Preparar Entorno ----
pacman::p_load(tidyverse,janitor,sf,scales)
rm(list=ls())
dev.off()
# Cargar Bases ----
dip_loc_mr_21 <- read_csv("input/oaxaca/elecciones_diputados_mr_oax_21.csv") %>% clean_names()
ayuntamientos_21 <- read_csv("input/oaxaca/elecciones_ayuntamientos_oax_21.csv") %>% clean_names()

## Mapa Sinaloa

mapa_mun_oax <- read_sf("/Users/NSMJ01TM8X/Desktop/mapas_mx/Oaxaca/MUNICIPIO.shp")
mapa_diploc_oax <- read_sf("/Users/NSMJ01TM8X/Desktop/mapas_mx/Oaxaca/DISTRITO_LOCAL.shp")

# Diputados Locales Oaxaca 2021 ----

dip_loc_mr_21 %>% glimpse()
dip_loc_mr_21 <- dip_loc_mr_21 %>% select(!c(clave_casilla,clave_acta,id_estado,seccion,id_casilla,tipo_casilla,ext_contigua,ubicacion_casilla,tipo_acta,tipo_documento,
                            observaciones,mecanismos_traslado,sha,fecha_hora_acopio,fecha_hora_captura,fecha_hora_verificacion,origen,digitalizacion,
                            contabilizada,total_boletas_sobrantes,total_personas_votaron, total_votos_sacados,total_votos_calculado,
                            representantes_pp_ci,total_rep_partido_ci_votaron, estado, lista_nominal, total_votos_asentado)) %>% 
  group_by(id_distrito_local, distrito_local) %>% summarise_all(as.numeric) %>% summarise_all(sum,na.rm = T) %>% 
  rename(DISTRITO_L = id_distrito_local) %>% mutate(DISTRITO_L = as.numeric(DISTRITO_L)) %>% 
  pivot_longer(!c(DISTRITO_L,distrito_local),names_to = "partido",values_to = "votos") %>% 
  mutate(coalicion = case_when(partido %in%c("pan","mc","prd","co_pan_prd")~"Por Oaxaca al Frente",
         partido %in%c("pes","morena","pt")~"Juntos Haremos Historia",
         partido %in%c("pri","pvem","panal") & 
           distrito_local %in% c("ACATLAN DE PEREZ FIGUEROA", " ASUNCION NOCHIXTLAN",
                                 "SAN PEDRO MIXTEPEC", "SAN PEDRO Y SAN PABLO AYUTLA",
                                 " HEROICA CIUDAD DE EJUTLA DE CRESPO", 
                                 "SANTIAGO PINOTEPA NACIONAL", "MATIAS ROMERO",
                                 "TLACOLULA DE MATAMOROS","TEOTITLAN DE FLORES MAGON",
                                 "LOMA BONITA", "SANTO DOMINGO TEHUANTEPEC", 
                                 "HEROICA CIUDAD DE TLAXIACO", "SANTA CRUZ XOXOCOTLAN",
                                 "SAN PEDRO POCHUTLA", "SANTA LUCIA DEL CAMINO") ~"PRI_PVEM_PANAL",
         
         T~partido)) %>% group_by(coalicion,DISTRITO_L) %>% summarise(votos = sum(votos)) %>% filter(votos>0) %>% 
  ungroup() %>% group_by(DISTRITO_L) %>% mutate(ganador = rank(desc(votos))) %>% filter(ganador<3) %>% 
  pivot_wider(DISTRITO_L,names_from = ganador,values_from = c(coalicion,votos))
  
dip_loc_mr_21 %>% as.data.frame()

# Pegar
mapa_diploc_oax <- mapa_diploc_oax %>% left_join(dip_loc_mr_21,by = "DISTRITO_L")
# Escribir
write_sf(mapa_diploc_oax,"output/mapa_diploc_oax_21.shp")


# Ayuntamientos Oaxaca 2021 ----

ayuntamientos_21 %>% glimpse()
ayuntamientos_21_f <- ayuntamientos_21 %>% 
  select(!c(clave_casilla,clave_acta,id_estado,estado,seccion,id_casilla,ext_contigua,ubicacion_casilla,tipo_acta, 
                               total_boletas_sobrantes, total_personas_votaron, total_rep_partido_ci_votaron, total_votos_sacados,
                               observaciones,contabilizada,mecanismos_traslado, sha, fecha_hora_acopio, fecha_hora_captura, fecha_hora_verificacion,
                               origen,digitalizacion,tipo_documento, tipo_casilla, representantes_pp_ci, lista_nominal, total_votos_asentado, 
                               total_votos_calculado)) %>% group_by(municipio) %>% summarize_all(as.numeric) %>% 
  group_by(id_municipio,municipio) %>% 
  summarise_all(sum,na.rm=T) %>% pivot_longer(!c(id_municipio,municipio),names_to = "partido",values_to = "votos") %>% 
  mutate(ganador = rank(desc(votos))) %>% filter(ganador<3) %>% 
  pivot_wider(names_from = ganador,values_from = c("partido","votos")) %>% rename(MUNICIPIO = id_municipio)
  

# Pegar
mapa_mun_oax <- mapa_mun_oax %>% left_join(ayuntamientos_21_f, by="MUNICIPIO") %>% arrange(MUNICIPIO)
# Escribir
write_sf(mapa_mun_oax,"output/mapa_muni_oax_21.shp")

## Pueblos Indigenas

indigenas_inpi_2015 <- read_csv("input/oaxaca/pueblos_indigenas_oax_2015.csv") %>% clean_names()


indigenas_inpi_2015 %>% filter(total>0) %>%  mutate(pueblo = ifelse(total<500,"Otros",pueblo_indigena)) %>% group_by(pueblo) %>% 
  summarise(total = sum(total)) %>% 
  ggplot(aes(reorder(pueblo,total),total,fill = pueblo))+
  geom_col()+
  geom_label(aes(label = comma(total)),size = 7)+
  labs(x="", y ="Personas", title = "Numero de Habitantes reconocidos de pueblos indigenas en Oaxaca", 
       subtitle = "Con mas de 500 habitantes ",fill = "", caption = "Fuente: Instituto Nacional de los Pueblos Indigenas (INPI) - 2015")+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 12,face = "bold", color = "black"),
        plot.title = element_text(size = 40, face = "bold", color = "#9f2441"),
        plot.subtitle = element_text(size = 22, face = "bold"),
        plot.caption = element_text(size = 14))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  scale_y_sqrt()














