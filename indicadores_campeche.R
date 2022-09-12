## Indicadores Campeche
# David A. Ortega - 28/07/22

# Preparar Entorno ----
pacman::p_load(tidyverse,janitor,sf,scales)
rm(list=ls())
dev.off()

## Pueblos Indigenas

indigenas_inpi_2015 <- read_csv("input/campeche/CAMPECHE 2015.csv") %>% clean_names()


indigenas_inpi_2015 %>% filter(total>0) %>%  mutate(pueblo = ifelse(total<500,"Otros",pueblo_indigena)) %>% group_by(pueblo) %>% 
  summarise(total = sum(total)) %>% 
  ggplot(aes(reorder(pueblo,total),total,fill = pueblo))+
  geom_col()+
  geom_label(aes(label = comma(total)),size = 7)+
  labs(x="", y ="Personas", title = "Numero de Habitantes reconocidos de pueblos indigenas en Campeche", 
       subtitle = "Con mas de 500 habitantes ",fill = "", caption = "Fuente: Instituto Nacional de los Pueblos Indigenas (INPI) - 2015")+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 12,face = "bold", color = "black"),
        plot.title = element_text(size = 40, face = "bold", color = "#9f2441"),
        plot.subtitle = element_text(size = 22, face = "bold"),
        plot.caption = element_text(size = 14))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  scale_y_sqrt()

indigenas_inpi_2015 %>% summarise(sum(total))
