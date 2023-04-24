
Data = data.frame(Nombre = c("Auricularia delicata", "Psathyrella candolleana","Hydropus brunneoumbonatus", "Earliella scabrosa", "Trogia cantharelloides",
                             "Ramaria sticta", "lentinus velutinus", "Cookeina tricholoma", "Coriolopsis polyzona", "Xylaria cubensis"),
                  Zona = c("Inundable con Palmeras", "Inundable con Palmeras","Inundable con Palmeras","Inundable con Palmeras","Inundable con Palmeras",
                           "Terraza alta con castaña", "Terraza alta con castaña", "Terraza alta con castaña", "Terraza alta con castaña", "Terraza alta con castaña"),
                  Valor = c(71, 56, 31, 28, 26, 18,18, 17, 16, 12))
Data



library(grid)
library(png)
library(ggimage)


Logo2 <- readPNG("PNG/lentinus velutinus.png", FALSE)
Logo_png2 <- rasterGrob(Logo2, x = unit(0.9, "npc"),y = unit(0.6, "npc"), width = unit(0.2, "npc"))


Logo <- readPNG("PNG/AURICULARIA.png", FALSE)
Logo_png <- rasterGrob(Logo, x = unit(0.9, "npc"),y = unit(0.1, "npc"), width = unit(0.2, "npc"))


library(tidyverse)
library(dplyr)

Data <- Data %>%
  mutate(Nombre = fct_reorder(Nombre , Valor, .desc = TRUE))

library(ggplot2)

GG= ggplot(data=Data ,aes(x= Nombre , y=Valor, fill=Zona )) + 
  scale_fill_manual(values = c("#fb8500", "#14213d"))+
  geom_bar( stat='identity', position='dodge', color="black") +
  coord_flip()+
  ylim(0,95)+
  geom_text(aes(label=paste0(round(Valor,0), ""), hjust=0.5) , position = position_dodge(0.90), size = 3.5, color="black",
            vjust=0.5, hjust=-0.5)+
  theme_bw()+
  annotation_custom(Logo_png2)+
  annotation_custom(Logo_png)+
  theme(axis.text.y  = element_text(color="black", size=10,
                                    family="serif"),
        axis.text.x  = element_text(color="black", size=10,
                                    family="serif"),
        legend.title = element_text(size = 11, color = "black", face='bold'),
        axis.title = element_text(size = 11, color = "black", face='bold'),
        legend.position = "top")+
  labs( x="Numero de Individuos", y="Macrohongos" , fill="Tipo de Bosque")


ggsave(plot = GG ,"Macrohongos_tipos.png", units = "cm", width = 20,height = 20, dpi = 1200)


