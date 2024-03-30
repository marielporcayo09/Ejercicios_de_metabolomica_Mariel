#Creador: Mariel Porcayo
#Gráfico de dispersión
#Se realiza una serie de experimentos comparativos de cerebros de tres animales destetados cuyas madres estuvieron expuestas a una dieta restrictivas (desnutridas) y se compararon con tres animales cuyas madres se expusieron a una dieta ad libitum (eunutridas). Se desea saber: ¿Qué miRNAs fueron los que más cambiaron entre los animales desnutridos (tratamiento) comparados con los animales eunutridos (control)?

#install. packages

library(pacman)

p_load("readr", #para llamar las bases de datos
       "ggplot", #para graficar
       "ggrepel", #para etiquetar datos en una gráfica
       "dplyr") #facilita el manejo de datos

datos <- read_csv(file = "https://raw.githubusercontent.com/ManuelLaraMVZ/Transcript-mica/main/DesnutridasvsEunutridas.csv")
head(datos)
#Extracción de genes controles (referencia)

Controles <- datos %>% 
  filter(Condicion=="Control")

head(Controles)

promedio_controles <- Controles %>% 
  summarise(Mean_C1 = mean(Cx1), 
            Mean_C2 = mean(Cx2), 
            Mean_C3 = mean(Cx3),
            Mean_T1 = mean(T1),
            Mean_T2 = mean(T2),
            Mean_T3 = mean(T3)) %>% 
  mutate(Gen="Promedio_controles") %>% 
  select(7,1,2,3,4,5,6)
promedio_controles

############################################
#extraer

genes <- datos %>% 
  filter(Condicion=="Target") %>% 
  select(-2)
head(genes)
#################

#Sacar el 2^-DCT

DCT <- genes %>% 
  mutate(DCT_C1=2^-(Cx1-promedio_controles$Mean_C1),
         DCT_C2=2^-(Cx1-promedio_controles$Mean_C2),
         DCT_C3=2^-(Cx1-promedio_controles$Mean_C3),
         DCT_T1=2^-(T1-promedio_controles$Mean_T1),
         DCT_T2=2^-(T2-promedio_controles$Mean_T2),
         DCT_T3=2^-(T3-promedio_controles$Mean_T3)) %>% 
  select(-2,-3,-4,-5,-6,-7)

DCT

promedio_genes <- DCT %>% 
  mutate(Mean_DCT_Cx=(DCT_C1+DCT_C2+DCT_C3)/3,
         Mean_DCT_Tx=(DCT_T1+DCT_T2+DCT_T3)/3)

promedio_genes

#Gráfica
library(ggplot2)
grafica_dispersion<- ggplot(promedio_genes,
                            mapping = aes(x = Mean_DCT_Cx,
                                          y = Mean_DCT_Tx),
                            colour_cut)+
  geom_point(size=3, color="blue")+
  labs(title = "Análisis de puntos",
       x="Condición Control (2^-DCT")+
  geom_smooth(method = "1m",
              alpha=0.05,
              linewidth=1, span=1)+ #para regresion
  theme_minimal()
geom_point()
grafica_dispersion

guardar <- ggsave("grafica_dispersion.jpeg",
                  plot = grafica_dispersion,
                  width=6, height = 4,
                  dpi = 300)

#Crear la gráfica de dispersión

grafica_dispersion <- ggplot(promedio_genes,
                             aes(x = Mean_DCT_Cx,
                                 y = Mean_DCT_Tx))+
  geom_point(size = 2,
             color = "blue")

grafica_dispersion
#agregar el más significa hacer capas en el diagrama

grafica_dispersion2 <- grafica_dispersion+
  labs(title = "Condición control vs tratamiento",
       caption = "Creador: Mariel Porcayo",
       x = expression("Control 2"^"-DCT"),
       y = expression("Tratamiento 2"^"-DCT"))+
  geom_smooth(method = "lm", color = "black")+
  theme_minimal()+ #Estilo de la gráfica
  theme(
    panel.background = element_rect(fill = "white"), #Fondo blanco
    panel.grid.major = element_blank(), #Sin enrejado
    axis.text = element_text(family = "Times New Roman", size = 12), #Texto de ejes tamaño 12 y tipo "Times"
    axis.title = element_text(family = "Times New Roman", size = 14, face = "bold"), #Texto de leyenda tamaño 14 y tipo #Times
    legend.title = element_text(family = "Times New Roman", size = 14),
    legend.text = element_text(family = "Times New Roman", size = 14) #Texto de etiquetas de leyendas tamaño 14 y tipo "Times"
  )
grafica_dispersion2

############################################

#Identificación de los genes
head(promedio_genes)

top_10 <- promedio_genes %>%
  select(1,8,9) %>% 
  top_n(10, Mean_DCT_Cx) %>% 
  arrange(desc(Mean_DCT_Cx))

head(top_10)  

grafica_dispersion3 <- grafica_dispersion2 +
  geom_label_repel(data = top_10, #Usar el date frame con los 10 mejores registros
                   mapping = aes(x = Mean_DCT_Cx,
                                 y = Mean_DCT_Tx,
                                 label = Gen),
                   label.padding = unit(0.2, "lines"))
grafica_dispersion3

ggsave("Dispersion2.jpeg", plot = grafica_dispersion3, height = 5, width = 7, dpi = 300)

