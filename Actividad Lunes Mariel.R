CHO <- 535000
VERO <- 1350000
Total <- CHO+VERO
Total
#nota
x <- 35
y <- 22
w <- 34
z <- 25
Total <- x+z
Total <- y+w
V1 <- c(1,2,3)
V2 <- c(4,5,6)
V3 <- c(7,8,9)
V4 <- c('M','D','Z')
DF_V <- data.frame(V4,V1,V2,V3)
View(DF_V)
install.packages('readr')
library(readr)
setwd("~/")
library(readr)
titulacion <- read_csv("D:/MARIEL/Escuela/UNIII/8VO SEMESTRE/Metabolómica/datos_titulacion.csv")
titulacion <- read_csv(file = 'https://raw.githubusercontent.com/ManuelLaraMVZ/titulacion_amino_acidos/main/datos_titulacion%20(2).csv')
#Internet
head(titulacion)
install.packages('ggplot2')
library('ggplot2')
grafica <- ggplot(titulacion,aes(x=Volumen,y=pH))+geom_point()+labs(title = 'Titulación de aminoácido',x='Volumen',y='pH')
grafica
