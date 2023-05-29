#1-Promedio de examinados por facultad
#Librerias y Llamado de datos
library(jsonlite) #Llamamos Json
library(ggplot2)
library(tidyverse)
library(gapminder)
library(dplyr)
datos<-fromJSON("https://tecnologica.utec.edu.sv/sistema/services/vistaexaminados.php") #Se cargan los datos de una URL

#Limpiando los datos y realizando un nuevo tibble con tipo de datos correctos. 

Tdatos<-transmute(datos,
                  codigo_asignatura,
                  tipo_asignatura,
                  Asignatura,
                  Seccion,
                  Horario, 
                  Dias, 
                  modalidad, 
                  Docente, 
                  inscritos_materia = as.numeric(inscritos_materia),
                  parcial, 
                  fechaparcial = as.Date(fechaparcial),
                  Fecha_Registro = as.POSIXct(Fecha_Registro, format ="%Y-%m-%d %H:%M:%S", tz = "UTC"),
                  asistencia = as.numeric(asistencia),
                  TipoExamen,
                  Coordinacion_Catedra,
                  Coordinador_Catedratico,
                  Escuela,
                  Facultad
)

#EJERCICIO 3
#cargando datos a Test
test <-Tdatos %>% 
  group_by(Escuela) %>% 
  summarise_all(mean) %>% 
  transmute(
    Escuela,
    inscritos_materia,
    asistencia)

test <-test %>% 
  group_by(Escuela) %>% 
  summarise_all(mean) %>% 
  mutate(
    Promedio=((test$asistencia/test$inscritos_materia)*100)
  )



##Mostrando Grafico Ejercicio 1

ggplot(test, aes(x=Escuela,y=promedio)) +
  geom_histogram(stat="identity",position = position_stack(), fill = "gray", colour="black") + 
  geom_text(aes(label=Promedio)) +
  ggtitle("Promedio de  los examinados de pregrado por Escuela") + 
  theme(plot.title = element_text(face = "bold", colour = "blue")) + 
  theme(axis.title = element_text(face = "bold", colour = "green"))

