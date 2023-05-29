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
                   inscritos_materia = as.integer(inscritos_materia),
                   parcial, 
                   fechaparcial = as.Date(fechaparcial),
                   Fecha_Registro = as.POSIXct(Fecha_Registro, format ="%Y-%m-%d %H:%M:%S", tz = "UTC"),
                   asistencia = as.integer(asistencia),
                   TipoExamen,
                   Coordinacion_Catedra,
                   Coordinador_Catedratico,
                   Escuela,
                   Facultad
)

promedio<-as.integer(promedio)
#1-Promedio de examinados por facultad
#EJERCICIO 1
#cargando datos a Test
test <-Tdatos %>% 
  group_by(Facultad) %>% 
  summarise_all(mean) %>% 
  transmute(
    Facultad,
    inscritos_materia,
    asistencia)

test <-test %>% 
  group_by(Facultad) %>% 
  summarise_all(mean) %>% 
  mutate(
    Promedio=((test$asistencia/test$inscritos_materia)*100)
  )
  


##Mostrando Grafico Ejercicio 1

ggplot(test, aes(x=Facultad,y=Promedio)) +
  geom_bar(stat="identity",position = position_stack(), fill = "green", colour="black") + 
  geom_text(aes(label=Promedio)) +
  ggtitle("Promedio de asistencia a los examenes de pregrado por facultad") + 
  theme(plot.title = element_text(face = "bold", colour = "blue")) + 
  theme(axis.title = element_text(face = "bold", colour = "green"))

#EJERCICIO 2
#cargando datos a Test
test_1 <-Tdatos %>% 
  group_by(Coordinacion_Catedra) %>% 
  summarise_all(mean) %>% 
  transmute(
    Coordinacion_Catedra,
    inscritos_materia,
    asistencia)

test_1 <-test_1 %>% 
  group_by(Coordinacion_Catedra) %>% 
  summarise_all(mean) %>% 
  mutate(
    Promedio=((test_1$asistencia/test_1$inscritos_materia)*100)
  )



##Mostrando Grafico Ejercicio 2

ggplot(test_1, aes(y=Coordinacion_Catedra,x=Promedio)) +
  geom_bar(stat="identity",position = position_stack(), fill = "yellow", colour="black") + 
  geom_text(aes(label=Promedio)) +
  ggtitle("Promedio de examinados de pregrado por catedra") + 
  theme(plot.title = element_text(face = "bold", colour = "blue")) + 
  theme(axis.title = element_text(face = "bold", colour = "green"))



#EJERCICIO 3
#cargando datos a Test
test_2 <-Tdatos %>%
  group_by(Escuela) %>%
  summarise_all(mean) %>%
  transmute(
    Escuela,
    inscritos_materia,
    asistencia)

test_2 <-test_2 %>%
  group_by(Escuela) %>%
  summarise_all(mean) %>%
  mutate(
    Promedio=((test_2$asistencia/test_2$inscritos_materia)*100)
  )



##Mostrando Grafico Ejercicio 3

ggplot(test_2, aes(x=Escuela,y=promedio)) +
  geom_histogram(stat="identity",position = position_stack(), fill = "gray", colour="black") + 
  geom_text(aes(label=Promedio)) +
  ggtitle("Promedio de  los examinados de pregrado por Escuela") + 
  theme(plot.title = element_text(face = "bold", colour = "blue")) + 
  theme(axis.title = element_text(face = "bold", colour = "green"))
#################


#EJERCICIO 4
#cargando datos a Test
test_3 <-Tdatos %>% 
  group_by(Coordinador_Catedratico) %>% 
  summarise_all(mean) %>% 
  transmute(
    Coordinador_Catedratico,
    inscritos_materia,
    asistencia)

test_3 <-test_3 %>% 
  group_by(Coordinador_Catedratico) %>% 
  summarise_all(mean) %>% 
  mutate(
    Promedio=((test_3$asistencia/test_3$inscritos_materia)*100)
  )



##Mostrando Grafico Ejercicio 4

ggplot(test_3, aes(x=promedio,y=Coordinador_Catedratico)) +
  geom_bar(stat="identity",position = position_stack(), fill = "gray", colour="black") + 
  geom_text(aes(label=Promedio)) +
  ggtitle("Promedio de  los examinados de pregrado por catedratico") + 
  theme(plot.title = element_text(face = "bold", colour = "blue")) + 
  theme(axis.title = element_text(face = "bold", colour = "green"))

############################
#EJERCICIO 5
#cargando datos a Test
test_4 <-Tdatos %>% 
  group_by(Dias) %>% 
  summarise_all(mean) %>% 
  transmute(
    Dias,
    inscritos_materia,
    asistencia)

test_4 <-test_4 %>% 
  group_by(Dias) %>% 
  summarise_all(mean) %>% 
  mutate(
    Promedio=((test_4$asistencia/test_4$inscritos_materia)*100)
  )



##Mostrando Grafico Ejercicio 5

ggplot(test_4, aes(x=Dias,y=promedio)) +
  geom_bar(stat="identity",position = position_stack(), fill = "pink", colour="blue") + 
  geom_text(aes(label=Promedio)) +
  ggtitle("Promedio de  los examinados de pregrado por dia") + 
  theme(plot.title = element_text(face = "bold", colour = "blue")) + 
  theme(axis.title = element_text(face = "bold", colour = "green"))

#EJERCICIO 6
#cargando datos a Test
test_5 <-Tdatos %>% 
  group_by(modalidad) %>% 
  summarise_all(mean) %>% 
  transmute(
    modalidad,
    inscritos_materia,
    asistencia)

test_5 <-test_5 %>% 
  group_by(modalidad) %>% 
  summarise_all(mean) %>% 
  mutate(
    Promedio=((test_5$asistencia/test_5$inscritos_materia)*100)
  )



##Mostrando Grafico Ejercicio 6

ggplot(test_5, aes(x=promedio,y=modalidad)) +
  geom_histogram(stat="identity",position = position_stack(), fill = "red", colour="blue") + 
  geom_text(aes(label=Promedio)) +
  ggtitle("Promedio de  los examinados de pregrado por modalidad") + 
  theme(plot.title = element_text(face = "bold", colour = "blue")) + 
  theme(axis.title = element_text(face = "bold", colour = "green"))


#EJERCICIO 7
#cargando datos a Test

test_6 <-Tdatos %>% 
  group_by(Seccion) %>% 
  summarise_all(mean) %>% 
  transmute(
    Seccion,
    inscritos_materia,
    asistencia)

test_6 <-test_6 %>% 
  group_by(Seccion) %>% 
  summarise_all(mean) %>% 
  mutate(
    Promedio=((test_6$asistencia/test_6$inscritos_materia)*100))


##Mostrando Grafico Ejercicio 7

ggplot(test_6, aes(y=Seccion,x=promedio,horiz = TRUE)) +
  geom_bar(stat="identity",position = position_stack(), fill = "white", colour="red") + 
  geom_text(aes(label=Promedio)) +
  ggtitle("Promedio de  los examinados de pregrado por seccion") + 
  theme(plot.title = element_text(face = "bold", colour = "blue")) + 
  theme(axis.title = element_text(face = "bold", colour = "green"))





#EJERCICIO 8
#cargando datos a Test

test_7 <-Tdatos %>% 
  group_by(TipoExamen) %>% 
  summarise_all(mean) %>% 
  transmute(
    TipoExamen,
    inscritos_materia,
    asistencia)

test_7 <-test_7 %>% 
  group_by(TipoExamen) %>% 
  summarise_all(mean) %>% 
  mutate(
    Promedio=((test_7$asistencia/test_7$inscritos_materia)*100))


##Mostrando Grafico Ejercicio 8

ggplot(test_7, aes(y=promedio,x=TipoExamen,horiz = TRUE)) +
  geom_histogram(stat="identity",position = position_stack(), fill ="green", colour="red") + 
  geom_text(aes(label=Promedio)) +
  ggtitle("Promedio de  los examinados de pregrado por seccion") + 
  theme(plot.title = element_text(face = "bold", colour = "blue")) + 
  theme(axis.title = element_text(face = "bold", colour = "green"))

##Mostrando Grafico Ejercicio 9

ggplot(data =Tdatos, aes(x = TipoExamen, y =asistencia))+
  geom_point()

##Mostrando Grafico Ejercicio 10
ggplot(data = Tdatos, aes(x =asistencia, y = Facultad))+
  geom_point()+
  facet_grid(.~ TipoExamen)

##Mostrando Grafico Ejercicio 11
ggplot(Tdatos, aes(x = inscritos_materia, y =asistencia, col = modalidad, shape = TipoExamen)) +
  geom_point()
