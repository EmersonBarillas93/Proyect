z<-read.csv(paste(getwd(),"/R/Data/vw_examinados.csv",sep = ""), 
            header = T)
hist(x$asistencia, main = "Distribucion de clases por horario", 
     xlab="Horario", ylab = "Frecuencia", col = "green")

plot(x$asistencia, main = "Distribucion de clases por horario", 
     xlab="Horario", ylab = "Frecuencia", col = "red")
