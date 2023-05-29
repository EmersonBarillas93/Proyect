
aux<-mtcars

col1<-aux$mpg

promedio<-mean(col1)

calcular<-function(valor)
{
  print(valor)
}

setosa<-subset(iris,Species=="setosa")
virginica<-subset(iris,Species=="virginica")
versicolor<-subset(iris,Species=="versicolor")

calcular(setosa)
calcular(virginica)
calcular(versicolor)

hist(iris$Sepal.Length, xlab = "Longitud",ylab = "Frecuencia",
     main = "Histograma de frecuencias", col = "blue")

graficar<-function(valor,color)
{
  hist(valor$Sepal.Length, xlab = "Longitud",ylab = "Frecuencia",
       main = "Histograma de frecuencias", col = color)
}

graficar(setosa,"green")
graficar(virginica,"red")
graficar(versicolor,"blue")

for (i in c(1:5)) {
  temp<-i*i
  print(temp)
}

a<-matrix(seq(5,50,5),nrow = 5)
b<-matrix(seq(5,50,5),nrow = 5)
c<-a*b

DataExaminados<-read.csv(file =  paste(getwd(),'/prueba.csv', sep = ""),
                         header = TRUE, sep = ',')

