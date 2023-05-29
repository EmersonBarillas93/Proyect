x<-"esta es una cadena"
x
x<-5.26
x
x<-6
x
x<-TRUE
x
x<-c(5,8,7,95,7)
x
x[3]
x[2]+x[5]
x[2]-x[5]
x[2]*x[5]
x[2]/x[5]
x[2]^x[5]
x[2]%%x[5]
x<-seq(5,50,5)
y<-rep(5,10)
x*y
x[3]*y[2:4]
x-y
x<-matrix(x,nrow = 5, byrow = FALSE)
y<-matrix(x,nrow = 5, byrow = TRUE)
x+y

x
y
x[3,2]+y[1,2]

sumar<-function(a,b)
{
  print(a+b)
}

z<-sumar(x,y)

sumar2<-function(a,b)
{
  print(a+b)
}

for (i in c(1:5)) {
  sumar2(i,3)
}
