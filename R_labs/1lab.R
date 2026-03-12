#1. Выбрать два города и рассчитать коэффициенты уравнения прямой (y=c+d∙x),
#проходящей через точки, соответствующие этим двум городам: c и d.
#2. Нарисовать прямую линию на графике диаграммы рассеяния. Прямая должна
#проходить через точки, соответствующие выбранным городам.
# x$cityname[x$salary>20000] # Вывод названий городов, удовлетворяющих условию

# setwd("C:/рабочая папка/")
x <- read.table('data_2.txt', header=TRUE, sep=',') 
print(x)

x1 <- x$dist[x$cityname=='Казань'] # x1
y1 <- x$salary[x$cityname=='Казань'] # y1
x2 <- x$dist[x$cityname=='Новосибирск'] # x2 
y2 <- x$salary[x$cityname=='Новосибирск'] # y2 

c = (y2 - y1) / (x2 - x1)
d = y1 - c * x1
#print(c)
#print(d)

plot(x=x$dist, 
     y=x$salary, 
     xlab="Расстояние до Москвы", ylab="Средняя заработная плата",
     main="Диаграмма рассеяния", 
     col="red", 
     pch=19, 
     xlim=c(0, max(x$dist) * 1.25))

text(x=x$dist, y=x$salary, labels=x$cityname, 
     pos=4,  
     offset=1,
     cex=1)  

legend("topleft",
       c("Город","Область"), 
       col=c("red","green"), 
       text.col = "blue", 
       pch = c(2,22), 
       bg="yellow")

abline(a=d, b=c, col="green")  
