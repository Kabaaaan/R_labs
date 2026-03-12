
T<-read.table("reg.txt", header=TRUE)

# A1 & A2
plot(x=T$A1, y=T$A2)
round(cor(T$A1, T$A2), digits = 2)
cor.test(T$A1, T$A2, conf.level = 0.9) 

# A1 & A3
plot(x=T$A1, y=T$A3)
abline(h=750, col="red")

round(cor(T$A1, T$A3), digits = 2)

T3<-T[-which(T$A3>750),c(1,3)] 
plot(x=T3$A1, y=T3$A3)

round(cor(T3$A1, T3$A3), digits = 2)
cor.test(T3$A1, T3$A3, conf.level = 0.9) 

# A1 & A4
plot(x=T$A1, y=T$A4)
round(cor(T$A1, T$A4), digits = 2)
cor.test(T$A1, T$A4, conf.level = 0.9) 

# A1 & A5
plot(x=T$A1, y=T$A5)
round(cor(T$A1, T$A5), digits = 2)
cor.test(T$A1, T$A5, conf.level = 0.9) 

# A1 & A6
plot(x=T$A1, y=T$A6)
abline(h=500, col="red")

round(cor(T$A1, T$A6), digits = 2)

T6<-T[-which(T$A6>500),c(1,6)] 
plot(x=T6$A1, y=T6$A6)

round(cor(T6$A1, T6$A6), digits = 2)
cor.test(T6$A1, T6$A6, conf.level = 0.9) 

# A1 & A7
plot(x=T$A1, y=T$A7)

round(cor(T$A1, T$A7), digits = 2)
cor.test(T$A1, T$A7, conf.level = 0.9) 

# A1 & A8
plot(x=T$A1, y=T$A8)

round(cor(T$A1, T$A8), digits = 2)
cor.test(T$A1, T$A8, conf.level = 0.9) 


# A1 & A9
plot(x=T$A1, y=T$A9)

round(cor(T$A1, T$A9), digits = 2)
cor.test(T$A1, T$A9, conf.level = 0.9) 

# A1 & A10
plot(x=T$A1, y=T$A10)

round(cor(T$A1, T$A10), digits = 2)
cor.test(T$A1, T$A10, conf.level = 0.9) 

# A1 & A11
plot(x=T$A1, y=T$A11)

round(cor(T$A1, T$A11), digits = 2)
cor.test(T$A1, T$A11, conf.level = 0.9) 

# A1 & A12
plot(x=T$A1, y=T$A12)
abline(h=600, col="red")

round(cor(T$A1, T$A12), digits = 2)

T12<-T[-which(T$A12<600),c(1,12)] 
plot(x=T12$A1, y=T12$A12)

round(cor(T12$A1, T12$A12), digits = 2)
cor.test(T12$A1, T12$A12, conf.level = 0.9) 