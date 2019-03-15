#Refine the data set
hmv=read.csv("Home Market Value.csv")
hmv$Market.Value=as.numeric(gsub('[$,]','',hmv$Market.Value))
hmv$Square.Feet=as.numeric(gsub('[,]','',hmv$Square.Feet))

# summarizing

summary(hmv)
plot(hmv)

hist(hmv$House.Age, cex.main=0.75)

#Data Visualizaton

par(mfrow=c(1,2))
hist(hmv$House.Age,20,cex.main=0.75)

hist(hmv$House.Age, cex.main=0.75)

hist(hmv$Square.Feet, cex.main=0.75)

hist(hmv$Market.Value, cex.main=0.75)

#commutative distribution 

p=ecdf(hmv$Market.Value)
plot(p,cex.main=0.75)

#Correlation
cor(hmv)


#Predict market value
nAge = c(26,28,29,30,31)
nSquareFeet = c(1650,1500,1800,2200,2400)
nData = data.frame(House.Age = nAge,Square.Feet = nSquareFeet)

hmv.pre = predict(lm.hmv,nData, level = 0.95, interval = "confidence")

hmv.pre = predict(lm.hmv, nData, level = 0.95, interval = "confidence")

z = hmv$Market.Value
x = hmv$Square.Feet
y = hmv$House.Age
lm.hmv = lm(z~x + y)
nData = data.frame(x = nSquareFeet, y = nAge)
hmv.pre = predict(lm.hmv, nData, level = 0.95, interval = "confidence")
hmv.pre