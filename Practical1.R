houses.raw <- read.csv("http://www.stats.ox.ac.uk/~laws/SB1/data/houses.csv")

samples <- 100

summary(houses.raw)

plot(houses.raw)

par(mfrow=c(2,3))
boxplot(price~id, data=houses.raw, xlab='Id', ylab='price')
boxplot(price~size, data=houses.raw, xlab='size', ylab='price')
boxplot(price~new, data=houses.raw, xlab='new', ylab='price')
boxplot(price~N1, data=houses.raw, xlab='Number of Bedrooms', ylab='price')
boxplot(price~N2, data=houses.raw, xlab='Number of Bathrooms', ylab='price')
boxplot(price~tax, data=houses.raw, xlab='tax', ylab='price')

lilm0 <- lm(price~id, data=houses.raw)
anova(lilm0)
lilm1 <- lm(price~size, data=houses.raw)
anova(lilm1)
lilm2 <- lm(price~new, data=houses.raw)
anova(lilm2)
lilm3 <- lm(price~N1, data=houses.raw)
anova(lilm3)
lilm4 <- lm(price~N2, data=houses.raw)
anova(lilm4)
lilm5 <- lm(price~tax, data=houses.raw)
anova(lilm5)

# install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(houses.raw)

lm1 <- lm(price~size+new+N1+N2+tax, data=houses.raw)
summary(base_model)

par(mfrow=c(1,1))
plot(fitted(base_model), rstudent(base_model), xlim=c(-50,800), 
     col = as.numeric(houses.raw$price), pch=16, xlab="Fitted Values", ylab="Studentised Residuals")
# identify(fitted(base_model), rstudent(base_model))
text(fitted(lm1), rstudent(lm1), houses.raw$id, adj = 1.5)
abline(h = 2)
abline(h = -2)

houses.ordered <- houses.raw[order(houses.raw$price),]
houses.ordered

hatvalues(lm1)
p <- lm1$rank
par(mfrow=c(1,1))
plot(cooks.distance(lm1), col = as.numeric(houses.raw$price), pch=16)
abline(h = 8/(samples - 2 * p))
text(1:samples, cooks.distance(lm1), houses.raw$id)

houses.outliers <- houses.raw[c(6, 9, 22, 64, 88),]

houses.cleaned <- houses.raw[-c(6, 64, 88),]
lm2 <- lm(price~size+new+N1+N2+tax, data=houses.cleaned)
summary(lm2)

summary(lm2)$cov.unscaled

par(mfrow=c(1,1))
library(MASS)
boxcox(lm2)

houses.transformed <- houses.cleaned
houses.transformed$price <- houses.transformed$price^0.6
lm3 <- lm(price~size+new+N1+N2+tax, data=houses.transformed)
summary(lm3)

par(mfrow=c(1,2))
qqnorm(res <- rstudent(lm3))
qqline(res)
hist(res,probability = T)
shapiro.test(resid(lm3))

step(lm1)
step(lm2)
step(lm3)


y1<-predict(lm3,newdata = data.frame(size=2000,new=0, N1=3,N2=3,tax=3900),
            interval="prediction")
y2<-predict(lm3,newdata = data.frame(size=3000,new=1, N1=6,N2=3,tax=6000),
            interval="prediction")
y3<-predict(lm3,newdata = data.frame(size=5000,new=1, N1=4,N2=4,tax=7000),
            interval="prediction")
y1^(5/3)
y2^(5/3)
y3^(5/3)
