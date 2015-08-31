# quiz 4, Regression Models

# question 1
q1 <- glm(use ~ wind, family = "binomial", shuttle)
exp(q1$coeff)
plot(shuttle$wind, q1$fitted, pch=19, col="blue", xlab="Wind Type", ylab = "Probability of Autolander Use")

# question 2
q2 <- glm(use ~ wind + magn, family = "binomial", shuttle)
exp(q2$coeff)
unique(shuttle$magn)
plot(shuttle$wind, q2$fitted, pch=19, col="blue", xlab="Wind Type and Speed", ylab = "Probability of Autolander Use")

#question 3
q3 <- glm(use ~ wind - 1, family = "binomial", shuttle)
exp(q1$coefficients)
exp(q3$coeff)
summary(q1)$coefficients
summary(q3)$coefficients

# question 4
data("InsectSprays")
q4 <- glm(count ~ spray - 1, family = poisson, data = InsectSprays)
summary(q4)$coef
2.6741486/2.7300291


# question 5
q5data <- subset(InsectSprays, spray == "A" | spray == "B")
# Swirl code:
# mdl2 <- glm(formula = simplystats ~ date, family = poisson, data = hits, offset = log(visits + 1))

# question 6
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
xMat <- cbind(1, x, 0)
q6 <- glm(y ~ xMat)
yhat <- predict(q6)
plot(x,y,frame=FALSE,pch=21,bg="lightblue",cex=2)
lines(x,yhat,col="red",lwd=2)
summary(q6)$coef
