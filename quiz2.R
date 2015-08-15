# quiz 2 Regression Models

# question 1
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
q1 <- lm(y ~ x)
summary(q1)

# question 2
q2 <- residuals(q1)
sd(q2)

# question 3
data(mtcars)
q3 <- lm(mpg ~ wt, data = mtcars)
q3Coef <- summary(q3)$coefficients
q3Coef
q395 <- q3Coef[2,1] + c(-1,1) * qt(.975, df = q3$df) * q3Coef[2,2] # used this command for the answer
q3
mean(mtcars$wt)
q3Beta_1 <- cor(mtcars$mpg, mtcars$wt) * (sd(mtcars$mpg)/sd(mtcars$wt))
q3mpg <- q3Coef[1,1] + q3Beta_1 * mean(mtcars$wt) # predicted MPG at mean weight
q3Coef[1,1] + (-6.486308) * mean(mtcars$wt) 
q3mpg + (q395[1] - q3Beta_1) 
# just add difference of slope and lower end point of the 95% confidence interval to expected value

  
# question 4

# question 5
q3
summary(q3)$df
summary(q3)
q5EV <- 37.285 + -5.344 * 3
predict.lm(q3, 3, df=summary(q3)$df, interval = "prediction")
sd(mtcars$wt) * sqrt(1 + 1/32 + ((3 - mean(mtcars$wt))^2/sum(mtcars$wt - mean(mtcars$wt)^2)))

y <- mtcars$mpg; x <- mtcars$wt; n <- length(y)
beta1 <- cor(y, x) * sd(y)/sd(x)
beta0 <- mean(y) - beta1 * mean(x)
e <- y - beta0 - beta1 * x
sigma <- sqrt(sum(e^2)/(n-2))
ssx <- sum((x - mean(x))^2)

fit <- lm(y ~ x)
plot(x, y, xlab = "Weight", ylab = "MPG", pch=21,col="black", bg="lightblue",cex=2)
abline(fit, lwd = 2)
xVals <- seq(min(x) , max(x), by = .1)
yVals <- beta0 + beta1 * xVals
se1 <-sigma*sqrt(1/n+(xVals-mean(x))^2/ssx)
se2 <- sigma * sqrt(1+ 1/n + (xVals - mean(x))^2/ssx)
--lines(xVals,yVals+2*se1)
--lines(xVals,yVals-2*se1)
lines(xVals,yVals+2*se2)
lines(xVals,yVals-2*se2)

q3Coef[2,1] + c(-1,1) * qt(.975, df = q3$df) * q3Coef[2,2]
predict(q3, 3, df = 30, interval = "prediction")

# question 9 
