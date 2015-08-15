# quiz 1 Regression models

#question 1
w <- c(2, 1, 3, 1)
x <- c(0.18, -1.54, 0.42, 0.95)
mean(w)
mean(x) * mean(w)

B_1 <- cor(w, x) * (sd(w)/sd(x))
B_0 <- mean(w) - B_1*mean(x)
rbind(c(B_0, B_1), coef(lm(w ~ x)))

lm(I(x - mean(x)) ~ I(w - mean(w)) - 1)

sum(w*(x - 1.077)^2)
sum(w*(x - .1471)^2)
sum(w*(x - .0025)^2)
sum(w*(x - .3)^2)

#question 2
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
q2 <- lm(x ~ y)
plot(x, y)
abline(0, q2, lwd = 3)
q2

q2b_1 <- cor(y, x) * sd(y)/sd(x)
q2b_0 <- mean(y) - q2b_1 * mean(x)

#question 3
data(mtcars)
wt <- lm(mtcars$mpg ~ mtcars$wt)

#question 6
q6 <- c(8.58, 10.46, 9.01, 9.64, 8.86)
sd(q6)
mean(q6)
q6[5] - mean(q6)
(q6[1] - mean(q6))/1
(1/5) * sum(q6)
(q6[1]-mean(q6))/sd(q6)

#question 7
q7x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
q7y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
# y = Bo + B1*x
lm(q7y ~ q7x)

# question 9

q9 <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
plot(q9)
hist(q9)
q9mean <- mean(q9)
sum(q9 - q9mean)^2

