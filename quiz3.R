#quiz 3

# question 1
data(mtcars)
summary(mtcars)
str(mtcars)
mpg <- mtcars$mpg
cylindar <- as.factor(mtcars$cyl)
wt <- mtcars$wt
q1 <- lm(mpg ~ cylindar + wt)
summary(q1)$coef

# question 2

# question 3
summary(q1)
mpgbycyl <- lm(mpg ~ cyl, mtcars)
summary(mpgbycyl)
mpgbycylwt <- lm(mpg ~ cyl + wt, mtcars)
summary(mpgbycylwt)
interaction(mpgbycyl, mpgbycylwt)
#should be first response: small p-value, reject, no interaction term

# question 4 
q4 <- lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
summary(q4)$coef
plot(q4)
# answer should be change in mpg/1 ton of wt by spec # of cylindars

# question 5
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
q5 <- lm(y ~ x)
hatvalues(q5)
plot(x, y)
abline(q5)
summary(q5)

# question 6
dfbetas(q5)
dfbeta(q5)

