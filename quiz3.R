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

# question 4 
q4 <- lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
summary(q4)$coef

# question 5
x <- c(0.586, 0.166, -.042, -.61, 4, 11.72)
y <- c(.549, -.026, -.127, -.7, 51, 1.344)
q5 <- lm(y ~ x)
hatvalues(q5)
plot(x, y)
abline(q5)
summary(q5)

# question 6
dfbetas(q5)

