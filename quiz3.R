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
