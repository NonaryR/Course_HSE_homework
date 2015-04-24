library("psych") # описательные статистики
library("dplyr") # манипуляции с данными
library("ggplot2") # графики
library("GGally") # еще графики

b <- data.frame(mtcars)
str(b)

# оценим модель парной регрессии y_i = \beta_1 + \beta_2 x_i + \varepsilon_i
model <- lm(data=b, mpg~disp+hp+wt)
summary(model)
y <-b$mpg
RSS <- deviance(model) # так называют RSS
TSS <- sum((y-mean(y))^2) # TSS
TSS
ESS <- TSS - RSS
r <- ESS/TSS
r

model2 <- lm(data=b, mpg~disp+hp+wt+am)
model2
y <-b$mpg
RSS <- deviance(model2) # так называют RSS
TSS <- sum((y-mean(y))^2) # TSS
TSS
ESS <- TSS - RSS
r <- ESS/TSS
r

model3 <- lm(data=b, mpg~cyl+hp+wt+am)
model3
y <-b$mpg
RSS <- deviance(model3) # так называют RSS
TSS <- sum((y-mean(y))^2) # TSS
TSS
ESS <- TSS - RSS
r <- ESS/TSS
r

model4 <- lm(data=b, mpg~disp+cyl+wt+am)
model4
y <-b$mpg
RSS <- deviance(model4) # так называют RSS
TSS <- sum((y-mean(y))^2) # TSS
TSS
ESS <- TSS - RSS
r <- ESS/TSS
r

model5 <- lm(data=b, mpg~disp+hp+cyl+am)
model5
y <-b$mpg
RSS <- deviance(model5) # так называют RSS
TSS <- sum((y-mean(y))^2) # TSS
TSS
ESS <- TSS - RSS
r <- ESS/TSS
r

comp <- mtable(model, model2, model3, model4, model5)
comp
