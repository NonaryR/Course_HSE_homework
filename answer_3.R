library("memisc")
library("lmtest")
library("ggplot2")
library("dplyr")
library("foreign")
library("vcd")
library("devtools")
library("hexbin")
library("pander")
library("sjPlot")
library("knitr")

df <- diamonds
help(df)
glimpse(df)
model_00 <- lm(data=df, log(price)~log(carat))

mtable(model_00)

model_02 <- lm(data = df, log(price) ~ log(carat) + y + x)
mtable(model_02)
summary(model_02)

model_03 <- lm(data = df, price ~ carat)
mtable(model_03)
sjp.lm(model_03)


model_04 <- lm(data = df, price ~ carat + depth + cut)
mtable(model_04)

#сравним 3 модели

model_03 <- lm(data = df, price ~ carat)

model_05 <- lm(data = df, price ~ carat + depth)
mtable(model_05)

model_04 <- lm(data = df, price ~ carat + depth + cut)

mtable(model_03, model_05, model_04)

# F-тест
waldtest(model_05, model_04) # H_0: model_05 H_a: model_04 
#Н_0 - отвергается

# тест Рамсея
resettest(model_03)


qplot(data=df, log(carat), log(price), color = clarity) + facet_wrap(~cut) 
