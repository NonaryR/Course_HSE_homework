library("ggplot2")
df <- diamonds
df
head(df)
mean(df$price)
df$cut
summary(df$cut)

model <- glm(data=df, price ~ carat + clarity)
model

model2 <- glm(data=df, price ~ carat)
summary(model2)


model3 <- glm(data=df, price ~ carat + table + y + x)
summary(model3)

summary(df)

model4 <- glm(data=df, price ~ carat + y + x)
summary(model4)
a <- vcov(model4)
a
qt(0.95, df = 27)

