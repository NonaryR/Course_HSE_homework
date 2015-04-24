library("dplyr")
library("psych")
library("lmtest")
library("sgof")
library("ggplot2") 
library("car")
library("rlms")
library("pander")
library("car") 
library("broom")
library("sandwich")

data <- read.rlms("r22i_os25a.sav", suppress=TRUE)
head(data)

# возьмем из этой таблицы нужные столбы и поместим в таблицу my_data
my_data <- data.frame(zarplata = data[,'rj13.2'], year_born = data[, 'rh6'], 
                      sex = data[,'rh5'], educ = data[,'r_diplom'], 
                      courses = data[,'rj72.11'], alco_on_work = data[,'rm83.4'])
head(my_data)

my_year <- my_data$rh6
year_born<-2013-my_year

my_data <- data.frame(zarplata = data[,'rj13.2'], year_born = year_born, 
                      sex = data[,'rh5'], educ = data[,'r_diplom'], 
                      courses = data[,'rj72.11'], alco_on_work = data[,'rm83.4'])

pander(head(my_data)[])

my_educ <-my_data$r_diplom
my_data$neuchi <-as.integer(my_educ=='незаконч среднее образование (7 - 8 кл)' ,
                           'окончил 0 - 6 кл', 
                           'незаконч среднее образование (7 - 8 кл) + что-то еще') 
my_data$srednee <-as.integer(my_educ=='законч среднее образование')
my_data$spec_sred <-as.integer(my_educ=='законч среднее специальное образование')
my_data$high <-as.integer(my_educ=='законч высшее образование и выше')

my_data <- subset(my_data, select = -c(r_diplom))

clear_data<-na.omit(my_data)

colnames(clear_data) <- c("Zarplata", "Vozrast", "Pol", "Courses", "Alco_on_work",
                          "SrSchool", "School", "Tech", "VUZ")
head(clear_data)


desc <- describe(clear_data)
pander(data.frame(desc[,3:5], desc[,8:9]))


summ <- summary(clear_data)
pander(data.frame(summ[, 1:5]))


colors = c("green", "gold", "red", "violet", "orange", "blue", "pink", 
           "cyan", "seagreen", "yellow", "tan", "tomato") 
hist(clear_data$Vozrast, xlab = "Возраст", main = "Гистограмма возраста", col = colors)
hist(clear_data$Zarplata, xlab = "Доход", main = "Гистограмма дохода", col = colors)


model_1 = lm(data = clear_data, Zarplata ~ Vozrast + Pol + 
             SrSchool + Tech + VUZ + Alco_on_work + Courses)
summary(model_1)

pander(a [,])

coeftest(model_1,vcov. = vcovHAC(model_1))
