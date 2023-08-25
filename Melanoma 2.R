
library(readr)
melanoma_2 <- read_csv("Statistics Ds/Assignment/melanoma-2.csv")


library(tidyverse)
melanoma_2 %>% 
  select(time, status, sex, age, year, thickness, ulcer) %>% 
  map_df(.f = ~ broom::tidy(summary(.x)), .id = "variable")

# Load the ggplot2 library
library(ggplot2)

# Create a bar plot
ggplot(melanoma_2, aes(x = sex)) +
  geom_bar(stat = "count") +
  labs(x = "sex", y = "count", title = "Female=0 Male=1")
  

# Create a bar plot
ggplot(melanoma_2, aes(x = status)) +
  geom_bar(stat = "count") +
  labs(x = "status", y = "count", title = "Death from melanoma=1. Alive=2. Death not melanoma=3")

# Create a bar plot
ggplot(melanoma_2, aes(x = ulcer)) +
  geom_bar(stat = "count") +
  labs(x = "ulcer", y = "count", title = "Present=1. Absent=0")

par(mfrow= c(3,1)) # 3 rows of histogram
hist(rio_csv$time,
     main = "Time",
     xlab="",
     col = "red")

hist(rio_csv$thickness,
     main = "Thickness",
     xlab="",
     col = "blue")

hist(rio_csv$age,
     main = "Age",
     xlab="",
     col = "green")

par(mfrow= c(1,1))
# Bar chart, need a table with frequency for each category
years <- table(melanoma_2$year)
barplot(years,
        col= "red",
        main = "Bar chart")
plot(years)
boxplot(melanoma_2$year,
        main="Boxplot of operation years", 
        ylab="Years", 
        xlab="", 
        col="green", 
        border="red", 
        horizontal=FALSE, 
        notch=TRUE)

library(ggplot2)
# Regression
fit <- lm(time ~ thickness, data = melanoma_2)
summary(fit)
cor(melanoma_2$thickness, melanoma_2$time)
#visualize the model
ggplot(melanoma_2, aes(x = thickness, y = time)) +
  geom_point() +
  geom_line(aes(y = predict(fit)), color = "blue")

fit2 <- lm(time ~ age, data = melanoma_2)
summary(fit2)
cor(melanoma_2$age, melanoma_2$time)
#Visulaize the model
ggplot(melanoma_2, aes(x = age, y = time)) +
  geom_point() +
  geom_line(aes(y = predict(fit2)), color = "blue")

fit3 <- lm(thickness ~ age, data = melanoma_2)
summary(fit3)
cor(melanoma_2$age, melanoma_2$thickness)
#Visualize the model
ggplot(melanoma_2, aes(x = age, y = thickness)) +
  geom_point() +
  geom_line(aes(y = predict(fit3)), color = "blue")

#Two sample significance test
t.test(time ~ sex, data = melanoma_2)
t.test(thickness ~ sex, data = melanoma_2)
t.test(age ~ sex, data = melanoma_2)

#Load the ggplot2 library
library(ggplot2)

# Create a Q-Q plot grouped by gender
ggplot(melanoma_2, aes(sample = age)) +
  stat_qq() +
  stat_qq_line()+
  labs(x = "sex", y = "age", title = "Q-Q Plot for age grouped by gender") +
  facet_wrap(~ sex)

ggplot(melanoma_2, aes(sample = thickness)) +
  stat_qq() +
  stat_qq_line()+
  labs(x = "sex", y = "thickness", title = "Q-Q Plot for thickness grouped by gender") +
  facet_wrap(~ sex)

ggplot(melanoma_2, aes(sample = time)) +
  stat_qq() +
  stat_qq_line()+
  labs(x = "sex", y = "time", title = "Q-Q Plot for time grouped by gender") +
  facet_wrap(~ sex)

hist(melanoma_2$age,
     col = "yellow",
     main = "Histogram of age")

hist(melanoma_2$thickness,
     col = "yellow",
     main = "Histogram of thickness")

hist(melanoma_2$time,
     col = "yellow",
     main = "Histogram of time")




# Extras for tumor thickness
par(mfrow = c(2,1)) # combining two histograms in one plot
hist(melanoma_2$status [melanoma_2$thickness > 1.94],
     xlim= c(0,3),
     breaks= 3,
     main = "Tumor thickness greater than median value(1.94) vs life status",
     xlab = "",
     col = "red")

hist(melanoma_2$status [melanoma_2$thickness < 1.94],
     xlim= c(0,3),
     breaks= 3,
     main = "Tumor thickness lesser than median value(1.94) vs life status",
     xlab = "",
     col = "blue")

par(mfrow= c(1,1))

