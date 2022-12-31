install.packages("pacman")
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, ggvis, httr, lubridate, plotly,
               rio, rmarkdown, shiny, stringr, tidyr)
# Package consisting of libraries to make data modelling and manipulation


rio_csv <- import("C:/Users/dipoa/OneDrive/Documents/Statistics Ds/Assignment/melanoma-2.csv")
head(rio_csv)
# Rio reads files in any format. txt, xlsx and csv


df <- tbl_df(rio_csv)
# Creating a new data frame variable to store our data


df.sum <- df %>%
  select(time, age, thickness) %>% # Select specific quantitative variables to summarize
  summarise_each(funs(Minimum = min, 
                      Q1 = quantile(., 0.25), 
                      Median = median, 
                      Q3 = quantile(., 0.75), 
                      Maximum = max,
                      Mean = mean, 
                      SD = sd))

df.stats.tidy <- df.sum %>% gather(stat, val) %>% # Reshaping using tidyr
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  select(var, Minimum, Q1, Median, Q3, Maximum, Mean, SD) # reorder columns

print(df.stats.tidy)
# Print statistical properties of variables

summary(rio_csv$age)
summary(rio_csv$year)

boxplot(rio_csv$year,
        col="red", # Color of box
        border= "blue", # Color of borders around the box
        main= "Graphical summary of year",
        xlab= " ",
        ylab= "Years") # Y-axis label
plot(rio_csv$year,
     col="red", 
     pch= 19, # Solid circles for points
     main= "Year distribution",
     xlab= "Index", # X-axis label
     ylab= "Years") # Y-axis label

boxplot(rio_csv$age,
        col="red", # Color of box
        border= "blue", # Color of borders around the box
        main= "Graphical summary of age",
        xlab= " ",
        ylab= "Years") # Y-axis label
plot(rio_csv$age,
     col="red",
     pch= 19, # Solid circles for points
     main= "Age distribution",
     xlab= "Index",
     ylab= "Ages")
# scatter plot of Numeric variables


hist(rio_csv$status,
     main = "Life status count",
     col = "blue",
     xlab="")
hist(rio_csv$ulcer,
     main = "Count of Ulceration",
     col = "red",
     xlab="")
hist(rio_csv$sex,
     main = "Gender count",
     col = "green",
     xlab="")
# Histogram for categorical variables

hist(rio_csv$sex [rio_csv$ulcer == 1],
     xlim= c(0,3),
     breaks= 3,
     main = paste("Presence of ulcer in men and women",
                  "0=female 1=male"),
     xlab = "",
     col = "blue")

par(mfrow = c(2,1)) # combining two histograms in one plot
hist(rio_csv$status [rio_csv$sex == 1],
     xlim= c(0,3),
     breaks= 3,
     main = "Life status of Men",
     xlab = "",
     col = "red")

hist(rio_csv$status [rio_csv$sex == 0],
     xlim= c(0,3),
     breaks= 3,
     main = "Life status of Women",
     xlab = "",
     col = "blue")

par(mfrow= c(1,1)) # Return to normal numbers of rows and column for histogram

par(mfrow = c(2,1)) # combining two histograms in one plot
hist(rio_csv$status [rio_csv$ulcer == 1],
     xlim= c(0,3),
     breaks= 3,
     main = "Presence of ulcer VS life status",
     xlab = "",
     col = "red")


hist(rio_csv$status [rio_csv$ulcer == 0],
     xlim= c(0,3),
     breaks= 3,
     main = "Absence of ulcer vs life status",
     xlab = "",
     col = "blue")

par(mfrow= c(1,1))

# Preparing for regression.
# Good to first check univariarte distribution.
par(mfrow= c(3,1)) # 3 rows of histogram

hist(rio_csv$time,
     main = "Time",
     xlab="")

hist(rio_csv$thickness,
     main = "Thickness",
     xlab="")

hist(rio_csv$age,
     main = "Age",
     xlab="")

par(mfrow= c(1,1))

plot (rio_csv$thickness,rio_csv$time, # scatter plot between x and y variable
      pch = 19,  # Soild circle
      cex = 1, # make 150% size
      col = "#cc0000", # Red
      main = "Correlation",
      xlab= "Thickness",
      ylab = "Time")

plot (rio_csv$age,rio_csv$time, # scatter plot between x and y variable
      pch = 19,  # Soild circle
      cex = 1, # make 150% size
      col = "blue",
      main = "Correlation",
      xlab= "Age",
      ylab = "Time")

plot (rio_csv$age, rio_csv$thickness, # scatter plot between x and y variable
      pch = 19,  # Soild circle
      cex = 1, # make 150% size
      col = "green",
      main = "Correlation",
      xlab= "Age",
      ylab = "Thickness")


par(mfrow= c(1,1))


# Regression
data <- rio_csv

# Define variable groups
x <-data[,7]
y <-data[,2]

reg1 <- lm(time ~ thickness,
           data = rio_csv)

reg1
summary(reg1)
confint(reg1)
coef(reg1)
library(stats)
cor(x,y)
hist(residuals(reg1))

predictions <- predict(reg1, data)


# visualize the model
ggplot(data, aes(x = thickness, y = time)) +
  geom_point() +
  geom_line(aes(y = predict(reg1)), color = "red")



# Second variables regression
x <-data[,5]
y <-data[,2]

reg2 <- lm(time ~ age,
           data = rio_csv)

reg2
summary(reg2)
confint(reg2)
coef(reg2)
library(stats)
cor(x,y)
hist(residuals(reg2))

predictions <- predict(reg2, data)


# visualize the model
ggplot(data, aes(x = age, y = time)) +
  geom_point() +
  geom_line(aes(y = predict(reg2)), color = "blue")


# Third variables regression
x <-data[,5]
y <-data[,7]

reg3 <- lm(thickness ~ age,
           data = rio_csv)

reg3
summary(reg3)
confint(reg3)
coef(reg3)
library(stats)
cor(x,y)
hist(residuals(reg3))

predictions <- predict(reg3, data)


# visualize the model
ggplot(data, aes(x = age, y = thickness)) +
  geom_point() +
  geom_line(aes(y = predict(reg3)), color = "green")


# t-test and boxplot
ggplot(rio_csv, aes(x = sex, y =age)) +
  geom_boxplot(aes(group = sex)) + theme_classic()
t.test(age ~ sex, data = rio_csv)


ggplot(rio_csv, aes(x = sex, y =thickness)) +
  geom_boxplot(aes(group = sex)) + theme_classic()
t.test(thickness ~ sex, data = rio_csv)

ggplot(rio_csv, aes(x = sex, y =time)) +
  geom_boxplot(aes(group = sex)) + theme_classic()
t.test(time ~ sex, data = rio_csv)

#QQplots
ggplot(data = rio_csv, aes(sample = time)) +
  geom_qq() +
  facet_wrap(~ sex) +
  labs(title = "QQplot of time grouped by sex",
       x = "sex",
       y = "time")

ggplot(data = rio_csv, aes(sample = thickness)) +
  geom_qq() +
  facet_wrap(~ sex) +
  labs(title = "QQplot of thickness grouped by sex",
       x = "sex",
       y = "thickness")

ggplot(data = rio_csv, aes(sample = age)) +
  geom_qq() +
  facet_wrap(~ sex) +
  labs(title = "QQplot of age grouped by sex",
       x = "sex",
       y = "age")

