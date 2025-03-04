rm(list = ls ()) #all variables stored previously
library(Hmisc) 
data <- read.csv("C:/Users/mulandi/Downloads/COVID19_line_list_data.csv")
describe(data)

data$death_clean <- as.integer(data$death != 0)

#death rate
sum(data$death_clean) / nrow(data)

#mean deathrate
dead = subset(data, death_clean == 1)
alive = subset(data, death_clean == 0)
mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm=TRUE)

