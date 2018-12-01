######################################################
# code runs binomial logistic regression to calculate 
# home run probabilities based on launch angle and
# exit velocity of batted balls at Coors Fields
# in 2018
######################################################


library(dplyr)
library(ggplot2)
library(plotly)

#Read in data
data = read.csv(file="c:/Users/DrewP/Documents/MATH4020/Final Project/2018AllBattedBallsCoorsField.csv", sep = ',')
data = select(data, launch_speed, launch_angle, events)

#
t = function(x){
  if(x == 'home_run'){return(1)}
  else{return(0)}
}

HR = lapply(data$events, t)
data = data.frame(data, HR = unlist(HR))

#Logistic regression model with multiple regression to model exit velocity
#and launch angle where launch angle is modeled as a quadratic
model = glm(HR~launch_speed+poly(launch_angle, 2), data=data, family='binomial')
summary(model)

#Graph of batted balls with distinction for home run or out
ggplotly(ggplot(data)+
  geom_point(aes(x = launch_speed, y = launch_angle, shape=HR, color=HR))+
  scale_shape_identity())
