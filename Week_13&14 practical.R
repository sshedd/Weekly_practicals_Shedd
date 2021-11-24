#Worked with Kat Grellman and Corey Nevels
#My computer won't accept the years() function, so cannot get counts in question 5. But it ran for Kat and Corey
install.packages("lubridate")
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(dplyr)

###Question #1
#Read in the data as a tibble
setwd('\\Users\\Sam\\Desktop\\Data science in R\\MBIO612-F2021\\week_13_14\\data')
data = tibble(read.csv("YERockfish.csv"))
View(data)

#Question #2
##Use the function as.POSIXct() to convert the date. The approach is exactly the same as converting to factor (as.factor()) or integer (as.integer())
data_1 = data %>%
  mutate(date = as.POSIXct(date, format = "%m/%d/%Y"))

#Question 3
#Use the function head() on your modified tibble to make sure the column is now of type dttm (date and time) instead of char
head(data_1)

#Question 4
#Count the number of lines in your file
nrow(data_1) 

#Question 5
#Plot the count of observations per year.
year(data_1)
count = data_1 %>%
  group_by(year = year(data_1$date)) %>%
  summarise(observations = n())
count
ggplot(count) +
  geom_point(mapping = aes(year, observations))

#Question 6
# remove year(s) for which there are less than 5 entries
filter_counts = counts %>%
  filter(observations>5)
filter = filter(observations>5)

#Question 7
#Count the number of entries and make sure there are less observations
sum(filter_counts$observations)
# 154<158

#Question 8 
#Model the fish maturity using the fish length
data2 = data1 %>%
  mutate(year=year(date)) %>%
  filter(year!=2004) %>%
  filter(year!=2008)
View(data2)
## removed entries for years 2004 and 2008

# prep data to model fish maturity using the fish length
## remove NA values
length_mat = data2 %>%
  select(length, maturity) %>%
  filter(!is.na(maturity))
View(length_mat)
# convert 'maturity' characters to integers; Immature=0, Mature=1
length_mat$maturity = ifelse(length_mat$maturity=="Immature",0,1)
length_mat$maturity = as.factor(length_mat$maturity)
num_maturity = as.numeric(levels(length_mat$maturity))[length_mat$maturity]

# predict maturity from length data
log_reg_model = glm(data=length_mat, maturity~length, family="binomial")
summary(log_reg_model)
# plot the data and show model fit
beta_0 = log_reg_model$coefficients[1]
beta_1 = log_reg_model$coefficients[2]
x_axis = seq(min(length_mat$length)-3, max(length_mat$length)+3, 0.05)
g_x = 1 / (1+exp(-(beta_0 + beta_1 * x_axis)))

#Question 9 
#Generate a plot to show the data and the fit of the model (sigmoid)
ggplot()+ 
  geom_point(aes(x=length_mat$length, y=num_maturity)) + 
  geom_line(aes(x_axis, g_x)) +
  xlab("Length") +
  ylab("Maturity") +
  theme(text = element_text(size = 14))
# based on visual approximation, length of picking a mature fish with 50% probability is ~38mm

#Question 10
# What is the length at which the probability of picking a mature fish is 0.5?
lrPerc = function(cf,p) (log(p/(1-p))-cf[[1]])/cf[[2]]
L50 = lrPerc(coef(log_reg_model), 0.5)
L50
# length = 38.76736cm

#Question 11
#Add an era column to your dataset such that
   #era has the value "pre_2000" if the year of the observation is pre 2002
   #era has the value "era2002 and after" otherwise
data3 = data2 %>%
  filter(!is.na(maturity)) %>%
  mutate(era = ifelse(data3$year<2002,"pre 2002", "2002 and after"))
view(data3)

#Question 12
  #Build a logistic regression for maturity as an outcome using era and length as predictive variables
  #Use an ANOVA to test whether maturity is a function of both length and era
# convert 'maturity' characters to integers; Immature=0, Mature=1
data3$maturity = ifelse(data3$maturity=="Immature",0,1)
data3$maturity = as.factor(data3$maturity)
num_data3 = as.numeric(levels(data3$maturity))[data3$maturity]
# convert 'era' characters to integers; pre 2002=0, post 2002=1
data3$era = ifelse(data3$era=="pre 2002",0,1)
data3$era = as.factor(data3$era)
num_data3 = as.numeric(levels(data3$era))[data3$era]

# build a logistic regression for maturity as an outcome using era and length as predictive variables
model_data = data3 %>%
  select(length, maturity, era)
head(model_data)
log_reg_model2 = glm(data=model_data, maturity~length*era, family="binomial")
summary(log_reg_model2)
anova(log_reg_model2)
# based on the ANOVA, 'era' does not appear to be a significant variable for predicting maturity


