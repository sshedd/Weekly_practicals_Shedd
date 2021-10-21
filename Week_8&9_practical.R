library(tidyverse)

#I am submitting for Katherine Grellman and Corey Nevels. We worked on this code together. 

##Q1. Load the data into a variable called survey_data

##The survey_data should be a tibble
##The files in the file are delimited. You can change read_csv's behavior to split on tabs, 
  ##rather that on comma (","), which is the default behavior
setwd("C:\\Users\\Sam\\Desktop\\Data science in R\\Weekly practicals_Shedd")
getwd()
survey_data <- read.csv("RIKZ.txt", sep="\t", header=TRUE)

##Q2. display the first 6 lines of the tables
head(survey_data)

##Q3 The columns C1 P1-P25, N1, CR1-28, M1-17 and I1-5 of the table represent the 
  #counts for 75 species grouped within five taxa: Chaetognatha (C), Polychaeta (P), 
  #Crustacea (CR), Mollusca (M), and Insecta (I). We're only interested in the richness, and we need to compute it as:
##1 if the group has a value > 0
##0 otherwise.
#Create a new column, call it richness, which represents the richness in each sample. 
  #The richness of sample 1 should be 11, since sample has non-null values only for the following groups:
#'C1''P6''P15''P16''P25''CR1''CR14''CR15''CR19''CR26''I3'

survey_data$richness = rowSums(survey_data[,2:76]>0)

##Q4 Create a copy of the variable survey_data that does not have columns C1 P1-P25, 
  #N1, CR1-28, M1-17 and I1-5. 
##Call this variable survey_data_richness

survey_data_richness <- survey_data[,77:90]

##Q6. Use the lm function to model the richness as a function of the remaining variables 
##but not including the variable week, which needs a special treatment we haven't covered yet!

lm_richness <- lm(richness~., data = survey_data_richness[,2:14])

summary(lm(richness~angle1, data = survey_data_richness))
summary(lm(richness~angle2, data = survey_data_richness))
summary(lm(richness~exposure, data = survey_data_richness))
summary(lm(richness~salinity, data = survey_data_richness))
summary(lm(richness~temperature, data = survey_data_richness))
summary(lm(richness~NAP, data = survey_data_richness))
summary(lm(richness~penetrability, data = survey_data_richness))
summary(lm(richness~grainsize, data = survey_data_richness))
summary(lm(richness~humus, data = survey_data_richness))
summary(lm(richness~chalk, data = survey_data_richness))
summary(lm(richness~sorting1, data = survey_data_richness))

ggplot(survey_data_richness) +
  geom_point(aes(x=angle1, y=richness)) +
  geom_abline(intercept = lm_richness$coefficients[1], slope = lm_richness$coefficients[2], color = 2, size=2, alpha=0.5)

ggplot(survey_data_richness) + 
  geom_point(aes(x=angle2, y=richness)) + 
  geom_abline(intercept = lm_richness$coefficients[1], slope = lm_richness$coefficients[2], color = 2, size=2, alpha=0.5)

ggplot(survey_data_richness) + 
  geom_point(aes(x=exposure, y=richness)) + 
  geom_abline(intercept = lm_richness$coefficients[1], slope = lm_richness$coefficients[2], color = 2, size=2, alpha=0.5)

ggplot(survey_data_richness) + 
  geom_point(aes(x=salinity, y=richness)) + 
  geom_abline(intercept = lm_richness$coefficients[1], slope = lm_richness$coefficients[2], color = 2, size=2, alpha=0.5)

ggplot(survey_data_richness) + 
  geom_point(aes(x=temperature, y=richness)) + 
  geom_abline(intercept = lm_richness$coefficients[1], slope = lm_richness$coefficients[2], color = 2, size=2, alpha=0.5)

ggplot(survey_data_richness) + 
  geom_point(aes(x=NAP, y=richness)) + 
  geom_abline(intercept = lm_richness$coefficients[1], slope = lm_richness$coefficients[2], color = 2, size=2, alpha=0.5)

ggplot(survey_data_richness) + 
  geom_point(aes(x=penetrability, y=richness)) + 
  geom_abline(intercept = lm_richness$coefficients[1], slope = lm_richness$coefficients[2], color = 2, size=2, alpha=0.5)

ggplot(survey_data_richness) + 
  geom_point(aes(x=grainsize, y=richness)) + 
  geom_abline(intercept = lm_richness$coefficients[1], slope = lm_richness$coefficients[2], color = 2, size=2, alpha=0.5)

ggplot(survey_data_richness) + 
  geom_point(aes(x=humus, y=richness)) + 
  geom_abline(intercept = lm_richness$coefficients[1], slope = lm_richness$coefficients[2], color = 2, size=2, alpha=0.5)

ggplot(survey_data_richness) + 
  geom_point(aes(x=chalk, y=richness)) + 
  geom_abline(intercept = lm_richness$coefficients[1], slope = lm_richness$coefficients[2], color = 2, size=2, alpha=0.5)

ggplot(survey_data_richness) + 
  geom_point(aes(x=sorting1, y=richness)) + 
  geom_abline(intercept = lm_richness$coefficients[1], slope = lm_richness$coefficients[2], color = 2, size=2, alpha=0.5)

ggplot(survey_data_richness) + 
  geom_point(aes(x=Beach, y=richness)) + 
  geom_abline(intercept = lm_richness$coefficients[1], slope = lm_richness$coefficients[2], color = 2, size=2, alpha=0.5)

##Q7. What do the various output of the lm mean? Interpret the results of your model
  
  #Only NAP and richness have a significant linear relationship 

lm_richness_NAP <- lm(richness~I(NAP), data = survey_data_richness)
summary(lm_richness_NAP)
ggplot(survey_data_richness) +
  geom_point(aes(x=NAP, y=richness)) + 
  geom_abline(intercept = lm_richness_NAP$coefficients[1], slope = lm_richness_NAP$coefficients[2], color = 2, size=2)

sum(residuals(lm_richness_NAP))

ggplot()+
  geom_density(aes(x=residuals(lm_richness_NAP)), bw=2)+
  xlim(-5,5)

# Q8. Build a model that includes all the parameters and assess the fit of the data
lm_richness <- lm(richness~., data = survey_data_richness[,2:14])
summary(lm_richness)

# Q9. Use an appropriate method that only selects a subset of the data. 
    #What do you conclude? Justify your answer.
survey_data_no_outliers <- survey_data_richness[-c(9,22),]
lm_richness_NAP <- lm(richness~I(NAP), data = survey_data_richness[-c(9,22),])
summary(lm_richness_NAP)
ggplot(survey_data_no_outliers) +
  geom_point(aes(x=NAP, y=richness)) + 
  geom_abline(intercept = lm_richness_NAP$coefficients[1], slope = lm_richness_NAP$coefficients[2], color = 2, size=2)

#The relationship is still not strong. It is statistically significant as the p value is very small. 
  #However, the R squared is still only 41%. I do not know what NAP is, so I am not sure of the relationship to species richness.
