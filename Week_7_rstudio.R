library(tidyverse)

##Question 1
set.seed(110)
data_1 = tibble(measure=rnorm(5, 10, 1))
data_1$site_name <- 'Site 1'
data_2 = tibble(measure=rnorm(5, 10, 1))
data_2$site_name <- 'Site 2'
complete_data = rbind(data_1, data_2)
complete_data

mean(complete_data[complete_data$site_name == "Site 1", ]$measure)
mean(complete_data[complete_data$site_name == "Site 2", ]$measure)

tapply(complete_data$measure, complete_data$site_name, mean)

##Question 2
set.seed(110)
data_1 = tibble(measure=rnorm(40, 10, 1))
data_1$site_name <- 'Site 1'
data_2 = tibble(measure=rnorm(40, 10, 1))
data_2$site_name <- 'Site 2'
complete_data = rbind(data_1, data_2)

ggplot(complete_data, aes(x=measure, fill = site_name)) + 
  geom_density(alpha = 0.4, bw =0.60) + 
  xlim(5,18)

bootstrap_concat = function(data_1, data_2){
  
  concat_data = c(data_1, data_2)
  len_concat_data = length(concat_data)
  
  len_data_1 = length(data_1)    
  shuffled_data = sample(concat_data)
  new_data_1 = shuffled_data[1:len_data_1]
  new_data_2 = shuffled_data[(len_data_1+1):len_concat_data]
  mean(new_data_1) - mean(new_data_2)
}

bootstrap_concat(data_1$measure, data_2$measure)
mean_under_null = replicate(1000, bootstrap_concat(data_1$measure, data_2$measure))

observed_value = mean(data_1$measure) - mean(data_2$measure)
observed_value

ggplot()+
  geom_histogram(aes(mean_under_null, y=..density..), bins =30, alpha=0.2, color="black") + 
  geom_density(aes(mean_under_null), bw=0.05, size=0.5) + 
  xlim(-5, 5) + 
  geom_point(aes(observed_value, 0), color="red", size=5)

#P value 
sum(mean_under_null <= observed_value) / length(mean_under_null)

#Question 3
coral_sp = c("P. lobata","P. lobata", "M. capitata", "M. capitata", "P. mendrina","P. mendrina")
ratios =c(0.60, 0.61, 0.19, 0.19, 0.21, 0.20)
coral_proportions = tibble(coral_sp, ratios)

coral_multinom = function(){
  true_proportions = c(0.60,0.19,0.21)
  sample_proportions = rmultinom(1, 200, prob=c(0.61,0.19,0.20))/200
  sample_tvd = sum(abs(true_proportions- sample_proportions))/2
  sample_tvd
}
coral_multinom()

subtract_abs = function(x){
  abs(x[1] - x[2])
}

sum(tapply(coral_proportions$ratios, coral_proportions$coral_sp, subtract_abs))/2

observed_stat = sum(tapply(coral_proportions$ratios, coral_proportions$coral_sp, subtract_abs))/2
tvds = replicate(1000, coral_multinom())
p_value = sum(tvds > observed_stat)/length(tvds)
p_value

ggplot()+ 
  geom_histogram(aes(tvds, ..density..), bins = 15) +
  geom_point(aes(observed_stat, 0), size=5, color="red")
