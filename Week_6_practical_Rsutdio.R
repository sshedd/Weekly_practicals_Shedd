library(tidyverse)
data_2018 = read_csv("C:\\Users\\Sam\\Desktop\\Data science in R\\Weekly practicals_Shedd\\survey_2018.csv")
data_2021 = read.csv("C:\\Users\\Sam\\Desktop\\Data science in R\\Weekly practicals_Shedd\\re_sample_2021.csv")
print(data_2018)

mean_coverage_2018 <- mean(data_2018$coverage)
sd_coverage_2018 <- sd(data_2018$coverage)
mean_coverage_2021 <- mean(data_2021$coverage)
sd_coverage_2021 <- sd(data_2021$coverage)

n = 10000

set.seed(42)
samples_means_18 = n %>%
  replicate(sample(data_2018$coverage, 20)) %>%
  apply(2, mean)

ggplot() + 
  geom_histogram(aes(x=samples_means_18, y=..density..), bins = 50, alpha=0.1, color="black", size=0.05) +
  geom_density(aes(x=samples_means_18), color="black", size=1) +
  geom_vline(aes(xintercept=mean_coverage_2021), color="red") 

sum(samples_means_18 >=mean_coverage_2021)/100000

##0.0076 is less than 0.05 so the null hypothesis can be rejected. However, the species increased coverage, not declined. 
