##Submitting with Corey Nevels

install.packages("geometry")
install.packages("moderndive")
install.packages("factoextra")
library(tidyverse)
library(moderndive)
library(geometry)
library(factoextra)

##Here you'll generate your own data to make sure you understand what PCA is doing
  #Generate 4 variables W, X, Y, and Z

#Q1. X and Y should not be correlated
  #They are independent
set.seed(44)
x = rnorm(30, 5, 2)
y = rnorm(30, 1, 2)

#Q2. W and X should have a mild correlation ( < 0.5)
w = 0.45 * x + rnorm(30, 0, 2)

#Q3. Y and Z should have a mild correlation ( > 0.9)
z = 1.05 * y + rnorm(30, 0, 0.2)

#Q4. Generate a variable outcome as a linear combination of W, X, Y, and Z
df_wxyz = data.frame(x, y, z, w)
head(df_wxyz)
#Q5. Model your outcome using W, X, Y, and Z.
  #Do your results match your model params?
lm_xy = lm(x~y, df_wxyz)
lm_yz = lm(y~z, df_wxyz)
lm_xw = lm(x~w, df_wxyz)
summary(lm_yz)
summary(lm_xw)

ggplot(df_wxyz, aes(x, y)) + 
  geom_point() + 
  geom_parallel_slopes(se=FALSE)
ggplot(df_wxyz, aes(x, w)) + 
  geom_point() + 
  geom_parallel_slopes(se=FALSE)
ggplot(df_wxyz, aes(y, z)) + 
  geom_point() + 
  geom_parallel_slopes(se=FALSE)

#Q6. Use PCA to reduce the dimensionality of your dataset
  #Can you explain why you don't need to include the outcome?
pca_wxyz = prcomp(df_wxyz, scale=TRUE)
str(pca_wxyz)
pca_wxyz$sdev^2 / sum(pca_wxyz$sdev^2)
  #####Because we made the data ourselves, so it is mathematically impossible 
  #####for the data to not follow the specified parameters (unless we mess up the coding of it)

#Q7. Use the bi-plot to visualize the contributions of your initial variables
fviz_pca_biplot(pca_wxyz)

#Q8.How efficient is the new lower-dimensional space representation at predicting the outcome?
  #Do your results match your model params?

  ####The Biplot shows that x and w and less correlated than the y and z. 

