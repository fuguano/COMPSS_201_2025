# In Class Week 8
rm(list = ls()) # clear memory
set.seed(22092008)
library(pacman)
p_load(ggplot2,dplyr,MASS, car, tidyr)

# First exercise- Multicollinearity

# In this exercise we will generate some data and amp up the 
# multicollinearity between right hand side variables,
# without changing the residual or the model! 
# We are interested in studying our standard errors. 
# Max updated this and made it fancy. 
# And most importantly it works now. 

# Step 1: Let's generate some data for a MRM with four right hand side variables. 
# Choose a sample size
n = 100
# number of loops
numloop <- 10000

# Object to hold coefficient estimates
b <- matrix(NA, nrow = numloop, ncol = 4) # vector to hold sample mean for each iteration
v <- matrix(NA, nrow = numloop, ncol = 4) # vector to hold sample mean for each iteration


# Make some mean for my rhs variables (mean zero, for no real reason)
mu <- matrix(c(0, 0, 0, 0, 0),ncol=1)
# correlation values between x1 and x2 ranging from 0 and 0.99
mcvalues <- c(0,0.5,0.9, 0.99)
# Loop over 4 values for the correlation between x1 and x2
for(mc in 1:4) {
  for(i in 1:numloop) {

# Make the correlation matrix
sigma = matrix(c(
  1,mcvalues[mc],0,0,0,
  mcvalues[mc],1,0,0,0,
  0,0,1,0,0,
  0,0,0,1,0,
  0,0,0,0,1),nrow=5)


# Create some data
x = as.data.frame(mvrnorm(n,mu, sigma))
# Ues these our lines are ugly. 
names(x)[1] <- "x1"
names(x)[2] <- "x2"
names(x)[3] <- "x3"
names(x)[4] <- "x4"
names(x)[5] <- "err"
# Generate some true outcomes
x$y = 1 + 1*x$x1 - 1* x$x2 + 1 *x$x3  -1 *x$x4 + x$err

# Run a regression of y on the xs. 
mod_1 = lm(y ~ 1+ x1 + x2 + x3 + x4, data=x)
b[i,mc] <-summary(mod_1)$coefficients[2]
# Calculate The VIFs for your model. What do you see?
vif_values <- vif(mod_1)
v[i,mc] <-vif_values[1]
  }

}

df <- as.data.frame(b)
df_long <- pivot_longer(df, everything(), names_to = "column", values_to = "value")
coefs_plot <- ggplot(df_long, aes(x = value, color=value, fill = column)) +
  scale_color_manual(name = "Correlation (ρ)", values=c("#002676", "#FC9313","#00553A", "#770747"), labels = c("ρ = 0", "ρ = 0.5", "ρ = 0.9", "ρ = 0.99")) +
  scale_fill_manual(name = "Correlation (ρ)", values=c("#002676", "#FC9313","#00553A", "#770747"), labels = c("ρ = 0", "ρ = 0.5", "ρ = 0.9", "ρ = 0.99")) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 100, position = "identity", alpha = 0.35) +
  labs(title = "Coefficient Distribution (x1)", x = "Coefficient Values", y = "Density") +
  theme_minimal()
print(coefs_plot)

df2 <- as.data.frame(v)
df_long2 <- pivot_longer(df2, everything(), names_to = "column", values_to = "value")
vifs_plot <- ggplot(df_long2, aes(x = value, color=value, fill = column)) +
  scale_color_manual(name = "Correlation (ρ)", values=c("#002676", "#FC9313","#00553A", "#770747"), labels = c("ρ = 0", "ρ = 0.5", "ρ = 0.9", "ρ = 0.99")) +
  scale_fill_manual(name = "Correlation (ρ)", values=c("#002676", "#FC9313","#00553A", "#770747"), labels = c("ρ = 0", "ρ = 0.5", "ρ = 0.9", "ρ = 0.99")) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 100, position = "identity", alpha = 0.35) +
  labs(title = "VIF Distribution (x1)", x = "VIF Values", y = "Density") +
  theme_minimal()
print(vifs_plot)

# Cool right! You see in graph 1 that the sampling distribution of the x1 coefficient really gets much wider. 
# In the second plot you see how the VIFs blow up. 


#### Second Exercise: Endogeneity
# We can use the same setup as before to demonstrate that OVB is an issue.
# Here will use a monte carlo type setup though
rm(list = ls()) # clear memory
n =100
numloop = 1000
b <- matrix(NA, nrow = numloop, ncol = 4) # vector to hold coefficient estiamte on x1 for each iteration
# Make some means.
mu <- matrix(c(0, 0, 0, 0, 0),ncol=1)

# Correlation values between x1 and x4 (we are going to leave out x4 and see what happens to the coefficient on x1.)
mcvalues <- c(0,0.5,0.9, 0.99)
# Loop over 4 values for the correlation between x1 and x4
for(mc in 1:4) {
  for(i in 1:numloop) {
    
    # Make the correlation matrix
    sigma = matrix(c(
      1,0,0,mcvalues[mc],0,
      0,1,0,0,0,
      0,0,1,0,0,
      mcvalues[mc],0,0,1,0,
      0,0,0,0,1),nrow=5)
# Create some data
x = as.data.frame(mvrnorm(n,mu, sigma))
# Ues these our lines are ugly. 
names(x)[1] <- "x1"
names(x)[2] <- "x2"
names(x)[3] <- "x3"
names(x)[4] <- "x4"
names(x)[5] <- "err"
# Generate some true outcomes with no problems anywhere. 
x$y = 1 + 1*x$x1 + 1*x$x2 +  1*x$x3  + 1*x$x4 + x$err
# Run a regression of y on the xs. 
mod_1 = lm(y ~ 1+ x1 + x2 + x3, data=x)
b[i,mc] <-summary(mod_1)$coefficients[2]
  }
}
  
df <- as.data.frame(b)
  df_long <- pivot_longer(df, everything(), names_to = "column", values_to = "value")
  coefs_plot <- ggplot(df_long, aes(x = value, color=value, fill = column)) +
    scale_color_manual(name = "Correlation (ρ)", values=c("#002676", "#FC9313","#00553A", "#770747"), labels = c("ρ = 0", "ρ = 0.5", "ρ = 0.9", "ρ = 0.99")) +
    scale_fill_manual(name = "Correlation (ρ)", values=c("#002676", "#FC9313","#00553A", "#770747"), labels = c("ρ = 0", "ρ = 0.5", "ρ = 0.9", "ρ = 0.99")) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = 100, position = "identity", alpha = 0.35) +
    labs(title = "Coefficient Distribution (x1)", x = "Coefficient Values", y = "Density") +
    theme_minimal()
  print(coefs_plot)
  
  
# Do you believe me now????? Sorry about the screwup in class. 
