# In Class Week 9
# This is (drumroll) yet another Monte Carlo, where this is run many times and
# then shows you the sampling distribution of the point estimates 2SLS gives you.
# This is a bit lengthy, so bear with me. #gobears

# Clear memory
rm(list = ls()) # clear memory

library(pacman)
p_load(ggplot2,MASS, AER, tidyr)
# AER is the standard IV package people use. 

# Sample size & number of loops & placeholders
N  <- 1000
numloop <- 1000

b <- matrix(NA, nrow = numloop, ncol = 3) # vector to hold y2 coefficient for each iteration and estimator
se <- matrix(NA, nrow = numloop, ncol = 3) # vector to hold standard error for y2 coefficient for each iteration and estimator


# In this exercise we will generate some data for an endogenous variable and generate 
# Some valid instruments. You can play with this setup to study a number of things.

# Step 1: Let's generate some data for an IV setup that has 
# one problem rhs variable (y2). In order to make this a problem right hand side variable, we generate a variable that is correlated with 
# y2 and omitted from the regression. We call this one y3. This induces the correlation between the error and y2, which violated the 
# exogeneity assumption. 
# For fun, we also include two exogenous variables (x1 and x2), 
# two instruments and a well behaved error terms

# Make some mean for my variables. I choose zero to make it simple. 
mu <- matrix(c(0,0,0,0,0,0,0),ncol=1)

# Make the correlation matrix
# There will be seven variable generated. 
# y2 and y3 (correlated) - I choose 0.7 for no reason other than I like the number 7. 
# x1 and x2 (uncorrelated - but that does not really matter, unless they are too correlated)
# z1 and z2 (these will be my "valid" instrumental variables. They have to be correlated with y2 and uncorrelated with the disturbance). I choose 0.5. No real reason. 
# err is out random "structural" disturbance. 

# So same (but bigger than last time.)

sigma = matrix(c(
  1,0.7,0,0,0.5,0.5,0,
  0.7,1,0,0,0,0,0,
  0,0,1,0,0,0,0,
  0,0,0,1,0,0,0,
  0.5,0,0,0,1,0,0,
  0.5,0,0,0,0,1,0,
  0,0,0,0,0,0,1),nrow=7)


# Now start the Monte Carlo (do this numloop times. You choose numloop. More takes longer.)
for(i in 1:numloop) {
# Create some data
x = as.data.frame(mvrnorm(n=N,mu, sigma))
# Name some things
names(x)[1] <- "y2"
names(x)[2] <- "y3"
names(x)[3] <- "x1"
names(x)[4] <- "x2"
names(x)[5] <- "z1"
names(x)[6] <- "z2"
names(x)[7] <- "err"
# # Generate some true outcomes (I choose alternating ones and -1 ones for my coefficients. Easier to see what is happening when things break down.)
x$y1 = 1 - 1*x$y2 + 1*x$y3 -1 *x$x1 + 1*x$x2 + x$err

# Run an OLS regression of y1 on  y2 and the x1 and x2 (leave out y3). This should be biased. Which way? 
# Positive! The coefficient on y3 is positive as is the correlation between y2 and y3. OVB formula strikes again!

mod_1 = lm(y1 ~ 1 + y2 + x1 + x2, data=x)
# record the coefficient on y2 for each loop. 
b[i,1] <- mod_1$coefficients[2]
out1 <-summary(mod_1)
# I want to show you the behavior of the standard errors later. So let's record them. 
se[i,1]  <- out1$coefficients[2,2]

# Just for fun. 
# IV by hand, with just z1 as a single instrument. Run a regression of y2 on z1 and the xs. save the predicted values. 
stage_1 <- lm(y2 ~ 1 + x1 + x2 + z1, data = x)
x$y2_hat <- stage_1$fitted.values
# Now run a regression of y1 on the predicted values you just calculated x$y_hat and x1 and x2. 
# What do you see?
stage_2 <- lm(y1 ~ 1+ y2_hat + x1 + x2, data=x)
b[i,2] <- stage_2$coefficients[2]
mod_2 <-summary(stage_2)

# I want to show you the behavior of the standard errors later. So let's record them. 
se[i,2]  <- mod_2$coefficients[2,2]



# Now use the canned command.
mod_3 <- iv_robust(y1  ~ y2 + x1 + x2| x1 + x2 + z1, data = x, se_type ="classical")
b[i,3] <- mod_3$coefficients[2]
se[i,3] <- mod_3$std.error[2]
}

df <- as.data.frame(b)
df_long <- pivot_longer(df, everything(), names_to = "column", values_to = "value")
coefs_plot <- ggplot(df_long, aes(x = value, color=value, fill = column)) +
  scale_color_manual(name = "Correlation (ρ)", values=c("#002676", "#FC9313","#00553A"), labels = c("OLS", "IV by Hand", "2SLS")) +
  scale_fill_manual(name = "Correlation (ρ)", values=c("#002676", "#FC9313","#00553A"), labels = c("OLS", "IV by Hand", "2SLS")) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 100, position = "identity", alpha = 0.35) +
  labs(title = "Coefficient Distribution (y1)", x = "Coefficient Values", y = "Density") +
  theme_minimal()
print(coefs_plot)

df2 <- as.data.frame(se)
df_long2 <- pivot_longer(df2, everything(), names_to = "column", values_to = "value")
ses_plot <- ggplot(df_long2, aes(x = value, color=value, fill = column)) +
  scale_color_manual(name = "Correlation (ρ)", values=c("#002676", "#FC9313","#00553A"), labels = c("OLS", "IV by Hand", "2SLS")) +
  scale_fill_manual(name = "Correlation (ρ)", values=c("#002676", "#FC9313","#00553A"), labels = c("OLS", "IV by Hand", "2SLS")) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 100, position = "identity", alpha = 0.35) +
  labs(title = "Standard Error Distribution (y1)", x = "SE Values", y = "Density") +
  theme_minimal()
print(ses_plot)


# Make object of coefficients
g1 <-data.frame(g1)
names(g1)[1] <- "Coefficient"
g2 <-data.frame(g2)
names(g2)[1] <- "Coefficient"
g1$model <- 'ols'
g2$model <- 'iv'
coefficients <- rbind(g1, g2)

# Dashed line will show you the true value of the sampling distribution. 
ggplot(coefficients, aes(x=Coefficient, color=model, fill=model)) +
  geom_vline(xintercept=-0.8, linetype="dashed") +
  geom_histogram(alpha=0.3, position="identity", bins=50) +
  labs(title="IV vs. OLS",x="Coefficients", y = "Count") +
  theme_classic(base_size = 15)




