# My favorite sandwich is a BLT. 
# My favorite Theorem is the CLT
# This is R. You can get really lost here. 
# But Prof. Max loves it here. 

# Fall 2025 - Lecture 2 

rm(list = ls()) # clear memory

set.seed(22092008) # set random number generator seed (my kid's birthday)

# Let's assume the size of a population
N<-1000000 # population size (this is truth we will sample from)

# Let's assume a smaple size (We will mess with this number) 
n <- 2 # sample size (size of sample we will draw for calculation of sample mean)
numloop <- 10000 # number of draws (we are gods! We can do this as many times as we wish!)

# Placeholder object
g <- integer(numloop) # vector to hold sample mean for each iteration

# VERSION 1: For normally distributed data
sig2 <- 1 # variance for population
mu <- 0 # mean of population
r <- as.matrix(rnorm(N, mean=mu,sd=sqrt(sig2)))

# VERSION 2: For uniformly distributed data
# ub <- 10
# lb <- -10
# r <- as.matrix(runif(N, min = lb, max = ub))
# sig2 <- (lb-ub)^2/12 # variance for uniform (SCIENCE!)

# Plot population
hist(r,prob=TRUE,breaks = 50,main = "Raw data")

# Holy moly this an ugly plot. A few lines of code, and you can make this so pretty!
library(ggplot2)

df <- data.frame(x = r)

ggplot(df, aes(x = x)) +
  geom_histogram(
    aes(y = ..density..),
    bins = 50,
    fill = "#002676",
    color = "#FDB515",
    alpha = 0.8,
    linewidth = 0.3
  ) +
  labs(
    title = "Population Distribution",
    subtitle = "Uniform(-10, 10) — The Raw Material for the CLT",
    x = "Value",
    y = "Density"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#C09748"),
    axis.text = element_text(color = "#C09748"),
    plot.background = element_rect(fill = "white", color = NA)
  )



# Draw `numloop' samples of size n and calculate mean each time. 

for(i in 1:numloop) {
  tmp <-sample(r, n, replace = FALSE, prob = NULL)
  g[i] <- mean(tmp)
}

# Calculate minimum and maximum of sample means (cleaner plot)
a <-min(g)
b <-max(g)


# Make histogram of sample means with 50 bins
hist(g,prob=TRUE,breaks = 50,main = "Sampling Distribution of the Sample Mean")

# Overlay normal distribution with predicted mean and variance
curve(dnorm(x,mean=0,sd=sqrt(sig2/n)),a,b,add=TRUE,lwd=2,col="red")


# 
# ### Challenge: Can you break the CLT? Come up with a population where the sample mean does not have the predicted distribution?
# # A classic example is the Cauchy Distribution (fat tailed). 
# rm(list = ls()) # clear memory
# set.seed(123)
# 
# numloop <- 10000
# n <- 5
# 
# # Generate 10,000 sample means from Cauchy
# g <- replicate(numloop, mean(rcauchy(n)))
# 
# # Plot histogram but restrict x-axis to [-25, 25]
# g_trim <- g[abs(g) <= 7]
# hist(g_trim,  xlim = c(-7, 7), breaks=50,
#      main = "Sample mean of Cauchy(0,1)",
#      xlab = "Sample mean",
#      col = "lightblue", border = "white")
# 
# # Add a vertical line at 0 for reference
# abline(v = 0, col = "red", lwd = 2)
# 
#  # Another (more obvious) are small samples from a bimodal distribution (where the mean bounces around). 
# rm(list = ls()) # clear memory
# set.seed(123)
# mixpop <- c(rnorm(5e5, -5, 1), rnorm(5e5, 5, 1))
# n <- 2   # intentionally tiny
# g <- replicate(10000, mean(sample(mixpop, n)))
# hist(g, breaks=50, main="Sample mean with bimodal population, n=2")
