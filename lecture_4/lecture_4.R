# Lecture 4
# Let's do some testing of differences in means!
# and then some regression stuff.
# The repo also contains a knitr version of this file, which makes nice pdf files.....

rm(list = ls())
set.seed(22092008)
library(pacman)

# ggplot makes pretty graphs. dplyr is awesome. More later. 
p_load(ggplot2, dplyr, crosstable, flextable, plyr, MASS,stargazer)

# The goal is to generate some data from the relationship:
# Health <- shifter + b1*Gend + b2*Income + b3* Ins +  rnorm(n,mean=0,sd=1) 
# We have to generate data for the variables (Gend, Income, Ins), a residual, and pick values
# for the coefficients. Let's assume a sample size of 1000. 


n <- 1000 # Sample Size
mu <- c(0, 0,0) # (means of my rhs variables)

# Let's generate some data as if health insurance were randomized. 
a <- 0.5 # Set to 0.5 as default
b <- 0.0 # Set to 0.1 as default
c <- 0.0 # Set to 0.8 as default

# Let's generate some data as if health insurance were not randomized. 
# a <- 0.5 #Gend Income Covariance
# b <- 0.1 #Gend Insurance Covariance
# c <- 0.8 #Income Insurance


# Some betas (slope coefficients that only god sees) for later. She knows all!
b1 <-1 # Gend Beta
b2 <-5 # Income Beta
b3 <-3 #Insurance
shifter <- 30

# Now generate the data, given a cetain covariance matrix. 
Sigma <- matrix(c(1, a, b, a, 1, c,b, c, 1), nrow=3)
data = mvrnorm(n, mu, Sigma, empirical=FALSE)

# Being lazy and generating some data columns from above. 
Gender = data[, 1]  # standard normal (mu=0, sd=1)
Income = data[, 2]  # standard normal (mu=0, sd=1)
Insurance= data[, 3]  # standard normal (mu=0, sd=1)

# Gender and income should be binary
Ins <- Insurance>0
Gend <- Gender>0
cor(Ins,Gend) 
cor(Ins,Income) 
cor(Gend,Income)

# We are going to generate some arbitrary Health Index
Health <- shifter +  rnorm(n,mean=0,sd=1) + b1*Gend + b2*Income + b3* Ins

# Let's do a manual comparison of Health Across the Insured and Uninsured.
mydata <- data.frame(Income, Gend, Health, Ins)
# Calculate Means by Group (using ddply). 
mu <- ddply(mydata, "Ins", summarise, grp.mean=mean(Health))

# Plot my health outcome by Insurance Status in Pretty Graph
ggplot(mydata, aes(x=Health, color=Ins, fill=Ins)) +
  scale_color_manual(values=c("#002676", "#FC9313")) +
  scale_fill_manual(values=c("#002676", "#FC9313")) +
  geom_histogram(alpha=0.1, position="identity", bins=50)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Ins),
             linetype="dashed") +
  labs(title="Health Outcomes by Insurance Status",x="Health Index", y = "Count")+
  theme_classic()

# Let's compare across treatment
# First - do this by hand. Difference in means. Unknown and unequal variances.

M1 <- mean(mydata[Ins == 'TRUE', 'Health']) 
M2 <- mean(mydata[Ins == 'FALSE', 'Health'])
n1 <- sum(Ins)
n2 <- n-n1
V1 <- var(mydata[Ins == 'TRUE', 'Health']) 
V2 <- var(mydata[Ins == 'FALSE', 'Health']) 
S <- sqrt((V1 / n1) + (V2 / n2))
statistic <- (M1 - M2 - 0) / S
print(statistic)

t_health <- t.test(Health ~ Ins)
print(t_health)
t_inc <- t.test(Income ~ Ins)
print(t_inc)
my_test_args=crosstable_test_args(show_method=FALSE)
ft1 <- crosstable(mydata,by="Ins", test=TRUE, funs=c(mean=mean),test_args=my_test_args) %>% 
  as_flextable()
print (ft1)


# Now let's turn to some simple regression analysis. 
# It is so simple, my teenager can do it. 
# In fact, I checked and he can. 
#That said, interpreting what it tells you is going to be the art form. 

setwd("/Users/auffhammer/Library/CloudStorage/Dropbox/06_Teaching/MACSS/2025/COMPSS_201_2025/lecture_4")
airfares <- read.csv("airfares.csv")
#Let's plot some data. 
ggplot(airfares, aes(x=dist, y=fare)) + 
  geom_point(alpha=0.5, shape=16, fill="#002676", color="#002676", size=2)+
  geom_smooth(method=lm, color="#FC9313")+
  labs(title="Airfare by Distance",
       x="Distance (Miles)", y = "Fare (US$)")+
  theme_classic()  


#Let's run a regression.  
planes <- lm(airfares$fare ~ airfares$dist)
# Let's record some residuals and join them to our data frame. 
airfares$res <- planes$resid

#Let's make some nice looking regression output. 

stargazer(planes, type='text', digits = 3, title = 'Linear Airfare Distance Regression', style = 'qje')

# Plot Residuals - Playing with colors (HEX Colors - official Cal!
# Also meesing with background and Axis Labels. )
ggplot(airfares, aes(x=dist, y=res)) + 
  geom_point(alpha=0.5, shape=16, fill="#002676", color="#002676", size=2)+
  geom_smooth(method=lm, se=FALSE, color="#FC9313")+
  labs(title="Airfare by Distance",
       x="Distance (Miles)", y = "Residuals")+
  theme_classic()  

# Another one. With Avocados. Which are a fruit. 

setwd("/Users/auffhammer/Library/CloudStorage/Dropbox/06_Teaching/MACSS/2025/COMPSS_201_2025/lecture_4")

avocado <- read.csv("avocado.csv")

#Let's plot some data. I am fitting a smoother (loess) to the data to see what the functional # form looks like. A bit more on this later. 

ggplot(avocado, aes(x=price_reg, y=quantity_reg)) + 
  geom_point(alpha=0.5, shape=16, fill="#002676", color="#002676", size=2)+
  geom_smooth(method=loess, color="#FC9313")+
  labs(title="Avocado Demand (Conventional)",
       x="Quantity (some units)", y = "Price (US$ per some unit)")+
  theme_classic()  

# Now run a regression of the linear model No transformation. 
avo_lin <- lm(avocado$quantity_reg ~ avocado$price_reg)

# Let's record some residuals and join them to our data frame. 
avocado$res <- avo_lin$resid

#Let's make some nice looking regression output. 
stargazer(avo_lin, type='text', digits = 3, title = 'Linear Price Regression', style = 'qje')

# Plot Residuals - same pretty graph as before. 
ggplot(avocado, aes(x=price_reg, y=res)) + 
  geom_point(alpha=0.5, shape=16, fill="#002676", color="#002676", size=2)+
  geom_smooth(method=lm, se=FALSE, color="#FC9313")+
  labs(title="Residual Plot - Avocado Demand",
       x="Price (US$ per something)", y = "Residuals")+
  theme_classic()  

# Transform our variables using natural logs. 
avocado$l_price <- log(avocado$price_reg)
avocado$l_q <- log(avocado$quantity_reg)

# Run the log log regression. 
avo_log <- lm(avocado$l_q ~ avocado$l_price)

# Let's record some residuals and join them to our data frame. 
avocado$resl <- avo_log$resid

# Plot Residuals - same pretty graph as before. 
ggplot(avocado, aes(x=l_price, y=resl)) + 
  geom_point(alpha=0.5, shape=16, fill="#002676", color="#002676", size=2)+
  geom_smooth(method=lm, se=FALSE, color="#FC9313")+
  labs(title="Residual Plot - Log-Log Avocado Demand Residuals",
       x="Log(Price) (US$ per something)", y = "Residuals")+
  theme_classic()  


