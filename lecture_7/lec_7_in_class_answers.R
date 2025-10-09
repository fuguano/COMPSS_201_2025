# In Class Week 7
rm(list = ls()) # clear memory
# Put your own working Directory Here# if you are on jupyter, use this one: 
#Jupyter#  setwd("~/COMPSS_201_2025/lecture_6")
setwd("/Users/auffhammer/Library/CloudStorage/Dropbox/06_Teaching/MACSS/2024/code/public-repository-1/week_7")
library(pacman)
p_load(ggplot2,dplyr)

#1. Read in data "class_7.csv"
class_7 <-read.csv("class_7.csv")

#2. Regress births on income. 
mod_1 <- lm(births ~ income + time, data=class_7)
class_7$res <-mod_1$resid
summary(mod_1)

#3. Plot your residuals against income
ggplot(class_7,aes(x=income, y=res),) +
  geom_point(alpha=0.8, shape=16, fill="#002676", color="#002676", size=1)+
  geom_line(color="#FC9313", size=0.1)+
  labs(title="Disturbances against Income",
       x="Income", y = "Disturbance")+
  theme_classic(base_size = 12) 

# -> Not much to see here. At least not to me. 

#4. Plot your residuals against time
ggplot(class_7,aes(x=time, y=res),) +
  geom_point(alpha=0.8, shape=16, fill="#002676", color="#002676", size=1)+
  geom_line(color="#FC9313", size=0.1)+
  labs(title="Disturbances against Time",
       x="Time", y = "Disturbance")+
  theme_classic(base_size = 12) 

# Wowza! That surely looks correlated across time!

#5. Test for AR(1) errors by using the residuals from your regression (regress residual on its lag)
# Use dplyr to add some lags. You could do this in base r too. But this is fancier. 
class_7 <- class_7 %>%
  arrange(time) %>%                    # ensure time is sorted
  mutate(lag_res = lag(res, n = 1))  %>%                   
  mutate(lag_time = lag(time, n = 1)) %>%
  mutate(lag_births = lag(births, n = 1)) %>%
  mutate(lag_income = lag(income, n = 1)) # create 1-period lag of residuals

mod_3 <- lm(res~ 0 + lag_res, data=class_7)
summary(mod_3)

# You strongly reject the null of no serial correlation. t= 55.03! Positive serial correlations as rho_hat>0


#6. Follow the steps in the slides on calculating the FGLS estimator. 
# Do the transformation. The third one transforms the intercept. 

class_7$ytrans <- class_7$births - mod_3$coefficients*class_7$lag_births
class_7$x1trans <- class_7$income - mod_3$coefficients*class_7$lag_income
class_7$x2trans <- class_7$time - mod_3$coefficients*class_7$lag_time
class_7$one <- 1-mod_3$coefficients

# Run model on transformed data. 
mod_5 <- lm(ytrans ~ -1 +one +x1trans + x2trans, data=class_7)
summary(mod_5)
