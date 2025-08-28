# Let's play with some very basic sampling strategies. Partly to make sure this R thing works on your machine or Jupyter

# Author of this crappy code: Max Auffhammer
# Date this code was written: 8/22/2025

# Clear memory
rm(list = ls()) 

# This next line loads a package called pacman, which will make it easier to install other packages later. 
install.packages("pacman")
library(pacman)

# ggplot makes pretty graphs. dplyr is awesome. More later. 
p_load(ggplot2, dplyr, magrittr, gapminder)


#Create a vector of data
#Always set seed for replicability
set.seed(09222008) 

some_population <- c(100, 200, 300, 400, 500) 

#Sample two values from the population with replacement
sampled_numbers <- sample (some_population, size = 2, replace = TRUE) 
print(sampled_numbers)

# But what about a deck of cards? And let's do no Replacement?
deck <- 1:52
shuffled_deck <- sample(deck, size = length(deck), replace = FALSE)
hand_size <- 5
hand <- shuffled_deck[1:hand_size]
print(hand)

# Cute Max. But we are scientists. We mostly sample from data frames. How do you do that? OK. 

# Create a data frame
some_population_df <- data.frame(
  Name = c("Shaily","Keyu","Santiago","David","Nayeli","Srishti","Chelsea","Chelsea","Kaiwen","Sang Min","Alex","Wenjia","Jerra","Citlalli","Lu","Mina","Muhammad","Shufan","Zhilie", "Eloise Wenxi", "Riza", "Iris", "Emelia", "Yuxuan", "Jonathan", "Christian", "Max", "Qinuo"),
  fav_number = c(25, 30, 35, 40, 45, 30, 35, 40, 45, 30, 35, 40, 45, 30, 35, 40, 45, 30, 35, 40, 45, 30, 35, 40, 45, 30, 35, 40)
)

# Sample 3 students' favorite number without replacement
sampled_df <- some_population_df[sample(nrow(some_population_df), size = 3, replace = FALSE), ]
print(sampled_df)


# Sometimes we want to take repeated samples (Monte Carlo!)
# Define a population vector
k_pop <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)

# Replicate sampling 5 times without replacement
replicated_samples <- replicate(5, sample(k_pop, size = 3, replace = FALSE))

# Print the replicated samples
print(replicated_samples)

#What about dplyr? It is an R package that makes it easier to manipulate and transform data. It’s part of the tidyverse collection of R packages, which are designed to work well together for data science tasks.

# you can do cute things with it like this. Gapminder is a sample panel dataset that has info on life expectancy, population and gdp per capita by country. 

# Let's take a look
cheese <- gapminder

# dplyr lets you chain tasks nicely. Say you want to figure out average life expectancy and country population by continent in 1973. You can think of %>% as "and then"....

gapminder %>%
  filter(year == 1972) %>%
  group_by(continent) %>%
  summarize(mean_lifeExp = mean(lifeExp),
            mean_pop = mean(pop)) %>%
  arrange(desc(mean_pop))

# How do we create data frames? These are objects we will most commonly deal with. 
# Create a sample data frame
# Always set seed for reproducability
set.seed(09222008) 
n = 10000
monte <- data.frame(
  ID = 1:n,
  Value = rnorm(n)
)

# Can we plot the data? Yes we can? 
# How about a nice histogram. 
# ggplot is lovely. You srt of layer elements on top each other. 
# It's like a graphical sum. And it looks pretty. 

ggplot(monte, aes(x = Value)) +
  geom_histogram( bins = 100,
    alpha = 1,
    color = "#002676",   # outline
    fill  = "#FDB515"    # bar fill
  ) +
geom_vline(xintercept = 0, linetype = "dashed") +
    labs(
    title = "Distribution of Standard Normal Random Variable",
    y = "Count"
  ) +
  theme_classic(base_size = 15) +
  theme(legend.position = "none")


# Ok. Let's say these are your data. You want to
# randomly sample 10 rows from the data frame without replacement
sampled_data <- monte %>%
  sample_n(10,replace =
             FALSE) %>%
  print

# Now let's get fancy. 
# How about a cluster sample?

set.seed(09222008) #Always set seed for replicability

df <- data.frame(sections = rep(1:10, each=50),ta_rating = rnorm(500, mean=5, sd=2.2))
head(df)
# How about selecting a random set of sections (not students!)

clusters <- sample(unique(df$sections), size=4, replace=F)
sample <- df[df$sections %in% clusters, ]
table(sample$sections)



# What about something a little more ambitious? What was the average annual temperature in the US?  
rm(list=ls()) # Clear everything. 

setwd("/Users/auffhammer/Library/CloudStorage/Dropbox/06_Teaching/MACSS/2025/COMPSS_201_2025/lecture_1")
weather <-  read.csv("lecture_1_weather.csv")
# sometimes instead of making a fancy map, you can just make a simple scatter
# to see whether your data were read in correctly. 

plot(weather$longitude,weather$latitude)

# For trivial pursuit night, we might be interested the in the average Maximum
# temperature across the united states. 

# If you have all the data, this is pretty easy. 
answer_correct = mean(x$tMax)

# America is weird. Fahrenheit?
answer_correct*9/5+32

# Decent approximation is often *2 +30
answer_correct*2+30

# OK. Let's try this clustering thing for real. 
# Let's say you do not have the whole data, but need to get it from each county directly. 
# This is costly. Each county you write to will give you all of their data.  

# Choose a number of counties ("clusters") 
c = 4
clusters_w <- sample(unique(weather$fips), size=c, replace=F)
sample_w <- weather[weather$fips %in% clusters_w, ]
mean(sample_w$tMax)

# What if we make it harder. Choose a number of counties, but we only get a 
# random number of grids (n) in each county? Can you figure it out? Solution next time. 
rm(list=ls()) # Clear everything. 
setwd("/Users/auffhammer/Library/CloudStorage/Dropbox/06_Teaching/MACSS/2025/COMPSS_201_2025/lecture_1")
weather <-  read.csv("lecture_1_weather.csv")
set.seed(22092008)
c = 4
n = 5

# Step 1: sample 4 counties
clusters_w <- sample(unique(weather$fips), size=c, replace=FALSE)

# Step 2: for each chosen county, draw 50 observations
sample_w <- do.call(rbind, lapply(clusters_w, function(f) {
  county_data <- subset(weather, fips == f)
  county_data[sample(1:nrow(county_data), size=n, replace=FALSE), ]
}))

# Check results
table(sample_w$fips)    # should show 50 for each county
nrow(sample_w)          # should be 200




