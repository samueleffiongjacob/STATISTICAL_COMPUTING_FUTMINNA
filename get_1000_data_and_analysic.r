######## THIS IS COMMENT and ## and anything after # means comment.

### SETTING THE SYSTEM working directory

setwd(dir='C:/Users/Samuel Effiong/Desktop/statistical computing 2 .. sta 418/leture 1')


#### confirm the  SYSTEM working directory
getwd()

## clear all old environmental variables
rm(list=ls())

#=============================================================================
# EXCEL approach
### let import our table of data from original question as a .csv file

Mini_Project_stat <- read.csv("pokemon.csv",header=TRUE, sep = ",")

new_ass<- read.csv("assignment2.csv", header = TRUE, sep = ",")

#displaying the head  
pokemone_head <- head(Mini_Project_stat)
pokemone_head

new_ass_head <- head(Mini_Project_stat)
new_ass_head
#displaying the tail
pokemone_tail <- head(Mini_Project_stat, n=10)
pokemone_tail

new_ass_tail <- head(Mini_Project_stat, n=10)
new_ass_tail
#displaying the summary

poke_sumary <- summary(Mini_Project_stat)
poke_sumary

new_ass_sumary <- summary(Mini_Project_stat)
new_ass_sumary
#=================================================================================================================================
#use package


library(tidyr)
library(dplyr)
library(ggplot2)
library(pander)
knitr::opts_chunk$set(echo = FALSE)

# ===========================================================================================================
#           MANUAL APPROUCH
first_name <- c("SULE","TAIWO","KABIRU","HENRY","JOHN")
sur_name <- c("ADOLE","ADEBAYO","HASSAN","JONATHAN","MUHAMMED")
firstname <- sample(first_name,1000,replace = TRUE)
surname <- sample(sur_name,1000,replace = TRUE)
sex <- rep("MALE",1000)
occupation <- c("electrician","mechanic","nurse","civil servant","trader")
occupation <- sample(occupation,1000,replace = TRUE)

# the state data set is the USA default data from r
state <- sample(state.name,1000,replace = TRUE)
local_govt <- sample(state.region,1000,replace = TRUE)
age <- sample(30:60,1000,replace = TRUE)
stipend_USD <- sample(500:1500,1000,replace = TRUE)
mydata <- data.frame(firstname,surname,sex,occupation,state,local_govt,age,stipend_USD)

head <- head(mydata)
tail < tail(mydata)
# ===========================================================================================================
# Using Uni variate statistics to `compare data`  for species defense and attack


Mini_Project_stat %>%
  group_by(species) %>%
  summarize(attack = n()) %>%
  arrange(desc(species)) %>%
  pander()
Mini_Project_stat


Mini_Project_stat %>%
  ggplot(aes(attack)) +
  geom_boxplot() +
  labs(title = "Boxplot to show the Distribution of attack ",
       x = "attack",
       y = " RESULT") +
  theme_minimal()


#========================================================================================================
# Define a function to generate a random name
generateRandomName <- function(length) {
  firstname <- c("a", "e", "i", "o", "u")
  surname <- c("b", "c", "d", "f", "g", "h", "j", "k", "l", "m", "n", "p", "q", "r", "s", "t", "v", "w", "x", "y", "z")
  ocupation <- c("dee","axe","battle","erer")
  name <- ""
  for (i in 1:length) {
    if (i %% 2 == 0) {
      name <- paste0(name, sample(firstname, 1))
    } else {
      name <- paste0(name, sample(surname, 1))
    }
  }
  
  return(name)
}

# Generate 10,000 random names
randomNames <- replicate(10000, generateRandomName(5))

# Print the first 10 names
print(head(randomNames))

#======================================================================================================
set.seed(42)

# Generate random names, surnames, ages, sexes, first names, and local governments:
# Create empty vectors to store the generated data
names <- c()
surnames <- c()
ages <- c()
sexes <- c()
first_names <- c()
local_govt <- c()

# Generate 10,000 individuals' data
for (i in 1:10000) {
  # Generate random name
  name <- faker::faker_name()
  names <- c(names, name)
  
  # Extract first name and surname
  name_parts <- strsplit(name, " ")
  first_name <- name_parts[[1]][1]
  surname <- name_parts[[1]][2]
  first_names <- c(first_names, first_name)
  surnames <- c(surnames, surname)
  
  # Generate random age between 18 and 65
  age <- sample(18:65, 1)
  ages <- c(ages, age)
  
  # Generate random sex (male or female)
  sex <- sample(c("Male", "Female"), 1)
  sexes <- c(sexes, sex)
  
  # Generate random local government
  local_gov <- faker::faker_address()$city
  local_govt <- c(local_govt, local_gov)
}
#=====================================================================================

install.packages('fakeR')
library(fakeR)

# Set the seed (optional, for reproducibility)
set.seed(42)

# Generate random data
num_records <- 10000

# Create empty data frame to store the generated data
data <- data.frame(
  first_name = fakename(num_records),
  surname = fakename(num_records),
  age = fakeage(num_records, from = 18, to = 65),
  sex = sample(c("Male", "Female"), num_records, replace = TRUE),
  local_government = faker::faker_address(num_records)$city
)

# Save the data frame as a CSV file
write.csv(data, file = "random_data.csv", row.names = FALSE)


