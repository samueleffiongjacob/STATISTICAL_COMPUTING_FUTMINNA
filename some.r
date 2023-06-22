### SETTING THE SYSTEM working directory

setwd(dir='C:/Users/Samuel Effiong/Desktop/statistical computing 2 .. sta 418/leture 1')

#### confirm the  SYSTEM working directory
getwd()

## clear all old environmental variables
rm(list=ls())

                       # LINEARNER MODEL


## 	Input the spreadsheet in the form of a data frame with seven variables
# Creating Data Frame using categorical
seven_variable <- data.frame(field_name = c('Grassland', 'Scrub', 'Arable', 'Grassland', 'Grassland', 'Arable', 'Scrub', ' Meadow', 'Scrub','Meadow', 'Meadow', 'Scrub','Orchard', 'Orchard', 'Scrub', 'Scrub', 'Orchard', 'Grassland', 'Meadow','Arable'),
                             Area=c(3.6, 5.1, 2.8, 2.4, 3.8, 3.1, 3.5, 2.1, 1.9, 1.5, 2.9, 3.3, 3.7, 1.8, 4.1, 3.9, 2.2, 4.4, 2.9, 0.8), 
                             Slope=c(11, 2, 3, 5, 0, 2, 3, 6, 0, 4, 10, 1, 2, 5, 7, 8, 0, 2, 1, 10), 
                             Soil.pH=c(4.1, 5.2, 4.3, 4.9, 4.2, 3.9, 4.2, 4.8, 5.7, 5.0, 5.2, 4.1, 4.0, 3.8, 4.9, 5.0, 4.9, 4.7, 3.5, 5.1),
                             Worm.density=c(4, 7, 2, 5, 6, 2, 3, 4, 9, 7, 8, 1, 2, 0, 6, 8, 4, 5, 1, 3),
                             vegetation=c('Grassland', 'Scrub', 'Arable', 'Grassland', 'Grassland', 'Arable', 'Scrub', ' Meadow', 'Scrub','Meadow', 'Meadow', 'Scrub','Orchard', 'Orchard', 'Scrub', 'Scrub', 'Orchard', 'Grassland', 'Meadow','Arable'))

### view the data frame 
seven_variable


###### creating Data Frame using logical (Damp  true=T)
true_variable <- data.frame(Area=c(3.6, 5.1, 2.8, 2.4, 3.8, 3.1, 3.5, 2.1, 1.9, 1.5, 2.9, 3.3, 3.7, 1.8, 4.1, 3.9, 2.2, 4.4, 2.9, 0.8), 
                            Slope=c(11, 2, 3, 5, 0, 2, 3, 6, 0, 4, 10, 1, 2, 5, 7, 8, 0, 2, 1, 10), 
                            Soil.pH=c(4.1, 5.2, 4.3, 4.9, 4.2, 3.9, 4.2, 4.8, 5.7, 5.0, 5.2, 4.1, 4.0, 3.8, 4.9, 5.0, 4.9, 4.7, 3.5, 5.1),
                            Worm.density=c(4, 7, 2, 5, 6, 2, 3, 4, 9, 7, 8, 1, 2, 0, 6, 8, 4, 5, 1, 3),
                            Damp =c('t', 't', 't', 't', 't', 't', 't', ' t', 't', 't', 't', 't', 't', 't', 't', 't', 't', 't', 't', 't'))
true_variable
###### creating Data Frame using logical (Damp  false=F)
false_variable <- data.frame(Area=c(3.6, 5.1, 2.8, 2.4, 3.8, 3.1, 3.5, 2.1, 1.9, 1.5, 2.9, 3.3, 3.7, 1.8, 4.1, 3.9, 2.2, 4.4, 2.9, 0.8), 
                             Slope=c(11, 2, 3, 5, 0, 2, 3, 6, 0, 4, 10, 1, 2, 5, 7, 8, 0, 2, 1, 10), 
                             Soil.pH=c(4.1, 5.2, 4.3, 4.9, 4.2, 3.9, 4.2, 4.8, 5.7, 5.0, 5.2, 4.1, 4.0, 3.8, 4.9, 5.0, 4.9, 4.7, 3.5, 5.1),
                             Worm.density=c(4, 7, 2, 5, 6, 2, 3, 4, 9, 7, 8, 1, 2, 0, 6, 8, 4, 5, 1, 3),
                             Damp =c('f', 'f', 'f', 'f', 'f', 'f', 'f', ' f', 'f', 'f', 'f', 'f', 'f', 'f', 'f', 'f', 'f', 'f', 'f', 'f'))
false_variable
######################################### 1 #########################

################################### 1i ########################################
### Summarize data frame
summary(seven_variable)
summary(false_variable)
summary(true_variable)

######################################### 1ii ######################################
#Subscripts for both the row and column of Area, Slope and Worm.density (columns 2, 3 & 7) from rows 3 to 12.
new_seven_variable <- seven_variable[, -4]
new_seven_variable[3:12, 2:4]

####################################################################
new_true_variable <- true_variable[,-3]
new_true_variable[3:12,1:3]

#######################################################################################
new_false_variable <- false_variable[-3]
new_false_variable[3:12,1:3]

################################################# categorical
unique_8 <- new_seven_variable[sample(13:nrow(new_seven_variable), 8),]
unique_8 

##################################################################### logical true
unique_8 <- new_true_variable[sample(13:nrow(new_seven_variable), 8),]
unique_8
##################################################### logical false
unique_8 <- new_false_variable[sample(13:nrow(new_seven_variable), 8),]
unique_8

######################################################(iv)	Sort the data frame on the basis of values in one of the columns (say, Slope)
###false
unique_8[order(unique_8$Slope), ]

########true
unique_8[order(unique_8$Slope), ]

###### seven
unique_8[order(unique_8$Slope), ]

############## 	Order the rows of the database on worm density within each Vegetation type
seven_variable 
seven_variable [order(seven_variable $vegetation), ]

########(vi)	Calculate the mean, median and standard deviation Slope for each of Vegetation type using aggregate function
print(aggregate(seven_variable$vegetation, list(seven_variable$vegetation),FUN=mean) )###mean
print(aggregate(seven_variable$vegetation, list(seven_variable$vegetation),FUN=median) )###median
print(aggregate(seven_variable$vegetation, list(seven_variable$vegetation),FUN=sd) )##standard deviation Slope



######################################################################### Calculate the mean, median and standard deviation Slope for each of Vegetation type using aggregate function
print(aggregate(seven_variable[c('Area','Slope','Soil.pH','Worm.density')],list(seven_variable$field_name), FUN=mean))###mean
print(aggregate(seven_variable[c('Area','Slope','Soil.pH','Worm.density')],list(seven_variable$field_name), FUN=median))###median
print(aggregate(seven_variable[c('Area','Slope','Soil.pH','Worm.density')],list(seven_variable$field_name), FUN=sd))##standard deviation Slope

################################################ 2. Use the same data frame in question 1 above to fit the regression model:
## Y = a + b1X1 + b2X2;
# Where Y represents the response variable
# a, b1, b2 are  coefficients
# x1, x2, are predictor variables.

## lm( y ~ x1+x2, data)

# for  each other group subtitle your own parameter to this formula lm( y ~ x1+x2, data)
#Groups: C, F, and I)

# Create the relationship model.
Linear_relationship <- lm(Soil.pH ~ Slope + Worm.density, data = seven_variable)

# Show the model.
Linear_relationship

# Get the Intercept and coefficients as vector elements.
cat("# # # # The Coefficient Values # # # ","\n")

intercept <- coef(Linear_relationship)[1]
print(intercept)

Slope <- coef(Linear_relationship)[2]
Worm.density <- coef(Linear_relationship)[3]

print(Slope)
print(Worm.density)

#use fitted model to predict the response value for the new observation

###Create Equation for Regression Model Based on the above intercept and coefficient values, we create the mathematical equation.
#the formula of regression becomes 

#Y = intercept+(Slope)*x1+(Worm.density)*x2 
# recall from question x1 is Worm.density and x2 is Slope

y <- 3.73704+ ( 0.1808369 )*x1 +(0.01251688 )*x2

#define new  fitted model
for_fited_model <- data.frame(Slope, Worm.density)

#use the fitted model to predict the rating
fittedmodel <- predict(Linear_relationship, newdata=for_fited_model)
fittedmodel 

### fit the regression model
y <- 3.73704+ ( 0.1808369 )*Slope +(0.01251688 )*Worm.density
y

##################### summary of regression
summary_of_regression <- summary(Linear_relationship)
summary_of_regression

## interpretation
#1. Residuals
#This refers to the difference between the actual response and the predicted response of the model. So for every point, there will be one actual response and one predicted response. Hence residuals will be as many as observations are. In our case, we have 20 observations, and 5 residuals.

# Min       1Q   Median       3Q      Max 
# -0.62206 -0.14875 -0.00465  0.17810  0.69528 

### Coefficients which as to do with slope and intercept is simple read on it.
#########################

####### extracting residuals
ridual <- summary(summary_of_regression$residuals)
ridual
#note the figures above are derived from  formula summary(Linear_relationship) above.
##################### summary of regression

#####################(i)	Residuals vs Fitted
# Give the chart file a name.
png(file = "Residuals vs Fitted.png")

# Plot the chart.
Residuals_Fitted <- plot(Linear_relationship$fitted.values, Linear_relationship$residuals,main="residuals vs.fitted",xlab="Fitted Value",ylab="Residual value")
abline(h=0,lwd=2,col = "red3")
Residuals_Fitted

# (ii)	Normal Q-Q
## get the list of models
res <- resid(Linear_relationship)
Normal_Q_Q <- qqnorm(res)

### (iii)	Scale Location

Scale_Location <- plot(Linear_relationship,main="Scale Location")
Scale_Location 

####(iv)	Residuals vs Leverage
Residuals_vs_Leverage <- plot(Linear_relationship,main="Residuals vs Leverage")
Residuals_vs_Leverage 


#### Plot: (iii) box plot of Slope and Vegetation (Groups: C, H, and J)
input <- seven_variable[,c('Slope','vegetation')]
print(head(input))
colors = c(rep("red",Slope),rep("blue",vegetation))
boxplot <- boxplot(Slope ~ vegetation, data = seven_variable, xlab = "Number of vegetation",
                   ylab = "number of Slope", main = "Mileage Data",col=colors)


boxplot