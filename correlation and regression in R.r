######## THIS IS COMMENT and ## and anything after # means comment.

### SETTING THE SYSTEM working directory

setwd(dir='C:/Users/Samuel Effiong/Desktop/statistical computing 2 .. sta 418/leture 1')


#### confirm the  SYSTEM working directory
getwd()

## clear all old environmental variables
rm(list=ls())
#===========================================================
# Define the data
x = c(61, 39, 70, 63, 83, 76, 48, 72, 54, 22, 67, 60)
y = c(83, 62, 76, 77, 89, 74, 48, 78, 76, 51, 63, 79)
n= 12
########################################################################
# MANUAL APPROCH
#############################################################
xy0 <- x * y
xy <- sum(xy)
ssxy0 = sum(x)*sum(y)/n
ssxy = sum(xy)- ssxy0
ssxy
####################################
ssx00 = sum(x)
ssx001 = (ssx00)^2
sxx0 = sum(x)^2 
ssx01 = ssx001 /n

ssxx = sxx0 - ssx01
ssxx
#================================================
syy0 <- sum(y)
syy001 <- (syy0)^2
syy002 <- sum(y)^2
syy002a <- syy001/n
ssyy <-  syy002 - syy002a
ssyy
#=====================================================
r0 <- ssxy
r1 <- sqrt(ssxx*ssyy)
r <- r0 / r1
r
#=======================================================
# CORELATION ANALYSICS 
xy0 <- x * y
xy <- sum(xy0)
cor1 <- n * sum(xy)
cor1i <- sum(x) * sum(y)
corli0 <- cor1 - cor1i
cor2 <- n *(sum(x**2)) - (sum(x)**2)
cor2i <- n *(sum(y**2))  - (sum(y)**2)
cor2i0 <- cor2 * cor2i
cor2i00 <- sqrt(cor2i0)
cor <- corli0 / cor2i00
cor
#++++++++++++++++++++++++++++++++++++++++++++++++++++

# REGRESSION ANALYSICS manuel

#Y = a+bx

# for a
xy0 <- x * y
xy <- sum(xy)
y1 <- y
y11 <- sum(y)

a01 <- n * sum(xy)
a011 <- sum(x) * sum(y)
a0 <- a01 - a011
a1 <- n *(sum(x**2)) - (sum(x)**2)
a <- ao /a1
a
#==============================   SECOND APPROCH
# Step 2: Calculate the means of x and y:

mean_x <- mean(x)
mean_y <- mean(y)

#==============================
#Step 3: Calculate the deviations from the means:
dev_x <- x - mean_x
dev_y <- y - mean_y

#==================================
#Step 4: Calculate the sum of squared deviations:
sum_sq_dev_x <- sum(dev_x^2)
sum_sq_dev_y <- sum(dev_y^2)
#==============================
# Step 5: Calculate the cross-product of deviations:

cross_prod_dev <- sum(dev_x * dev_y)

#============
# Step 6: Calculate the slope b:
b <- cross_prod_dev / sum_sq_dev_x

#==============

# Step 7: Calculate the intercept a:
a <- mean_y - b * mean_x


cat("Intercept (a):", a, "\n")
cat("Slope (b):", b, "\n")

####################################################
# AUTOMATIC APPROCH below
##############################################################################
# correlation
correlation <- cor(x, y)
correlation
#################################################################

# REGRESSION ANALYSIC

reg <-  lm(y~x)
reg1 <- summary(reg)
reg1

#============================================= or
# REGRESSION ANALYSIC

# Create a data frame
data_text <- data.frame(x, y)

# Perform regression analysis
model <- lm(y ~ x, data = data_text)

# Print the coefficients
coefficients <- coef(model)
a <- coefficients[1]  # Coefficient for variable 'a'


# Extract the coefficient 'b'
b <- coef(model)[[2]]

# Print the summary of the regression model
sumarry <- summary(model)
sumarry
####################################################

boxplot(x,y, main ="regrsion data plot" , xlab="x varaible" ,ylab ="y varaible")
