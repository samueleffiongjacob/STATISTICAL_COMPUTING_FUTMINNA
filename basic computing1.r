rm(list=ls())
ls()
setwd(dir='C:/Users/Samuel Effiong/Desktop/statistical computing 2 .. sta 418/leture 1')
getwd()
############################## BASIC COMPUTATION IN R #########################
A=10
A#

B<-10
C=110
D=B+C
D#

E=sqrt(10)
E#

F=1:10
PRINT(F)

F=1:10
print(F)
length(F)

############# BASIC COMPUTING IN R 2 FACTOR################
G = c(5,4,3,2,0.1,53,44,3,2,14,5,4,3,2,0.1,53,44,3,2,14,53,44,3,2,14)
H = c(25,14,13,21,15,55,0.4,3,20,11,5,4,3,2,0.1,53,44,3,2,14,53,44,3,2,14)
Z=G+H
Z#
length(G)
length(H)

####################GRAPHIC PLOT FOR 2 FACTOR IN R)
plot(x,y,type="l",lwd=3,col=6,xlab="x",ylab="y")

plot(G,H,type="l",lwd=3,col=6,glab="G",hlab="H")

hist(y,col='blue')
hist(H,col='blue')

########## DIRECT CODES FOR MEAN,VARIANCE, STD DEVIATION ###########
I=c(5,4,3,2,0.1,53,44,3,2,14,5,4,3,2,0.1,53,44,3,2,14,53,44,3,2,14)
mean(I)
var(I)
sd(I)
median(I)

##################LETS NOW WRITE OUR OWN CODES FOR MEAN,VARIANCE#######
x =c(5,4,3,2,0.1,5,4,3,2,4,5)
sum(x)
length(x)
mean=sum(x)/length(x)
print(mean)

#####################VARIANCE###########
J =c(5,4,3,2,0.1,5,4,3,2,4,5)
sum(J)
length(J)
mean=sum(J)/length(J)
sumsq=sum((c(x)-mean)^2)
var=sumsq/(length(x)-1)
print(var)

##############STD DEVIATION##########
K =c(5,4,3,2,0.1)
L=c(8,12,10,6,4)
sum(K)
sum(L)
mean=sum(x)/(sum(f)-1)
sumsq=sum(L*(c(x)-mean)^2)
var=sumsq/(sum(L)-1)
std=sqrt(var)
print(std)

##############STD DEVIATION##########
M =c(5,4,3,2,0.1,5,4,3,2,4,5)
sum(M)
length(M)
mean=sum(M)/length(x)
sumsq=sum((c(M)-mean)^2)
var=sumsq/(length(M)-1)
std=sqrt(var)
print(std)

#Now if you have y,x1 and you need to fit a simple regression
# Y = b0 + b1x1 + e
N = c(0.99,1.02,1.15,1.29,1.46,1.36,0.87,1.23,1.55,1.4,1.19,1.15,0.98,1.01,
1.11,1.2,1.26,1.32,1.43,0.95)
O =c(90.01,89.05,91.43,93.74,96.73,94.45,87.59,91.77,99.42,93.65,93.54,92.52,
90.56,89.54,89.85,90.39,93.65,93.41,94.98,87.3)

###### EDA#######
plot(N,O,type="l",lwd=3,col=3,nlab="N",olab="O")
hist(O,col='pink')
plot(density(N,O),col='red',nlab="N",olab="O")
abline(v=sum(N,O[0]), lwd=1.5, col='gold')

###### Continue Analysis#######
sta222 = data.frame(O=O,N=N)
out = lm(O ~ N, data = sta222)
names(out)
extractAIC(out)
s = summary(out)
print(s)
names(s)

#############writing our own Program for regression#############
P = c(0.99,1.02,1.15,1.29,1.46,1.36,0.87,1.23,1.55,1.4,1.19,1.15,0.98,1.01,
1.11,1.2,1.26,1.32,1.43,0.95)
Q =c(90.01,89.05,91.43,93.74,96.73,94.45,87.59,91.77,99.42,93.65,93.54,92.52,
90.56,89.54,89.85,90.39,93.65,93.41,94.98,87.3)
sum(P)
sum(Q)
sum(P*Q)
length(Q)
length(P)
sum(P^2)
meanP=sum(P)/length(P)###or use## mean(x1)
meanQ=sum(Q)/length(Q)#######or use## mean(y)
slope=((length(Q)*sum(P*Q))-(sum(P)*sum(Q)))/((length(P)*sum(P^2))-(sum(P)^2))
intercept=meanQ-(slope*meanP)
print(slope);
print(intercept)

#############CORELLATION COEFFICIENT##########
R=((length(P)*sum(P*Q))-(sum(P)*sum(Q)))/
sqrt(((length(P)*sum(P^2))-(sum(P)^2))*((length(Q)*sum(Q^2))-(sum(Q)^2)))
print(R)
q()