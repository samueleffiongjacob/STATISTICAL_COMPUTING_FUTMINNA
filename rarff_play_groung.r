
bo <- mean(y)
bo
b01 <- mean(x)
b01
b001 <- a * bo
b001
b <- b01- b001
b
variance_x <- var(x)
variance_y <- var(y)

b10 <- sum((variance_x - x) * (variance_y -y))
b101 <- (sum(variance_x - x)**2)
b1 <- b10 / b101 
b1
regression1<- a + b*x
regression <-  y11 - regression1
regression