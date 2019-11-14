############# Task 1:  #############
# Task 1:  Load "longley" from the datasets package

library(datasets)

longley

############# Task 2:  #############
# Task 2:  Plot and determine the 3 variables best correlated to 'Employed'

y <- longley$Employed
X1 <- longley$GNP
X2 <- longley$Unemployed
X3 <- longley$Armed.Forces
X4 <- longley$Population
X5 <- longley$Year

c1 <- 'red'
c2 <- 'blue'
c3 <- 'green'
c4 <- 'black'
c5 <- 'purple'

plot (X1, y, col = c1)
plot (X2, y, col = c2)
plot (X3, y, col = c3)
plot (X4, y, col = c4)
plot (X5, y, col = c5)

# The 3 variables corresponding to Employed are: GNP, Population, and Year

############# Task 3:  #############
# Task 3: Create a regression model for each of the 3 variables selected in 2. above, and
# select the best one

model.GNP <- lm (y ~ X1)
model.GNP
# B0 (Intercept): 51.84359    B1: 0.034750.03475    *This one (GNP) is best!!!
# model: Employed = 51.84359 + 0.034750*GNP + e

model.Pop <- lm (y ~ X4)
model.Pop                # 0.4849   

model.Yr <- lm (y ~ X5)
model.Yr                 # 0.7165


############# Task 4:  #############
# Task 4: Create the model matrises for the winner in 3. above

Emp.matrx <- as.matrix(cbind(c(y), c(X1)))
Emp.matrx

# or another matrises might be:
#  x <- X1
#  X <- as.matrix(cbind(1, x)) # Add vector of 1s for the intercept term

############# Task 5:  #############
# Task 5: Recalculate the regression parameters and predicted values for 4. above

# y = B0 + B1*X1
# Employed = B0 + B1*GNP

library(tidyverse)

input <- longley[,c("Employed", 'GNP')]
B0 <- 51.84359
B1 <- 0.034750

predicted <- input %>%
  mutate (Intercept = B0, 
          Empl.predicted = Intercept + B1*GNP)
view(predicted)

# To appreciate how they compared, here's a plot fo show their distribution...
plot (X1, y, col = c1, main = 'Actual (Red) vs Predicted (Blue) Employed')
points(X1, predicted$Empl.predicted, col = c2)

