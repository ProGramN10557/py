library(ISLR)
library(ggplot2)
library(gridExtra)
install.packages("gridExtra")
install.packages("ggplot2")
Default=read.csv('Default.csv')
summary(Default)

resLogit <- glm(formula = default ~ scale(balance) + scale(income),
                family  = binomial(link = "logit"),
                data    = Default)
 
summary(resLogit) 

Default$predProbLogit <- predict(resLogit, type = "response")
Default$predClassLogit <- factor(predict(resLogit, type = "response") > 0.5,
                                 levels = c(FALSE,TRUE), labels = c("No","Yes"))

glm(default~balance + student + income, family = "binomial", data = Default)


my_logit <- glm(default~balance + student + income, family = "binomial", data = Default)
anova(my_logit, test = "Chisq")

summary(my_logit)

my_logit2 <- glm(default~student, family= "binomial", data = Default)
summary(my_logit2)$coef

summary(my_logit)$coef


set.seed(1)

# Create a sample of 5000 observations
train <- sample(10000,5000)

# Defaultx is a subset of the Default data that does not include the training data that we will fit the model on 
Defaultx <- Default[-train,]

# Fit the logistic model using the training data.  
glm.fit <- glm(default~ balance + student, data = Default, family = binomial, subset = train)

# Use the logistic model to fit the same logistic model, but use the test data.  
glm.probs <- predict(glm.fit, Defaultx, type = "response")

# Make a vector that contains 5000 no responses.   
glm.pred <- rep("No", 5000)

# Replace the no reponsees in the glm.pred vector where the probability is greater than 50% with "Yes"
glm.pred[glm.probs > .5] = "Yes"

# Create a vector that contains the defaults from the testing data set, Defaultx
defaultVector <- Defaultx$default 


# Calculate the mean of the values where the predicted value from the training equals the held out set.  
mean(glm.pred == defaultVector)


# Using the technique above we can see that ~97.14% of the observations in the test set were classified correctly using the logistic model training set. As this is just one of a variety of validation methods, for technical completeness,
#below we also implement a 5-Fold cross validation set:


# Seed the random number generator 
set.seed(2)

# Fit a logistic model using default and income values
glm.fit1 <- glm(default~balance + student, data = Default, family = binomial)

# Create a vector with three blank values
cv.error <- rep(0,3)


# Store the results of each K  validation set into cv.error.  Use K= {3,5,10} 
cv.error[1] <- cv.glm(Default, glm.fit1, K=3)$delta[1]
cv.error[2] <- cv.glm(Default, glm.fit1, K=5)$delta[1]
cv.error[3] <- cv.glm(Default, glm.fit1, K=10)$delta[1]

mean(cv.error) 