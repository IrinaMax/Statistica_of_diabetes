# Statistica_of_diabetes
This is my statistical experimenting models using diabetes incidents
file://localhost/Users/irinamahmudjanova/Desktop/Pima_diabet/MD2.html
---
title: "Statistics and ML techniques using diabetes incidents in Pima."
author: "Irina Max"
date: "November 16, 2016"
output:
  html_document:
    highlight: null
  pdf_document: default
---


I'll look into exploring this data set, on forecasting the onset of diabetes in a population of Pima Indians using various tools in statistical learning. It'll mainly be logistic regression since the response output, with or without diabetes, is of a binary format (yes/no or true/false), and in this case, 1/0.

file preview shows a header row
```{r}
diabetPima <- read.csv("~/Desktop/Pima_diabet/Pima Indians Diabetes Binary Classification dataset.csv", header = TRUE)
```
first look at the data set using summary() and str() to understand what type of data are you working with
```{r}
summary(diabetPima)
str(diabetPima)
```

The summary shows the mean, quartile etc values of the variables if they are numeric. The Outcome variable is supposed to be a factor with two levels, 1 and 0 and we're going to change that later. And note that some of the variables carry 0 values which is not quite possible. E.g. it is not possible for someone's BMI or BloodPressure be 0. So there must be some problem with collection of the data and we're going to do some tidying of the data.


```{r}
# modify the data columns names slightly for easier typing
names(diabetPima)
diabetPima <- setNames(diabetPima, c("preg", "glucose", "blpress", "triceps", "insul",                              "mass", "dpf","age", "class"))
diabetPima$class <- factor(diabetPima$class)

str(diabetPima)
print(paste0("number of observations = ", dim(diabetPima)[1]))
print(paste0("number of predictors = ", dim(diabetPima)[2]))
par(mfrow = c(2, 2))
```

```{r, echo=FALSE}
# the $ notation can be used to subset the variable you're interested in.
hist(diabetPima$preg)
hist(diabetPima$age)
hist(diabetPima$glucose)
hist(diabetPima$mass)
```

The graphs show some of the distributions of the variables. Age and number of times pregnant are not normal distributions as expected since the underlying population should not be normally distributed either. This 768 observations are just a sample of the original population. On the other hand, the glucose level and BMI seem to follow a normal distribution. When performing any analysis, it is always good to know what is the distribution of the data so all the assumptions for different tests or models can be met.

```{r, echo=FALSE}
par(mfrow = c(1, 2))

# boxplot
with(diabetPima, boxplot(dpf ~ class, 
                         ylab = "Diabetes Pedigree Function", 
                         xlab = "Presence of Diabetes",
                         main = "Figure A",
                         ##  horizontal = TRUE,
                         outline = FALSE))

# subsetting based on response
with <- diabetPima[diabetPima$class == 1, ]
without <- diabetPima[diabetPima$class == 0, ]

# density plot
plot(density(with$glucose), 
     xlim = c(0, 250),
     ylim = c(0.00, 0.02),
     xlab = "Glucose Level",
     main = "Figure B",
     lwd = 2)
lines(density(without$glucose), 
      col = "red",
      lwd = 2)
legend("topleft", 
       col = c("black", "red"), 
       legend = c("With Diabetes", "Without Diabetes"), 
       lwd = 2,
       bty = "n")

# simple two sample t-test with unequal variance
t.test(with$dpf, without$dpf)
```

Other plots such as boxplot or density plot can also be used to look at the difference in values of the variables between those with diabetes and those without. We can see from Figure B that the distribution to shifted towards the left for those without diabetes. This means those without diabetes generally have a lower blood glucose level.

```{r}
library(reshape2)
cor_melt <- melt(cor(diabetPima[, 1:8]))
cor_melt
cor_melt <- cor_melt[which(cor_melt$value > 0.3 & cor_melt$value != 1), ]
cor_melt <- cor_melt[1:4, ]
cor_melt
```

We can also create a table of the correlations between the variables, and keep only those pairs with correlation values higher than 0.3. However, this is not a good indicator of correlations between the variables as there might be some other unknown interaction effects not taken into account to. 

Next, we'll use LASSO regression to fit a model for this data set, and perform simple 
predictions by splitting the data set into training and validation set.

Using a portion of the data set as the training data, we fitted a lasso regression model for the data. The important variables are glucose level, BMI, age and diabetes pedigree function. We then use the remaining data as the validation set and predict the presence of diabetes using the function. We manage to achieve an accuracy of 0.755.
There are other methods of model fitting such as logistic regression or support vector machine. It depends heavily of the type of data you have before choosing the type of method used for model fitting.
Fitting with logistic regression using glm(). We need to take the exp() of the predicted values in order to get the probability response. Plotting the model from glm() gives some diagnostic plots needed to identify those observations with high leverages or that are outliers. These points may have to be removed for the model to be fitted better to the training set.

```{r}
library(glmnet)

# creating a random set of observations to be in the training set
set.seed(100)
inTrain <- sample(x = seq(1, 768), size = 294, replace = FALSE)

# preparing the inputs for Cross-validation function cv.glmnet()
# you can use ?glmnet to understand more
x <- model.matrix(class ~ . - 1, data = diabetPima)
y <- diabetPima$class

# model fitting with lasso (alpha = 1)
# since response is binary, we'll set the [family = "binomial"] in the argument
# lasso regression also perform variable selection to determine which are the important variables
par(mfrow = c(1, 1))
fit.lasso.cv <- cv.glmnet(x[inTrain, ], y[inTrain], alpha = 1, family = "binomial")
```

```{r, echo=FALSE}
plot(fit.lasso.cv)
```

```{r}
print(paste0("minimum binomial deviance = ", round(min(fit.lasso.cv$cvm), 3)))
print(paste0("log(lambda) with minimum binomial deviance = ", round(log(fit.lasso.cv$lambda.min), 3)))
coef(fit.lasso.cv)
```

Pprediction with the validation data set

```{r, eval=FALSE}

pred_l <- predict(fit.lasso.cv, newx = x[-inTrain, ])
pred_l <- exp(pred_l) / (1 + exp(pred))
pred_l <- ifelse(pred_l >= 0.5, 1, 0)
table(pred_l, y[-inTrain])

# calculate the accuracy
correct_pred <- sum(table(pred_l, y[-inTrain])[c(1, 4)])
total <- length(y[-inTrain])
acc <- correct_pred / total
print(paste0("accuracy = ", round(acc, 3)))
```
Loading required package: Matrix
Loading required package: foreach
Loaded glmnet 2.0-5
```{r}
library(Matrix)
library(foreach)
library(caret)
fit.glm <- glm(class ~ ., data = diabetPima[inTrain, ], family = binomial)
pred.glm.logistic <- predict(fit.glm, diabetPima[-inTrain, ])
pred.glm <- exp(pred.glm.logistic) / (1 + exp(pred.glm.logistic))
pred.glm <- as.integer(pred.glm >= 0.5)
confusionMatrix(pred.glm, y[-inTrain])[2:3]
par(mfrow = c(2, 2))
```
```{r, echo=FALSE}
plot(fit.glm)
```

Loading required package: lattice
Loading required package: ggplot2
Random forest. There are a handful of tuning parameters that can be adjusted to ensure a better fit using random forest, namely the mtry and ntree. However, I did not go into details on adjusting these parameters.

```{r}
library(randomForest)
set.seed(123)
fit.rf <- randomForest(class ~ .,
                       diabetPima[inTrain, ],
                       mtry = 3, # number of predictors to use for generation of tree 
                       ntree = 500, # number of trees to create
                       importance = TRUE)
pred.rf <- predict(fit.rf, diabetPima[-inTrain, ])
confusionMatrix(pred.rf, y[-inTrain])[2:3]
importance(fit.rf)
```
```{r, echo=FALSE}
varImpPlot(fit.rf)
```

Lastly, the most easily interpreted model, CART. The tree model can be quite hard to read when there are too many terminal nodes.

```{r, eval=FALSE}
library(tree)
set.seed(123)
fit.tree <- tree(class ~ ., 
                 data = diabetPima[inTrain, ])
pred.tree <- predict(fit.tree, diabetPima[-inTrain, ], type = "class")
confusionMatrix(pred.tree, y[-inTrain])[2:3]
par(mfrow = c(1, 1))
text(fit.tree, pretty = 0)

text(fit.tree, pretty = 0)
plot(fit.tree)
```
```{r, echo=FALSE}
plot(fit.tree)
text(fit.tree, pretty = 0)
```
For some fitting methods, there are tuning parameters built into the model that compensate for the number of predictors added into the model during the fitting. These compensations prevent the model from over-fitting the training set data, and in certain ways optimize the bias-variance trade-off. These parameters can be adjusted using cross validation to allow the model to fit better to the data set. Thus, there are a lot more work to do then just running the data through all the default statistical learning methods.
> 
>library("CHAID")
> 
> ### fit tree to subsample
> diabetPima$preg <- factor(diabetPima$preg)
> diabetPima$glucose <- factor(diabetPima$glucose)
> diabetPima$blpress <- factor(diabetPima$blpress)
> diabetPima$insul <- factor(diabetPima$insul)
> diabetPima$mass <- factor(diabetPima$mass)
> diabetPima$age <- factor(diabetPima$age)
> diab.chaid <- diabetPima[sample(1:nrow(diabetPima), 100),]
> chaid.diabetes <- chaid(age~ preg+ glucose + insul+ mass+ class,
+ data = diabetPima)
>print(chaid.diabetes)
>plot(chaid.diabetes, main = "CHAID tree diabetes incidents classification")
