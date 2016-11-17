# Statistica_of_diabetes
This is my statistical experimenting models using diabetes incidents
> #A first tryout with sharing a kernel on Kaggle using the data set available. I'll look 
> ##into exploring this data set, on forecasting the onset of diabetes in a population of 
> ##Pima Indians using various tools in statistical learning. It'll mainly be logistic regression 
> ##since the response output, with or without diabetes, is of a binary format (yes/no or true/false), and in this case, 1/0.
> 
> # file preview shows a header row
> diabetPima <- read.csv("~/Desktop/Pima_diabet/Pima Indians Diabetes Binary Classification dataset.csv", header = TRUE)
> 
> # first look at the data set using summary() and str() to understand what type of data are you working
> # with
>     summary(diabetPima)
 Number.of.times.pregnant
 Min.   : 0.000          
 1st Qu.: 1.000          
 Median : 3.000          
 Mean   : 3.845          
 3rd Qu.: 6.000          
 Max.   :17.000          
 Plasma.glucose.concentration.a.2.hours.in.an.oral.glucose.tolerance.test
 Min.   :  0.0                                                           
 1st Qu.: 99.0                                                           
 Median :117.0                                                           
 Mean   :120.9                                                           
 3rd Qu.:140.2                                                           
 Max.   :199.0                                                           
 Diastolic.blood.pressure..mm.Hg. Triceps.skin.fold.thickness..mm.
 Min.   :  0.00                   Min.   : 0.00                   
 1st Qu.: 62.00                   1st Qu.: 0.00                   
 Median : 72.00                   Median :23.00                   
 Mean   : 69.11                   Mean   :20.54                   
 3rd Qu.: 80.00                   3rd Qu.:32.00                   
 Max.   :122.00                   Max.   :99.00                   
 X2.Hour.serum.insulin..mu.U.ml. Body.mass.index..weight.in.kg..height.in.m..2.
 Min.   :  0.0                   Min.   : 0.00                                 
 1st Qu.:  0.0                   1st Qu.:27.30                                 
 Median : 30.5                   Median :32.00                                 
 Mean   : 79.8                   Mean   :31.99                                 
 3rd Qu.:127.2                   3rd Qu.:36.60                                 
 Max.   :846.0                   Max.   :67.10                                 
 Diabetes.pedigree.function  Age..years.    Class.variable..0.or.1.
 Min.   :0.0780             Min.   :21.00   Min.   :0.000          
 1st Qu.:0.2437             1st Qu.:24.00   1st Qu.:0.000          
 Median :0.3725             Median :29.00   Median :0.000          
 Mean   :0.4719             Mean   :33.24   Mean   :0.349          
 3rd Qu.:0.6262             3rd Qu.:41.00   3rd Qu.:1.000          
 Max.   :2.4200             Max.   :81.00   Max.   :1.000          
> str(diabetPima)
'data.frame':	768 obs. of  9 variables:
 $ Number.of.times.pregnant                                                : int  6 1 8 1 0 5 3 10 2 8 ...
 $ Plasma.glucose.concentration.a.2.hours.in.an.oral.glucose.tolerance.test: int  148 85 183 89 137 116 78 115 197 125 ...
 $ Diastolic.blood.pressure..mm.Hg.                                        : int  72 66 64 66 40 74 50 0 70 96 ...
 $ Triceps.skin.fold.thickness..mm.                                        : int  35 29 0 23 35 0 32 0 45 0 ...
 $ X2.Hour.serum.insulin..mu.U.ml.                                         : int  0 0 0 94 168 0 88 0 543 0 ...
 $ Body.mass.index..weight.in.kg..height.in.m..2.                          : num  33.6 26.6 23.3 28.1 43.1 25.6 31 35.3 30.5 0 ...
 $ Diabetes.pedigree.function                                              : num  0.627 0.351 0.672 0.167 2.288 ...
 $ Age..years.                                                             : int  50 31 32 21 33 30 26 29 53 54 ...
 $ Class.variable..0.or.1.                                                 : int  1 0 1 0 1 0 1 0 1 1 ...
> 
> ##The summary shows the mean, quartile etc values of the variables if they are numeric. The Outcome variable is supposed to be a factor with two levels, 1 and 0 and we're going to change that later. And note that some of the variables carry 0 values which is not quite possible. E.g. it is not possible for someone's BMI or BloodPressure be 0. So there must be some problem with collection of the data and we're going to do some tidying of the data.
> # modify the data columns names slightly for easier typing
> names(diabetPima)
[1] "Number.of.times.pregnant"                                                
[2] "Plasma.glucose.concentration.a.2.hours.in.an.oral.glucose.tolerance.test"
[3] "Diastolic.blood.pressure..mm.Hg."                                        
[4] "Triceps.skin.fold.thickness..mm."                                        
[5] "X2.Hour.serum.insulin..mu.U.ml."                                         
[6] "Body.mass.index..weight.in.kg..height.in.m..2."                          
[7] "Diabetes.pedigree.function"                                              
[8] "Age..years."                                                             
[9] "Class.variable..0.or.1."                                                 
> diabetPima <- setNames(diabetPima, c("preg", "glucose", "blpress", "triceps", "insul", "mass", 
+                                      "dpf","age", "class"))
> diabetPima$class <- factor(diabetPima$class)
> 
> # removing those observation rows with 0 in any of the variables
> #for (i in 2:6) {
> # diabetPima <- diabetPima[-which(diabetPima[, i] == 0), ]
> #}
> 
> # modify the data column names slightly for easier typing
> 
> #names(diabetes) <- tolower(names(diabetes))
> 
> str(diabetPima)
'data.frame':	768 obs. of  9 variables:
 $ preg   : int  6 1 8 1 0 5 3 10 2 8 ...
 $ glucose: int  148 85 183 89 137 116 78 115 197 125 ...
 $ blpress: int  72 66 64 66 40 74 50 0 70 96 ...
 $ triceps: int  35 29 0 23 35 0 32 0 45 0 ...
 $ insul  : int  0 0 0 94 168 0 88 0 543 0 ...
 $ mass   : num  33.6 26.6 23.3 28.1 43.1 25.6 31 35.3 30.5 0 ...
 $ dpf    : num  0.627 0.351 0.672 0.167 2.288 ...
 $ age    : int  50 31 32 21 33 30 26 29 53 54 ...
 $ class  : Factor w/ 2 levels "0","1": 2 1 2 1 2 1 2 1 2 2 ...
> print(paste0("number of observations = ", dim(diabetPima)[1]))
[1] "number of observations = 768"
> print(paste0("number of predictors = ", dim(diabetPima)[2]))
[1] "number of predictors = 9"
> par(mfrow = c(2, 2))
> 
> # the $ notation can be used to subset the variable you're interested in.
> hist(diabetPima$preg)
> hist(diabetPima$age)
> hist(diabetPima$glucose)
> hist(diabetPima$mass)
> ##The graphs show some of the distributions of the variables. Age and number of times pregnant are not normal distributions as expected since the underlying population should not be normally distributed either. This 392 observations are just a sample of the original population. On the other hand, the glucose level and BMI seem to follow a normal distribution. When performing any analysis, it is always good to know what is the distribution of the data so all the assumptions for different tests or models can be met.
> 
> par(mfrow = c(1, 2))
> 
> # boxplot
> with(diabetPima, boxplot(dpf ~ class, 
+                          ylab = "Diabetes Pedigree Function", 
+                          xlab = "Presence of Diabetes",
+                          main = "Figure A",
+                          ##  horizontal = TRUE,
+                          outline = FALSE))
> 
> # subsetting based on response
> with <- diabetPima[diabetPima$class == 1, ]
> without <- diabetPima[diabetPima$class == 0, ]
> 
> # density plot
> plot(density(with$glucose), 
+      xlim = c(0, 250),
+      ylim = c(0.00, 0.02),
+      xlab = "Glucose Level",
+      main = "Figure B",
+      lwd = 2)
> lines(density(without$glucose), 
+       col = "red",
+       lwd = 2)
> legend("topleft", 
+        col = c("black", "red"), 
+        legend = c("With Diabetes", "Without Diabetes"), 
+        lwd = 2,
+        bty = "n")
> 
> # simple two sample t-test with unequal variance
> t.test(with$dpf, without$dpf)

	Welch Two Sample t-test

data:  with$dpf and without$dpf
t = 4.5768, df = 454.51, p-value = 6.1e-06
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 0.06891135 0.17262065
sample estimates:
mean of x mean of y 
 0.550500  0.429734 

> 
> #Other plots such as boxplot or density plot can also be used to look at the difference in values 
> #of the variables between those with diabetes and those without. We can see from Figure B that the 
> #distribution to shifted towards the left for those without diabetes. This means those without 
> #diabetes generally have a lower blood glucose level.
> 
> library(reshape2)
> cor_melt <- melt(cor(diabetPima[, 1:8]))
> cor_melt
      Var1    Var2       value
1     preg    preg  1.00000000
2  glucose    preg  0.12945867
3  blpress    preg  0.14128198
4  triceps    preg -0.08167177
5    insul    preg -0.07353461
6     mass    preg  0.01768309
7      dpf    preg -0.03352267
8      age    preg  0.54434123
9     preg glucose  0.12945867
10 glucose glucose  1.00000000
11 blpress glucose  0.15258959
12 triceps glucose  0.05732789
13   insul glucose  0.33135711
14    mass glucose  0.22107107
15     dpf glucose  0.13733730
16     age glucose  0.26351432
17    preg blpress  0.14128198
18 glucose blpress  0.15258959
19 blpress blpress  1.00000000
20 triceps blpress  0.20737054
21   insul blpress  0.08893338
22    mass blpress  0.28180529
23     dpf blpress  0.04126495
24     age blpress  0.23952795
25    preg triceps -0.08167177
26 glucose triceps  0.05732789
27 blpress triceps  0.20737054
28 triceps triceps  1.00000000
29   insul triceps  0.43678257
30    mass triceps  0.39257320
31     dpf triceps  0.18392757
32     age triceps -0.11397026
33    preg   insul -0.07353461
34 glucose   insul  0.33135711
35 blpress   insul  0.08893338
36 triceps   insul  0.43678257
37   insul   insul  1.00000000
38    mass   insul  0.19785906
39     dpf   insul  0.18507093
40     age   insul -0.04216295
41    preg    mass  0.01768309
42 glucose    mass  0.22107107
43 blpress    mass  0.28180529
44 triceps    mass  0.39257320
45   insul    mass  0.19785906
46    mass    mass  1.00000000
47     dpf    mass  0.14064695
48     age    mass  0.03624187
49    preg     dpf -0.03352267
50 glucose     dpf  0.13733730
51 blpress     dpf  0.04126495
52 triceps     dpf  0.18392757
53   insul     dpf  0.18507093
54    mass     dpf  0.14064695
55     dpf     dpf  1.00000000
56     age     dpf  0.03356131
57    preg     age  0.54434123
58 glucose     age  0.26351432
59 blpress     age  0.23952795
60 triceps     age -0.11397026
61   insul     age -0.04216295
62    mass     age  0.03624187
63     dpf     age  0.03356131
64     age     age  1.00000000
> cor_melt <- cor_melt[which(cor_melt$value > 0.3 & cor_melt$value != 1), ]
> cor_melt <- cor_melt[1:4, ]
> cor_melt
    Var1    Var2     value
8    age    preg 0.5443412
13 insul glucose 0.3313571
29 insul triceps 0.4367826
30  mass triceps 0.3925732
> #We can also create a table of the correlations between the variables, and keep only those pairs 
> #with correlation values higher than 0.3. However, this is not a good indicator of correlations 
> #between the variables as there might be some other unknown interaction effects not taken into 
> #account to. 
> #BTW  we can remove all row with 0 then result will be different
> # removing those observation rows with 0 in any of the variables
> for (i in 2:6) {
+   diabetPima_1 <- diabetPima[-which(diabetPima[, i] == 0), ]
+   
+ }
> cor_melt1 <- melt(cor(diabetPima_1[, 1:8]))
> cor_melt1 <- cor_melt[which(cor_melt$value > 0.3 & cor_melt$value != 1), ]
> cor_melt1 <- cor_melt[1:4, ]
> cor_melt1
    Var1    Var2     value
8    age    preg 0.5443412
13 insul glucose 0.3313571
29 insul triceps 0.4367826
30  mass triceps 0.3925732
> # modify the data column names slightly for easier typing
> 
> 
> 
> #Next, we'll use LASSO regression to fit a model for this data set, and perform simple 
> #predictions by splitting the data set into training and validation set.
> 
> #Using a portion of the data set as the training data, we fitted a lasso regression model for the data. The important variables are glucose level, BMI, age and diabetes pedigree function. We then use the remaining data as the validation set and predict the presence of diabetes using the function. We manage to achieve an accuracy of 0.755.
> #There are other methods of model fitting such as logistic regression or support vector machine. It depends heavily of the type of data you have before choosing the type of method used for model fitting.
> #Fitting with logistic regression using glm(). We need to take the exp() of the predicted values in order to get the probability response. Plotting the model from glm() gives some diagnostic plots needed to identify those observations with high leverages or that are outliers. These points may have to be removed for the model to be fitted better to the training set.
> library(glmnet)
> 
> # creating a random set of observations to be in the training set
> set.seed(100)
> inTrain <- sample(x = seq(1, 768), size = 294, replace = FALSE)
> 
> # preparing the inputs for Cross-validation function cv.glmnet()
> # you can use ?glmnet to understand more
> x <- model.matrix(class ~ . - 1, data = diabetPima)
> y <- diabetPima$class
> 
> # model fitting with lasso (alpha = 1)
> # since response is binary, we'll set the [family = "binomial"] in the argument
> # lasso regression also perform variable selection to determine which are the important variables
> par(mfrow = c(1, 1))
> fit.lasso.cv <- cv.glmnet(x[inTrain, ], y[inTrain], alpha = 1, family = "binomial")
> plot(fit.lasso.cv)
> 
> print(paste0("minimum binomial deviance = ", round(min(fit.lasso.cv$cvm), 3)))
[1] "minimum binomial deviance = 0.986"
> print(paste0("log(lambda) with minimum binomial deviance = ", round(log(fit.lasso.cv$lambda.min), 3)))
[1] "log(lambda) with minimum binomial deviance = -4.456"
> coef(fit.lasso.cv)
9 x 1 sparse Matrix of class "dgCMatrix"
                       1
(Intercept) -5.955961689
preg         0.043911367
glucose      0.024986763
blpress      .          
triceps      .          
insul        .          
mass         0.055035437
dpf          0.368860689
age          0.006252545
> 
> # prediction with the validation data set
> pred_l <- predict(fit.lasso.cv, newx = x[-inTrain, ])
> pred_l <- exp(pred_l) / (1 + exp(pred))
Error: object 'pred' not found
> pred_l <- ifelse(pred_l >= 0.5, 1, 0)
> table(pred_l, y[-inTrain])
      
pred_l   0   1
     0 302  97
     1  15  60
> 
> # calculate the accuracy
> correct_pred <- sum(table(pred_l, y[-inTrain])[c(1, 4)])
> total <- length(y[-inTrain])
> acc <- correct_pred / total
> print(paste0("accuracy = ", round(acc, 3)))
[1] "accuracy = 0.764"
> #Loading required package: Matrix
> #Loading required package: foreach
> #Loaded glmnet 2.0-5
> library(Matrix)
> library(foreach)
> library(caret)
> fit.glm <- glm(class ~ ., data = diabetPima[inTrain, ], family = binomial)
> pred.glm.logistic <- predict(fit.glm, diabetPima[-inTrain, ])
> pred.glm <- exp(pred.glm.logistic) / (1 + exp(pred.glm.logistic))
> pred.glm <- as.integer(pred.glm >= 0.5)
> confusionMatrix(pred.glm, y[-inTrain])[2:3]
$table
          Reference
Prediction   0   1
         0 267  58
         1  50  99

$overall
      Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull AccuracyPValue 
  7.721519e-01   4.790043e-01   7.317009e-01   8.091687e-01   6.687764e-01   5.278874e-07 
 McnemarPValue 
  5.005814e-01 

> par(mfrow = c(2, 2))
> plot(fit.glm)
> #Loading required package: lattice
> #Loading required package: ggplot2
> #Random forest. There are a handful of tuning parameters that can be adjusted to ensure a better 
> #fit using random forest, namely the mtry and ntree. However, I did not go into details on adjusting these parameters.
> library(randomForest)
> set.seed(123)
> fit.rf <- randomForest(class ~ .,
+                        diabetPima[inTrain, ],
+                        mtry = 3, # number of predictors to use for generation of tree 
+                        ntree = 500, # number of trees to create
+                        importance = TRUE)
> pred.rf <- predict(fit.rf, diabetPima[-inTrain, ])
> confusionMatrix(pred.rf, y[-inTrain])[2:3]
$table
          Reference
Prediction   0   1
         0 252  49
         1  65 108

$overall
      Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull AccuracyPValue 
  0.7594936709   0.4707437952   0.7184123075   0.7973026259   0.6687763713   0.0000103941 
 McnemarPValue 
  0.1600573462 

> importance(fit.rf)
                 0          1 MeanDecreaseAccuracy MeanDecreaseGini
preg     8.2860851 -3.7578081             5.385266         8.946319
glucose 28.2726316 23.8183621            34.853037        38.814502
blpress  0.5804179  1.1012853             1.185428        10.654958
triceps  0.8904373  0.9024279             1.234623         9.550938
insul    5.0094420  0.8933067             4.537233        10.690334
mass    10.6796934 12.2747294            15.194645        20.904031
dpf      7.8532795  3.0677422             7.678650        18.546117
age     10.5558052 11.4125002            15.626298        19.407760
> varImpPlot(fit.rf)
> #Lastly, the most easily interpreted model, CART. The tree model can be quite hard to read when there are too many terminal nodes.
> library(tree)
> set.seed(123)
> fit.tree <- tree(class ~ ., 
+                  data = diabetPima[inTrain, ])
> pred.tree <- predict(fit.tree, diabetPima[-inTrain, ], type = "class")
> confusionMatrix(pred.tree, y[-inTrain])[2:3]
$table
          Reference
Prediction   0   1
         0 243  50
         1  74 107

$overall
      Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull AccuracyPValue 
    0.73839662     0.43144576     0.69636513     0.77742457     0.66877637     0.00061975 
 McnemarPValue 
    0.03887940 

> par(mfrow = c(1, 1))
> plot(fit.tree)
> text(fit.tree, pretty = 0)
> #For some fitting methods, there are tuning parameters built into the model that compensate for the number of predictors added into the model during the fitting. These compensations prevent the model from over-fitting the training set data, and in certain ways optimize the bias-variance trade-off. These parameters can be adjusted using cross validation to allow the model to fit better to the data set. Thus, there are a lot more work to do then just running the data through all the default statistical learning methods.
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
