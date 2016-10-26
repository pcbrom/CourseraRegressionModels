---
title: "Vehicle performance analysis"
author: "PEDRO CARVALHO BROM"
---

```{r }

setwd("/home/pcbrom/Dropbox/Trabalho e Estudo/Cursos Livres/Regression Models/CurseProject")

## Instructions

# You work for Motor Trend, a magazine about the automobile industry. Looking at 
# a data set of a collection of cars, they are interested in exploring the 
# relationship between a set of variables and miles per gallon (MPG) (outcome). 
# They are particularly interested in the following two questions:

# “Is an automatic or manual transmission better for MPG”
# "Quantify the MPG difference between automatic and manual transmissions"

# do multiple cores
doMC::registerDoMC(4)

# get data
data(mtcars)

# dictionary
# [, 1]	 mpg    Miles/(US) gallon
# [, 2]	 cyl    Number of cylinders
# [, 3]	 disp   Displacement (cu.in.)
# [, 4]	 hp     Gross horsepower
# [, 5]	 drat   Rear axle ratio
# [, 6]	 wt     Weight (1000 lbs)
# [, 7]	 qsec   1/4 mile time
# [, 8]	 vs     V/S
# [, 9]	 am     Transmission (0 = automatic, 1 = manual)
# [,10]	 gear   Number of forward gears
# [,11]	 carb   Number of carburetors

# see some lines
head(mtcars)

# see structure of data
str(mtcars)

# adjust some variables to factor / adjust some factors
mtcars$cyl = as.factor(mtcars$cyl)
mtcars$gear = as.factor(mtcars$gear)
mtcars$carb = as.factor(mtcars$carb)
mtcars$am = as.factor(mtcars$am)
levels(mtcars$am) = c("auto", "manu")

# summary of data
summary(mtcars)

# exploratory data analyses

suppressWarnings(suppressMessages({require(corrr); require(gridExtra)}))
rdf = correlate(mtcars[, -c(2,9:11)])
p1 = rplot(rdf, print_cor = T, legend = F, colours = heat.colors(20))
p2 = network_plot(rdf, legend = T, colours = heat.colors(20))
grid.arrange(p2, p1, ncol = 1, nrow = 2)

# Here mpg correlated with all numeric variables, shows an explanatory potential 
# for consumption, but overall the database has correlation between the 
# variables. This can lead us to an inflation undesirable variance. So here we 
# have two paths. The first is to evaluate a lean model can u still provide good 
# degree of variability explanation and the other is to make an approach using 
# the PCA matrix.

suppressWarnings(suppressMessages({require(ggplot2); require(Hmisc)}))
mtcars$wtCut = cut2(mtcars$wt, 2)
qplot(gear, mpg, col = wtCut, data = mtcars, geom = "boxplot")

# mpg influenced when considering the amount of speed and vehicle weight. It is 
# remarkable that better performance meets vehicle 4 gears and weighing less 
# than 2000 lbs A pect that calls the attention is the outlier with next 
# performance 32 mpg with the vehicle of higher lbs.

mtcars[mtcars$gear == 4 & mtcars$wtCut == "[2.00,5.42]" & mtcars$mpg > 30, ]

# As well a Fiat 128 weighs 2200 lbs ??? I believe this is an error in the 
# database, for this car, according to the manufacturer, weighs about 1600 lbs.
# It's explain the outlier.

mtcars[mtcars$gear == 4 & mtcars$wtCut == "[2.00,5.42]" & mtcars$mpg < 20, ]

suppressWarnings(suppressMessages({require(jpeg); require(grid)}))
img1 = readJPEG("Fiat_128_Kent_UK2.JPG")
img2 = readJPEG("800px-MercedesBenz_250C_1970.JPG")
g1 = rasterGrob(img1, interpolate = T)
g2 = rasterGrob(img2, interpolate = T)
p3 = qplot(1:10, 1:10, geom = "blank", 
           xlab = "This is a bug database.\nIt is not an outlier.\nImage from Wikipedia.", 
           ylab = "", main = "Fiat 128\nWeighs about 1600 lbs") +
    annotation_custom(g1, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          #axis.title.x=element_blank(),
          axis.title.y=element_blank(),legend.position="none",
          panel.background=element_blank(),panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())
p4 = qplot(1:10, 1:10, geom = "blank", 
           xlab = "This really is an outlier.\nImage from Wikipedia.", 
           ylab = "", main = "Mercedes 280C") +
    annotation_custom(g2, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          #axis.title.x=element_blank(),
          axis.title.y=element_blank(),legend.position="none",
          panel.background=element_blank(),panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())
grid.arrange(p3, p4, ncol = 2, nrow = 1)

# It is remarkable as the amount of speed influences the variability of 
# consumption to categorize cars cúmero cylinders. 4 cylinder is Earl found the 
# best performances and, in general, cars with 5 speed though not necessarily 
# have the best consumption shows little variability. So if you buy a car and do 
# not know much about technical data give preference for the 5-speed you'll be 
# fine. Or rather, prefer lighter cars with 5-speed and 4 cylinders.

p5 = qplot(cyl, mpg, col = gear, data = mtcars, geom = "boxplot") +
    theme(legend.position = "none")
p6 = qplot(carb, mpg, col = gear, data = mtcars, geom = "boxplot")  +
    theme(legend.position = "none", axis.title.y = element_blank(),
          axis.text.y = element_blank(), axis.ticks = element_blank())
p7 = qplot(am, mpg, col = gear, data = mtcars, geom = "boxplot")  +
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(), 
          axis.ticks = element_blank())
grid.arrange(p5, p6, p7, ncol = 3, nrow = 1)

# Adding information to the previous paragraph, if you can choose your car with 
# 2 carburetors and manual shift is better.

## strategy for model selection

# Right. So far we have learned that the weight amount of speed and number of 
# cylinders become information that really differentiate the car performance. So 
# the strategy is basically to use this data to model mpg.

db = subset(mtcars, select = -c(wtCut))

fit1 = lm(mpg ~ ., data = db)
summary(fit1)

fit2 = lm(mpg ~ cyl + hp + wt + gear + carb, data = db)
summary(fit2)

fit3 = lm(mpg ~ cyl + wt, data = db)
summary(fit3)

fit4 = lm(mpg ~ cyl * wt, data = db)
summary(fit4)

anova(fit3, fit4, fit1, fit2)

fit5 = lm(mpg ~ cyl + wt - 1, data = db)
summary(fit5)

# Considering the models worked, we use the fit3 as a reference to assess 
# whether it is worthwhile to add variables to the Anova tool. See the results 
# suggest that we should keep only the most simplified model (mpg ~ cyl + wt). 
# For this we obtain an Adjusted R² 0.82, ie, there is a good explanation of 
# variability of vehicle performance when we use the reference weight and number 
# of cylinders. If we consider a new model in which we remove the intercept 
# (mpg ~ cyl + wt - 1), we obtain Adjusted R-squared: 0.9851, and we have to 
# compare the reference level to see if there was a change in the expected 
# value. To fit3 the intercept has the same value as the level of 4-cylinder, 
# ie, using a model without the intercept allows the coefficients have a 
# marginal interpretation more interpretable.

# Interpretation of coefficients. (fit5)

# A regression model returns on an average equation ous is, for each unit of 
# weight in lbs, have a marginal reduction, average mpg in 3.2056 units, 
# considering the other variables fixed. When evaluating the cylinders have a 
# categorization of performance. Suppose the best performance, 4 cyl. The 
# average marginal gain is about 34 mpg when considering fixed wt. If we want 
# to compare the gain in mpg performance between the cylinders, we practice 
# the following:

cyl4 = coef(fit5)[[1]]
cyl6 = coef(fit5)[[2]]
cyl8 = coef(fit5)[[3]]

cyl4/cyl6
cyl4/cyl8
cyl6/cyl8

# This means that the engine with 4 cylinders has a yield of 14% if compared to 
# 6. Similarly have 4 to 8 cyl approximately 22% more yield and 6 to 8 has a 
# yield about 6.5%.

# Here we create a model with coefficients evaluated standard deviation. The 
# purpose is to study which factor is more important for vehicle performance 
# analysis.

fit6 = lm(I((mpg - mean(mpg))/sd(mpg)) ~ cyl + I((wt - mean(wt))/sd(wt)) - 1, 
          data = db); summary(fit6)

# The car 4 cylinders has a positive effect suggests that MPG is an improvement 
# for this condition. On the other hand, we have wt indicating about the same 
# order of magnitude, that when we increase the weight will naturally lose the 
# car performance.

fit7 = lm(mpg ~ am - 1, data = db)
summary(fit7)
coef(fit7)[[2]]/coef(fit7)[[1]]

# Using only the type of transmission reference has generated a new model (fit7) 
# to describe mpg. It is easy to see that on average, the marginal mpg for 
# manual cars is higher than the automatic. When we compare numerically we have 
# a 42% yield of hand vehicles more compared to automatic. Here we emphasize the 
# idea that a manual car is more economical than an automatic car.

# So which model should you use?
# R: fit3
summary(fit3)

# What is the confidence interval for the coefficients?
confint(fit3)

# This indicates that given this sample, the performance "mpg" can vary for the 
# wt variable -4.75 to -1.66, that is, if we repeat the sampling experiment many 
# times, we have the true value of the coefficient (population and unknown) 95% 
# often within the indicated range. The same interpretation is for the cylinders.

# The model is good or have some kind of problem?

# The risiduals meet the assumption of normality?
# The test (p-value = 0.259) accept the hypothesis of normality.
shapiro.test(residuals(fit3))

# Residual plot and some diagnostics
par(mfrow = c(2,2)); plot(fit3, pch = 19, col = "grey"); par(mfrow = c(1,1))

# Evaluating Residuals vs Fitted apparently have possible interference, on 
# quadratic dependency problem. Approximately normal distribution is ok. 
# Scale-Location is within up to 2 nd, so is reasonably ok. Residuals vs 
# Leverage. Here are some points "pulling" / potentially influencing the model, 
# but are still within an appropriate range.

# We have to make a homoscedasticity test to draw any conclusion. 
suppressWarnings(suppressMessages(require(lmtest)))
bptest(fit3, studentize = F)

# Here it depends on what the researcher must accept in his working hypothesis. 
# To run performance is reasonable mágido number of 5%, so it would be 
# inconclusive because 6% is very close to 5%. In this case we must increase the 
# sample size to assess the consistency of constant variance test or consider 
# the possibility of studying vehicle categories separately. Anyway ... 
# apparently would not absurd, for this model suggest that the variance of the 
# errors suffer some kind of inflation as the Residuals chart vs Fitted does not 
# demonstrate any "cone". So we can consider that the constructed model is 
# minimally reasonable and is good enough to explain the variability of vehicle 
# performance considering only wt and cyl.

## Creating the same model by machine learning

set.seed(123)
suppressWarnings(suppressMessages(require(caret)))
inTrain = createDataPartition(y = db$mpg, p = .7, list = F)
training = db[inTrain, ]; testing = db[-inTrain, ]
modFit = train(mpg ~ cyl + wt, data = training, method = "lm")
modFit$modelType; modFit$metric; modFit$finalModel
modFit

# Predicting new values

pred = predict(modFit, testing)
testing$predRigth = pred == testing$mpg

# Evaluating prediction

table("predict values" = round(pred, 1), "original values" = testing$mpg)

# Okay. That was interesting. Here we evaluate the quality of the model looking 
# directly at the matrix of predicted and original values. The model will have a 
# good quality when the results are closer to the main diagonal. Note that those 
# who are not in the main, are "practically stuck" in it. This confirms that the 
# model is well adjusted. In general it is recommended that have more lines of 
# information to ensure the robustness of the model and stability of the 
# coefficients.

suppressWarnings({
    modFitPCA = train(mpg ~ ., data = training, method = "lm", 
                      preProcess = "pca")
})
modFitPCA

# Initially I commented that use PCA might be useful. We will test a model using 
# the rotated matrix. The RMSE had no big change and the gain on the adjustment 
# was also not the best. In other words the simplest model (fit3) ended up being 
# the best and most interpretable among all presented and built by ML (modFit) 
# the result will be slightly higher than using classical statistics.


# To conclude we will clearly answer the questions of Motor Trend.

# “Is an automatic or manual transmission better for MPG”
# R: Manual transmission is better for MPG.

# "Quantify the MPG difference between automatic and manual transmissions"
# R: Auto = 17.147, Manu = 24.392.
#    Manu, Auto ratio = 1.42251, ie, 42% yield over manual for the automatic.
