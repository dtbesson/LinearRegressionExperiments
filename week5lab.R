# load the babies data
babies = read.table(file = "babies.txt", header = TRUE)
attach(babies)
View(babies)

dim(babies)
# there are 1174 entries and 7 attributes

# see if the mom's weight is a good predictor of birth weight
fit1 = lm(formula = birth.weight ~ mom.weight)
summary(fit1)
# Rsquared is very small, indicating a weak linear relationship

# plot this simple linear regression model
plot(mom.weight, birth.weight, type = 'p', pch = 16, col = 'black',
     main = "Birth weight predicted by mom's weight",
     xlab = "Mom's weight",
     ylab = "Birth weight")
abline(fit1, col = 'red')

# observe the correlation between birth weight and all the other predictors
cor(babies, birth.weight)
# we can see that gestation has the strongest correlation with birth weight

# observe the relationship between gestation and birth weight
fit2 = lm(formula = birth.weight ~ gestation)
summary(fit2)
# Rsquared is higher on this one, but it's still somewhat low

# plot the linear regression
plot(gestation, birth.weight, type = 'p', pch = 16, col ='black',
     main = "Birth weight predicted by gestation period",
     xlab = "Gestation period",
     ylab = "Birth weight")
abline(fit2, col = 'red')

# plot the cook's distances to identify outliers
case.numbers = 1:length(gestation)
influence = cooks.distance(fit2)
plot(case.numbers, influence)
# there are 2 big outliers to find

# apply identify to the linear regression plot to find the row index of these outliers
identify(gestation, birth.weight, tolerance = 5)
# row indices 239 and 820 need to be removed

# remove these outliers as a new dataset
trimbabies = babies[c(-239,-820),]
# attach this new dataset instead
detach(babies)
attach(trimbabies)

# repeat the linear regression for gestation and birth weight to check for improvements
fit2improved = lm(formula = birth.weight ~ gestation)
summary(fit2improved)
# our Rsquared is slightly higher, signifying an improvement

# run a multiple linear regression with gestation and smoking
fit3 = lm(formula = birth.weight ~ gestation + mom.smokes)
summary(fit3)

# we can run some diagnostics on this linear model
par(mfrow = c(2,2))
plot(fit3)



# load and observe the email data
load("email.Rdata")
attach(email)
dim(email)
View(email)

# fit a logistic regression model between spam and to_multiple
modelfit = glm(formula = spam ~ to_multiple, family = binomial, data = email)
summary(modelfit)

# repeat, with all predictors
modelfit2 = glm(formula = spam ~ ., family = binomial, data = email)
summary(modelfit2)
# they seem to have a correlation

# we can plot the predicted probabilities given by our logistic regression
# the values are given in modelfit2$fitted.values

# a boxplot
plot(x = factor(spam, labels = c("not spam", "spam")),
  y = modelfit2$fitted.values)

# a jitterplot
# set some vertical randomness to ensure points arent stacked on top of each other
jitter = rnorm(nrow(email), sd = 0.09)
plot(x = modelfit2$fitted.values,
     y = spam + jitter, xlab = "Predicted probability", ylab = "",
     xlim = 0:1, ylim = c(-0.5,1.5),
     col = adjustcolor("darkblue", alpha = 0.2), pch = 16, axes = FALSE)
axis(1)
axis(2, at = c(0,1), labels = c("not spam", "spam"))










