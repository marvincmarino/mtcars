## Coursera Project Work ##

# question 1: is an automatic or manual transmission better for MPG?
# question 2: quantify the MPG difference between automatic and manual transmission

# answer 1:  - manual transmission is better then automatic for mpg
# answer 2: strategies:
# - estimate 2 models splitted by am
# - estimate 1 model and interpret the dummy am

library(leaps)

data(mtcars)

#### import checks ####
head(mtcars)
dim(mtcars)
str(mtcars)
names(mtcars)
pairs(mtcars)

#### generate subset: automatic and manual cars ####
cars_auto = subset(mtcars, am == 0)
cars_manu = subset(mtcars, am == 1)

# dimensions
dim(mtcars)
dim(cars_auto); dim(cars_manu)

#### *** univariate mpg analysis #####

# sample means mpg by group
mean(cars_auto$mpg); mean(cars_manu$mpg)
sd(cars_auto$mpg); sd(cars_manu$mpg)

# % increase in mpg based on the mean
(mean(cars_manu$mpg) - mean(cars_auto$mpg))/mean(cars_auto$mpg)

#### mpg plots ####
par(mfrow = c(2, 1))
hist(cars_auto$mpg)
abline(v = mean(cars_auto$mpg), col = "red")
hist(cars_manu$mpg)
abline(v = mean(cars_manu$mpg), col = "red")

# CON: mean of manu is higher but has also higher variance

#### 95% confidence interval for mean difference ####

# Question: is the unconditional mean difference significant?
t.test(cars_manu$mpg, cars_auto$mpg, paired = F, var.equal = F)

#### Conclusions ####
# the 95% interval does not contain 0 >> the (unconditional) mean difference is significant at 95% / p-value 0.1% 


#### Permutation test ####
# what if I shuffle the am groups and calculate the mean?

# get target variable and group vectors
y = mtcars$mpg
group = mtcars$am
y; group

# baseline group means and difference
baselineMeans = tapply(mtcars$mpg, mtcars$am, mean)
baselineMeansDiff = baselineMeans[2] - baselineMeans[1]

tStat = function(w, g) mean(w[g == 1]) - mean(w[g == 0])
observedDiff = tStat(y, group)

# check if function works - should be 0:
baselineMeansDiff - observedDiff

# execute shuffle:
permutations = sapply(1:100000, function(i) tStat(y, sample(group)))

# shuffle experiment results plots:
par(mfrow = c(2, 1), mar = c(4, 4, 2, 2))
hist(permutations, main = "Distribution of shuffled group mean differences") # distribution of difference of averages of permuted groups
plot(permutations, type = "b", main = "Shuffled group mean trials", xlab = "trial", ylab = "shuffled group mean differences", ylim = c(-14, 14))
abline(h = observedDiff, col = "red", lwd = 3)

# there is not even 1 case where by chance I get a difference value greater than the observed!
mean(permutations > observedDiff)


#### *** bivariate analysis ####

#### mpg / hp ####
# visual exploration
par(mfrow = c(1, 1))
with(mtcars, plot(hp, mpg, type = "n")) # no data
with(cars_auto, points(hp, mpg, col = "red", pch = 20))
with(cars_manu, points(hp, mpg, col = "blue", pch = 20))
legend("topright", pch = 20, col = c("red", "blue"), legend = c("auto", "manu")) # add legend

# OBS: seems like slope is same, but there is a shift in the intercepts

# estimate group models
model1_auto = lm(mpg ~ hp, data = cars_auto)
model1_manu = lm(mpg ~ hp, data = cars_manu)
summary(model1_auto); summary(model1_manu)

# model without extreme values for manu
cars_manuRed = cars_manu[cars_manu$hp < 250, ]; dim(cars_manu); dim(cars_manuRed)
model1_manuRed = lm(mpg ~ hp, data = cars_manuRed)

# plot models
par(mfrow = c(1, 1))
with(mtcars, plot(hp, mpg, type = "n", main = "Regression mpg vs. hp - by transmission type")) # no data
with(cars_auto, points(hp, mpg, col = "red", pch = 20))
with(cars_manu, points(hp, mpg, col = "blue", pch = 20))
legend("topright", pch = 20, col = c("red", "blue"), legend = c("auto", "manu")) # add legend
abline(model1_auto, col = "red", lwd = 2)
abline(model1_manu, col = "blue", lwd = 2)
abline(model1_manuRed, col = "blue", lty = 5)
abline(v = 175, lty = 2)

#### Conclusions: 
# given an hp level, manu cars have higher higher mpg - compared to auto

#### mpg / wt ####
# visual exploration
par(mfrow = c(1, 1))
with(mtcars, plot(wt, mpg, type = "n", main = "mpg vs. weight - by transmission type")) # no data
with(cars_auto, points(wt, mpg, col = "red", pch = 20))
with(cars_manu, points(wt, mpg, col = "blue", pch = 20))
legend("topright", pch = 20, col = c("red", "blue"), legend = c("auto", "manu")) # add legend
abline(v = 3.2, lty = 2)

# OBS: seems like heavy cars are all auto / manu are lighter

#### mpg / drat ####
# visual exploration
par(mfrow = c(1, 1))
with(mtcars, plot(drat, mpg, type = "n", main = "mpg vs. drat - by transmission type")) # no data
with(cars_auto, points(drat, mpg, col = "red", pch = 20))
with(cars_manu, points(drat, mpg, col = "blue", pch = 20))
legend("topright", pch = 20, col = c("red", "blue"), legend = c("auto", "manu")) # add legend

# OBS: seems 2 different behavior for auto / manu

# estimate group models
model1_auto = lm(mpg ~ drat, data = cars_auto)
model1_manu = lm(mpg ~ drat, data = cars_manu)
summary(model1_auto); summary(model1_manu)
plot(model1_auto); plot(model1_manu)

# plot models
par(mfrow = c(1, 1))
with(mtcars, plot(drat, mpg, type = "n", main = "mpg vs. drat - by transmission type")) # no data
with(cars_auto, points(drat, mpg, col = "red", pch = 20))
with(cars_manu, points(drat, mpg, col = "blue", pch = 20))
legend("topright", pch = 20, col = c("red", "blue"), legend = c("auto", "manu")) # add legend
abline(model1_auto, col = "red", lwd = 2)
abline(model1_manu, col = "blue", lwd = 2)
abline(v = 175, lty = 2)

#### mpg / disp ####
# visual exploration
par(mfrow = c(1, 1))
with(mtcars, plot(disp, mpg, type = "n", main = "mpg vs. disp - by transmission type")) # no data
with(cars_auto, points(disp, mpg, col = "red", pch = 20))
with(cars_manu, points(disp, mpg, col = "blue", pch = 20))
legend("topright", pch = 20, col = c("red", "blue"), legend = c("auto", "manu")) # add legend
labels = with(mtcars, paste(as.character(disp), as.character(mpg), sep = ",")) # generate point labels
with(mtcars, text(disp, mpg, labels = labels, cex = 0.7, pos = 2)) 

# OBS: seems like auto cars have higher displacement
abline(v = 167.6, lty = 2)

#### mpg / cyl ####
# visual exploration
par(mfrow = c(1, 1))
with(mtcars, plot(cyl, mpg, type = "n")) # no data
with(cars_auto, points(cyl, mpg, col = "red", pch = 20))
with(cars_manu, points(cyl, mpg, col = "blue", pch = 20))
legend("topright", pch = 20, col = c("red", "blue"), legend = c("auto", "manu")) # add legend

# OBS: you have almost perfect separation: low cyl are manu / many cyl are auto


#### 4 bivariate analysis: hp / wt / drat / disp ####
par(mfrow = c(2, 2), mar = c(2, 3, 2, 3))

# plot1
with(mtcars, plot(hp, mpg, type = "n", main = "mpg vs. hp - by transmission type")) # no data
with(cars_auto, points(hp, mpg, col = "red", pch = 20))
with(cars_manu, points(hp, mpg, col = "blue", pch = 20))
legend("topright", pch = 20, col = c("red", "blue"), legend = c("auto", "manu")) # add legend
model1_auto = lm(mpg ~ hp, data = cars_auto)
model1_manu = lm(mpg ~ hp, data = cars_manu)
abline(model1_auto, col = "red", lwd = 2)
abline(model1_manu, col = "blue", lwd = 2)
abline(v = 175, lty = 2)

# plot2
with(mtcars, plot(wt, mpg, type = "n", main = "mpg vs. weight - by transmission type")) # no data
with(cars_auto, points(wt, mpg, col = "red", pch = 20))
with(cars_manu, points(wt, mpg, col = "blue", pch = 20))
legend("topright", pch = 20, col = c("red", "blue"), legend = c("auto", "manu")) # add legend
abline(v = 3.2, lty = 2)

# plot 3
with(mtcars, plot(drat, mpg, type = "n", main = "mpg vs. drat - by transmission type")) # no data
with(cars_auto, points(drat, mpg, col = "red", pch = 20))
with(cars_manu, points(drat, mpg, col = "blue", pch = 20))
legend("topright", pch = 20, col = c("red", "blue"), legend = c("auto", "manu")) # add legend
model2_auto = lm(mpg ~ drat, data = cars_auto)
model2_manu = lm(mpg ~ drat, data = cars_manu)
abline(model2_auto, col = "red", lwd = 2)
abline(model2_manu, col = "blue", lwd = 2)
abline(v = 175, lty = 2)

# plot 4
with(mtcars, plot(disp, mpg, type = "n", main = "mpg vs. disp - by transmission type")) # no data
with(cars_auto, points(disp, mpg, col = "red", pch = 20))
with(cars_manu, points(disp, mpg, col = "blue", pch = 20))
legend("topright", pch = 20, col = c("red", "blue"), legend = c("auto", "manu")) # add legend
labels = with(mtcars, paste(as.character(disp), as.character(mpg), sep = ",")) # generate point labels
with(mtcars, text(disp, mpg, labels = labels, cex = 0.7, pos = 2))
abline(v = 167.6, lty = 2)



#### *** regressions ####

### analyse covariance matrix for regressor selection:
z <- cor(mtcars)
require(lattice)
levelplot(z)

#### lm with subgroups by transmission type ####

# prepare: remove am from both subsets
namesColumnDrop = c("am")
dim(cars_auto)
cars_auto_reg = cars_auto[, !names(cars_auto) %in% namesColumnDrop]
cars_manu_reg = cars_manu[, !names(cars_manu) %in% namesColumnDrop]
dim(cars_auto_reg); dim(cars_manu_reg)

# model estimation / with all regressors
model2_auto = lm(mpg ~ ., data = cars_auto_reg)
model2_manu = lm(mpg ~ ., data = cars_manu_reg)

summary(model2_auto); summary(model2_manu)
plot(model2_auto); plot(model2_manu)

#### lm with all variables / no split ####
# prepare data
data = mtcars
data$am = as.factor(data$am)

model3 = lm(mpg ~ ., data = data)

summary(model3)
plot(model3)
hist(model3$residuals)

#### lm with logs #####
# good link for interpreting models with log:
# http://www.ats.ucla.edu/stat/mult_pkg/faq/general/log_transformed_regression.htm

data$log_mpg = log(data$mpg) # add log of y
data = data[, -1] # remove normal

# model with all variables
model4 = lm(log_mpg ~ ., data = data)
summary(model4)
confint(model4, conf.level = 0.95)

exp(model4$coef[9])

#### CON: interpr - you have a 4% increase in mpg when car has manual drive

# try the best subset selection approach
par(mfrow = c(1, 1))
refit.full = regsubsets(log_mpg ~ ., data = data)
summaryBest = summary(refit.full)
plot(summaryBest$cp, xlab = "N# of variables", ylab = "cp", main = "cp vs. n# vars", type = "l")
points(summaryBest$cp, pch = 20)

# estimate model with 3 coef
model5 = lm(log_mpg ~ hp + wt + am, data = data)
summary(model5)
plot(model5)

u_hat = model5$residuals
shapiro.test(u_hat)

confint(model5, conf.level = 0.95)

exp(model5$coef[4])
