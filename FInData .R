#import data
library(readxl)
library(dplyr)
library(boot)
data <- read_excel("japan_public_corporations_1119.xlsx")
data <- as.data.frame(data)

#omit NA
data <- na.omit(data)

#dependent var
data <- data %>%
  mutate(cash_holding = data$cash/data$at) %>%
  mutate(dividend_pay = ifelse(data$totaldividend > 0, "Yes", "No")) %>%
  mutate(total_asset = log(data$at)) %>% #independent var
  mutate(capex_ratio = data$capexp/data$at) %>%
  mutate(leverage_ratio = data$ltermdebt/data$at) %>%
  mutate(age = log(data$year-data$yearfound+1)) %>%
  mutate(sales = log(data$sales)) %>%
  mutate(ebitda = data$ebitda/data$at)

data$dividend_pay = as.factor(data$dividend_pay) 

write.csv(data,"data.csv", row.names = FALSE)
#Part 1
#winsorize
library(DescTools)

data$cash_holding <- Winsorize(data$cash_holding,probs=c(0.01, 0.99), na.rm = TRUE)
data$total_asset <- Winsorize(data$total_asset,probs=c(0.01, 0.99), na.rm = TRUE)
data$capex_ratio <- Winsorize(data$capex_ratio,probs=c(0.01, 0.99), na.rm = TRUE)
data$leverage_ratio <- Winsorize(data$leverage_ratio,probs=c(0.01, 0.99), na.rm = TRUE)
data$age <- Winsorize(data$age,probs=c(0.01, 0.99), na.rm = TRUE)
data$sales<- Winsorize(data$sales,probs=c(0.01, 0.99), na.rm = TRUE)
data$ebitda <- Winsorize(data$ebitda,probs=c(0.01, 0.99), na.rm = TRUE)

#summary statistics
data <- data %>%
  select(year, cash_holding, dividend_pay, total_asset, capex_ratio, leverage_ratio, age, sales, ebitda)

library(stargazer)
stargazer(as.data.frame(data), type = "latex", summary.stat = c("mean", "median", "sd", "p25", "p75"))

#Part 3(a)
#model cash holdings as a function of the predictors
lm.fits = lm(cash_holding~total_asset+capex_ratio+leverage_ratio+age+sales+ebitda, data=data)
summary(lm.fits)
sqrt(1)
stargazer(lm.fits, type = "latex")


#Part 3(b)
#model dividend payout decision as a function of the predictors
library(ISLR)
glm.fits_b=glm(dividend_pay~total_asset+capex_ratio+leverage_ratio+age+sales+ebitda,data=data,family=binomial)
summary(glm.fits_b)

# show only coefficients
coef(glm.fits_b)

# coefficients with statistics
summary(glm.fits_b)$coef

# get p-values
summary(glm.fits_b)$coef[,4]

glm.probs=predict(glm.fits_b,type="response")
glm.probs[1:5] ## show the 1st five observations (rows) of predicted values

contrasts(data$dividend_pay)

# convert predicted probabilty into class labels
glm.pred=rep("No",nrow(data))
glm.pred[glm.probs>.5]="Yes"

# create a confusion table
table(glm.pred,data$dividend_pay)

# % of predicting correctly the movement [1 - this % = training error rate]
mean(glm.pred==data$dividend_pay)

# convert predicted probabilty into class labels
glm.pred3=rep("No",nrow(data))
glm.pred3[glm.probs>.7]="Yes"

# create a confusion table
table(glm.pred3,data$dividend_pay)

# % of predicting correctly the movement [1 - this % = training error rate]
mean(glm.pred3==data$dividend_pay)

#LDA
library(MASS)
lda.fit_b = lda(dividend_pay~total_asset+capex_ratio+leverage_ratio+age+sales+ebitda, data = data)
lda.fit_b
lda.prob_b = predict(lda.fit_b, type = "response")
lda.prob.class_b = lda.prob_b$class

contrasts(data$dividend_pay)
# Confusion Matrix
table(lda.prob.class_b, data$dividend_pay)

# Test error rate
mean(lda.prob.class_b == data$dividend_pay)

#Part 3(c)

#Split Data

#train
train_c=(data$year<2016)
#test
data.2016 = data[!train_c, ]
dividend_pay.2016 = data$dividend_pay[!train_c]

#Logistic
glm.fits_c <- glm(dividend_pay~total_asset+capex_ratio+leverage_ratio+age+sales+ebitda,data=data,family=binomial,subset=train_c)

glm.probs <- predict(glm.fits_c, data.2016, type = "response")
glm.pred = rep("No", nrow(data.2016))
glm.pred[glm.probs > 0.5] = "Yes"
table(glm.pred, dividend_pay.2016)
mean(glm.pred == dividend_pay.2016)
mean(glm.pred != dividend_pay.2016)

#LDA
lda.fit_c = lda(dividend_pay~total_asset+capex_ratio+leverage_ratio+age+sales+ebitda, data = data, subset = train_c)
lda.fit_c
lda.pred_c = predict(lda.fit_c, data.2016)
names(lda.pred_c)

lda.class_c = lda.pred_c$class
# Confusion Matrix
table(lda.class_c, dividend_pay.2016)

# Test error rate
mean(lda.class_c == dividend_pay.2016)

# Part 4

#Run 10 fold cross validation

data[,6:24] <- sapply(data[,6:24], as.numeric)
#Replace NAs with zeros

data[is.na(data)] = 0

data <- na.omit(data)

data[] <- lapply(data, function(i) if(is.numeric(i)) ifelse(is.infinite(i), 0, i) else i)

library(tidyverse)
library(caret)

k <- 10
fold <- sample(k, nrow(data), replace = TRUE)

set.seed(1)
cv_errors = data.frame(delta1 = 0, delta2 = 0)

for (i in 1:10){
  model_kfold = glm(cash_holding~poly(age,i) + total_asset + ebitda + sales + capex_ratio + leverage_ratio, data=data)
  cv_errors[i, ] = cv.glm(data, model_kfold, K=10)$delta
}
cv_errors

training.samples <- data$age %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data <- data[training.samples, ] 
test.data <- data[-training.samples, ]
# Build the model
model <- lm(cash_holding ~ poly(age, 5, raw = TRUE), data = train.data) # Make predictions
predictions <- model %>% 
  predict(test.data)
# Model performance
data.frame(RMSE = RMSE(predictions, test.data$age), R2 = R2(predictions, test.data$cash_holding))
ggplot(train.data, aes(age, cash_holding) ) + geom_point() + stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE))
