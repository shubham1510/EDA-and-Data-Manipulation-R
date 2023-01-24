#1
#c
source_path <- 'C:\\Users\\shubh\\Desktop\\MSBA\\Advanced Statistics\\HW1'
setwd(source_path)
#2
#a
data <- read.csv('crx.csv',header = FALSE,na.strings=c("","NA"))
#b
head(data,10)
#c
data['V16']
#3
#a
sapply(data, class)
#b
library(pastecs)
sapply(dplyr::select_if(data, is.numeric),stat.desc)
#c
sapply(dplyr::select_if(data, is.character),unique)
#d
table(data$V16)
print('The target is little skewed with more - class than + class. Balanced Classification technique cannot be applied.')
#e
table(data$V1)
print('There are 12 missing values and the data is heavily skewed with b having the highest distribution.')
#f
library(moments)
kurtosis(data$V8)
print('The kurtosis is extremely positive, hence we can say it is from gaussian distribution')
#g
#i
table(data$V10,data$V16)
prop.table(table(data$V10,data$V16))
prop.table(table(data$V10,data$V16),margin = 1)
prop.table(table(data$V10,data$V16),margin = 2)
#ii
print('Assuming t in V10 implies true that is has undergraduate degree.')
accuracy <- (209 + 297)/690
print('The model would predict 295 with undergraduate degree as + and 395 without undergraduate degree as -.')
sprintf('The overall accuracy will be %.2f%%',accuracy*100)
#4
#a
apply(is.na(data),2,which)
#b
calc_mode <- function(x){
  distinct_values <- unique(x)
  distinct_tabulate <- tabulate(match(x, distinct_values))
  distinct_values[which.max(distinct_tabulate)]
}
library(dplyr)
library(tidyr)
data_categorical <- select_if(data, is.character) %>% mutate(across(everything(), ~replace_na(.x, calc_mode(.x))))
#5
#a
data$V16 <- ifelse(data$V16 == '+',1,0)
#b
library(classInt)
#Replacing NA in V2 with median before creating bins
data$V2[is.na(data$V2)] <- median(data$V2, na.rm=TRUE)
#Creating a new column with 3 bins for V2 using equal widths
x <- classIntervals(data$V2, 3, style = 'equal')
x$brks
x$brks[1]
data$V2_bins <- ifelse(data$V2 < x$brks[2],'Bin 1',ifelse(data$V2 < x$brks[3],'Bin 2','Bin 3'))
#c
data_cont <- dplyr::select_if(data, is.numeric)
library(Hmisc)
hist.data.frame(data_cont)
print('For V2, we apply the min-max scaling. From the distribution we observe, the data is slightly skewed and this will reduce the effect of outliers.')
print('For other numerical data, we observe the distribution is heavily skewed and logarithmic scaling is more relevant.')
data$V2_scaled <- (data$V2 - min(data$V2))/(max(data$V2) - min(data$V2))
data$V3_scaled <- log(data$V3)
data$V8_scaled <- log(data$V8)
data$V11_scaled <- log(data$V11)
data$V14_scaled <- log(data$V14)
data$V15_scaled <- log(data$V15)
#6
#a
boxplot(data$V3 ~ data$V16)
print('The variable V3 has higher values for + category of V16.')
boxplot <- boxplot(data$V3)
outliers <- boxplot$out
min(outliers)
for (i in 1:nrow(data)) {
  if (data[i,'V3'] >= min(outliers)) {
    print(i)
  }
}
#b
plot(data$V2,data$V3)
abline(lm(data$V3 ~ data$V2),col = 'red')
print('The regression line shows the expected value (mean value) of V3 at given V2. V2 is not a good predictor for V3 and some transformation may be required.')
#c
plot(density(data$V2))
#7
#a
cor(data$V3,data$V15)
#b
library(corrplot)
#Replacing NA in V14 with median before creating correlation plots
data_cont$V14[is.na(data_cont$V14)] <- median(data_cont$V14, na.rm=TRUE)
corrplot(cor(data_cont),method = 'number')
print('No two continuous variables are highly correlated. Hence, no variable seems redundant.')
#c
print('If two variables had been highly correlated, then using one of them or creating a transformed variable using the correlated variables would have been ideal for use in any ML algorithm.')

#1
df <- ISLR::Auto
model1 <- lm(mpg ~ horsepower,data = df)
summary(model1)
print('From the model we can see mpg and horsepower have a strong negative linear relationship. p-value for horsepower is very low and statistically significant.')
#2
plot(df$horsepower,df$mpg)
abline(lm(mpg ~ horsepower,data = df),col = 'red')
#3
predictions <- predict(model1)
residuals_square <- model1$residuals^2
abs_residuals <- abs(model1$residuals)
plot(predictions,abs_residuals)
plot(df$horsepower,abs_residuals)
print('After plotting absolute residuals against fitted values and against independent variable (x) horsepower, we observe a conical pattern indicating heteroscedasticity.')
#4
confidence_interval <- predict(model1, newdata = data.frame(horsepower = 98), interval = 'confidence',level = .95)
prediction_interval <- predict(model1, newdata = data.frame(horsepower = 98), interval = 'prediction',level = .95)
sprintf('The predicted mpg at 98 horsepower is %.2f',confidence_interval[1])
sprintf('The confidence interval for mpg prediction at 0.95 confidence level at 98 horsepower is between %.2f and %.2f',confidence_interval[2],confidence_interval[3])
sprintf('The prediction interval for mpg prediction at 0.95 confidence level at 98 horsepower is between %.2f and %.2f',prediction_interval[2],prediction_interval[3])
