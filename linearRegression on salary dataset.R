
# Simple Linear Regression
# Importing the dataset
getwd()
dataset<-read.csv("D:/B.tech 7 Semester/Predictive analysis int 234/Salary_Data.csv")
View(dataset)
str(dataset)
# Splitting the dataset into the Training set and Test set
#install.packages('caTools')
#used for running window statistic functions, R/W for GIF and
#ENVI binary file
library(caTools)
split = sample.split(dataset$Salary,
                     SplitRatio = 0.7)
split
#0.7 means 70% training and 30% testing
#Used to split the data used during classification into train and test
#subsets.
#if (0<=Split Ratio<1) then Split Ratio fraction of points from data-set
#will be set toT RUE
trainingset = subset(dataset, split == TRUE)
View(trainingset)
testset = subset(dataset, split == FALSE)
View(testset)
# Fitting Simple Linear Regression to the Training set
lm.r= lm(formula = Salary ~ YearsExperience,
         data = trainingset)
#lm is sued to fit linear models. formula is an object of class formula. provides symbolic
#description of the model to be fitted
coef(lm.r)
#used to extract cofficients from objects
# Predicting the Test set results

ypred = predict(lm.r, newdata = testset)

#install.packages("ggplot2")
library(ggplot2)

# Visualising the Training set results
ggplot() + geom_point(aes(x = trainingset$YearsExperience,
                          y = trainingset$Salary),
                      colour = 'red') +
  geom_line(aes(x = trainingset$YearsExperience,
                y = predict(lm.r, newdata = trainingset)),
            colour = 'blue') +
  
  ggtitle('Salary vs Experience (Training set)') +
  xlab('Years of experience') +
  ylab('Salary')

# Visualising the Test set results
#ggplot() used to declare the input data frame for a graphic. geom_point()
#used to create
#scatterplots. aes() is used for aesthetics such as color, fill, shape
#geom_line() connects the observations in order of the variable
#on the x axis.
ggplot() +
  geom_point(aes(x = testset$YearsExperience, y = testset$Salary),
             colour = 'red') +
  geom_line(aes(x = trainingset$YearsExperience,
                y = predict(lm.r, newdata = trainingset)),
            colour = 'blue') +
  ggtitle('Salary vs Experience (Test set)') +
  xlab('Years of experience') +
  ylab('Salary')


# prediction of salary from Years-Experience

new_experience <- data.frame(YearsExperience = 8.5)
predicted_salary <- predict(lm.r, newdata = new_experience)
predicted_salary
