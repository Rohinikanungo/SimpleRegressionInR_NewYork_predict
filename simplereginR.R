#Building a Simple Linear Regression model

# Importing the dataset
dataset = read.csv('Predicting_Salaries.csv')

# Splitting the dataset into the Training set and Test set
#install.packages('caTools')
library(caTools)
set.seed(123)
#75% to train model, 25% =test model
split=sample.split(dataset$AnnualSalary, SplitRatio = 3/4)
training_set=subset(dataset, split==TRUE)
testing_Set=subset(dataset, split==FALSE)
#fITTING simple LINEAR REGRESSION to training and testing set

linearreg= lm(formula = AnnualSalary ~ YearsOfExperience, data=training_set)
#prediction testing set results

y_pred=predict(linearreg, newdata=testing_Set)

#visualising the Training set results
library(ggplot2)
library(scales)

ggplot() +
  geom_point(aes(x=training_set$YearsOfExperience, y=training_set$AnnualSalary),
             colour = 'red') +
  geom_line (aes( x= training_set$YearsOfExperience, y=predict(linearreg, newdata= training_set)),
             colour = 'navy') +
  ggtitle ('Annual Salaries of Data Scientists vs Experience in Years (Training Set)') +
  xlab ('Years of Experience') +
  ylab ('Annual Salary') +
  scale_x_continuous(limits = c(0, 12)) + 
  scale_y_continuous(limits = c(0, 150000)) 

ggplot()+
geom_line (aes( x= training_set$YearsOfExperience, y=predict(linearreg, newdata= training_set)),
           colour = 'navy') +
  ggtitle ('Annual Salaries of Data Scientists vs Experience in Years (Test Set)') +
  xlab ('Years of Experience') +
  ylab ('Annual Salary') 