#Group Case Study - HR Analytics Case Study

# All coding are done in MacOS - Sierra 
# Coding done by Anindya

install.packages("MASS")
install.packages("car")
install.packages("e1071")
install.packages("caret")
install.packages("ggplot2")
install.packages("cowplot")
install.packages("caTools")
install.packages("ROCR")
install.packages("dplyr")

library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(ROCR)
library(dplyr)

#Data Loading and Data cleaning/Preparation
#Load the data into data frames
employee_survey_data<-read.csv("employee_survey_data.csv",stringsAsFactors = F)
manager_survey_data<-read.csv("manager_survey_data.csv",stringsAsFactors = F)
general_data<-read.csv("general_data.csv",stringsAsFactors = F)
in_time <- read.csv("in_time.csv",stringsAsFactors = F)
out_time <- read.csv("out_time.csv",stringsAsFactors = F)

# Structure of the data set
str(employee_survey_data)    
str(manager_survey_data)    
str(general_data)    
str(in_time)    
str(out_time) 

#Renaming employee id column in in_time and out_time data set
colnames(in_time) [1]  <- "EmployeeID"
colnames(out_time) [1] <- "EmployeeID"

#Check for duplicates
length(unique(tolower(employee_survey_data$EmployeeID)))  # 4410, no duplicates 
length(unique(tolower(manager_survey_data$EmployeeID)))  # 4410, no duplicates 
length(unique(tolower(general_data$EmployeeID)))  #4410, no duplicates
length(unique(tolower(in_time$EmployeeID))) #4410, no duplicates
length(unique(tolower(out_time$EmployeeID))) #4410, no duplicates


# Fixing the data quality issues with in_time and out_time
# Remove the holidays
# We have 5 holidays, removing the columns
in_time<-subset(in_time,select = -c(X2015.01.01,X2015.01.14,X2015.01.26,X2015.03.05,X2015.05.01))
out_time<-subset(out_time,select = -c(X2015.01.01,X2015.01.14,X2015.01.26,X2015.03.05,X2015.05.01))

in_time <- sapply(in_time, function(x) as.POSIXlt(x, origin="1970-01-01","%y-%m-%d %H:%M:%S"))
in_time<-as.data.frame(in_time)

out_time <- sapply(out_time, function(x) as.POSIXlt(x, origin="1970-01-01","%y-%m-%d %H:%M:%S"))
out_time<-as.data.frame(out_time)

hours<-out_time-in_time
hours<-sapply(hours,function(x) as.numeric(x))

hours<-as.data.frame(hours)

EmployeeID<-seq(from = 1, to = 4410, by = 1)

#aggregating mean of each row #roll up
hours$AvgWorkHrs<-apply(hours,1,mean,na.rm=TRUE)

#creating Average work hours per employeee data frame
AvgWorkHrs<-cbind(EmployeeID,hours$AvgWorkHrs)
AvgWorkHrs<-as.data.frame(AvgWorkHrs)

colnames(AvgWorkHrs)[2]<-"AverageHours"

# Checking for differences in Employee Id in the data frames
setdiff(employee_survey_data$EmployeeID,manager_survey_data$EmployeeID)
setdiff(employee_survey_data$EmployeeID,general_data$EmployeeID)
setdiff(employee_survey_data$EmployeeID,AvgWorkHrs$EmployeeID)

# Merge all data frame to creata a master data frame
master_data_frame<-merge(employee_survey_data,manager_survey_data,by="EmployeeID",all=F)
master_data_frame<-merge(master_data_frame,general_data, by="EmployeeID", all=F)
master_data_frame<-merge(master_data_frame,AvgWorkHrs, by="EmployeeID", all=F)

str(master_data_frame)

# Converting numerical attributes to categorical values from data dictionary
master_data_frame$EnvironmentSatisfaction[which(master_data_frame$EnvironmentSatisfaction==1)]<-'Low'
master_data_frame$EnvironmentSatisfaction[which(master_data_frame$EnvironmentSatisfaction==2)]<-'Medium'
master_data_frame$EnvironmentSatisfaction[which(master_data_frame$EnvironmentSatisfaction==3)]<-'High'
master_data_frame$EnvironmentSatisfaction[which(master_data_frame$EnvironmentSatisfaction==4)]<-'Very High'

master_data_frame$JobSatisfaction[which(master_data_frame$JobSatisfaction==1)]<-'Low'
master_data_frame$JobSatisfaction[which(master_data_frame$JobSatisfaction==2)]<-'Medium'
master_data_frame$JobSatisfaction[which(master_data_frame$JobSatisfaction==3)]<-'High'
master_data_frame$JobSatisfaction[which(master_data_frame$JobSatisfaction==4)]<-'Very High'

master_data_frame$WorkLifeBalance[which(master_data_frame$WorkLifeBalance==1)]<-'Bad'
master_data_frame$WorkLifeBalance[which(master_data_frame$WorkLifeBalance==2)]<-'Good'
master_data_frame$WorkLifeBalance[which(master_data_frame$WorkLifeBalance==3)]<-'Better'
master_data_frame$WorkLifeBalance[which(master_data_frame$WorkLifeBalance==4)]<-'Best'

master_data_frame$JobInvolvement[which(master_data_frame$JobInvolvement==1)]<-'Low'
master_data_frame$JobInvolvement[which(master_data_frame$JobInvolvement==2)]<-'Medium'
master_data_frame$JobInvolvement[which(master_data_frame$JobInvolvement==3)]<-'High'
master_data_frame$JobInvolvement[which(master_data_frame$JobInvolvement==4)]<-'Very High'

master_data_frame$PerformanceRating[which(master_data_frame$PerformanceRating==1)]<-'Low'
master_data_frame$PerformanceRating[which(master_data_frame$PerformanceRating==2)]<-'Good'
master_data_frame$PerformanceRating[which(master_data_frame$PerformanceRating==3)]<-'Excellent'
master_data_frame$PerformanceRating[which(master_data_frame$PerformanceRating==4)]<-'Outstanding'

master_data_frame$Education[which(master_data_frame$Education==1)]<-'Below College'
master_data_frame$Education[which(master_data_frame$Education==2)]<-'College'
master_data_frame$Education[which(master_data_frame$Education==3)]<-'Bachelor'
master_data_frame$Education[which(master_data_frame$Education==4)]<-'Master'
master_data_frame$Education[which(master_data_frame$Education==5)]<-'Doctor'

# Removing the columns which has only one value
master_data_frame$EmployeeCount<-NULL
master_data_frame$Over18<-NULL
master_data_frame$StandardHours<-NULL

# Understanding the structure of the final master file
str(master_data_frame)

# EDA - Categorical variables
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")

plot_grid(ggplot(master_data_frame, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar(), 
          ggplot(master_data_frame, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(master_data_frame, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(master_data_frame, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h") 


plot_grid(ggplot(master_data_frame, aes(x=JobInvolvement,fill=Attrition))+ geom_bar(), 
          ggplot(master_data_frame, aes(x=PerformanceRating,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(master_data_frame, aes(x=JobRole,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(master_data_frame, aes(x=EducationField,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h") 

plot_grid(ggplot(master_data_frame, aes(x=BusinessTravel,fill=Attrition))+ geom_bar(), 
          ggplot(master_data_frame, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(master_data_frame, aes(x=Education,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(master_data_frame, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h") 

# Histogram and Boxplots for numeric variables 
# Removing the outliers in the numeric attributes

box_theme2<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())


plot_grid(ggplot(master_data_frame, aes(Age))+ geom_histogram(binwidth = 10),
          ggplot(master_data_frame, aes(x="",y=Age))+ geom_boxplot(width=0.1)
          +coord_flip()+box_theme2, align = "v",ncol = 1)

plot_grid(ggplot(master_data_frame, aes(DistanceFromHome))+ geom_histogram(binwidth = 10),
          ggplot(master_data_frame, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)
          +coord_flip()+box_theme2, align = "v",ncol = 1)

plot_grid(ggplot(master_data_frame, aes(JobLevel))+ geom_histogram(binwidth = 10),
          ggplot(master_data_frame, aes(x="",y=JobLevel))+ geom_boxplot(width=0.1)
          +coord_flip()+box_theme2, align = "v",ncol = 1)

plot_grid(ggplot(master_data_frame, aes(MonthlyIncome))+ geom_histogram(binwidth = 10),
          ggplot(master_data_frame, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)
          +coord_flip()+box_theme2, align = "v",ncol = 1)
#outliers exists. Removing them
boxplot1<-boxplot(master_data_frame$MonthlyIncome)
master_data_frame <- master_data_frame[!master_data_frame$MonthlyIncome %in% boxplot1$out,]


plot_grid(ggplot(master_data_frame, aes(NumCompaniesWorked))+ geom_histogram(binwidth = 10),
          ggplot(master_data_frame, aes(x="",y=NumCompaniesWorked))+ geom_boxplot(width=0.1)
          +coord_flip()+box_theme2, align = "v",ncol = 1)


plot_grid(ggplot(master_data_frame, aes(PercentSalaryHike))+ geom_histogram(binwidth = 10),
          ggplot(master_data_frame, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)
          +coord_flip()+box_theme2, align = "v",ncol = 1)

plot_grid(ggplot(master_data_frame, aes(StockOptionLevel))+ geom_histogram(binwidth = 10),
          ggplot(master_data_frame, aes(x="",y=StockOptionLevel))+ geom_boxplot(width=0.1)
          +coord_flip()+box_theme2, align = "v",ncol = 1)

plot_grid(ggplot(master_data_frame, aes(TotalWorkingYears))+ geom_histogram(binwidth = 10),
          ggplot(master_data_frame, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)
          +coord_flip()+box_theme2, align = "v",ncol = 1)

#outliers exists. Removing them
boxplot1<-boxplot(master_data_frame$TotalWorkingYears)
master_data_frame <- master_data_frame[!master_data_frame$TotalWorkingYears %in% boxplot1$out,]


plot_grid(ggplot(master_data_frame, aes(TrainingTimesLastYear))+ geom_histogram(binwidth = 10),
          ggplot(master_data_frame, aes(x="",y=TrainingTimesLastYear))+ geom_boxplot(width=0.1)
          +coord_flip()+box_theme2, align = "v",ncol = 1)
#outliers exists. Removing them
boxplot1<-boxplot(master_data_frame$TrainingTimesLastYear)
master_data_frame <- master_data_frame[!master_data_frame$TrainingTimesLastYear %in% boxplot1$out,]


plot_grid(ggplot(master_data_frame, aes(YearsAtCompany))+ geom_histogram(binwidth = 10),
          ggplot(master_data_frame, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)
          +coord_flip()+box_theme2, align = "v",ncol = 1)
#outliers exists. Removing them
boxplot1<-boxplot(master_data_frame$YearsAtCompany)
master_data_frame <- master_data_frame[!master_data_frame$YearsAtCompany %in% boxplot1$out,]



plot_grid(ggplot(master_data_frame, aes(YearsSinceLastPromotion))+ geom_histogram(binwidth = 10),
          ggplot(master_data_frame, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)
          +coord_flip()+box_theme2, align = "v",ncol = 1)

#outliers exists. Removing them
boxplot1<-boxplot(master_data_frame$YearsSinceLastPromotion)
master_data_frame <- master_data_frame[!master_data_frame$YearsSinceLastPromotion %in% boxplot1$out,]


plot_grid(ggplot(master_data_frame, aes(YearsWithCurrManager))+ geom_histogram(binwidth = 10),
          ggplot(master_data_frame, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)
          +coord_flip()+box_theme2, align = "v",ncol = 1)
#outliers exists. Removing them
boxplot1<-boxplot(master_data_frame$YearsWithCurrManager)
master_data_frame <- master_data_frame[!master_data_frame$YearsWithCurrManager %in% boxplot1$out,]




plot_grid(ggplot(master_data_frame, aes(AverageHours))+ geom_histogram(binwidth = 10),
          ggplot(master_data_frame, aes(x="",y=AverageHours))+ geom_boxplot(width=0.1)
          +coord_flip()+box_theme2, align = "v",ncol = 1)

#outliers exists. Removing them
boxplot1<-boxplot(master_data_frame$AverageHours)
master_data_frame <- master_data_frame[!master_data_frame$AverageHours %in% boxplot1$out,]


str(master_data_frame)

# Normalize continuous variables
# Scaling them for logistic regression
master_data_frame$Age<- scale(master_data_frame$Age) 
master_data_frame$DistanceFromHome<- scale(master_data_frame$DistanceFromHome) 
master_data_frame$JobLevel<- scale(master_data_frame$JobLevel) 
master_data_frame$MonthlyIncome<- scale(master_data_frame$MonthlyIncome) 
master_data_frame$NumCompaniesWorked<- scale(master_data_frame$NumCompaniesWorked) 
master_data_frame$PercentSalaryHike<- scale(master_data_frame$PercentSalaryHike) 
master_data_frame$StockOptionLevel<- scale(master_data_frame$StockOptionLevel) 
master_data_frame$TotalWorkingYears<- scale(master_data_frame$TotalWorkingYears) 
master_data_frame$TrainingTimesLastYear<- scale(master_data_frame$TrainingTimesLastYear) 
master_data_frame$YearsAtCompany<- scale(master_data_frame$YearsAtCompany) 
master_data_frame$YearsSinceLastPromotion<- scale(master_data_frame$YearsSinceLastPromotion) 
master_data_frame$YearsWithCurrManager<- scale(master_data_frame$YearsWithCurrManager) 
master_data_frame$AverageHours<- scale(master_data_frame$AverageHours)

# Checking attrition rate 
master_data_frame$Attrition<- ifelse(master_data_frame$Attrition=="Yes",1,0)
attrition_rate <- sum(master_data_frame$Attrition)/nrow(master_data_frame)
attrition_rate # 16.54%

# Removing NA from the categorical data set
sapply(master_data_frame, function(x) sum(is.na(x))) 

master_data_frame <- master_data_frame[!is.na(master_data_frame$EnvironmentSatisfaction),]
master_data_frame <- master_data_frame[!is.na(master_data_frame$JobSatisfaction),]
master_data_frame <- master_data_frame[!is.na(master_data_frame$WorkLifeBalance),]
master_data_frame <- master_data_frame[!is.na(master_data_frame$NumCompaniesWorked),]
master_data_frame <- master_data_frame[!is.na(master_data_frame$TotalWorkingYears),]



master_data_frame_cat<-master_data_frame[,c(2,3,4,5,6,9,10,12,13,14,16,17)]

# converting categorical attributes to factor
master_data_frame_categorical<- data.frame(sapply(master_data_frame_cat, function(x) factor(x)))
str(master_data_frame_categorical)

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(master_data_frame_categorical, function(x) 
  data.frame(model.matrix(~x-1,data = master_data_frame_categorical))))


hr_analytics_data_frame<-cbind(master_data_frame[,c(7,8,11,15,18,19,20,21,22,23,24,25,26,27)],dummies) 
View(hr_analytics_data_frame)


# splitting the data between train and test
set.seed(100)

indices = sample.split(hr_analytics_data_frame$Attrition, SplitRatio = 0.7)
train = hr_analytics_data_frame[indices,]
test = hr_analytics_data_frame[!(indices),]


# Model building
#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) # AIC: 1187.5


# Stepwise selection
model_2<- stepAIC(model_1, direction="both")
summary(model_2) #AIC: 1157.8

# Removing multicollinearity through VIF check
vif(model_2)

# All variables have significantly low VIF, so checking for p values
#Removing JobLevel

model_3<-glm(formula = Attrition ~ Age + DistanceFromHome + 
               NumCompaniesWorked + TotalWorkingYears + YearsWithCurrManager + 
               AverageHours + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + 
               EnvironmentSatisfaction.xMedium + JobSatisfaction.xHigh + 
               JobSatisfaction.xLow + JobSatisfaction.xMedium + WorkLifeBalance.xBad + 
               WorkLifeBalance.xBetter + PerformanceRating.xExcellent + 
               BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
               Department.xHuman.Resources + Education.xBachelor + Education.xCollege + 
               EducationField.xHuman.Resources + EducationField.xLife.Sciences + 
               EducationField.xMedical + JobRole.xLaboratory.Technician + 
               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
               JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried, 
             family = "binomial", data = train)

summary(model_3) 
#AIC: 1158.1.Not much change, so its fine
vif(model_3)


# Removing WorkLifeBalance.xBad

model_4<-glm(formula = Attrition ~ Age + DistanceFromHome + 
               NumCompaniesWorked + TotalWorkingYears + YearsWithCurrManager + 
               AverageHours + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + 
               EnvironmentSatisfaction.xMedium + JobSatisfaction.xHigh + 
               JobSatisfaction.xLow + JobSatisfaction.xMedium + 
               WorkLifeBalance.xBetter + PerformanceRating.xExcellent + 
               BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
               Department.xHuman.Resources + Education.xBachelor + Education.xCollege + 
               EducationField.xHuman.Resources + EducationField.xLife.Sciences + 
               EducationField.xMedical + JobRole.xLaboratory.Technician + 
               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
               JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried, 
             family = "binomial", data = train)

summary(model_4) 
#AIC: 1158.9 ,Not much change, so its fine
vif(model_4)

# Removing EducationField.xHuman.Resources
model_5<-glm(formula = Attrition ~ Age + DistanceFromHome + 
               NumCompaniesWorked + TotalWorkingYears + YearsWithCurrManager + 
               AverageHours + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + 
               EnvironmentSatisfaction.xMedium + JobSatisfaction.xHigh + 
               JobSatisfaction.xLow + JobSatisfaction.xMedium + 
               WorkLifeBalance.xBetter + PerformanceRating.xExcellent + 
               BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
               Department.xHuman.Resources + Education.xBachelor + Education.xCollege + 
              EducationField.xLife.Sciences + 
               EducationField.xMedical + JobRole.xLaboratory.Technician + 
               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
               JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried, 
             family = "binomial", data = train)

summary(model_5) 
#AIC: 1160 ,Not much change, so its fine
vif(model_5)

# Removing Education.xBachelor
model_6<-glm(formula = Attrition ~ Age + DistanceFromHome + 
               NumCompaniesWorked + TotalWorkingYears + YearsWithCurrManager + 
               AverageHours + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + 
               EnvironmentSatisfaction.xMedium + JobSatisfaction.xHigh + 
               JobSatisfaction.xLow + JobSatisfaction.xMedium + 
               WorkLifeBalance.xBetter + PerformanceRating.xExcellent + 
               BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
               Department.xHuman.Resources + Education.xCollege + 
               EducationField.xLife.Sciences + 
               EducationField.xMedical + JobRole.xLaboratory.Technician + 
               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
               JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried, 
             family = "binomial", data = train)

summary(model_6) 
#AIC: 1161.5 ,Not much change, so its fine
vif(model_6)


# Removing Education.xCollege
model_7<-glm(formula = Attrition ~ Age + DistanceFromHome + 
               NumCompaniesWorked + TotalWorkingYears + YearsWithCurrManager + 
               AverageHours + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + 
               EnvironmentSatisfaction.xMedium + JobSatisfaction.xHigh + 
               JobSatisfaction.xLow + JobSatisfaction.xMedium + 
               WorkLifeBalance.xBetter + PerformanceRating.xExcellent + 
               BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
               Department.xHuman.Resources + 
               EducationField.xLife.Sciences + 
               EducationField.xMedical + JobRole.xLaboratory.Technician + 
               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
               JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried, 
             family = "binomial", data = train)

summary(model_7) 
#AIC: 1161.6 ,Not much change, so its fine
vif(model_7)


# Removing DistanceFromHome

model_8<-glm(formula = Attrition ~ Age + 
               NumCompaniesWorked + TotalWorkingYears + YearsWithCurrManager + 
               AverageHours + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + 
               EnvironmentSatisfaction.xMedium + JobSatisfaction.xHigh + 
               JobSatisfaction.xLow + JobSatisfaction.xMedium + 
               WorkLifeBalance.xBetter + PerformanceRating.xExcellent + 
               BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
               Department.xHuman.Resources + 
               EducationField.xLife.Sciences + 
               EducationField.xMedical + JobRole.xLaboratory.Technician + 
               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
               JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried, 
             family = "binomial", data = train)

summary(model_8) 
#AIC: 1163.3 ,Not much change, so its fine
vif(model_8)


# Removing JobRole.xResearch.Scientist
model_9<-glm(formula = Attrition ~ Age + 
               NumCompaniesWorked + TotalWorkingYears + YearsWithCurrManager + 
               AverageHours + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + 
               EnvironmentSatisfaction.xMedium + JobSatisfaction.xHigh + 
               JobSatisfaction.xLow + JobSatisfaction.xMedium + 
               WorkLifeBalance.xBetter + PerformanceRating.xExcellent + 
               BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
               Department.xHuman.Resources + 
               EducationField.xLife.Sciences + 
               EducationField.xMedical + JobRole.xLaboratory.Technician + 
               JobRole.xResearch.Director + 
               JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried, 
             family = "binomial", data = train)

summary(model_9) 
#AIC: 1165.7 ,Not much change, so its fine
vif(model_9)

# Removing JobRole.xLaboratory.Technician
model_10<-glm(formula = Attrition ~ Age + 
               NumCompaniesWorked + TotalWorkingYears + YearsWithCurrManager + 
               AverageHours + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + 
               EnvironmentSatisfaction.xMedium + JobSatisfaction.xHigh + 
               JobSatisfaction.xLow + JobSatisfaction.xMedium + 
               WorkLifeBalance.xBetter + PerformanceRating.xExcellent + 
               BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
               Department.xHuman.Resources + EducationField.xLife.Sciences + 
               EducationField.xMedical  + JobRole.xResearch.Director + 
               JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried, 
             family = "binomial", data = train)

summary(model_10) 
#AIC: 1166.5 ,Not much change, so its fine
vif(model_10)

# Removing JobRole.xSales.Executive

model_11<-glm(formula = Attrition ~ Age + 
                NumCompaniesWorked + TotalWorkingYears + YearsWithCurrManager + 
                AverageHours + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + 
                EnvironmentSatisfaction.xMedium + JobSatisfaction.xHigh + 
                JobSatisfaction.xLow + JobSatisfaction.xMedium + 
                WorkLifeBalance.xBetter + PerformanceRating.xExcellent + 
                BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
                Department.xHuman.Resources + EducationField.xLife.Sciences + 
                EducationField.xMedical  + JobRole.xResearch.Director + 
                MaritalStatus.xDivorced + MaritalStatus.xMarried, 
              family = "binomial", data = train)

summary(model_11) 
#AIC: 1166.3 ,Not much change, so its fine
vif(model_11)

# Removing PerformanceRating.xExcellent

model_12<-glm(formula = Attrition ~ Age + 
                NumCompaniesWorked + TotalWorkingYears + YearsWithCurrManager + 
                AverageHours + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + 
                EnvironmentSatisfaction.xMedium + JobSatisfaction.xHigh + 
                JobSatisfaction.xLow + JobSatisfaction.xMedium + 
                WorkLifeBalance.xBetter + 
                BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
                Department.xHuman.Resources + EducationField.xLife.Sciences + 
                EducationField.xMedical  + JobRole.xResearch.Director + 
                MaritalStatus.xDivorced + MaritalStatus.xMarried, 
              family = "binomial", data = train)

summary(model_12) 
#AIC: 1169.8 ,Not much change, so its fine
vif(model_12)

# Removing BusinessTravel.xNon.Travel

model_13<-glm(formula = Attrition ~ Age + 
                NumCompaniesWorked + TotalWorkingYears + YearsWithCurrManager + 
                AverageHours + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + 
                EnvironmentSatisfaction.xMedium + JobSatisfaction.xHigh + 
                JobSatisfaction.xLow + JobSatisfaction.xMedium + 
                WorkLifeBalance.xBetter + BusinessTravel.xTravel_Frequently + 
                Department.xHuman.Resources + EducationField.xLife.Sciences + 
                EducationField.xMedical  + JobRole.xResearch.Director + 
                MaritalStatus.xDivorced + MaritalStatus.xMarried, 
              family = "binomial", data = train)

summary(model_13) 
#AIC: 1174.3 ,Not much change, so its fine
vif(model_13)

# Removing JobRole.xResearch.Director

model_14<-glm(formula = Attrition ~ Age + 
                NumCompaniesWorked + TotalWorkingYears + YearsWithCurrManager + 
                AverageHours + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + 
                EnvironmentSatisfaction.xMedium + JobSatisfaction.xHigh + 
                JobSatisfaction.xLow + JobSatisfaction.xMedium + 
                WorkLifeBalance.xBetter + BusinessTravel.xTravel_Frequently + 
                Department.xHuman.Resources + EducationField.xLife.Sciences + 
                EducationField.xMedical + 
                MaritalStatus.xDivorced + MaritalStatus.xMarried, 
              family = "binomial", data = train)

summary(model_14) 
#AIC: 1178.2 ,Not much change, so its fine
vif(model_14)

# Removing WorkLifeBalance.xBetter

model_15<-glm(formula = Attrition ~ Age + 
                NumCompaniesWorked + TotalWorkingYears + YearsWithCurrManager + 
                AverageHours + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + 
                EnvironmentSatisfaction.xMedium + JobSatisfaction.xHigh + 
                JobSatisfaction.xLow + JobSatisfaction.xMedium + 
                BusinessTravel.xTravel_Frequently + 
                Department.xHuman.Resources + EducationField.xLife.Sciences + 
                EducationField.xMedical + 
                MaritalStatus.xDivorced + MaritalStatus.xMarried, 
              family = "binomial", data = train)

summary(model_15) 
#AIC: 1183.9 ,Not much change, so its fine
vif(model_15)


# Removing Age

model_16<-glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + YearsWithCurrManager + 
                AverageHours + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + 
                EnvironmentSatisfaction.xMedium + JobSatisfaction.xHigh + 
                JobSatisfaction.xLow + JobSatisfaction.xMedium + 
                BusinessTravel.xTravel_Frequently + 
                Department.xHuman.Resources + EducationField.xLife.Sciences + 
                EducationField.xMedical + 
                MaritalStatus.xDivorced + MaritalStatus.xMarried, 
              family = "binomial", data = train)

summary(model_16) 
#AIC: 1189.5 ,Not much change, so its fine
vif(model_16)

# Removing NumCompaniesWorked

model_17<-glm(formula = Attrition ~ TotalWorkingYears + YearsWithCurrManager + 
                AverageHours + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + 
                EnvironmentSatisfaction.xMedium + JobSatisfaction.xHigh + 
                JobSatisfaction.xLow + JobSatisfaction.xMedium + 
                BusinessTravel.xTravel_Frequently + 
                Department.xHuman.Resources + EducationField.xLife.Sciences + 
                EducationField.xMedical + 
                MaritalStatus.xDivorced + MaritalStatus.xMarried, 
              family = "binomial", data = train)

summary(model_17) 
#AIC: 1192.8 ,Not much change, so its fine
vif(model_17)


# Removing EnvironmentSatisfaction.xMedium

model_18<-glm(formula = Attrition ~ TotalWorkingYears + YearsWithCurrManager + 
                AverageHours + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + 
                JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + 
                BusinessTravel.xTravel_Frequently + 
                Department.xHuman.Resources + EducationField.xLife.Sciences + 
                EducationField.xMedical + 
                MaritalStatus.xDivorced + MaritalStatus.xMarried, 
              family = "binomial", data = train)

summary(model_18) 
#AIC: 1199.8 ,Not much change, so its fine
vif(model_18)

# Removing EnvironmentSatisfaction.xHigh

model_19<-glm(formula = Attrition ~ TotalWorkingYears + YearsWithCurrManager + 
                AverageHours + EnvironmentSatisfaction.xLow + 
                JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + 
                BusinessTravel.xTravel_Frequently + 
                Department.xHuman.Resources + EducationField.xLife.Sciences + 
                EducationField.xMedical + 
                MaritalStatus.xDivorced + MaritalStatus.xMarried, 
              family = "binomial", data = train)

summary(model_19) 
#AIC: 1202.9 ,Not much change, so its fine
vif(model_19)

# Removing EducationField.xMedical

model_20<-glm(formula = Attrition ~ TotalWorkingYears + YearsWithCurrManager + 
                AverageHours + EnvironmentSatisfaction.xLow + 
                JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + 
                BusinessTravel.xTravel_Frequently + 
                Department.xHuman.Resources + EducationField.xLife.Sciences + 
                MaritalStatus.xDivorced + MaritalStatus.xMarried, 
              family = "binomial", data = train)

summary(model_20) 
#AIC: 1208.9 ,Not much change, so its fine
vif(model_20)

# Removing EducationField.xLife.Sciences

model_21<-glm(formula = Attrition ~ TotalWorkingYears + YearsWithCurrManager + 
                AverageHours + EnvironmentSatisfaction.xLow + 
                JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + 
                BusinessTravel.xTravel_Frequently + 
                Department.xHuman.Resources + 
                MaritalStatus.xDivorced + MaritalStatus.xMarried, 
              family = "binomial", data = train)

summary(model_21) 
#AIC: 1216.5 ,Not much change, so its fine
vif(model_21)

# Removing Department.xHuman.Resources
model_22<-glm(formula = Attrition ~ TotalWorkingYears + YearsWithCurrManager + 
                AverageHours + EnvironmentSatisfaction.xLow + 
                JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + 
                BusinessTravel.xTravel_Frequently + 
                MaritalStatus.xDivorced + MaritalStatus.xMarried, 
              family = "binomial", data = train)

summary(model_22) 
#AIC: 1224.2 ,Not much change, so its fine
vif(model_22)

# Removing JobSatisfaction.xHigh
model_23<-glm(formula = Attrition ~ TotalWorkingYears + YearsWithCurrManager + 
                AverageHours + EnvironmentSatisfaction.xLow + 
                JobSatisfaction.xLow + JobSatisfaction.xMedium + 
                BusinessTravel.xTravel_Frequently + 
                MaritalStatus.xDivorced + MaritalStatus.xMarried, 
              family = "binomial", data = train)

summary(model_23) 
#AIC: 1235 ,Not much change, so its fine
vif(model_23)


# Testing predictions
test_pred = predict(model_23, type = "response", 
                    newdata = test[,-1])
summary(test_pred)

test$pred<-test_pred

# Let's use the probability cutoff of 50%.

test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))

table(test_actual_attrition,test_pred_attrition)

# Confusion Matrix
test_pred_attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))

test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf

# Finding optimal cutoff
###################################################

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

summary(test_pred)

s = seq(.01,.80,length=100)
OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
cutoff

# Let's choose a cutoff value of 0.1456 for final model

test_cutoff_attrition <- factor(ifelse(test_pred >=0.1456, "Yes", "No"))
conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")
acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]

acc
sens
spec

# KS -statistic - Test Data

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)

pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)


# Lift & Gain Chart 

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)
