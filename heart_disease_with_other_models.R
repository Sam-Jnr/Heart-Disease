#importing the required packages to your workspace


install.packages("tidyverse")
require(tidyverse)  # For data cleaning, sorting, and visualization
install.packages("DataExplorer") # For Exploratory Data Analysis  
require(DataExplorer)
install.packages("ggpubr") # To prepare publication-ready plots
require(ggpubr)
require(gridExtra) # To plot several plots in one figure
install.packages("GGally")
install.packages("caTools")
library(caTools) # For classification model
require(GGally) # For correlations
require(rpart) # For classification model
require(rpart.plot)
install.packages("rattle")
library(rattle) # Plot nicer descision trees
require(randomForest) # For Random Forest model
install.packages("aod") #For Logistic Regression
require(aod)
install.packages("ROCR") #To plot ROC curve
require(ROCR)
install.packages("e1071") #For Support Vector Machines
require(e1071)
install.packages("dplyr") # package installations are only needed the first time you use it
require(dplyr) # needs to be run every time you start R and want to use %>%

#if issue occurs with code it means the above packages were not installed correctly so try re-installing them

# importing the dataset and creating a backup of it, at the same time
data <- read.csv("C:/Users/x16331191/Downloads/OneDrive_2020-08-04/Heart Disease/heart.csv")#change file to wherever file is stores
data_bckup <- data
names(data)[1] <- "age" # renaming the age column, due to some data issue

# remove values corresponding to NA in original dataset
data_shaped <- data
data_shaped <- data %>% filter(thal != 0 & ca != 4)

# Reshaping the categorical features to names, so that it helps in interpreting during plots
# Categorical data reshaping
data_shaped <- data_shaped %>% mutate(sex = case_when(sex == 0 ~ "female",sex == 1 ~ "male"))
data_shaped <- data_shaped %>% mutate(fbs = case_when(fbs == 0 ~ "<=120",fbs == 1 ~ ">120"))
data_shaped <- data_shaped %>% mutate(exang = case_when(exang == 0 ~ "no",exang == 1 ~ "yes"))
data_shaped <- data_shaped %>% mutate(cp = case_when(cp == 3 ~ "typical angina",cp == 1 ~ "atypical angina",cp == 2 ~ "non-anginal",cp == 0 ~ "asymptomatic angina"))
data_shaped <- data_shaped %>% mutate(restecg = case_when(restecg == 0 ~ "hypertrophy",restecg == 1 ~ "normal",restecg == 2 ~ "wave abnormality"))
data_shaped <- data_shaped %>% mutate(datatarget = case_when(target == 1 ~ "asymptomatic",target == 0 ~ "heart-disease"))
data_shaped <- data_shaped %>% mutate(slope = case_when(slope == 2 ~ "upsloping",slope == 1 ~ "flat",slope == 0 ~ "downsloping"))
data_shaped <- data_shaped %>% mutate(thal = case_when(thal == 1 ~ "fixed defect",thal == 2 ~ "normal",thal == 3 ~ "reversable defect"))

# Factorizing some of the features to help during plots and modeling
# Factorizing categorical variables
data_shaped <- data_shaped %>% mutate(sex = as.factor(sex))
data_shaped <- data_shaped %>% mutate(fbs = as.factor(fbs))
data_shaped <- data_shaped %>% mutate(exang = as.factor(exang))
data_shaped <- data_shaped %>% mutate(cp = as.factor(cp))
data_shaped <- data_shaped %>% mutate(slope = as.factor(slope))
data_shaped <- data_shaped %>% mutate(ca = as.factor(ca))
data_shaped <- data_shaped %>% mutate(thal = as.factor(thal))

# Attribute definition/Data dictionary to define each of the data points
# Attribute Information:
# age: age in years
# sex: (1 = male; 0 = female)
# cp: chest pain type (typical angina, atypical angina, non-angina, or asymptomatic angina)
# trestbps: resting blood pressure (in mm Hg on admission to the hospital)
# chol: serum cholestoral in mg/dl
# fbs: Fasting blood sugar (< 120 mg/dl or > 120 mg/dl) (1 = true; 0 = false)
# restecg: resting electrocardiographic results (normal, ST-T wave abnormality, or left ventricular hypertrophy)
# thalach: Max. heart rate achieved during thalium stress test
# exang: Exercise induced angina (1 = yes; 0 = no)
# oldpeak: ST depression induced by exercise relative to rest
# slope: Slope of peak exercise ST segment (0 = upsloping, 1 = flat, or 2 = downsloping)
# ca: number of major vessels (0-3) colored by flourosopy 4 = NA
# thal: Thalium stress test result 3 = normal; 6 = fixed defect; 7 = reversable defect 0 = NA
# target: Heart disease status 1 or 0 (0 = heart disease 1 = asymptomatic)

# 1. Density plot for all features
plot_density(data_shaped, ggtheme = theme_classic2(), 
             geom_density_args = list("fill" = "black", "alpha" = 0.6))

# Multivariate analysis to see interaction between different features
# Male and Female count (68.32% Male and 31.68% female)
a1 <- ggplot(data_shaped, aes(x = sex, fill = sex)) +
  geom_bar(width = 0.5) + 
  scale_fill_manual(values = c("#386cb0","#fdb462"))+
  theme_classic2() +
  theme(legend.position='none')

# Age per gender
b1 <- ggplot(data_shaped, aes(x= sex, y = age, fill = sex)) +
  geom_violin(width = 0.5) +
  geom_boxplot(width = 0.2) +
  ylim(0, 90) +
  stat_compare_means(aes(label = ..p.signif..), method = "t.test") +
  scale_fill_manual(values = c("#386cb0","#fdb462"))+
  theme_classic2() +
  theme(legend.position='none')

# trestbps
c1 <- ggplot(data_shaped, aes(x = sex, y = trestbps, fill = sex)) +
  geom_violin(width = 0.5) +
  geom_boxplot(width = 0.2) + 
  labs(y = "blood pressure (mmHg)") +
  ylim(0,250) +
  stat_compare_means(aes(label = ..p.signif..), method = "t.test") +
  scale_fill_manual(values = c("#386cb0","#fdb462"))+
  theme_classic2() +
  theme(legend.position='none')

# chol
d1 <- ggplot(data_shaped, aes(x = sex, y = chol, fill = sex)) +
  geom_violin(width = 0.5) +
  geom_boxplot(width = 0.2) + 
  labs(y = "cholestorol (mg/dl)") +
  ylim(0,500) +
  stat_compare_means(aes(label = ..p.signif..), method = "t.test") +
  scale_fill_manual(values = c("#386cb0","#fdb462"))+
  theme_classic2() +
  theme(legend.position='none')

# oldpeak
e1 <- ggplot(data_shaped, aes(x = sex, y = oldpeak, fill = sex)) +
  geom_violin(width = 0.5) +
  geom_boxplot(width = 0.2) + 
  labs(y = "ST depression") +
  ylim(0,10) +
  stat_compare_means(aes(label = ..p.signif..), method = "t.test") +
  scale_fill_manual(values = c("#386cb0","#fdb462"))+
  theme_classic2() +
  theme(legend.position='none')

# thalach
f1 <- ggplot(data_shaped, aes(x = sex, y = thalach, fill = sex)) +
  geom_violin(width = 0.5) +
  geom_boxplot(width = 0.2) + 
  labs(y = "Max. heart rate") +
  ylim(0,250) +
  stat_compare_means(aes(label = ..p.signif..), method = "t.test") +
  scale_fill_manual(values = c("#386cb0","#fdb462"))+
  theme_classic2() +
  theme(legend.position='none')

# Using gridExtra to plot all the charts in a single sheet. Space optimization 
# and help in interpretability ;)
suppressWarnings(ggarrange(a1, b1, c1, d1, e1, f1, 
                           ncol = 2, nrow = 3,
                           align = "v"))

# Multivariate analysis part 2 | Bar plots in grids -> code section
# Disease status
g1 <- ggplot(data_shaped, aes(x = datatarget, fill = sex)) +
  geom_bar(width = 0.5, position = 'dodge') + 
  labs(x = "") +
  coord_flip() +
  scale_fill_manual(values = c("#3DACE1","#BD3DE1"))+
  theme_classic2() +
  theme(legend.position='none')

# cp
h1 <- ggplot(data_shaped, aes(cp, group = sex, fill = sex)) +
  geom_bar(position = "dodge") +
  labs(x = "", y = "chest pain") +
  coord_flip() +
  scale_fill_manual(values = c("#3DACE1","#BD3DE1"))+
  theme_classic2() +
  theme(legend.position='none')

# restecg
i1 <- ggplot(data_shaped, aes(restecg, group = sex, fill = sex)) +
  geom_bar(position = "dodge") +
  labs(x = "", y = "rest. electrocardiographic") +
  coord_flip() +
  scale_fill_manual(values = c("#3DACE1","#BD3DE1"))+
  theme_classic2() +
  theme(legend.position='none')

# slope
j1 <- ggplot(data_shaped, aes(slope, group = sex, fill = sex)) +
  geom_bar(position = "dodge") +
  labs(x = "", y = "peak exercise ST") +
  coord_flip() +
  scale_fill_manual(values = c("#3DACE1","#BD3DE1"))+
  theme_classic2() +
  theme(legend.position='none')

# thal 
k1 <- ggplot(data_shaped, aes(thal, group = sex, fill = sex)) +
  geom_bar(position = "dodge") +
  labs(x = "", y = "Thalium stress test") +
  coord_flip() +
  scale_fill_manual(values = c("#3DACE1","#BD3DE1"))+
  theme_classic2() +
  theme(legend.position='none')

# fbp
l1 <- ggplot(data_shaped, aes(fbs, group = sex, fill = sex)) +
  geom_bar(position = "dodge") +
  labs(x = "", y = "Fasting blood sugar") +
  coord_flip() +
  scale_fill_manual(values = c("#3DACE1","#BD3DE1"))+
  theme_classic2() +
  theme(legend.position='none')

# exang
m1 <- ggplot(data_shaped, aes(exang, group = sex, fill = sex)) +
  geom_bar(position = "dodge") +
  labs(x = "", y = "Exercise induced angina") +
  coord_flip() +
  scale_fill_manual(values = c("#3DACE1","#BD3DE1"))+
  theme_classic2() +
  theme(legend.position='none')

# ca
n1 <- ggplot(data_shaped, aes(ca, group = sex, fill = sex)) +
  geom_bar(position = "dodge") +
  labs(x = "", y = "flourosopy") +
  coord_flip() +
  scale_fill_manual(values = c("#3DACE1","#BD3DE1"))+
  theme_classic2() +
  theme(legend.position='none')

ggarrange(g1, h1, i1, j1, k1, l1, m1, n1, 
          ncol = 2, nrow = 4,
          align = "v")

# From this first plot, it appears that this dataset contains more males patients with a 
# higher proportion of heart disease compared to female patients.

# Filtering the males from the dataset to do further analysis on the males 
# and their heart problem status
data_male_only <- data_shaped %>% filter(sex == "male")

a2 <- ggplot(data_male_only, aes(x = datatarget, fill = datatarget)) +
  geom_bar(width = 0.5, position = 'dodge') + 
  scale_fill_manual(values = c("#3AC66C","#D04888"))+
  theme_classic2() +
  theme(legend.position='none')

# Age per gender
b2 <- ggplot(data_male_only, aes(x= datatarget, y = age, fill = datatarget)) +
  geom_violin(width = 0.5) +
  geom_boxplot(width = 0.2) +
  ylim(0, 90) +
  stat_compare_means(aes(label = ..p.signif..), method = "t.test") +
  scale_fill_manual(values = c("#3AC66C","#D04888"))+
  theme_classic2() +
  theme(legend.position='none')

# trestbps
c2 <- ggplot(data_male_only, aes(x = datatarget, y = trestbps, fill = datatarget)) +
  geom_violin(width = 0.5) +
  geom_boxplot(width = 0.2) + 
  labs(y = "blood pressure (mmHg)") +
  ylim(0,250) +
  stat_compare_means(aes(label = ..p.signif..), method = "t.test") +
  scale_fill_manual(values = c("#3AC66C","#D04888"))+
  theme_classic2() +
  theme(legend.position='none')

# chol
d2 <- ggplot(data_male_only, aes(x = datatarget, y = chol, fill = datatarget)) +
  geom_violin(width = 0.5) +
  geom_boxplot(width = 0.2) + 
  labs(y = "cholestorol (mg/dl)") +
  ylim(0,500) +
  stat_compare_means(aes(label = ..p.signif..), method = "t.test") +
  scale_fill_manual(values = c("#3AC66C","#D04888"))+
  theme_classic2() +
  theme(legend.position='none')

# oldpeak
e2 <- ggplot(data_male_only, aes(x = datatarget, y = oldpeak, fill = datatarget)) +
  geom_violin(width = 0.5) +
  geom_boxplot(width = 0.2) + 
  labs(y = "ST depression") +
  ylim(0,10) +
  stat_compare_means(aes(label = ..p.signif..), method = "t.test") +
  scale_fill_manual(values = c("#3AC66C","#D04888"))+
  theme_classic2() +
  theme(legend.position='none')

# thalach
f2 <- ggplot(data_male_only, aes(x = datatarget, y = thalach, fill = datatarget)) +
  geom_violin(width = 0.5) +
  geom_boxplot(width = 0.2) + 
  labs(y = "Max. heart rate") +
  ylim(0,250) +
  stat_compare_means(aes(label = ..p.signif..), method = "t.test") +
  scale_fill_manual(values = c("#3AC66C","#D04888"))+
  theme_classic2() +
  theme(legend.position='none')

ggarrange(a2, b2, c2, d2, e2, f2, 
          ncol = 2, nrow = 3,
          align = "v")

# Male patients with heart disease are significantly older, have higher cholesterol level, 
# and reduced maximum heart rate response to the thallium test.

# Correlation plot using GGally package

data_correlation <- data %>% filter(thal != 0 & ca != 4) # remove values corresponding to NA in original dataset
GGally::ggcorr(data_correlation, geom = "circle")

# Findings and inisghts from our EDA
# From the correlation study it seems that the parameters cp, restecg, thalach, slope
# are the most usefull to predict the risk of heart disease

# From the EDA anlysis it semms that age, sex, cholesterol, restecg are also usefull

# For prediction the following variables seems the most usefull age, sex, cholesterol, restecg, cp,
# thalach, slope

## Cholestrol
# The Bivariate plot between cholestrol levels and target suggests that the Patients likely 
# to suffer from heart diseases are having higher cholestrol levels in comparison to the patients 
# with target 0(likely to not suffer from the heart diseases.

# Modeling
# Selecting the features that were significant (determined from ) for our model building
data_model <- data %>% select(target, age, sex, chol, restecg, cp, thalach, slope)
# Define target as a factor. rpart classification would not work otherwise.
data_model$target <- factor(data_model$target) 

# Build a simple classification decision tree with rpart. Run the model until the accuracy 
# reach the selected minimum.
accuracy <- 0

while(accuracy <= 0.88) {
  split_values <- sample.split(data_model$target, SplitRatio = 0.65)
  train_set <- subset(data_model, split_values == T) #Define the 65% data to go here
  test_set <- subset(data_model, split_values == F) #Rest of 35% data goes here
  mod_class <- rpart(target~., data=train_set) # using the train set to help the model understand the datapoints
  result_class <- predict(mod_class, test_set, type = "class") # We did a prediction based on the train set, onto the test set
  table <- table(test_set$target, result_class) #confusion matrix which helps us determine Type I or II errors
  accuracy <- (table["0","0"] + table["1","1"])/sum(table)} #accuracy of the model

cat("Model accuracy", round(accuracy, digits = 2)*100, "%") #Display the model accuracy

fancyRpartPlot(mod_class, caption = NULL) #plot the decision tree

# Random Forest
# Split data into train and validation set
set.seed(103)
train <- sample(nrow(data_model), 0.7*nrow(data_model), replace = FALSE)
TrainSet <- data_model[train,]
ValidSet <- data_model[-train,]
summary(TrainSet)
summary(ValidSet)

# Creating a Random Forest model with default parameters
model1 <- randomForest(target ~ ., data = TrainSet,  ntree = 1000, mtry = 1, importance = TRUE)
model1

# Predicting on train set
predTrain <- predict(model1, TrainSet, type = "class")
# Checking classification accuracy
table(predTrain, TrainSet$target) 

# Predicting on Validation set
predValid <- predict(model1, ValidSet, type = "class")
# Checking classification accuracy
mean(predValid == ValidSet$target)   
table(predValid,ValidSet$target)

varImpPlot(model1)  

# From modeling, it is evident that Decision tree has a better accuracy as compared to Random Forest




###################################################### Logistic Regression

#Splitting the data set

train1 <- sample(nrow(data_model), 0.7*nrow(data_model), replace = FALSE)
Training <- data_model[train1,]
Validating <- data_model[-train1,]
summary(Training)
summary(Validating)

#Creating a Logistic regression model
mylogit <- glm(target ~. , data = Training, family = "binomial")
summary(mylogit)

#confint function to obtain confidence intervals for the coefficient estimates
confint(mylogit)

# Performing chi square test to find the significance of variables on the
# dependent variables
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 2:5 )

#odds ratios only
exp(coef(mylogit))

# odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))

# Predicting on training data set
Training$predictedr <- predict(mylogit, Training, type = "response")
Training

# Finding the accuracy of the model
trainingtable <- table(Training$predictedr > 0.5, Training$target) 
accuracytrainL <- (trainingtable["FALSE","0"] + trainingtable["TRUE","1"])/sum(trainingtable)
accuracytrainL

#ROC curve for the training data set
ROCRpred <- prediction(Training$predictedr , Training$target)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))


# Predicting on valdiating data set
Validating$predicted <- predict(mylogit, Validating, type = "response")
validtable <- table(Validating$predicted > 0.5 , Validating$target)
validtable
accuracyvalidL <- (validtable["FALSE","0"] + validtable["TRUE","1"])/sum(validtable)
accuracyvalidL


#ROC curve for the test data set
ROCRpredvalid <- prediction(Validating$predicted , Validating$target)
ROCRperfvalid <- performance(ROCRpredvalid, 'tpr','fpr')
plot(ROCRperfvalid, colorize = TRUE, text.adj = c(-0.2,1.7))

## The accuracy of the Logistic regression model is 0.7582418 for the test data
# and 0.7783019 for the training data



################################################# Support Vector Machines

# Splitting the data into train and test data

train2 <- sample(nrow(data_model), 0.7*nrow(data_model), replace = FALSE)
trains <- data_model[train2,]
tests <- data_model[-train2,]
summary(trains)
summary(tests)

# Fitting the data with the svm model
svmfit = svm(target ~ ., data = trains, method = "C-classification" 
             , kernel = "radial", cost = 10, gamma=0.1)
summary(svmfit)

# Shows us the Support Vectors we have
svmfit$SV

#Predict using the model we created on the train data
predictsvm <- predict(svmfit, trains)
predictsvm

#Finding the model accuracy for train data
straintable <- table(trains$target, predictsvm)
straintable
accuracytrainS <- (straintable["0","0"] + straintable["1","1"])/sum(straintable)
accuracytrainS

#Predict using the model we created on the test data
predictsvmtest <- predict(svmfit, tests)
predictsvmtest

#finding the model accuracy for the test data
stesttable <- table(tests$target, predictsvmtest)
stesttable
accuracytests <- (stesttable["0","0"] + stesttable["1","1"])/sum(stesttable)
accuracytests

## The accuracy of the svm model is 08915094 for the training data and 
# 0.7362637 for the test data

