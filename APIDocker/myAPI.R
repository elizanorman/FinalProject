library(tidyverse)
library(reshape2)
library(caret)
library(MLmetrics)
library(DescTools)
diabetesData <- read_csv("diabetes_binary_health_indicators_BRFSS2015.csv", show_col_types = FALSE)

diabetesData$Diabetes_binary <- factor(diabetesData$Diabetes_binary,
                                       levels = c(0,1),
                                       labels = c("No_diabetes", "Diabetes"))

diabetesData$HighChol <- factor(diabetesData$HighChol,
                                levels = c(0,1),
                                labels = c("No high cholesterol", "High cholesterol"))

diabetesData$Smoker <- factor(diabetesData$Smoker,
                              levels = c(0,1),
                              labels = c("Never smoked at least 100 cigs", "Has smoked at least 100 cigs"))

diabetesData$PhysActivity <- factor(diabetesData$PhysActivity,
                                    levels = c(0,1),
                                    labels = c("No physical activity", "Physical activity"))

diabetesData$Sex <- factor(diabetesData$Sex,
                           levels = c(0,1),
                           labels = c("Female", "Male"))

diabetesData$Age <- factor(diabetesData$Age,
                           levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13),
                           labels = c("20-24","25-29", "30-34", "35-39", "40-44", "45-49","51-54","55-59","60-64", "65-69", "70-74", "75-79", "80 or older"))

diabetesData$HvyAlcoholConsump <- factor(diabetesData$HvyAlcoholConsump,
                                         levels = c(0,1),
                                         labels = c("No", "Yes"))

diabetesData$DiffWalk <- factor(diabetesData$DiffWalk,
                                levels = c(0,1),
                                labels = c("No serious difficulty walking up stairs", "Serious difficulty walking stairs"))

newDiabetesData <- diabetesData |>
  select(Diabetes_binary, HighChol, BMI, Smoker, PhysActivity, HvyAlcoholConsump, Sex, Age, DiffWalk)
set.seed(50)

index <- createDataPartition(newDiabetesData$Diabetes_binary, p = 0.7, list = FALSE)
trainData <- newDiabetesData[index, ]
testData <- newDiabetesData[-index, ]


trctrl2 <- trainControl(
  method = "repeatedcv",
  number = 3,
  summaryFunction = mnLogLoss,  # Use custom log-loss function
  classProbs = TRUE,           # Needed for logistic regression
)


randForest <- train(Diabetes_binary ~ ., 
                    data = trainData, 
                    method = "rf",
                    trControl=trctrl2,
                    ntree = 50,
                    preProcess = c("center", "scale"),
                    tuneGrid = data.frame(mtry = 1:5))
randForest

#* Model fitting widget
#* @serializer json
#* @param HighChol High Cholesterol
#* @param BMI BMI
#* @param Smoker Smoker
#* @param PhysActivity Physical Activity
#* @param HvyAlcoholConsump Heavy Alcohol Consumption
#* @param Sex Sex
#* @param Age Age
#* @param DiffWalk Difficulty Walking
#* @get /pred
function(HighChol = "No high cholesterol", BMI = 28.38236, Smoker = "Never smoked at least 100 cigs", PhysActivity = "Physical activity", HvyAlcoholConsump = "No", Sex = "Female", Age = "60-64", DiffWalk = "No serious difficulty walking up stairs"){
  BMI <- as.numeric(BMI)
  new_data <- data.frame(HighChol = HighChol, BMI = BMI, Smoker= Smoker, PhysActivity = PhysActivity, HvyAlcoholConsump = HvyAlcoholConsump, Sex = Sex, Age = Age, DiffWalk = DiffWalk)
  predict(randForest, newdata = new_data)
}

#* Personal info
#* @serializer json
#* @get /info
function(){
  "Eliza Norman "
}

#http://localhost:PORT/pred?Sex=Female&Age=60-64
#http://localhost:PORT/pred?PhysActivity=Physical%20activity
#http://localhost:PORT/pred?HighChol=No%20high%20cholesterol&BMI=25&Smoker=Never%20smoked%20at%20least%20100%20cigs