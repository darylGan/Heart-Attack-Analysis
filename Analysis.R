#read the file
data <- read.csv("dataset/cleanedData.csv",header = T )
str(data)

#change the type into factor type
data$sex <- as.factor(data$sex)
data$cp <- as.factor(data$cp)
data$fbs <- as.factor(data$fbs)
data$restecg <- as.factor(data$restecg)
data$exng <- as.factor(data$exng)
data$slp <- as.factor(data$slp)
data$caa <- as.factor(data$caa)
data$thall <- as.factor(data$thall)
data$hd <- as.factor(data$hd)

str(data)


#Split data into training set and testing set
set.seed(10) 
split_index <- sample(2,nrow(data),replace = T , prob = c(0.8,0.2))
train_set <- data[split_index == 1,]
test_set <- data[split_index == 2,]

#Build the logistic model
logistic_model <- glm(hd ~ cp+thalachh+slp+exng+oldpeak+caa+trtbps+chol+fbs,data=train_set,family="binomial",)
summary(logistic_model)

#Discard some less significant variables
logistic_model <- glm(hd ~ cp+exng+oldpeak+caa,data=train_set,family="binomial",)
summary(logistic_model)

#Train the model using training data set
train_predict <- predict(logistic_model, train_set, type = 'response')
train_predict <- ifelse(train_predict >=0.5 , TRUE , FALSE)
train_matrix <- table(Prediction = train_predict , Actual = train_set$hd)
train_accuracy <- sum(train_predict==train_set$hd)/nrow(train_set)*100

#Test the model using testing data set
test_predict <- predict(logistic_model, test_set, type = 'response')
test_predict <- ifelse(test_predict >=0.5 ,  TRUE , FALSE)
test_matrix <- table(Prediction = test_predict , Actual = test_set$hd)
test_accuracy <-sum(test_predict == test_set$hd)/nrow(test_set)*100

print(train_matrix)
print(train_accuracy)
print(test_matrix)
print(test_accuracy)

      
