library(ggplot2)
#read the file
data <- read.csv("Heart-Attack-Analysis/dataset/cleanedData.csv",header = T )
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
set.seed(3) 
split_index <- sample(2,nrow(data),replace = T , prob = c(0.8,0.2))
train_set <- data[split_index == 1,]
test_set <- data[split_index == 2,]

#Build the logistic model
logistic_model <- glm(hd ~ cp+thalachh+slp+exng+oldpeak+caa+trtbps+chol+fbs,data=train_set,family="binomial",)
summary(logistic_model)

#Discard less statistically significant variables
logistic_model <- glm(hd ~ cp+exng+oldpeak+caa,data=train_set,family="binomial",)
summary(logistic_model)

#Have a look on how the variables affecting the prediction
#cp
a<- xtabs(~ hd + cp, data=data)
barplot(t(a),beside=T,ylim=c(0,120),xlab="Heart Disease",ylab="Frequency",col=rainbow(4),axis.lty="solid")
legend("top",rownames(t(a)),cex =0.8,fill=rainbow(4),title="Chest Pain types")

#exng
a<- xtabs(~ hd +exng, data=data)
barplot(t(a),beside=T,ylim=c(0,150),xlab="Heart Disease",ylab="Frequency",col=rainbow(2),axis.lty="solid")
legend("topleft",rownames(t(a)),cex =0.8,fill=rainbow(2),title="Exercise induced angina")

#oldpeak
a<- xtabs(~ hd + oldpeak, data=data)
ggplot(data, aes(oldpeak, colour = hd, fill = hd)) +
  geom_density(alpha = 0.5) 

#caa
a<- xtabs(~ hd + caa, data=data)
barplot(t(a),beside=T,ylim=c(0,150),xlab="Heart Disease",ylab="Frequency",col=rainbow(4),axis.lty="solid")
legend("topleft",rownames(t(a)),cex =0.8,fill=rainbow(4),title="Number of Major Vessels coloured by Flourosopy")


#Train the model using training data set
train_predict <- predict(logistic_model, train_set, type = 'response')
train_predict <- ifelse(train_predict >=0.5 , TRUE , FALSE)
train_matrix <- table(Prediction = train_predict , Actual = train_set$hd)
print(train_matrix)

train_accuracy <- sum(train_predict==train_set$hd)/nrow(train_set)*100
print(train_accuracy)

#Test the model using testing data set
test_predict <- predict(logistic_model, test_set, type = 'response')
test_predict <- ifelse(test_predict >=0.5 ,  TRUE , FALSE)
test_matrix <- table(Prediction = test_predict , Actual = test_set$hd)
print(test_matrix)

test_accuracy <-sum(test_predict == test_set$hd)/nrow(test_set)*100
print(test_accuracy)
