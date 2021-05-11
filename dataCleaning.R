
#set working directory
setwd("C:/Users/Daryl Gan/Desktop/Heart-Attack-Analysis/dataset")


#read the datasets into Rstudio
data <- read.csv("rawDataset.csv", header = TRUE)


#descriptive analysis
#view dataset
View(data)

#view type of class
class(data)

#view number of rows & columns
dim(data)

#view first 6 rows of data
head(data)

#summarise data
summary(data)

#view the structure of the dataset
str(data)


#convert 0 to 'F' for female and 1 to 'M' for male
data[data$sex == 0,]$sex <- "F"
data[data$sex == 1,]$sex <- "M"

#convert 0 to "Healthy" and 1 to "Unhealthy"
data$output <- ifelse(test = data$output == 0, yes = "Healthy", no = "Unhealthy")


#convert data type
data$sex <- as.factor(data$sex)

data$cp <- as.factor(data$cp)

data$fbs <- as.factor(data$fbs)

data$restecg <- as.factor(data$restecg)

data$exng <- as.factor(data$exng)

data$slp <- as.factor(data$slp)

data$output <- as.factor(data$output)


#check the structure of the dataset again
str(data)


#check number of rows of data that has NA values
nrow(data[is.na(data),])

nrow(data)


#remove rows of data that has NA values
data <- na.omit(data)

nrow(data)


#remove duplicate rows
data <- unique(data)


#check if dataset is imbalanced
xtabs(~ output + sex, data=data)

xtabs(~ output + cp, data=data)

xtabs(~ output + fbs, data=data)

xtabs(~ output + restecg, data=data)

xtabs(~ output + exng, data=data)

xtabs(~ output + slp, data=data)

xtabs(~ output + caa, data=data)

xtabs(~ output + thall, data=data)


#descriptive analysis
str(data)

dim(data)

summary(data)


#write new dataset (after cleaning) into new csv file
write.csv(data, "cleanedData.csv")
