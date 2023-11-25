library(readxl)
xlsx_file <- "F:/Titanic - Modified.xlsx"
csv_file <- "F:/Titanic - Modified.csv"

dataset <- read_xlsx(xlsx_file)
write.csv(data, file = csv_file, row.names = FALSE)
dataset<-read.csv("F:/Titanic - Modified.csv")
dataset

install.packages("dplyr")
library(dplyr)
summary(dataset)




names(dataset)





gender_counts <- table(dataset$gender)
barplot(gender_counts, col = "yellow", border = "white",
        xlab = "Gender", ylab = "Count", main = "Gender")

age_counts <- table(dataset$age)
barplot(age_counts, col = "red", border = "black",
        xlab = "Age", ylab = "Count", main = "Age")

sibs_counts <- table(dataset$sibs)

barplot(sibs_counts, col = "skyblue", border = "black",
        xlab = "Sibs", ylab = "Count", main = "Sibs")

parch_counts <- table(dataset$parch)
barplot(parch_counts, col = "orange", border = "black",
        xlab = "Parch", ylab = "Count", main = "Parch")

fare_counts <- table(dataset$fare)
barplot(fare_counts, col = "purple", border = "black",
        xlab = "Fare", ylab = "Count", main = "Fare")

embarked_counts <- table(dataset$embarked)
barplot(embarked_counts, col = "yellow", border = "black",
        xlab = "Embarked", ylab = "Count", main = "Embarked")

class_counts <- table(dataset$class)
barplot(class_counts, col = "orange", border = "black",
        xlab = "Class", ylab = "Count", main = "Class")

who_counts <- table(dataset$who)
barplot(who_counts, col = "red", border = "black",
        xlab = "WHo", ylab = "Count", main = "WHo")

alone_counts <- table(dataset$alone)
barplot(alone_counts, col = "skyblue", border = "black",
        xlab = "Person", ylab = "Count", main = "Person")

survived_counts <- table(dataset$survived)
barplot(survived_counts, col = "orange", border = "black",
        xlab = "Survived", ylab = "Count", main = "Survived")






completeness <- colMeans(!is.na(dataset))
print(completeness)





colSums(is.na(dataset))

dataset$age <- ifelse(is.na(dataset$age),mean(dataset$age, na.rm = TRUE),dataset$age)
colSums(is.na(dataset))
embarked <- names(table(dataset$embarked))[which.max(table(dataset$embarked))]
dataset$embarked <- ifelse(is.na(dataset$embarked), embarked, dataset$embarked)
colSums(is.na(dataset))
class <- names(table(dataset$class))[which.max(table(dataset$class))]
dataset$class <- ifelse(is.na(dataset$class), class, dataset$class)
colSums(is.na(dataset))
gender <- names(table(dataset$gender))[which.max(table(dataset$gender))]
dataset$gender <- ifelse(is.na(dataset$gender), gender, dataset$gender)
colSums(is.na(dataset))






str(dataset)
dataset$age_Category <- cut(dataset$age, breaks = c(0, 18, 30, 50, Inf), labels = c("Children", "Adult", "Senior", "Older"))
dataset





dataset %>% summarise_if(is.numeric,mean)
dataset %>% summarise_if(is.numeric,median)
dataset %>% summarise_if(is.numeric,sd)





names(dataset)[1]<- "PessGender"
names(dataset)[2]<- "PessAge"
names(dataset)[3]<- "PessSibsp"
names(dataset)[4]<- "PessParch"
names(dataset)[5]<- "PessFare"
names(dataset)[6]<- "PessEmbarked"
names(dataset)[7]<-  "PessClass"
names(dataset)[8]<- "PessPerson"
names(dataset)[9]<- "PessAlone"
names(dataset)[10]<- "PessSurvived"
names(dataset)
dataset$PessGender <- factor(dataset$PessGender,
                       levels = c(0,1),
                       labels = c("Female", "Male"))
dataset$PSurvived<-factor(dataset$PessSurvived,
                       levels=c(0,1),
                       labels = c("Female","Male"))
dataset





normalized_data <- dataset[, c("PessAge", "PessFare")]
normalized_data$PessAge <- (dataset$PessAge - min(dataset$PessAge, na.rm = TRUE)) / (max(dataset$PessAge, na.rm = TRUE) - min(dataset$PessAge, na.rm = TRUE))
print(normalized_data$PessAge)
normalized_data$PessFare <- (dataset$PessFare - min(dataset$PessFare, na.rm = TRUE)) / (max(dataset$PessFare, na.rm = TRUE) - min(dataset$PessFare, na.rm = TRUE))
print(normalized_data$PessFare)





detect_outliers_zscore <- function(x, threshold = 3) {
  z_scores <- (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  outliers <- x[abs(z_scores) > threshold]
  return(outliers)
}
outliers_age <- detect_outliers_zscore(dataset$PessAge)
outliers_fare <- detect_outliers_zscore(dataset$PessFare)
dataset <- subset(dataset, !(PessAge %in% outliers_age) & !(PessFare %in% outliers_fare))
summary(dataset)





for (col in names(dataset)) {
  if (is.numeric(dataset[[col]])) {
    cat("Column:", col, "\n")
    cat("Min:", min(dataset[[col]], na.rm = TRUE), "\n")
    cat("Mean:", mean(dataset[[col]], na.rm = TRUE), "\n")
    cat("Max:", max(dataset[[col]], na.rm = TRUE), "\n")
    cat("Standard Deviation:", sd(dataset[[col]], na.rm = TRUE), "\n")
    cat("\n")
    hist(dataset[[col]], main = paste("Histogram", col), xlab = col, col = "red", border = "black")
    cat("\n")
  }
}

for (col in names(dataset)) {
  if (is.factor(dataset[[col]])) {
    cat("Column:", col, "\n")
    
    cat("Frequency Table:\n")
    cat(table(dataset[[col]]), "\n")
    cat("\n")
    
    barplot(table(dataset[[col]]), main = paste("Bar", col), xlab = col, ylab = "Frequency", col = "skyblue")
    cat("\n")
  }
}





install.packages("readr")
library(readr)
set.seed(43)
sample_size <- 0.8
sampled_data <- dataset[sample(nrow(dataset), size = sample_size * nrow(dataset), replace = FALSE), ]
cat("Sampled Data Dimensions:", dim(dataset)," \n ")




cols_with_missing <- colnames(dataset)[apply(is.na(dataset), 2, any)]
for (col in cols_with_missing) {
  if (is.numeric(data[[col]])) {
    dataset[[col]] <- ifelse(is.na(dataset[[col]]), mean(dataset[[col]], na.rm = TRUE), dataset[[col]])
  } else {
    dataset[[col]] <- ifelse(is.na(dataset[[col]]), "Unknown", dataset[[col]])
  }
}

dataset <- unique(dataset)
print(cols_with_missing)



outlier_threshold <- 3  
z_scores <- (dataset$PessAge - mean(dataset$PessAge)) / sd(dataset$PessAge)
outlier_indices <- which(abs(z_scores) > outlier_threshold)
noisy_values <- dataset$PessAge[outlier_indices]
cat("Noisy Values (Outliers):\n")
print(noisy_values)
median_age <- median(dataset$PessAge, na.rm = TRUE)
dataset$PessAge[outlier_indices] <- median_age

z_scores <- (dataset$PessFare - mean(dataset$PessFare)) / sd(dataset$PessFare)
outlier_indices <- which(abs(z_scores) > outlier_threshold)
noisy_values <- dataset$PessFare[outlier_indices]
cat("Noisy Values (Outliers):\n")
print(noisy_values)
median_fare <- median(dataset$PessFare, na.rm = TRUE)
dataset$PessFare[outlier_indices] <- median_fare
dataset



invalid_age_indices <- which(dataset$PessAge < 0)
invalid_fare_indices <- which(dataset$PessFare < 0)
invalid_age_values <- dataset$PessAge[invalid_age_indices]
invalid_fare_values <- dataset$PessFare[invalid_fare_indices]
cat("Invalid Age Values:\n")
print(invalid_age_values)
cat("Invalid Fare Values:\n")
print(invalid_fare_values)


write_csv(data, "F:/Titanic - Cleaned.csv")

















