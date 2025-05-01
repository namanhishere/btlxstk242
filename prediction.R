#load the data
dataRaw <- read.csv("C:/Users/namanhishere/Downloads/BRCA.csv")

rows_with_na <- which(apply(dataRaw, 1, function(x) any(is.na(x) | x == "")))
length(rows_with_na)/nrow(dataRaw)

colSums(is.na(dataRaw) | dataRaw == "")


#remove the row which empty Patient_Status
data <- dataRaw[dataRaw$Patient_Status != "", ] 

data$Patient_Status_Map <- ifelse(is.na(data$Patient_Status), 0,
                                     ifelse(data$Patient_Status == "Alive", 1, 0))


#choose data
data <- data[c("Age","Protein1","Protein2","Protein3","Protein4","Tumour_Stage","Surgery_type","Patient_Status")]



summary(data)

data$Patient_Status <- factor(data$Patient_Status, levels = c("Dead", "Alive"))










# 5.1 tìm khoảng tin cậy

# vẽ qqplot
qqnorm(data$Age, main = "Normal Q-Q Plot of Age")
qqline(data$Age)

shapiro.test(data$Age)

# tìm khoảng tin cậy
n <- length(data$Age)
xtb = mean(data$Age)
s = sd(data$Age)
T_alpha = qt(p = .05/2, df = n - 1, lower.tail = FALSE)
Epsilon = T_alpha * s / sqrt(n)
Left_CI = xtb - Epsilon
Right_CI = xtb + Epsilon
print(data.frame(n, xtb, s, T_alpha, Left_CI, Right_CI))

#5.2 
#loc set
alive_patients = subset(data, Patient_Status=="Alive");
dead_patients = subset(data, Patient_Status=="Dead");


qqnorm (alive_patients$Age)
qqline (alive_patients$Age)
shapiro.test (alive_patients$Age)

qqnorm (dead_patients$Age)
qqline (dead_patients$Age)
shapiro.test (dead_patients$Age)

n0 <- length(alive_patients$Age)
mean0 <- mean(alive_patients$Age)
sd0 <- sd(alive_patients$Age)

n1 <- length(dead_patients$Age)
mean1 <- mean(dead_patients$Age)
sd1 <- sd(dead_patients$Age)

z <- (mean0 - mean1) / sqrt((sd0^2 / n0) + (sd1^2 / n1))
z

z_alpha_2 <- qnorm(p = 0.05 / 2, lower.tail = FALSE)
z_alpha_2


#5.3 logistic

library(caTools)

set.seed(123)
split <- sample.split(data$Patient_Status, SplitRatio = 0.7)
train_set <- subset(data, split == TRUE)
test_set <- subset(data, split == FALSE)


logistic_model <- glm(Patient_Status ~ .,
                       data = train_set, family = "binomial")
summary(logistic_model)
step(logistic_model, direction = "backward")

#set.seed(69)
#split <- initial_split(data, prop = 0.8, strata = y)

# --- Bước 5: Kiểm tra mô hình ---
print("Tóm tắt mô hình hồi quy logistic:")
summary(logistic_model)

# First, ensure you have fitted the final model
final_model <- glm(Patient_Status ~ Protein2 + Surgery_type, 
                   family = "binomial", data = train_set)

# Then, get the summary
summary(final_model) 

predicted <- predict(final_model,test_set,type="response")
test_set$predicted<-ifelse(predicted > 0.5,"Alive","Dead")
head(test_set,10)
table(test_set$predicted,test_set$Patient_Status)


library(caret) 
confusionMatrix(as.factor(test_set$predicted), 
                as.factor(test_set$Patient_Status),positive="Alive")

library(pROC)
actual_numeric <- ifelse(test_set$Patient_Status == "Dead", 0, 1)
predicted_numeric <- ifelse(test_set$predicted == "Dead", 0, 1)
roc_curve <- roc(response = actual_numeric, predictor = predicted_numeric)

print(paste("AUC (Area Under Curve):", round(auc(roc_curve), 4)))

# Vẽ đồ thị ROC
plot(roc_curve, main = "ROC Curve", print.auc = TRUE, col = "blue")



deviance_residuals <- residuals(final_model, type = "deviance") 
# Kiem tra phan phoi chuan 
hist(deviance_residuals, main = "Histogram of Deviance Residuals", 
     xlab = "Deviance Residuals", col = "green", breaks = 20) 


shapiro.test(deviance_residuals) 


library(car)
vif(final_model)
