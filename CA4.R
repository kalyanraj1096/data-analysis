#setting work directory
getwd()
setwd("C:/Users/Kalyan Raj/Documents/data-analysis")



#loading the data and remocing unwanted characters
Tdischharges_df <- read.csv("total discharges.csv",header = TRUE, na.strings=c("**","*","~",","))



#replacing the unwanted characters for all the columns
replaceCommas<-function(x){
  x<-as.numeric(gsub("\\,", "", x))
}

Tdischharges_df$Male.U15 <- replaceCommas(Tdischharges_df$Male.U15)
Tdischharges_df$Male.15to44 <- replaceCommas(Tdischharges_df$Male.15to44)
Tdischharges_df$Male.45to64 <- replaceCommas(Tdischharges_df$Male.45to64)
Tdischharges_df$Male.65.and.above <- replaceCommas(Tdischharges_df$Male.65.and.above)
Tdischharges_df$Male.Total <- replaceCommas(Tdischharges_df$Male.Total)
Tdischharges_df$Female.U15 <- replaceCommas(Tdischharges_df$Female.U15)
Tdischharges_df$Female.15to44 <- replaceCommas(Tdischharges_df$Female.15to44)
Tdischharges_df$Female.45to64 <- replaceCommas(Tdischharges_df$Female.45to64)
Tdischharges_df$Female.65.and.above <- replaceCommas(Tdischharges_df$Female.65.and.above)
Tdischharges_df$Female.Total <- replaceCommas(Tdischharges_df$Female.Total)
Tdischharges_df$Total.Discharges <- replaceCommas(Tdischharges_df$Total.Discharges)
Tdischharges_df$Total.U15 <- replaceCommas(Tdischharges_df$Total.U15)
Tdischharges_df$Total.15to44 <- replaceCommas(Tdischharges_df$Total.15to44)
Tdischharges_df$Total.45to64 <- replaceCommas(Tdischharges_df$Total.45to64)
Tdischharges_df$Total.65.and.above <- replaceCommas(Tdischharges_df$Total.65.and.above)


#mice
library(mice)
md.pattern(Tdischharges_df)

#vim
library(VIM)
missing_values <- aggr(Tdischharges_df, prop = FALSE, numbers = TRUE)
summary(missing_values)
matrixplot(Tdischharges_df)


#mean of the vlaue
Tdischharges_df[!complete.cases(Tdischharges_df),]


#list rows with missing values
sum(is.na(Tdischharges_df$Male.U15))

#removing data
str(Tdischharges_df)
any(is.na(Tdischharges_df$Male.U15))

Tdischharges_df <- na.omit(Tdischharges_df)

#------------------------------------------------------------------------------------------------

#Co-relation
library(ggplot2) 
hist(Tdischharges_df$Total.Discharges, main = "highest discharges", xlab = "discharges")
plot <- ggplot(Tdischharges_df, aes(x = Male.Total, y = Female.Total))
plot <- plot + stat_smooth(method = "lm", col = "red", se = FALSE)
plot <- plot + geom_point()
print(plot)

#subset numeric data
subset_df <- subset(Tdischharges_df, select=c(Male.U15,Male.15to44,Male.45to64,Male.65.and.above,Male.Total,
                                              Female.U15,Female.15to44,Female.45to64,Female.65.and.above,Female.Total,
                                              Total.U15,Total.15to44,Total.45to64,Total.65.and.above,Total.Discharges))
colnames(subset_df)

str(subset_df)



#-------------------------------------------------------------------------------------------------------

#PCA

pca <- prcomp(subset_df, center = TRUE, scale. = TRUE)
summary(pca)
str(pca)

library("factoextra")
eig_values <- get_eigenvalue(pca)
eig_values


library("FactoMineR")
pca2 <- PCA(subset_df, graph = FALSE)
print(pca2)
pca2_eig_values <- get_eigenvalue(pca2)
pca2_eig_values
fviz_eig(pca, addlabels = TRUE, ylim = c(0, 50))
pca_for_variables <- get_pca_var(pca)
pca_for_variables

library("corrplot")
corrplot(pca_for_variables$cos2, is.corr = FALSE)
fviz_pca_var(pca, col.var = "black")
head(pca_for_variables$cos2, 10)
fviz_cos2(pca, choice = "var", axes = 1:2)
fviz_pca_var(pca, col.var = "cos2",
             gradient.cols = c("red", "Blue", "Green"), 
             repel = TRUE # Avoid text overlapping
)  
head(pca_for_variables$contrib, 20)
fviz_pca_var(pca, col.var = "contrib",
             gradient.cols = c("red", "Blue", "Green"),
)

#------------------------------------------------------------------------------------------------



qqnorm(subset_df$Total.Discharges)
qqline(subset_df$Total.Discharges = 'red')

#install.packages("pwr")
library(pwr)
#calculating the effective size
effective_size <- cohen.ES(test = "r", size = "small")
effective_size

#Considering effective size and alpha as 5% ,Power analysis is calculated. #pwr.t.test for corelation.
power_analysis <-pwr.t.test(d=0.5,n=NULL,sig.level=0.05,  power=0.95, type="one.sample",alternative="two.sided")
power_analysis
#plotting power analysis
plot(power_analysis)


#-----------------------------------------------------------------------------------------------


#Hypothesis Testing
test <- cor.test(Tdischharges_df$Male.Total, Tdischharges_df$Female.Total,
                 method = 'spearman', exact = FALSE) 

test

#----------------------------------------------------------------------------------------------------



library(e1071)
library(hydroGOF)

#total discharges for male

Tdischharges_df$Male.U15 <- ifelse(Tdischharges_df$Male.U15 == "0 to 15", 1, 0)
Tdischharges_df$Male.15to44 <- ifelse(Tdischharges_df$Male.15to44 == "15 to 44", 1, 0)
Tdischharges_df$Male.45to64 <- ifelse(Tdischharges_df$Male.45to64 == "45 to 64", 1, 0)
Tdischharges_df$Male.65.and.above <- ifelse(Tdischharges_df$Male.65.and.above == "65 and above", 1, 0)
Tdischharges_df$Male.Total <- ifelse(Tdischharges_df$Male.Total == "total", 1,0)

#type of disease 

Tdischharges_df$Diagnosis <- ifelse(Tdischharges_df$Diagnosis == "all diagnosis", 1, 0)


#linear regression model
model <- lm(Male.45to64~., Tdischharges_df)

predictedY <- predict(model, Tdischharges_df)

#plotting

plot(Tdischharges_df$Male.45to64,predictedY,pch=16, col="blue", main = "Actual Vs Predicted Length of Stay", xlab = "Actual", ylab = "Predicted")

abline(a = NULL, b = NULL, h = NULL, v = NULL, reg = NULL,
       coef = NULL, untf = FALSE)



prediction <- Tdischharges_df(Tdischharges_df, n.ahead = 3 * 12)
prediction$pred


regressor <- svm(formula=Male.45to64~., 
                 data = Tdischharges_df,
                 type = 'eps-regression')
# Predicting a new result
y_pred = predict(regressor)

plot(Tdischharges_df$Male.45to64, y_pred, pch=16,cex=1, col="blue",main="Actual Vs Predicted Length of Stay",xlab = "Actual", ylab = "Predicted")


##################################################################################################################################
##################################################################################################################################


#PREDICTION_MODELLING



#assigning the variables into a new dataframe
subset_dff <- subset(Tdischharges_df, select=c(Total.Discharges, Total.U15,Total.15to44,Total.45to64,Total.65.and.above))
colnames(subset_dff)

str(subset_dff)

colnames(subset_dff)[colnames(subset_dff) == "Total.Discharges"] <- "Total_Discharges"
colnames(subset_dff)[colnames(subset_dff) == "Total.U15"] <- "Total_U15"
colnames(subset_dff)[colnames(subset_dff) == "Total.15to44"] <- "Total_15to44"
colnames(subset_dff)[colnames(subset_dff) == "Total.45to64"] <- "Total_45to64"
colnames(subset_dff)[colnames(subset_dff) == "Total.65.and.above"] <- "Total_65_and_above"

#Training and testing datasets
set.seed(1)
no_rows_data <- nrow(subset_dff)
sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE)

training_data <- subset_dff[sample, ]

testing_data <- subset_dff[-sample, ]


#Building the MLR model
#The function lm creates the relationship model between the predictor and the response variable.

fit <- lm(Total_Discharges ~ Total_U15 + Total_15to44 + Total_45to64 + Total_65_and_above, data=training_data)


#MLR model evaluation
summary(fit)


#Model summary
confint(fit)


#Regression diagnostics
#Normality and studentized residuals
#Studentized residuals example

library(car)
qqPlot(fit, labels=row.names(subset_dff), id.method="identify", simulate=TRUE, main="Q-Q Plot")

#---------------------------------------------------------------------------------------------------------------------------------

#This code generates a histogram of the studentized residuals and superimposes a normal curve, 
#kernel-density curve, and rug plot.

student_fit <- rstudent(fit)
hist(student_fit, 
     breaks=10, 
     freq=FALSE, 
     xlab="Studentized Residual", 
     main="Distribution of Errors")

rug(jitter(student_fit), col="brown")

curve(dnorm(x, mean=mean(student_fit), sd=sd(student_fit)), add=TRUE, col="blue", lwd=2)

lines(density(student_fit)$x, density(student_fit)$y, col="red", lwd=2, lty=2)

legend("topright", legend = c( "Normal Curve", "Kernel Density Curve"), lty=1:2, col=c("blue","red"), cex=.7)

#-----------------------------------------------------------------------------------------------------------------------------------

#plot the partial resudials of each factor.

crPlots(fit)

influencePlot(fit, main="Factors Influence Plot")

#-------------------------------------------------------------------------------------------------------------------------------

# AIC values comparison of normal and sqrt values.


subset_dff <- subset_dff[lapply(subset_dff,length)>0]
summary(powerTransform(training_data$`Total_Dischanrges`))
lapply(subset_dff, matrix, nrow=1)

sqrt_subset_dff <- sqrt(training_data$`Total_Discharges`)
training_data$Diagnosis_sqrt <- sqrt_subset_dff

fit_model1 <- lm(Total_Discharges ~ Total_U15 + Total_15to44 + Total_45to64 + Total_65_and_above, data=training_data)
fit_model2 <- lm(Diagnosis_sqrt ~ Total_U15 + Total_15to44 + Total_45to64 + Total_65_and_above, data=training_data)

AIC(fit_model1,fit_model2)

spreadLevelPlot(fit_model2)

################################################################################################################################################
# Stepwise Regression for normal value

library(MASS)
library(leaps)
fit_test <- lm(Total_Discharges ~ Total_U15 + Total_15to44 + Total_45to64 + Total_65_and_above, data=training_data)
stepAIC(fit_test, direction="backward")

leaps <-regsubsets(Total_Discharges ~ Total_U15 + Total_15to44 + Total_45to64 + Total_65_and_above, data=training_data, nbest=4)
plot(leaps, scale="adjr2")




library(MASS)
fit_test <- lm(Diagnosis_sqrt ~ Total_U15 + Total_15to44 + Total_45to64 + Total_65_and_above, data=training_data)
stepAIC(fit_test, direction="backward")

leaps <-regsubsets(Diagnosis_sqrt ~ Total_U15 + Total_15to44 + Total_45to64 + Total_65_and_above, data=training_data, nbest=4)
plot(leaps, scale="adjr2")

###################################################################################################################################################

# Predicting the Testing data values using predict function.

predicted_Diagnosis <- predict(fit_model1, testing_data)
predicted_Diagnosis_sqrt <- predict(fit_model2, testing_data)
Total_sqrt <- predicted_Diagnosis_sqrt ^2

act_predict <- data.frame(cbind(actuals = 'Total_Discharges', predicted = predicted_Diagnosis))
head(act_predict)

act_predict_sqrt <-  data.frame(cbind(actuals = 'Total_Discharges', predicted = Total_sqrt))
head(act_predict_sqrt)

##################################################################################################################################
correlation_accuracy <- cor(act_predict)
correlation_accuracy

correlation_accuracy <- cor(act_predict_sqrt)
correlation_accuracy

##########################################################################################################################
min_max_accuracy <- mean(apply(act_predict, 1, min) / apply(act_predict, 1, max))
min_max_accuracy

min_max_accuracy <- mean(apply(act_predict_sqrt, 1, min) / apply(act_predict_sqrt, 1, max))
min_max_accuracy

#####################################################################################################################


sigma(fit_model1)/ mean('Total_Discharges')

sigma(fit_model2)/ mean('Total_Discharges')

summary(subset_dff)


