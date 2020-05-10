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


