#read data
#SECTION 1
install.packages("readxl")
library(readxl)
data <- read_excel("C:/Users/janet/Documents/CA2Exam/Dataset_2024.xlsx")
data
#Structure of data
str(data)
View(data)
#renaming Variables
colnames(data)[colnames(data)=="Age (years)"]<-"Age"
colnames(data)[colnames(data)==" Body fat (%)"]<-"Body_fat"
colnames(data)[colnames(data)=="Chest circumference (cm)"]<-"Chest_circumference"
colnames(data)[colnames(data)=="Density (g/cm³) "]<-"Density"
colnames(data)[colnames(data)==" Knee circumference (cm)"]<-"Knee_circumference"
colnames(data)[colnames(data)==" Weight (lbs) "]<-"Weight"
data
#summary of data which contain mean,median,standard deviation,1st quartile,3rd quartile,min,max
summary(data)
#preprocessing steps
sum(is.na(data))
#omit the data
# Removing rows with missing values
data <- na.omit(data)
data

# SECTION 2
# Examine initial linearity between variables in the dataset
library(psych)
pairs(data)
windows(20,10)

pairs.panels(data,
             smooth = FALSE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals
# Examine linearity in more detail using scatter plots

windows(20,12)
par(mfrow= c(4,2))

scatter.smooth(x = data$Age,
               y = data$`Body fat (%)`,
               xlab = "Age (years)",
               ylab = "Body fat", main = "Correlation of age ~ body fat")

scatter.smooth(x = data$Chest_circumference,
               y = data$`Body fat (%)`,
               xlab = "chest circumferences",
               ylab = "body fat", main = "Correlation of body fat ~ chest circumferences ")


scatter.smooth(x = data$`Density (g/cm³)`,
               y = data$`Body fat (%)`,
               main = "Correlation of Density ~ Body fat",
               xlab = "Density",
               ylab = "Body fat")

scatter.smooth(x = data$`Knee circumference (cm)` ,
               y = data$`Body fat (%)`,
               main = "Correlation of knee circumference ~ Body fat ",
               xlab = "knee circumference ",
               ylab = "Body fat")

scatter.smooth(x = data$`Weight (lbs)` ,
               y = data$`Body fat (%)`,
               main = "Correlation of Weight ~ body fat",
               xlab = "weight",
               ylab = "body fat")

# Examining correlation between Body fat and Independent variables
# here we can see brief interpretations of each correlation coefficient to explain the relationship between body fat percentage and the predictor
cor(data)
correlation_matrix<-cor(data)
windows(20,16)
corPlot(correlation_matrix)
attach(data)
# Examining the other variables
paste("Correlation for Body fat and Age: ", round(cor(`Body fat (%)`, Age),2))
paste("Correlation for Body fat and chest circumference: ", round(cor(`Body fat (%)`, Chest_circumference),2))
paste("Correlation for Body fat and Density: ", round(cor(`Body fat (%)`, `Density (g/cm³)`),2))
paste("Correlation for Body fat and Knee circumferences: ", round(cor(`Body fat (%)`, `Knee circumference (cm)`),2))
paste("Correlation for Body fat and Weight: ", round(cor(`Body fat (%)`, `Weight (lbs)`),2))
# It appears that the variables 'Density' and 'Age' have very low correlation with Body fat. 


#SECTION 3
# Check for outliers


windows(20,10)
par(mfrow = c(4, 2)) # divide graph area in 3 rows by 2 columns
attach(data)

boxplot(Age,
        main = "Age") # box plot for 'Age'

boxplot(`Body fat (%)`,
        main = "body fat") # box plot for 'body fat'

boxplot(Chest_circumference,
        main = "chest circumference") # box plot for 'chest circumference'

boxplot(`Density (g/cm³)`,
        main = "density") # box plot for 'density'


boxplot(`Knee circumference (cm)`,
        main = "knee circumference") # box plot for 'Knee circumference'


boxplot(`Weight (lbs)`,
        main = "Weight") # box plot for 'Weight'

# chest circumference variable contains outliers.
# Use boxplot.stats() function to generate relevant outliers
outlier_values <- boxplot.stats(Chest_circumference)$out # outlier values.
paste("chest circumferences outliers: ", paste(outlier_values, sep =", "))
# Remove population outliers
data<- subset(data,
                 data$Chest_circumference != 136.2
                 & data$Chest_circumference!= 128.3)
# Use boxplot.stats() function to generate relevant outliers
outlier_values <- boxplot.stats(Chest_circumference)$out # outlier values.
paste("chest circumferences outliers: ", paste(outlier_values, sep =", "))
data<- subset(data,
              data$Chest_circumference != 136.2
              & data$Chest_circumference!= 128.3)

# weight variable contains outliers.
# Use boxplot.stats() function to generate relevant outliers
outlier_values <- boxplot.stats(`Weight (lbs)`)$out # outlier values.
paste("weight outliers: ", paste(outlier_values, sep =", "))
data<- subset(data,
              data$`Weight (lbs)` != 363.15
              & data$Chest_circumference!= 262.75)

# knee circumferences variable contains outliers.
# Use boxplot.stats() function to generate relevant outliers
outlier_values <- boxplot.stats(`Knee circumference (cm)`)$out # outlier values.
paste("knee circumferences outliers: ", paste(outlier_values, sep =", "))
data<- subset(data,
              data$`Knee circumference (cm)` != 49.1
              & data$`Knee circumference (cm)`!= 45
              &data$`Knee circumference (cm)`!=46)
#SECTION 4




# Skewness function to examine normality
# install.packages("e1071")
library(e1071)
windows(30,20)
par(mfrow = c(4,2)) # divide graph area into 1 row x 2 cols

# skewness of < -1 or > 1 = highly skewed
# -1 to -0.5 and 0.5 to 1 = moderately skewed
# Skewness of -0.5 to 0.5 = approx symetrical


plot(density(data$Age),
     main = "Density plot : Age",
     ylab = "Frequency", xlab = "Age",
     sub = paste("Skewness : ", round(e1071::skewness(data$Age), 2)))
polygon(density(data$Age), col = "red")


plot(density(data$`Body fat (%)`),
     main = "Density plot : body fat",
     ylab = "Frequency", xlab = "body fat",
     sub = paste("Skewness : ", round(e1071::skewness(data$`Body fat (%)`), 2)))
polygon(density(data$`Body fat (%)`), col = "red")

plot(density(data$Chest_circumference),
     main = "Density plot : chest circumferences",
     ylab = "Frequency", xlab = "chest circumferences",
     sub = paste("Skewness : ", round(e1071::skewness(data$Chest_circumference), 2)))
polygon(density(data$Chest_circumference), col = "red")


plot(density(data$`Density (g/cm³)`),
     main = "Density plot : density",
     ylab = "Frequency", xlab = "density",
     sub = paste("Skewness : ", round(e1071::skewness(data$`Density (g/cm³)`), 2)))
polygon(density(data$`Density (g/cm³)`), col = "red")


plot(density(data$`Knee circumference (cm)`),
     main = "Density plot : knee circumferences",
     ylab = "Frequency", xlab = "knee_circumferences",
     sub = paste("Skewness : ", round(e1071::skewness(data$`Knee circumference (cm)`), 2)))
polygon(density(data$`Knee circumference (cm)`), col = "red")


plot(density(data$`Weight (lbs)`),
     main = "Density plot : weight",
     ylab = "Frequency", xlab = "weight",
     sub = paste("Skewness : ", round(e1071::skewness(data$`Weight (lbs)`), 2)))
polygon(density(data$`Weight (lbs)`), col = "red")





# Minimal skewness = -0.11 - slightly skewed to the left. 
# NB a skewness value <-1 or >1 = highly skewed. 
# Skewness -1 to -.05 and 0.5 to 1 = moderately skewed. 
# And skewness -0.5 to 0.5 = approx symetric.

paste("Skewness for Age : ", round(e1071::skewness(data$Age), 2))
paste("Skewness for Body fat : ", round(e1071::skewness(data$`Body fat (%)`), 2))
paste("Skewness for chest circumferences : ", round(e1071::skewness(data$Chest_circumference), 2))
paste("Skewness for Density : ", round(e1071::skewness(data$`Density (g/cm³)`), 2))
paste("Skewness for knee circumference: ", round(e1071::skewness(data$`Knee circumference (cm)`), 2))
paste("Skewness for Weight : ", round(e1071::skewness(data$`Weight (lbs)`), 2))
# chest_circumference = 1.8 => highly skewed
# body fat and knee circumference= 0.88 => moderatly skewed
# all other variables seem okay


# It seems that population must be converted
# Data is visually skewed to the right
windows(16,10)
par(mfrow=c(2,4))


#Check linearity using histogram
hist(data$Chest_circumference)
hist(data$`Knee circumference (cm)`)
hist(data$`Body fat (%)`)

# Check normality of all variables using normality test
shapiro.test(data$Age)
shapiro.test(data$`Body fat (%)`) 
shapiro.test(data$Chest_circumference)
shapiro.test(data$`Density (g/cm³)`)
shapiro.test(data$`Knee circumference (cm)`) 
shapiro.test(data$`Weight (lbs)`)
# If p-value < 0.05 then variable is not normally distributed
#age is not normaly distributed p value=0.001
#body fat is normaly distributed p value =0.1
#chest circumference is not normally distributed p value=0.006
#density is normaly distributed p value=0.5
#knee circumference is normaly distributed p value =0.26
#weigt is noemaly distributed p value=0.06


#SECTION 5
attach(data)
library(MASS)
View(data)
box_cox_transform <- boxcox(`Body fat (%)`~ Age)
box_cox_transform
windows(20,10)
lamda <-box_cox_transform$x[which.max(box_cox_transform$y)]
lamda
normalised_Age <-(`Body fat (%)`^lamda-1)/lamda
normalised_Age
hist(normalised_Age)
shapiro.test(normalised_Age)

##Modify the variable Population
data$new_age<- normalised_Age
shapiro.test(data$Age)
cor(data)
#SECTION 6
##Regression model using transformed variables
str(data)
attach(data)
model_1 <- lm(`Body fat (%)` ~ 
                 new_age + 
                 Chest_circumference +
                 `Density (g/cm³)` + 
                 `Knee circumference (cm)` + 
                 `Weight (lbs)`)

model_1
summary(model_1)
#new age,chest circumference and weight have p value >0,05

attach(data)
model_2 <- lm(`Body fat (%)` ~
                `Density (g/cm³)` + 
                `Knee circumference (cm)` )

model_2
summary(model_2)

AIC(model_1)
AIC(model_2)
BIC(model_1)
BIC(model_2)
#model_1 have less AIC and BIC values.so we can say that model1 is better.

##residuals normally distributed
shapiro.test(residuals(model_2))
##residuals diffrent from zero
t.test(residuals(model_2),mu=0) 


# randomness of residuals (autocorrelation) -- Durbin-Watson test
#install.packages("lmtest")
library(lmtest)
dwtest(model_2)

#Check for multi_collinearity
library(faraway)
v1 <-vif(model_2)
v1

#SECTION 7
#### Regression model created using original variables
str(data)
attach(data)
model_3 <- lm(`Body fat (%)` ~ 
                new_age + 
                Chest_circumference +
                `Density (g/cm³)` + 
                `Knee circumference (cm)` + 
                `Weight (lbs)`)

model_3
summary(model_3)
#new age,chest circumference and weight have p value >0,05

attach(data)
model_4 <- lm(`Body fat (%)` ~
                `Density (g/cm³)` + 
                `Knee circumference (cm)` )

model_4
summary(model_4)

AIC(model_3)
AIC(model_4)
BIC(model_3)
BIC(model_4)
#model 3 have less AIC and BIC values .so we can say that model_3 is better model

#SECTION 8
##residuals normally distributed
shapiro.test(residuals(model_4))
##residuals diffrent from zero
t.test(residuals(model_4),mu=0) 

# randomness of residuals (autocorrelation) -- Durbin-Watson test
#install.packages("lmtest")
library(lmtest)
dwtest(model_4)

#Check for multi_collinearity
library(faraway)
v1 <-vif(model_4)
v1


