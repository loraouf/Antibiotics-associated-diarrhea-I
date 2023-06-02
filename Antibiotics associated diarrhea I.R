#Data summary
load('AAD.RData')
data <- AAD; rm(AAD)
str(data)
summary(data)

#Calculate the following: mean, median, minimum, maximum, first and third quartile.

#Patient.ID
#table(data$Patient.ID)
#tapply(data$D1.Shannon.diversity,list(ID=data$Patient.ID),mean,na.rm=TRUE)

#Antibiotic.class
table(data$Antibiotic.class)
tapply(data$D1.Shannon.diversity,list(Antibiotic=data$Antibiotic.class),mean,na.rm=TRUE)

#D1.Shannon.diversity
mean(data$D1.Shannon.diversity, trim = 0, na.rm = TRUE)
median(data$D1.Shannon.diversity, na.rm = TRUE)
min(data$D1.Shannon.diversity, na.rm = TRUE)
max(data$D1.Shannon.diversity,na.rm = TRUE)
quantile(data$D1.Shannon.diversity,na.rm=TRUE,c(0.25,0.75))


#D6.Shannon.diversity
mean(data$D6.Shannon.diversity, na.rm = TRUE)
median(data$D6.Shannon.diversity, na.rm = TRUE)

min(data$D6.Shannon.diversity, na.rm = TRUE)
max(data$D6.Shannon.diversity,na.rm = TRUE)
quantile(data$D6.Shannon.diversity,na.rm=TRUE,c(0.25,0.75))
#cor
cor(data$D1.Shannon.diversity,data$D6.Shannon.diversity, use="complete.obs")

#D1.Chao1.diversity
mean(data$ D1.Chao1.diversity, trim = 0, na.rm = TRUE)
median(data$D1.Chao1.diversity, na.rm = TRUE)
min(data$D1.Chao1.diversity, na.rm = TRUE)
max(data$D1.Chao1.diversity,na.rm = TRUE)
quantile(data$D1.Chao1.diversity,na.rm=TRUE,c(0.25,0.75))


#D6.Chao1.diversity
mean(data$D6.Chao1.diversity, trim = 0, na.rm = TRUE)

median(data$D6.Chao1.diversity, na.rm = TRUE)
min(data$D6.Chao1.diversity, na.rm = TRUE)
max(data$D6.Chao1.diversity,na.rm = TRUE)
quantile(data$D6.Chao1.diversity,na.rm=TRUE,c(0.25,0.75))
#cor
cor(data$D1.Chao1.diversity,data$D6.Chao1.diversity, use="complete.obs")


#D1.D6.Jaccard.distance
mean(data$D1.D6.Jaccard.distance, trim = 0, na.rm = TRUE)
median(data$D1.D6.Jaccard.distance, na.rm = TRUE)
min(data$D1.D6.Jaccard.distance, na.rm = TRUE)
max(data$D1.D6.Jaccard.distance,na.rm = TRUE)
quantile(data$D1.D6.Jaccard.distance,na.rm=TRUE,c(0.25,0.75))

#Outcome
table(data$Outcome)
tapply(data$D1.Shannon.diversity,c(Outcome=data$Outcome),mean,na.rm=TRUE)
#---------------------------------------------------------------------------------------

#Graphics

#Generate a bar chart of a categorical variable for the Outcome (AAD, CDI ,ND)
barplot(table(data$Outcome), xlab="Outcome",ylab="Frequency",main=" categorical variable for the Outcome ")

#Generate a bar chart graph with mean Outcome in BPL, FQN, OBL
barplot(tapply(data$D1.Shannon.diversity, data$Antibiotic.class, mean,  na.rm = TRUE), xlab= "antibiotics", ylab="Mean",main=" Mean in outcome ")

####################################
#Make a histogram of a continuous variable: “D1 Chao” as well as “D6 Chao”.
hist(data$D1.Chao1.diversity,xlab = "D1 Chao",main="continuous variable “D1 chao”")
hist(data$D6.Chao1.diversity,xlab = "D6 Chao",main="continuous variable “D6 chao”")

#Make a scatterplot of 2 continuous variables D1 Chao and D6 Chao, and add the regression lines for each antibiotics
plot(D1.Chao1.diversity~D6.Chao1.diversity, data=data,main=" scatterplot D1 chao , D6 Chao ")
plot(data$D1.Chao1.diversity[data$Antibiotic.class=="FQN"],data$D6.Chao1.diversity[data$Antibiotic.class=="FQN"],xlab="D1 chao",ylab="D6 chao2", col="red")
points(data$D1.Chao1.diversity[data$Antibiotic.class=="OBL"],data$D6.Chao1.diversity[data$Antibiotic.class=="OBL"],col="green")
points(data$D1.Chao1.diversity[data$Antibiotic.class=="PBL"],data$D6.Chao1.diversity[data$Antibiotic.class=="PBL"],col="blue")
abline(lm(data$D1.Chao1.diversity[data$Antibiotic.class=="FQN"]~data$D6.Chao1.diversity[data$Antibiotic.class=="FQN"]),col="red")
abline(lm(data$D1.Chao1.diversity[data$Antibiotic.class=="OBL"]~data$D6.Chao1.diversity[data$Antibiotic.class=="OBL"]),col="green")
abline(lm(data$D1.Chao1.diversity[data$Antibiotic.class=="PBL"]~data$D6.Chao1.diversity[data$Antibiotic.class=="PBL"]),col="blue")

#Make a boxplot of Jacard distance and a separate boxplots per Antbiotics (as.factors).
boxplot(D1.D6.Jaccard.distance~Antibiotic.class, data=data)

#---------------------------------------------------------------------------
#Outlier detection
#	Explore the data for any existing outliers, identify them (do NOT remove them if found).
boxplot(data$D1.Shannon.diversity)$out
boxplot(data$D6.Shannon.diversity)$out
boxplot(data$D1.Chao1.diversity)$out
boxplot(data$D6.Chao1.diversity)$out
boxplot(data$D1.D6.Jaccard.distance)$out
#----------------------------------------------------------------------------

#Testing for normality/ homoscedasticity (for all features mentioned above)
#QQ plot
par(mfrow=c(1,2))
qqnorm(data[data$Antibiotic.class=="OBL",]$D1.Shannon.diversity, xlab="D1.Shannon")
qqline(data[data$Antibiotic.class=="OBL",]$D1.Shannon.diversity, xlab="D1.Shannon")

qqnorm(data[data$Antibiotic.class=="OBL",]$D6.Shannon.diversity, xlab="D6.Shannon")
qqline(data[data$Antibiotic.class=="OBL",]$D6.Shannon.diversity, xlab="D6.Shannon")

qqnorm(data[data$Antibiotic.class=="OBL",]$D1.D6.Jaccard.distance, xlab="D1.D6.Jaccard.distance")
qqline(data[data$Antibiotic.class=="OBL",]$D1.D6.Jaccard.distance, xlab="D1.D6.Jaccard.distance")

qqnorm(data[data$Antibiotic.class=="FQN",]$D1.Shannon.diversity, xlab="D1.Shannon")
qqline(data[data$Antibiotic.class=="FQN",]$D1.Shannon.diversity, xlab="D1.Shannon")

qqnorm(data[data$Antibiotic.class=="FQN",]$D6.Shannon.diversity, xlab="D6.Shannon")
qqline(data[data$Antibiotic.class=="FQN",]$D6.Shannon.diversity, xlab="D6.Shannon")

qqnorm(data[data$Antibiotic.class=="FQN",]$D1.D6.Jaccard.distance, xlab="D1.D6.Jaccard.distance")
qqline(data[data$Antibiotic.class=="FQN",]$D1.D6.Jaccard.distance, xlab="D1.D6.Jaccard.distance")

qqnorm(data[data$Antibiotic.class=="PBL",]$D1.Shannon.diversity, xlab="D1.Shannon")
qqline(data[data$Antibiotic.class=="PBL",]$D1.Shannon.diversity, xlab="D1.Shannon")

qqnorm(data[data$Antibiotic.class=="PBL",]$D6.Shannon.diversity, xlab="D6.Shannon")
qqline(data[data$Antibiotic.class=="PBL",]$D6.Shannon.diversity, xlab="D6.Shannon")

qqnorm(data[data$Antibiotic.class=="PBL",]$D1.D6.Jaccard.distance, xlab="D1.D6.Jaccard.distance")
qqline(data[data$Antibiotic.class=="PBL",]$D1.D6.Jaccard.distance, xlab="D1.D6.Jaccard.distance")

#####Chao

qqnorm(data[data$Antibiotic.class=="OBL",]$D1.Chao1.diversity, xlab="D1.Chao1")
qqline(data[data$Antibiotic.class=="OBL",]$D1.Chao1.diversity, xlab="D1.Chao1")

qqnorm(data[data$Antibiotic.class=="FQN",]$D6.Chao1.diversity, xlab="D1.Chao1")
qqline(data[data$Antibiotic.class=="FQN",]$D6.Chao1.diversity, xlab="D1.Chao1")

qqnorm(data[data$Antibiotic.class=="PBL",]$D1.Chao1.diversity, xlab="D1.Chao1")
qqline(data[data$Antibiotic.class=="PBL",]$D1.Chao1.diversity, xlab="D1.Chao1")



# shapiro
par(mfrow=c(1,2))
shapiro.test(data$D1.Shannon.diversity )
shapiro.test(data$D6.Shannon.diversity)
shapiro.test(data$D1.Chao1.diversity )
shapiro.test(data$D6.Chao1.diversity )
shapiro.test(data$D1.D6.Jaccard.distance)

#hist
par(mfrow=c(1,2))
hist(data$D1.Shannon.diversity,xlab="D1.Shannon",main="D1.shannon")
hist(data$D6.Shannon.diversity,xlab="D6.shannon",main="D6.shannon")
hist(data$D1.Chao1.diversity,xlab="D1.chao1",main="D1.chao1")
hist(data$D6.Chao1.diversity,xlab="D6.chao1",main="D6.chao1")
hist(data$D1.D6.Jaccard.distance,xlab="D1.D6.Jaccard.distance",main="D1.D6.Jaccard.distance")


# Check the homoscedasticity.
homoscedaticity = lm(D1.Shannon.diversity~D6.Shannon.diversity,data= data)
par(mfrow = c(1,1))
hist(homoscedaticity$residuals)
plot(homoscedaticity, 1)
plot(homoscedaticity, 2)
plot(homoscedaticity, 3)
plot(homoscedaticity, 4)
plot(homoscedaticity, 5)
plot(homoscedaticity, 6)

library(car)
#leveneTest(D1.Shannon.diversity~Antibiotic.class, data)
#leveneTest(D6.Shannon.diversity~Antibiotic.class, data)
#leveneTest(D1.Chao1.diversity~Antibiotic.class, data)
#leveneTest(D6.Chao1.diversity~Antibiotic.class, data)
#leveneTest(D1.D6.Jaccard.distance~Outcome, data)
bartlett.test(list(data$D1.Shannon.diversity,data$D6.Shannon.diversity))
bartlett.test(list(data$D1.Chao1.diversity,data$D6.Chao1.diversity))
var.test(data$D1.Shannon.diversity,data$D6.Shannon.diversity)
var.test(data$D1.Chao1.diversity,data$D6.Chao1.diversity)
boxplot(D1.Shannon.diversity~Antibiotic.class, data)
boxplot(D1.Shannon.diversity~Outcome, data)
boxplot(D6.Shannon.diversity~Antibiotic.class, data)
boxplot(D6.Shannon.diversity~Outcome, data)
boxplot(D1.Chao1.diversity~Antibiotic.class, data)
boxplot(D1.Chao1.diversity~Outcome, data)
boxplot(D6.Chao1.diversity~Antibiotic.class, data)
boxplot(D6.Chao1.diversity~Outcome, data)
boxplot(D1.D6.Jaccard.distance~Antibiotic.class, data)

#-------------------------------------------------------------------------------------------
#99% confidence interval
t.test(data[data$Antibiotic.class=="OBL",]$D6.Shannon.diversity, conf.level = 0.99)
t.test(data[data$Antibiotic.class=="FQN",]$D6.Shannon.diversity, conf.level = 0.99)
t.test(data[data$Antibiotic.class=="PBL",]$D6.Shannon.diversity, conf.level = 0.99)


#95% confidence interval
t.test(data[data$Antibiotic.class=="OBL",]$D6.Shannon.diversity, conf.level = 0.95)
t.test(data[data$Antibiotic.class=="FQN",]$D6.Shannon.diversity, conf.level = 0.95)
t.test(data[data$Antibiotic.class=="PBL",]$D6.Shannon.diversity, conf.level = 0.95)

#90% confidence interval
t.test(data[data$Antibiotic.class=="OBL",]$D6.Shannon.diversity, conf.level = 0.90)
t.test(data[data$Antibiotic.class=="FQN",]$D6.Shannon.diversity, conf.level = 0.90)
t.test(data[data$Antibiotic.class=="PBL",]$D6.Shannon.diversity, conf.level = 0.90)

#D6,chao
#99% confidence interval
t.test(data[data$Antibiotic.class=="OBL",]$D6.Chao1.diversity, conf.level = 0.99)
t.test(data[data$Antibiotic.class=="FQN",]$D6.Chao1.diversity, conf.level = 0.99)
t.test(data[data$Antibiotic.class=="PBL",]$D6.Chao1.diversity, conf.level = 0.99)


#95% confidence interval
t.test(data[data$Antibiotic.class=="OBL",]$D6.Chao1.diversity, conf.level = 0.95)
t.test(data[data$Antibiotic.class=="FQN",]$D6.Chao1.diversity, conf.level = 0.95)
t.test(data[data$Antibiotic.class=="PBL",]$D6.Chao1.diversity, conf.level = 0.95)

#90% confidence interval
t.test(data[data$Antibiotic.class=="OBL",]$D6.Chao1.diversity, conf.level = 0.90)
t.test(data[data$Antibiotic.class=="FQN",]$D6.Chao1.diversity, conf.level = 0.90)
t.test(data[data$Antibiotic.class=="PBL",]$D6.Chao1.diversity, conf.level = 0.90)

means <- tapply(data$D1.D6.Jaccard.distance,list(Antibiotic.class=AAD$Antibiotic.class),mean,na.rm=TRUE)
sd <- tapply(AAD$D1.D6.Jaccard.distance,list(Antibiotic.class=AAD$Antibiotic.class),sd,na.rm=TRUE)
x = means
s = sd
n = 335
#90%
margin_error <- qt(0.90,df=n-1)*s/sqrt(n)
lowerinterval <- x - margin_error
upperinterval <- x + margin_error
lowerinterval 
upperinterval 

#95%
margin_error <- qt(0.95,df=n-1)*s/sqrt(n) 
margin_error
lowerinterval <- x - margin_error
upperinterval <- x + margin_error
lowerinterval 
upperinterval 

#99%
margin_error <- qt(0.99,df=n-1)*s/sqrt(n)
lowerinterval <- x - margin_error
upperinterval <- x + margin_error
lowerinterval
upperinterval

#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 
## hypothesis testing
## Assuming normality and equal variances, hypothesis testing
t.test(data[data$Outcome == 'CDI', "D1.Shannon.diversity"], data[data$Outcome == 'CDI', "D6.Shannon.diversity"], var.equal = TRUE)
t.test(data[data$Outcome == 'CDI', "D1.Chao1.diversity"], data[data$Outcome == 'CDI', "D6.Chao1.diversity"], var.equal = TRUE)
## p-value > 0.05 (not significant). Chao diversity is not different in day 6 from day 1 for CDI outcome.


## testing normality
library(fBasics)
normalTest(data[data$Outcome == 'CDI', "D1.Shannon.diversity"]) ## p-value 0.7488 (not significant; normally distributed)
normalTest(data[data$Outcome == 'CDI', "D6.Shannon.diversity"]) ## p-value 0.1407 (not significant; normally distributed)
normalTest(data[data$Outcome == 'CDI', "D1.Chao1.diversity"]) ## p-value 0.636 (not significant; normally distributed)
normalTest(data[data$Outcome == 'CDI', "D6.Chao1.diversity"]) ## p-value 0.04323 (significant; NOT normally distributed)

## homogeneity of variance test
var.test(data[data$Outcome == 'CDI', "D1.Shannon.diversity"], data[data$Outcome == 'CDI', "D6.Shannon.diversity"]) ## p-value = 0.05039 (not significant; variances are not different)
var.test(data[data$Outcome == 'CDI', "D1.Chao1.diversity"], data[data$Outcome == 'CDI', "D6.Chao1.diversity"]) ## p-value = 0.7106 (not significant; variances are not different)

## 
t.test(data[data$Antibiotic.class == 'PBL','D6.Chao1.diversity'], data[data$Antibiotic.class == 'FQN','D6.Chao1.diversity'], var.equal = FALSE) 
## p-value = 0.228 (not significant). The two antibiotics are not different from one another on the level of chao diversity.

var.test(data[data$Antibiotic.class == 'PBL','D6.Chao1.diversity'], data[data$Antibiotic.class == 'FQN','D6.Chao1.diversity'])
## p-value = 0.3004. variances are not statistically different (not in heteroscedasiticy)


## Chao for all antibiotics together
t.test(data[,'D1.Chao1.diversity'], data[,'D6.Chao1.diversity']) ## p-value = 2.306e-05 (significant)


## post-hoc test
## Chao for FQN,  
t.test(data[data$Antibiotic.class == 'FQN','D1.Chao1.diversity'], data[data$Antibiotic.class == 'FQN','D6.Chao1.diversity']) ## p-value = 0.002091 (significant)

## Chao for OBL 
t.test(data[data$Antibiotic.class == 'OBL','D1.Chao1.diversity'], data[data$Antibiotic.class == 'OBL','D6.Chao1.diversity']) ## p-value = 0.004721 (significant)

## Chao for PBL 
t.test(data[data$Antibiotic.class == 'PBL','D1.Chao1.diversity'], data[data$Antibiotic.class == 'PBL','D6.Chao1.diversity']) ## p-value = 0.05336 (not significant)


interaction.plot(data$Outcome, data$Antibiotic.class, data$D6.Chao1.diversity)

#Linear Model
#Fit a linear regression to the data and interpret the regression coefficient (for the one of the hypotheses mentioned above)
linear.regression<-lm(D6.Chao1.diversity ~ D1.Chao1.diversity + Antibiotic.class, data)

plot(linear.regression)
summary(linear.regression)


#Calculate and interpret a 95% confidence interval of the regression slope [bonus]
confint(linear.regression, level = 0.95)

#Estimating the average CHAO change for with changing the Antibiotics
coef(linear.regression)["Antibiotic.classPBL"] - coef(linear.regression)["Antibiotic.classOBL"]


#Fit a linear regression to the data and interpret the regression coefficient (for the one of the hypotheses mentioned above), taking into account repeated measures!

data$Antibiotic.class <- as.factor(data$Antibiotic.class)
mod <- manova(cbind(D6.Chao1.diversity, D1.Chao1.diversity) ~  Antibiotic.class + Outcome, data)
anova(mod)
coef(mod)
summary(aov(mod))