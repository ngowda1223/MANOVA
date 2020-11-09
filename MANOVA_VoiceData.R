library(psych) #install.packages("psych") 
library(psy) #install.packages("psy") 
library(lsr)  #install.packages("lsr") 
library(ggplot2) #install.packages("ggplot2") 
library(car) #install.packages("car") 
library(MASS) #install.packages("MASS") 
library(plyr) #install.packages("plyr") 
library(foreign) #install.packages("foreign") 
library(stringr) #install.packages("stringr")
library(msm) #install.packages("msm")
library(sandwich) #install.packages("sandwich")
library(coin) #install.packages("coin")
library(xlsx) #install.packages("xlsx") #install.packages("readxl")
library(rJava) #install.packages("rJava")
library(readxl) #to read excel
library(mvnormtest) #install.packages("mvnormtest") #for normality test
library(dplyr) #install.packages("dplyr") #to drop columns
library(biotools) #install.packages("biotools") #for V-CoV Matrices of Groups test


#H0 = TTCT, Glances, and Steering Reversals have no effect on the preference between Hard-key and Voice modalities of interaction.
#If MANOVA shows statistical significant interaction effects, then we drop H0 and run T-test/ANOVA on pairs of DVs.

studydata <- read_excel("C:\\Users\\Admin\\Downloads\\FakeDataSet.xlsx") 
attach(studydata)
#search()
#detach(studydata)

studydata_clean <- select(studydata, -c(Sample, Participant, Modality)) #dropping categorical data and misc data columns for normality test below


#Testing Independence Assumption
ICC(studydata[,3:6])

#Testing of Normality Assumption (p-value should be greater than 0.05?)
transpose_studydata <- t(studydata_clean)
mshapiro.test(transpose_studydata)

#Testing Positive Determinant of Variance-Covariance Matrix Assumption (needs to be highly positive?)
cov(studydata[,3:6])
det(cov(studydata[,3:6]))

#Testing Equality of Variance-Covariance Matrices of Groups (p-value should be greater than 0.05?)
group = factor(Modality)
#factor(group)
#group    #to print the data
boxM(studydata_clean, group)

#MANOVA
group = factor(Modality)
Y = cbind(TTCT, Glances, Preference, SteeringRev)
MANOVA.Result = manova(Y~group)

summary(MANOVA.Result, test = "Wilks")
summary(MANOVA.Result, test = "Pillai")
summary(MANOVA.Result, test = "Hotelling-Lawley")
summary(MANOVA.Result, test = "Roy")
