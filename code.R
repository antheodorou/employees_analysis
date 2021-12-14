#Q1
require(foreign)
salary <- read.spss("salary.sav", to.data.frame = "T")
str(salary)

#Q2
library(psych)
df <- salary[unlist(lapply(salary, is.numeric))] #get only the numeric variables
summary(df)

for (j in seq(2)) {
  par(mfrow=c(2,4))
  if(j == 1){
    for (i in colnames(df)) {
      hist(df[,i], probability = TRUE, xlab = i, main = paste("Histogram of", i))
      lines(density(df[,i]), col = "red")
    }
  } else {
    for (i in colnames(df)) {
      qqnorm(df[,i], main = paste("Normal Q-Q Plot for", i), xlab = i)
      qqline(df[,i])
    }
  }
}

#Q3
length(salary$salbeg)

#normality tests
library(nortest)
lillie.test(salary$salbeg)
shapiro.test(salary$salbeg)

#symmetry tests
mean(salary$salbeg)
median(salary$salbeg)

library(lawstat)
symmetry.test(salary$salbeg)

wilcox.test(salary$salbeg, mu = 1000)

#Q4
dif <- salary$salnow - salary$salbeg
length(dif)

#normality tests
lillie.test(dif)
shapiro.test(dif)

#visualization of the difference
par(mfrow=c(1,2))
hist(dif, probability = TRUE)
lines(density(dif), col = "red")
qqnorm(dif)
qqline(dif)

#symmetry tests
mean(dif)
median(dif)
symmetry.test(dif)

wilcox.test(dif)

par(mfrow=c(1,1))
boxplot(dif)

#Q5
dataset <- data.frame(salary = salary$salbeg, sex = factor(salary$sex, levels(salary$sex)))
length(dataset$sex[dataset$sex == "MALES"])
length(dataset$sex[dataset$sex != "MALES"])

by(salary$salbeg, salary$sex,lillie.test) 
by(salary$salbeg, salary$sex,shapiro.test)


with(dataset, tapply(salary, sex, mean))
with(dataset, tapply(salary, sex, median))
with(dataset, tapply(salary, sex, symmetry.test))

by(salary$salbeg, salary$sex, wilcox.test)

boxplot(salbeg~sex, data = salary)

#Q6
library(Hmisc)
age_cut <- cut2(salary$age, g= 3)
dataset2 <- data.frame(salary = salary$salbeg, age = factor(age_cut, levels(age_cut)))

anova1 <- aov(salary~age_cut, data = dataset2)
anova1
summary(anova1)

oneway.test(salary~age_cut, data = dataset2)

#normality
library(nortest)
lillie.test(anova1$residuals)
shapiro.test(anova1$residuals)
qqnorm(anova1$residuals)
qqline(anova1$residuals)

#homoscedasticity
bartlett.test(salary~age_cut, data = dataset2)
fligner.test(salary~age_cut, data = dataset2)
library(car)
leveneTest(salary~age_cut, data = dataset2)

kruskal.test(salary~age_cut, data = dataset2)

pairwise.wilcox.test(dataset2$salary, dataset2$age)
boxplot(salary~age_cut, data = dataset2)

#Q7
tab <- table(salary$minority, salary$sex)
tab
tab2 <- tab[1,]
tab2

round(prop.table(tab2), 2)
chisq.test(tab2)