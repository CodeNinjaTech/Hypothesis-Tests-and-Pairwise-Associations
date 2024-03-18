##################################################################################################################################
##################################################################################################################################
##                                                                                                                              ##
##                                                  ANSWER TO QUESTION 1                                                        ##
##                                                                                                                              ##
##################################################################################################################################
##################################################################################################################################

# install.packages("haven")
# library(haven)
# salary <- as.data.frame(read_sav(file))
file <- "D:\\Lefteris files\\Documents\\Business Analytics\\1. Fall Semester\\Statistics for BA I\\Assignments\\Lab_Assignments\\salary.sav"
# install.packages("foreign")
library(foreign)
salary <- read.spss(file, to.data.frame=TRUE)
str(salary)
# salary is a data frame containing 474 observations of the following 11 variables:
# id, a numeric vector, labeled as employee code
# salbeg, a numeric vector, labeled as beginning salary
# sex, a factor, labeled as sex of employee, with two levels: 0 as males and 1 as females
# time, a numeric vector, labeled as job seniority
# age, a numeric vector, labeled as age of employee
# salnow, a numeric vector, labeled as current salary
# edlevel, a numeric vector, labeled as educational level (not clear though what value depicts which educational level)
# work, a numeric vector, labeled as work experience
# jobcat, a factor, labeled as employment category, with the following seven levels:
#         value            level
#             1         CLERICAL
#             2   OFFICE TRAINEE
#             3 SECURITY OFFICER
#             4  COLLEGE TRAINEE
#             5  EXEMPT EMPLOYEE
#             6      MBA TRAINEE
#             7        TECHNICAL
# minority, a factor, labeled as minority classification, with two levels: 0 as white and 1 as nonwhite
# sexrace, a factor, labeled as sex & race classification, with the following four levels:
#         value            level
#             1      WHITE MALES
#             2   MINORITY MALES
#             3    WHITE FEMALES
#             4 MINORITY FEMALES


##################################################################################################################################
##################################################################################################################################
##                                                                                                                              ##
##                                                  ANSWER TO QUESTION 2                                                        ##
##                                                                                                                              ##
##################################################################################################################################
##################################################################################################################################

# Id, although numeric, is not statistically significant to be visualized as every employee has a unique id in ascending order.
nums <- sapply(salary,class)=='numeric'
nums[1] <- FALSE
salary.num <- salary[,nums]
summary <- summary(salary.num)
# install.packages("psych")
library(psych)
describe <- round(t(describe(salary.num)),2)
# Histogram and Q-Q plot of a normal distribution (rnorm)
# Normal (or Gaussian) distribution is a type of continuous probability distribution for a real-valued random variable
# 1. A variable that is normally distributed has a histogram that is bell-shaped, with only one peak, and is symmetric 
# around the mean.
# 2. (Q-Q) plot, shows the distribution of the data against the expected normal distribution. For normally distributed data, 
# observations should lie approximately on a straight line. As all the points fall approximately along this reference line, 
# we can assume normality.
set.seed(1) 
norm <- rnorm(1000)
round(t(describe(norm)),2)
hist(norm, freq = FALSE)
lines(x = density(norm), col = "red")
qqnorm(norm)
qqline(norm)

# All plots in a function
eda.plots <- function(data, ask=F){
  graphics.off()
  numeric.only <- sapply(data,class)=='numeric'
  y <- data[,numeric.only]
  n<-ncol(y)
  for (i in 1:n){
    if (!ask) win.graph()
    par(mfrow=c(2,2), ask=ask)
    y1 <- y[,i]
    vioplot(y1)
    hist(y1, probability=TRUE, main=names(y)[i])
    lines(density(y1), col=2)
    qqnorm(y1, main=names(y)[i])
    qqline(y1)
    boxplot(y1, main=names(y)[i], horizontal=TRUE)
  }
}
# install.packages("vioplot")
library(vioplot)
eda.plots(salary.num, ask=T)
graphics.off()

# Q-Q Plots
# Observations should lie approximately on the straight Q-Q line. As all the points do not fall approximately along this 
# reference line, we can not assume normality, by only viewing Q-Q Plots.
par(mfrow=c(2,3))
for (i in 1:length(salary.num)){
  qqnorm(salary.num[,i], main = paste("Q-Q Plot of" , names(salary.num)[i]))
  qqline(salary.num[,i])
}

# Log Q-Q Plots
# As natural logarithm of 0 is undefined we add to the variable work a small positive value.
# As before, even after log transformation, a large amount of points do not fall on the Q-Q line, even for variables time, work
sapply(salary.num,min)
salary.num[,6] <- salary.num[,6]+0.5
salary.num.log<-log(salary.num)
p<-ncol(salary.num.log)
par(mfrow=c(2,3))
for (i in 1:p){
  qqnorm(salary.num.log[,i], main = paste("Q-Q Plot of Log" , names(salary.num)[i]))
  qqline(salary.num.log[,i])
}

# Histograms and Density Plots (red) against Normal Distribution Density Plot (blue)
# As before, the two density plots are not even close. Even variable time that seems symmetric, is not bell-shaped and 
# has many peaks.
p<-ncol(salary.num)
par(mfrow=c(2,3))
for (i in 1:p){
  hist(salary.num[,i], main=names(salary.num)[i], probability=TRUE)
  lines(density(salary.num[,i]), col=2)
  index <- seq(min(salary.num[,i]), max(salary.num[,i]), length.out=500)
  ynorm <- dnorm(index, mean=mean(salary.num[,i]), sd(salary.num[,i]) )
  lines(index, ynorm, col=4, lty=3, lwd=3 )
}
par(mfrow=c(1,1))


##################################################################################################################################
##################################################################################################################################
##                                                                                                                              ##
##                                                  ANSWER TO QUESTION 3                                                        ##
##                                                                                                                              ##
##################################################################################################################################
##################################################################################################################################

# Hypothesis test for a single continuous variable
# Variable: salbeg
# Null Hypothesis:  h0: μ = 1000
# Alternative:      h1: μ != 1000

# install.packages("nortest")
# install.packages("lawstat")
# I believe constructing a function to handle the answer explains all cases and choices of tests
library(nortest)
library(lawstat)
single_cont <- function(data, column, condition, sl = 0.05){
  var1 <- data[,column]
  par(mfrow=c(1,2))
  # Can we assume normality?
  test1 <- 1
  if (length(var1) > 50){
    test1 <- lillie.test(var1)$p.value
  }
  test2 <- shapiro.test(var1)$p.value
  qqnorm(var1, main = paste("Q-Q Plot of" , names(data)[column]))
  qqline(var1)
  if ((test1 < sl) | (test2 < sl)){
    # Is the sample large?
    if (length(var1) > 50){
      # Is the mean a sufficient descriptive measure for central location?
      test3 <- symmetry.test(var1)$p.value
      boxplot(var1, main = paste("Boxplot of" , names(data)[column]), horizontal=TRUE)
    }
    if ((length(var1) <= 50) || (test3 < sl)){
      final <- wilcox.test(var1, mu = condition)$p.value
    }
  }
  if (((test1 >= sl) && (test2 >= sl)) || (test3 >= sl)){
    final <- t.test(var1, mu = condition)$p.value
  }
  if (final < sl){
    message <- paste("We reject the null hypothesis. P-value =", round(final,2))
  } else {
    message <- paste("We cannot reject the null hypothesis. P-value =", round(final,2))
  }
  return(message)
  par(mfrow=c(1,1))
}
single_cont(salary, 2, 1000)


##################################################################################################################################
##################################################################################################################################
##                                                                                                                              ##
##                                                  ANSWER TO QUESTION 4                                                        ##
##                                                                                                                              ##
##################################################################################################################################
##################################################################################################################################

# Hypothesis test for difference between the two dependent values/measurements
# Variable: diff of salnow and salbeg
# Null Hypothesis:  h0: μ = 0
# Alternative:      h1: μ != 0

x <- salary$salnow - salary$salbeg
length(x)
# Above 50, so KS + SW
lillie.test(x)
shapiro.test(x)
qqnorm(x, main = paste("Q-Q Plot of diff of" , names(salary)[6], "and", names(salary)[2]))
qqline(x)
# So we cannot assume normality and the sample is large
symmetry.test(x)
boxplot(salary$salnow,salary$salbeg, main = paste("Boxplot of" , names(salary)[2], "and", names(salary)[6]), horizontal=TRUE)
# So the mean is not a sufficient descriptive measure of central location for the difference as it is not symmetric
wilcox.test(x, mu = 0)
mean(salary$salnow)/mean(salary$salbeg)
median(salary$salnow)/median(salary$salbeg)
boxplot(x, main = paste("Boxplot of diff of" ,  names(salary)[6], "and", names(salary)[2]), horizontal=TRUE)
# So we reject the null hypothesis. There is significant difference between the beginning and current salary and in fact the
# current salary is about the double of the beginning


##################################################################################################################################
##################################################################################################################################
##                                                                                                                              ##
##                                                  ANSWER TO QUESTION 5                                                        ##
##                                                                                                                              ##
##################################################################################################################################
##################################################################################################################################

# Hypothesis test for two samples - Testing for the association between a continuous and a categorical variable of two levels
# Variables: continuous: salbeg, categorical: sex
# Null Hypothesis:  h0: μ1 = μ2
# Alternative:      h1: μ1 != μ2

dataset1 <- salary[,2:3]
n1 <- nrow(subset(dataset1, dataset1$sex=="MALES"))
n2 <- nrow(subset(dataset1, dataset1$sex=="FEMALES"))
n1 > 50
n2 > 50
# As at least 1 of the 2 samples amount is more than 50 (the other one is also above 50), we test normality with KS, SW, Q-Q Plot
by(dataset1$salbeg, dataset1$sex, lillie.test)
by(dataset1$salbeg, dataset1$sex, shapiro.test)
males <- subset(dataset1, dataset1$sex=="MALES")
females <- subset(dataset1, dataset1$sex=="FEMALES")
par(mfrow=c(1,2))
qqnorm(males$salbeg, main = paste("Q-Q Plot of" , toupper(names(dataset1)[1]), "of", levels(dataset1$sex)[1]))
qqline(males$salbeg)
qqnorm(females$salbeg, main = paste("Q-Q Plot of" , toupper(names(dataset1)[1]), "of", levels(dataset1$sex)[2]))
qqline(females$salbeg)
par(mfrow=c(1,1))
# So we cannot assume normality for either of the two samples and both samples are large
by(dataset1$salbeg, dataset1$sex, symmetry.test)
par(mfrow=c(1,2))
boxplot(males$salbeg, main = paste("Boxplot of" , toupper(names(dataset1)[1]), "of", levels(dataset1$sex)[1]))
boxplot(females$salbeg, main = paste("Boxplot of" , toupper(names(dataset1)[1]), "of", levels(dataset1$sex)[2]))
par(mfrow=c(1,1))
# So the mean is not a sufficient descriptive measure of central location for either of the two samples
# Wilcoxon rank‐sum test (or Mann‐Whitney)
wilcox.test(males$salbeg, females$salbeg)
mean(males$salbeg)/mean(females$salbeg)
median(males$salbeg)/median(females$salbeg)
boxplot(males$salbeg, females$salbeg, names=levels(dataset1$sex)[1:2], 
        main = paste("Boxplot of" , toupper(names(dataset1)[1]), "of", levels(dataset1$sex)[1], "and", levels(dataset1$sex)[2]), 
        notch = TRUE)
# So we reject the null hypothesis. There is significant difference between the beginning salary of men and women and in fact the
# beginning salary of men is greater than the respective of women


##################################################################################################################################
##################################################################################################################################
##                                                                                                                              ##
##                                                  ANSWER TO QUESTION 6                                                        ##
##                                                                                                                              ##
##################################################################################################################################
##################################################################################################################################

# install.packages("Hmisc")
library(Hmisc)
salary$age_cut <- cut2(salary$age, g = 3)
# Hypothesis test for three samples - Testing for the association between a continuous and a categorical variable of three levels
# Variables: continuous: salbeg, categorical: age_cut
# Null Hypothesis:  h0: μ1 = μ2 = μ3
# Alternative:      h1: μk != μj, for some k != j ε {1,2,3}
# ANOVA: Analysis of Variance

dataset2 <- salary[, c(2,12)]
nrow(dataset2) > 50

# ASSUMPTIONS: 
# Residuals’ normality or the sample size to be large (n>50)
# Equal variances
# As total number of samples (n1 + n2 + n3) above 50, we test normality with KS, SW, Q-Q Plot
anova1 <- aov(salbeg~age_cut, data = dataset2)
lillie.test(anova1$residuals)
shapiro.test(anova1$residuals)
qqnorm(anova1$residuals)
qqline(anova1$residuals)
bartlett.test(salbeg~age_cut, data = dataset2)
fligner.test(salbeg~age_cut, data = dataset2)
library(car)
leveneTest(salbeg~age_cut, data = dataset2)
# So we cannot assume neither normality nor homoscedasticity for either of the residuals and the total number of samples is large
boxplot(salbeg~age_cut, data = dataset2, main = 
          paste("Boxplot of" , toupper(names(dataset2)[1]), "of", levels(dataset2$age_cut)[1], ",", 
                levels(dataset2$age_cut)[2], "and",levels(dataset2$age_cut)[3]))
# So the mean is not a sufficient descriptive measure of central location for either of the three samples
# Kruskal-Wallis Test (Equality of medians)
kruskal.test(salbeg~age_cut, data = dataset2)
# We reject the null hypothesis
pairwise.wilcox.test(dataset2$salbeg, dataset2$age_cut)
boxplot(salbeg~age_cut, data = dataset2, main = 
          paste("Boxplot of" , toupper(names(dataset2)[1]), "of", levels(dataset2$age_cut)[1], ",", 
                levels(dataset2$age_cut)[2], "and",levels(dataset2$age_cut)[3]))
# So we reject the null hypothesis. There is significant difference in the beginning salary of at least one age group 
# and in fact the beginning salary of the 2nd age group ([29.7, 39.8)) is greater than the respective of the other two


##################################################################################################################################
##################################################################################################################################
##                                                                                                                              ##
##                                                  ANSWER TO QUESTION 7                                                        ##
##                                                                                                                              ##
##################################################################################################################################
##################################################################################################################################

# Hypothesis test for two categorical values - Testing for the association between two categorical variables (independent samples)
# Variables: categorical: minority, categorical: sex
# Null Hypothesis:  h0: πmales = πfemales
# Alternative:      h1:  πmales != πfemales
tab1 <- table(salary$sex, salary$minority)
prop.table(tab1, 2)
prop.test(tab1)
chisq.test(tab1)
fisher.test(tab1)
# We cannot reject the null hypothesis and so the proportion of white male employees is equal to the proportion of white female
# employees, or otherwise there is independence between gender and minority of employees.