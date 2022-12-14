teach_data<-read.csv(file.choose())
library(dplyr)
set.seed(1234)
dplyr::sample_n(teach_data, 10)
sapply(teach_data,class)
str(teach_data)
teach_data$Age<-as.factor(teach_data$Age)
style=teach_data$Teaching.style
style<-as.factor(style)
age=teach_data$Age
age<-as.factor(age)
mathematics=teach_data$Mathematic
science=teach_data$Science
str(teach_data)
head(teach_data,6)
tail(teach_data,6)
shapiro.test(mathematics)
shapiro.test(science)
library(dlookr)
dim(teach_data)
diagnose_outlier(teach_data)
plot_outlier(teach_data)
# 1-way anova for style and mathematics grades
boxplot(mathematics~style,xlab='style',ylab='math grade')
model<-lm(mathematics~style,data = teach_data)
res1=resid(model)
fit1=fitted(model)
plot(fit1,res1, ylab="residuals", xlab = "fitted values")
abline(h=0)
lag.plot(res1, diag=FALSE, do.lines=FALSE)
hist(res1, breaks= 8, xlab="residuals", ylab="Frequency", main=NULL)
bartlett.test(mathematics~style,data = teach_data)
aov_out1=aov(mathematics~style)
summary(aov_out1)
tapply(mathematics,style,mean)
TukeyHSD(aov_out1)
kruskal.test(mathematics~style,data = teach_data)
plot(aov_out1)
# 2-way anova for mathematics grades
boxplot(mathematics~style*age,data = teach_data,xlab='style and age',ylab='math grades')
model1=lm(mathematics~style*age,data = teach_data)
res2=resid(model1)
fit2=fitted(model1)
plot(fit2, res2, ylab ="residuals", xlab="fitted values")
abline(h=0)
lag.plot(res1, diag=FALSE, do.lines = FALSE)
hist(res1, breaks=6, xlab = "residuals", ylab="Frequency", main=NULL)
bartlett.test(mathematics~age,data = teach_data)
aov_out2<-aov(mathematics~style*age,data = teach_data)
summary(aov_out2)
TukeyHSD(aov_out2,which = ('style'))
TukeyHSD(aov_out2,which = ('age'))
plot(aov_out2)
library(HH)
interaction2wt(mathematics~style*age)
data.means <- tapply(mathematics, list(style,age),
                     mean)
data.means
# manova for style ~ math grade*science grade
response=cbind(mathematics,science)
cov(response)
install.packages("mvnormtest")
library(mvnormtest)
mshapiro.test(t(response))
model2=manova(response~style,data=teach_data)
summary(model2)
summary.aov(model2)
summary(model2,intercept = TRUE,test = 'Wilks')
fit_block=aov(mathematic~style+teach_data$Block+style:teach_data$Block,data = teach_data)
summary(fit_block)
TukeyHSD(fit_block)
#citation
citation('dplyr')
citation('dlookr')
citation('HH')
citation('mvnormtest')
