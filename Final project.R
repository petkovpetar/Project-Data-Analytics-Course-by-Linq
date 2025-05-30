#Data Analytics Course by Linq - Project 
#The data is a random sample of 117 cases from the files of a large real estate agency in the United States regarding home sales from
#February 15 to April 30, 1993. The data come from many different cities (and corresponding local real estate agencies) and are used as
#a basis for the entire agency.

#### DATA CLEANING ####

#A
Data=read.table("D:/data analytics course/projects/data.txt", header=T, sep="", na.strings="*", stringsAsFactors=TRUE)
View(Data)
str(Data)
#using the  command read.table to read the file, with na.strings as "*"
#str of our Data to see the types of the different variables

#B

Data$NE=factor(Data$NE, levels=c(0, 1), labels=c("NO", "YES"))
Data$COR=factor(Data$COR, levels=c(0, 1), labels=c("NO", "YES"))
#making NE and COR as factors corresponding to No and Yes according to what level they are

#C
omit=na.omit(which(apply(is.na(Data), 1, sum)>=2))
Data=Data[-omit,]
#making a new variable that consists of each row that has 2 or more NA's, then we are deleting these rows from our data set

#D

Data$SQFT=round(Data$SQFT/10764*100)
colnames(Data)=c("PRICE", "SQM", "AGE", "FEATS", "NE", "COR", "TAX")
#converting square foot to square meters and renaming our columns

#E

Dna=apply(is.na(Data), 2, which) #finding which column has NA's
lmAGE=lm(Data$AGE~Data$PRICE+Data$SQM+Data$FEATS, Data)
lmTAX=lm(Data$TAX~Data$PRICE+Data$SQM+Data$FEATS, Data) #running linear regression the columns that have NA's to the ones that
#dont have any

na=apply(is.na(Data), 1, which)
aNA=which(is.na(Data$AGE))
for (i in aNA) {
  Data$AGE[is.na(Data$AGE)] <- round(predict(lmAGE, na, interval = "prediction"))
}
tNA=which(is.na(Data$TAX))
for (i in tNA) {
  Data$TAX[is.na(Data$TAX)] <- round(predict(lmTAX, na, interval = "prediction"))
}
#finding the NA's and replacing them



# Descriptive statistics and visualization

#A

library(psych)
describe(Data[,c(1,2,3,4,7)]) #finding the descriptive measures of the arithmetical variables
summary(Data[,c(5,6)]) #finding the number of observations of the categorical variables
round(prop.table(table(Data$FEATS)), 3)*100 #finding the relative frequency table of FEATS 
#multiplying by 100 to have more readable results


#B
plot(Data[,c(1,2,3,4,7)]) 

library(PerformanceAnalytics)
chart.Correlation(Data[,c(1,2,3,4,7)]) #scatterplot for the arithmetical variables and finding their linear relation

 

#C

cols=colorRampPalette(c("red", "green"))(n=2)
barplot(t(prop.table(table(Data$SQM, Data$COR))), col = cols, main="Stacked barplot for SquareMeters and Corner")
legend("topright", legend=c("No", "Yes"), col=c("red", "green"), pch=15, cex=1.5)

barplot(t(prop.table(table(Data$SQM, Data$NE))), col = cols, main="Stacked barplot for SquareMeters and NorthEast")
legend("topright", legend=c("No", "Yes"), col=c("red", "green"), pch=15, cex=1.5)

barplot(t(prop.table(table(Data$FEATS, Data$COR))), col = cols, main="Stacked barplot for Features and Corner")
legend("topright", legend=c("No", "Yes"), col=c("red", "green"), pch=15, cex=1.5)

barplot(t(prop.table(table(Data$FEATS, Data$NE))), col = cols, main="Stacked barplot for SquareMeters and NorthEast")
legend("topright", legend=c("No", "Yes"), col=c("red", "green"), pch=15, cex=1.5)

#stacked barplots for the discrete and categorical variables 

#D

par(mfrow=c(2,1), mai=c(0.8,0.9,0.3,0.1))

colb=colorRampPalette(c("red","green"))(n=2)
boxplot(Data$PRICE~Data$COR, horizontal=T, las=1, cex.axis=0.60, xlab = "Price", ylab="Corner", col=colb)

colc=colorRampPalette(c("black","cyan"))(n=2)
boxplot(Data$PRICE~Data$NE, las=1, cex.axis=0.50, col=colc,horizontal=T, xlab="Price", ylab="NorthEast")

mtext("Boxplots of PRICE for the categorical variables", side=3, line=-1.5, outer=T, col="Black", cex=1.5)

#side by side boxplots of price for the categorical variables using different colors

#E

pny=Data$PRICE[which(Data$NE=="YES")]
pnn=Data$PRICE[which(Data$NE=="NO")]
pcy=Data$PRICE[which(Data$COR=="YES")]
pcn=Data$PRICE[which(Data$COR=="NO")]

t.test(pny, pnn, conf.level = 0.9)
t.test(pcy, pcn, conf.level = 0.9)

#finding the statistical mean of PRICE for the different levels of the categorical variables

#F
attach(Data)
skew= which(skewness(Data[,c(1,2,3,4,7)])>1)
shapiro.test(PRICE) ; shapiro.test(SQM) ; shapiro.test(AGE)
hist(PRICE) ; hist(SQM) ; hist(AGE)
shapiro.test(log(PRICE)) ; shapiro.test(log(SQM)) ; shapiro.test(log(AGE))
hist(log(PRICE)) ; hist(log(SQM)) ; hist(log(AGE))
Data$PRICE=round(log(PRICE),1) ; Data$SQM=round(log(SQM),1) ; Data$AGE=round(log(AGE),1)
colnames(Data)=c("logPRICE", "logSQM", "logAGE", "FEATS", "NE", "COR", "TAX")

#finding which variables have skewness greater than 1, then applying normality tests, rejecting normality, applying log to the variables
#and checking if the problems with normality have been resolved (they were), then replacing the name of the respected columns

#G

plot(Data[,c(1,2,3,4,7)]) 

library(PerformanceAnalytics)
chart.Correlation(Data[,c(1,2,3,4,7)])

cols=colorRampPalette(c("red", "green"))(n=2)
barplot(t(prop.table(table(Data$logSQM, Data$COR))), col = cols, main="Stacked barplot for logSquareMeters and Corner")
legend("topright", legend=c("No", "Yes"), col=c("red", "green"), pch=15, cex=1.5)

barplot(t(prop.table(table(Data$logSQM, Data$NE))), col = cols, main="Stacked barplot for logSquareMeters and NorthEast")
legend("topright", legend=c("No", "Yes"), col=c("red", "green"), pch=15, cex=1.5)

barplot(t(prop.table(table(Data$FEATS, Data$COR))), col = cols, main="Stacked barplot for Features and Corner")
legend("topright", legend=c("No", "Yes"), col=c("red", "green"), pch=15, cex=1.5)

barplot(t(prop.table(table(Data$FEATS, Data$NE))), col = cols, main="Stacked barplot for Features and NorthEast")
legend("topright", legend=c("No", "Yes"), col=c("red", "green"), pch=15, cex=1.5)


par(mfrow=c(2,1), mai=c(0.8,0.9,0.3,0.1))

colb=colorRampPalette(c("red","green"))(n=2)
boxplot(Data$logPRICE~Data$COR, horizontal=T, las=1, cex.axis=0.60, xlab = "logPrice", ylab="Corner", col=colb)

colc=colorRampPalette(c("black","cyan"))(n=2)
boxplot(Data$logPRICE~Data$NE, las=1, cex.axis=0.50, col=colc,horizontal=T, xlab="logPrice", ylab="NorthEast")

mtext("Boxplots of logPRICE for the categorical variables", side=3, line=-1.5, outer=T, col="Black", cex=1.5)

#applying the same set of commands to the new log variables



#Data Mining


#A
l=lm(Data$logPRICE~Data$logAGE+Data$logSQM+Data$FEATS+Data$TAX+Data$NE+Data$COR)
summary(l)

#running linear regression of the new logPRICE and all the other variables independent


#B
attach(Data)
sub=Data[,-c(5,6)]

lpfinal=step(lm(logPRICE~., data=sub),direction="both")
summary(lpfinal)

par(mfrow=c(2,3))
plot(lpfinal, which=1:6)

ls=shapiro.test(rstandard(lpfinal))$p.value

yhat=lpfinal$fitted
qhat=cut(yhat, breaks=quantile(yhat, probs=seq(0,1,1/length(lpfinal))))
lb=bartlett.test(rstandard(lpfinal)~qhat)$p.value
lw=dwtest(lpfinal)$p.value

lr=residualPlot(lpfinal, plot=F)

print(ls) ; print(lb) ; print(lw) ; print(lr)

#running the stepwise algorithm and providing the diagnostic graphs

#C
catFEATS=ordered(cut(Data$FEATS, breaks =c(-0.01,3,7,9), labels=c("Low","Moderate","High"))) #new variable with 3 levels of features

apc=aov(Data$logPRICE~catFEATS) #running an anova model
summary(apc) #comparing the different levels of catFEATS to the logPRICE to check if the mean teams are statistically equal



#D
sub=Data[,-5] #removing the NE variable so we get better results
bNE=as.numeric(as.factor(Data$NE))-1 #converting NE to a numeric with 0 and 1

finalNE=step(glm(bNE~.-1, family = "binomial", data=sub), direction="both") 
summary(finalNE) #finding which logistic regression model is the best for NE

TEST=data.frame(logPRICE=1200, logSQM=180, logAGE=15, FEATS=5, COR="NO", TAX=1000)
logitp=predict(finalNE, TEST)
p=exp(logitp)/(1+exp(logitp)) #we are finding the possibility if a house will be in NE (NorthEast) with the set values of the above 
#variables


#E
library(tree)
tcor=tree(COR~., data=Data)
summary(tcor)
tablecor=round((109-15)/109,2) #finding the correct number of predictions

plot(tcor, lwd=2, col=5)
text(tcor, cex=0.9)
mtext("Decision Tree for COR", side=3, line=1, cex=1.5)

TEST=data.frame(logPRICE=1000, logSQM=150, logAGE=17, FEATS=4, ΝΕ="YES", TAX=800)
logitp=predict(finalNE, TEST)
p1=exp(logitp)/(1+exp(logitp)) #finding if a house is COR with the set values of the above variables
