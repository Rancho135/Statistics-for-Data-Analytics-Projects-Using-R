"
****************************************************************
Name:Agbadu Goodnews
Student Number:A00238219

QMM1001 MID-TERM CASE
****************************************************************
"



pd <- read.csv(file.choose(), header = TRUE)

#Create a frequency table and interpret it.

#frequency table for categorical variable News
frequency_table1 <- table(pd$News)
frequency_table1

#frequency table for categorical variable Games
frequency_table2 <- table(pd$Games)
frequency_table2

#frequency table for categorical variable stress
frequency_table3 <- table(pd$Stress)
frequency_table3

#Create an appropriate visual display and interpret it.

#Bar Chart for Stress
frequency_table3 <- table(pd$Stress)
barplot(frequency_table3, main = "Goodnews Stress Level", ylab="Number of days", xlab="Stress (In scale 1-5)", col = rainbow(5), ylim=c(0, 10))

#Bar chart for news 
frequency_table1 <- table(pd$News)
pie(frequency_table1, main = "Time Spent Listening to News", col = rainbow(2))

#Bar chart for Games 
frequency_table2 <- table(pd$Games)
pie(frequency_table2, main = "Time Spent on Games", col = rainbow(2))

#Create a contingency table for the two categorical variables of your choice (variable 7 and variable 8) and interpret your findings in relation to what you do each day.
contingency_table <- table(pd$Games, pd$Stress)
contingency_table

#Using a pivot table to create a contingency table for the variables Games and Stress.
table(pd$Games, pd$Stress)
install.packages("rpivotTable")
library(rpivotTable)
rpivotTable(pd)


#EACH quantitative variable:

#Create a properly labelled histogram and comment on the shape of the distribution (mode, symmetry, outliers).

hist(pd$Zoom, breaks=5, xlab="Count of Zoom", col= rainbow(7), border = "yellow", main="The Amount of Time Spent on Zoom", ylim = c(0, 12))

hist(pd$Study, breaks=5, xlab="Count of Study", col= rainbow(7), border = "yellow", main="The Amount of Time Spent on Studying", ylim = c(0, 15))

hist(pd$Sleep, breaks=5, xlab="Count of Sleep", col= rainbow(7), border = "yellow", main="The Amount of Time Spent Sleeping", ylim = c(0, 10))

hist(pd$House, breaks=5, xlab="Count of House", col= rainbow(7), border = "yellow", main="The Amount of Time I Left Home", ylim = c(0, 15))

hist(pd$Calls, breaks=5, xlab="Count of calls", col= rainbow(7), border = "yellow", main="The Amount of Time I Answered Calls", ylim = c(0, 20))


#Calculate the mean, median, standard deviation and IQR for each variable (these can be displayed in a tabular format).

#Summary statistics
summary(pd$Zoom)
summary(pd$Study)
summary(pd$Sleep)
summary(pd$House)
summary(pd$Calls)

#IGR
IQR(pd$Calls, type = 1)
IQR(pd$Zoom, type = 1)
IQR(pd$Study, type = 1)
IQR(pd$House, type = 1)
IQR(pd$Sleep, type = 1)

#Calculate if there are any outliers (using standardized values for symmetric data and boxplots and the 1.5 IQR rule for asymmetric data) and comment on why these may have occurred.
summary(pd$Zoom, quantile.type = 1)
(mean.time<-mean(pd$Zoom)) #brackets allow you to store and print at the same time
(sd.time<-sd(pd$Zoom)) #brackets allow you to store and print at the same time
summary(pd$Study, quantile.type = 1)
(mean.time<-mean(pd$Study)) #brackets allow you to store and print at the same time
(sd.time<-sd(pd$Study)) #brackets allow you to store and print at the same time

summary(pd$Sleep, quantile.type = 1)
(mean.time<-mean(pd$Sleep)) #brackets allow you to store and print at the same time
(sd.time<-sd(pd$Sleep)) #brackets allow you to store and print at the same time

summary(pd$House, quantile.type = 1)
(mean.time<-mean(pd$House)) #brackets allow you to store and print at the same time
(sd.time<-sd(pd$House)) #brackets allow you to store and print at the same time

summary(pd$Calls, quantile.type = 1)
(mean.time<-mean(pd$Calls)) #brackets allow you to store and print at the same time
(sd.time<-sd(pd$Calls)) #brackets allow you to store and print at the same time

#Calculate if there are any outliers 
pd$standard.time<-(pd$Zoom- mean.time)/sd.time
(outliers.time<-subset(pd, standard.time > 3 | standard.time < -3))

#boxplot

bp.time<-boxplot(pd$Zoom, col="blue", outcol="red", main="Hours Spent on Zoom Boxplot", ylab="Time (H)")
bp.time$out #prints the outlier values in time (seconds)
(bpoutliers.time<-subset(pd, Zoom == bp.time$out))

bp.time<-boxplot(pd$Study, col="blue", outcol="red", main="Hours Spent on Study Boxplot", ylab="Time (H)")
bp.time$out #prints the outlier values in time (seconds)
(bpoutliers.time<-subset(pd, Study == bp.time$out))

bp.time<-boxplot(pd$Sleep, col="blue", outcol="red", main="Hours Spent on Sleeping Boxplot", ylab="Time (H)")
bp.time$out #prints the outlier values in time (seconds)
(bpoutliers.time<-subset(pd, Sleep == bp.time$out))

bp.time<-boxplot(pd$House, col="blue", outcol="red", main="Hours Spent Spent Outside the House Boxplot", ylab="Time (H)")
bp.time$out #prints the outlier values in time (seconds)
(bpoutliers.time<-subset(pd, House == bp.time$out))

bp.time<-boxplot(pd$Calls, col="blue", outcol="red", main="Hours Spent on Calls Boxplot", ylab="Time (H)")
bp.time$out #prints the outlier values in time (seconds)
(bpoutliers.time<-subset(pd,Calls == bp.time$out))

#Determine the correlation coefficients between all pairs of quantitative variables.
pdcor<- subset(pd,select = c(Zoom,Study,Sleep,House,Calls))
cor(pdcor)


#TWO quantitative variables with the highest correlation:

#What is the direction and strength of the relationship?

#Create a scatterplot of House vs Zoom
plot(pd$House, pd$Zoom,
     xlab = "Zoom (Hours spent in Zoom)",
     ylab = "House (Hours)",
     main = "Relationship between Hours Spent in Zoom and Outside the House",
     pch= 20, col = "red")
abline(pd.line, col= "green")

text(2, 6.5, cex= 1, "House (Count) = -0.3558863+ Zoom (Hours) 3.4659450" , col = "blue")


#Check the conditions required for regression – are they met?

#Fit the regression line
pd.line<-lm(House~Zoom, pd)
#Residual plots
plot(pd.line)

#Determine the equation of the regression line. State the equation of the line and interpret the slope and y-intercept values.

#equation of the regression line
pd.line<-lm(House~Zoom, pd) #order must be y ~ x
pd.line$coefficients

#make a prediction for 5.5 hours.

new.prediction<-data.frame(Zoom = 5.5)
new.prediction
predict.lm(pd.line, new.prediction)

#end.

