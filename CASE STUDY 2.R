"
****************************************************************
Name:Agbadu Goodnews
Student Number:A00238219

QMM1001 Case Study 2

####################################################################
****************************************************************
"
#Part 1: Does watching the news affect how you spend your day?

#1. Create a contingency table for the NEWS variable and one of your personalized categorical variables (variable 7 or variable 8). You should choose whichever variable you think may have a relationship to watching the news. 
#Explain why you chose the categorical variable you did and why you think it is related to watching the news.
pd <- read.csv(file.choose(), header = TRUE)
library(rpivotTable)
rpivotTable(pd)

#2. Using the contingency table, you will formulate four questions to capture how watching the news relates to your chosen personalized categorical variable. 
#Each question must be answered using a different type of probability rule:
#a. Question 1 must be answered using the rule of complement (NOT)
#probability of not watching the news at all
#P(News != Yes ) = 44.4/100 = 44.4%


#b. Question 2 must be answered using an addition rule for probability (OR)
#probability of being stressed at 4 or being stressed at 5
#p(stress = 4 or stress = 5) = 12.2+11.1 = 23.3%


#c. Question 3 must be answered using a conditional probability (GIVEN)
#probability of watching the news given my stress level a 5? this is denoted P(News = YES | Stress = 5)
#1/10 = 0.1
#10%


#d. Question 4 must be answered using a multiplication rule for probability (AND)
#what is the probability that i watch the news and my stress level a 1 ? this is denoted P(News = YES AND Stress = 1)
#34.4/100 = 34.4%

#In the report, clearly explain each question you created and then write down the final probability using the correct notation (for example P(A or B) = value). 
#Interpret the probability by relating the result back to factors that affect your studies.

#3. Are any of the events in the contingency table disjoint? Explain how you know and how these findings relate to your daily activities.

#a. disjoint the events can occur together

#b. non-disjoint the two events cannot occur together

#c. disjoint events can occur together, disjoint events cannot be independent

#d. disjoint events can occur together 

#4. Check for independence between two events of your choice in the contingency table. 
#In your report, show the formula you used (you must show one of the three ways) and
#explain how you know the events are independent or not. 
#Finally, if the events are independent or dependent explain how this relates to your daily activities.

#b. P(A AND B) != P(A)*P(B) 
#21/90 = 0.2333333!= 11/90*10/90 = 0.01358025  dependent

#c. #P(A AND B) != P(A)*P(B) 
#1/90 = 0.01111111 != 50/90*10/90 = 0.0617284 dependent

#d P(A AND B) != P(A)*P(B) 
#31/90 = 0.3444444 != 50/90*34/90 = 0.2098765 dependent



#Part 2: Do you watch the news more than other people?

#1. Statista reports that 59% = 0.59 of Canadians access the news daily. This will be used as the population proportion. 
#Find and report the proportion of days that you watch the news AND the proportion of days that you do not watch the news. 
#The proportion of days that you watch the news will be used as the sample proportion. Compare your sample proportion and 
#the population proportion by stating if you watch the news more than, less than, 
#or about the same amount as the general Canadian population and comment on why this is the case.
wnews<-table(pd$News) 
wnews #YES = 50, NO = 40
#Sample proportion of YES <-50/90 = 55.6%
#Sample proportion of NO <- 40/90 = 44.4%
#Number of days is 90, range of data is from 10-Sep-21 to 08-Dec-21.


#2. What would the mean and standard deviation of the normal model be? Report these values. 
#Number of days is 90, range of data is from 10-Sep-21 to 08-Dec-21.

p1<-0.59
q1<-1-p
n1<-90

#p=mean #0.59 because 0.59 is a proportion of Canadian population that access the news daily.

sd1<-sqrt(p1*q1/n1)
sd1 #0.07011895


#3. Find the probability of getting a value LESS THAN OR EQUAL to your sample proportion using 0.59 as 
#the population proportion. Interpret what this value means.

#Find the sample proportion of the number of days that you watched the news. 
wnews<-table(pd$News) #No =40 Yes = 50
wnews
p.yes<-50/n1 #proportion in sample
p.yes # sample proportion of the number of days that you watched the news is 0.5555556 ie 55.6%

#probability of getting a value less than or equal to your sample proportion given that the population proportion is 0.59?
pnorm(p.yes, p1, sd1, lower.tail=TRUE) #0.3116323


#4. Create a 95% confidence interval for the proportion of days that you watch the news. 
#In your report, include the confidence interval using the correct notation and interpret what it means. 
#Does it seem that you watch the news more than 50% of the time? Explain how you know.

CI<-function(p.hat, n, cl){
  #p.hat = sample proportion, enter as decimal
  #n = sample size
  #cl = confidence level, enter as decimal
  
  q.hat<-1-p.hat #find probability of failure
  se<-sqrt(p.hat*q.hat/n) #find standard error
  z.crit<-qnorm(cl/2+0.5, 0, 1, lower.tail=TRUE) #find critical value based on level of confidence
  lower<-p.hat-z.crit*se #lower bound of confidence interval
  upper<-p.hat+z.crit*se #upper bound of confidence interval
  
  return(c(lower, upper)) #Return the confidence interval as output
}

wnews<-table(pd$News) 
wnews #YES = 50, NO = 40
#Number of days is 90, range of data is from 10-Sep-21 to 08-Dec-21.
p.hat<- 50/90
n1<-90
CI(p.hat, n1, 0.95) #0.4528960 0.6582151

#We are 95% confident that the true proportion of the time i watched the news is between 45% and 66%.


#5. Test the hypothesis that you watch the news more or less than the general population of Canadians (that watch the news 59% of the time). 
#Make a choice for the alternative hypothesis (more or less) based on how much you watch the news. 
#In your report, write out the hypotheses using the proper notation, state the p-value, 
#make a final decision (reject or do not reject) and interpret that decision in context (do you watch the news more or less than a general Canadian?).

p.test<-function(p.hat, n, p0, alpha=0.05, alternative=c("one", "two")){
  #conduct a hypothesis test for a proportion and return the p-value and decision
  #p.hat = proportion in the sample, enter as a decimal
  #n = sample size, 
  #p0 = population proportion, enter as decimal
  #alpha = level of significance, enter as a decimal with default 0.05 = 5%
  #alternative = "one" or "two" for a one-sided (> or <) or two-sided test respectively
  alternative<-match.arg(alternative)
  q0<-1-p0
  SD<-sqrt(p0*q0/n)
  z.test<-(p.hat-p0)/SD
  z.test<-ifelse(z.test<0, z.test, -z.test)
  p.value<-ifelse(alternative=="one", pnorm(z.test, 0, 1, lower.tail=TRUE), 2*pnorm(z.test, 0, 1, lower.tail=TRUE))
  decision<-ifelse(p.value<alpha, 
                   "Reject the null hypothesis", 
                   "Do not reject the null hypothesis")
  output<-data.frame(p.value, decision)
  return(output)
}


wnews<-table(pd$News) 
wnews #YES = 50, NO = 40
#Number of days is 90, range of data is from 10-Sep-21 to 08-Dec-21.

#The general population of Canadians watches the news 59% of the time. Depending on your sample proportion test the following:
n1 <-90
p.hat <- 50/n1
p.hat #0.5555556
p.hat*100 #0.5555556 or 55.6%
#Write out the null and alternative hypotheses. Conduct the hypothesis test at the 0.05 level of significance. .
#H0: p =  0.59
#HA: p <  0.59

p.test(p.hat, n1, 0.59, 0.05, "one") #0.2532207

#p > level of significance, Do not reject the null hypothesis

#DECISION: There is no significant evidence that i watch the news less than the general population of Canadians.








