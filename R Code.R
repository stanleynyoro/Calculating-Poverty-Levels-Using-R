library(memisc)
library(gmodels)
library(dplyr)
library(ggplot2)

setwd("D:\\Stanley\\myfiles")
mydata<-read.csv("Data.csv",header=TRUE)
attach(mydata)

# Compute adult equivalences
# According to FAO
# Adult  Female   Males
# <1      0.3     0.3
# 1-6     0.5     0.5
# 7-13    0.7     0.7
# 14-19   0.9     0.9
# 20-59   0.9     1.0
# >59     0.7     0.9
mydata=rename(mydata,hhhead_gender=hh_gender,hhhead_age=hh_age)
###Equivalance fo household head
mydata<-mutate(mydata,equiv1=ifelse(hhhead_gender=="Male" & hhhead_age<=1,0.3,
                             ifelse(hhhead_gender=="Male" & hhhead_age>1 & hhhead_age<=6,0.5,
                             ifelse(hhhead_gender=="Male" & hhhead_age>6 & hhhead_age<=13,0.7,
                             ifelse(hhhead_gender=="Male" & hhhead_age>13 & hhhead_age<=19,0.9,
                             ifelse(hhhead_gender=="Male" & hhhead_age>19 & hhhead_age<=59,1.0,
                             ifelse(hhhead_gender=="Male" & hhhead_age>59,0.9,
                             ifelse(hhhead_gender=="Female" & hhhead_age<=1,0.3,
                             ifelse(hhhead_gender=="Female" & hhhead_age>1 & hhhead_age<=6,0.5,
                             ifelse(hhhead_gender=="Female" & hhhead_age>6 & hhhead_age<=13,0.7,
                             ifelse(hhhead_gender=="Female" & hhhead_age>13 & hhhead_age<=19,0.9,
                             ifelse(hhhead_gender=="Female" & hhhead_age>19 & hhhead_age<=59,0.9,
                             ifelse(hhhead_gender=="Female" & hhhead_age>59,0.7," ")))))))))))))
##Equivalence sfor spouse
mydata<-mutate(mydata,equiv2=ifelse(spouse_gender=="Male" & spouse_age<=1,0.3,
                             ifelse(spouse_gender=="Male" & spouse_age>1 & spouse_age<=6,0.5,
                             ifelse(spouse_gender=="Male" & spouse_age>6 & spouse_age<=13,0.7,
                             ifelse(spouse_gender=="Male" & spouse_age>13 & spouse_age<=19,0.9,
                             ifelse(spouse_gender=="Male" & spouse_age>19 & spouse_age<=59,1.0,
                             ifelse(spouse_gender=="Male" & spouse_age>59,0.9,
                             ifelse(spouse_gender=="Female" & spouse_age<=1,0.3,
                             ifelse(spouse_gender=="Female" & spouse_age>1 & spouse_age<=6,0.5,
                             ifelse(spouse_gender=="Female" & spouse_age>6 & spouse_age<=13,0.7,
                             ifelse(spouse_gender=="Female" & spouse_age>13 & spouse_age<=19,0.9,
                             ifelse(spouse_gender=="Female" & spouse_age>19 & spouse_age<=59,0.9,
                             ifelse(spouse_gender=="Female" & spouse_age>59,0.7," ")))))))))))))


#Equivalence for the child 1
mydata<-mutate(mydata,equiv3=ifelse(child1_gender=="Male" & child1_age<=1,0.3,
                             ifelse(child1_gender=="Male" & child1_age>1 & child1_age<=6,0.5,
                             ifelse(child1_gender=="Male" & child1_age>6 & child1_age<=13,0.7,
                             ifelse(child1_gender=="Male" & child1_age>13 & child1_age<=19,0.9,
                             ifelse(child1_gender=="Male" & child1_age>19 & child1_age<=59,1.0,
                             ifelse(child1_gender=="Male" & child1_age>59,0.9,
                             ifelse(child1_gender=="Female" & child1_age<=1,0.3,
                             ifelse(child1_gender=="Female" & child1_age>1 & child1_age<=6,0.5,
                             ifelse(child1_gender=="Female" & child1_age>6 & child1_age<=13,0.7,
                             ifelse(child1_gender=="Female" & child1_age>13 & child1_age<=19,0.9,
                             ifelse(child1_gender=="Female" & child1_age>19 & child1_age<=59,0.9,
                             ifelse(child1_gender=="Female" & child1_age>59,0.7," ")))))))))))))

#Equivalence for child 2
mydata<-mutate(mydata,equiv4=ifelse(child2_gender=="Male" & child2_age<=1, 0.3,
                             ifelse(child2_gender=="Male" & child2_age>1 & child2_age<=6,0.5,
                             ifelse(child2_gender=="Male" & child2_age>6 & child2_age<=13,0.7,
                             ifelse(child2_gender=="Male" & child2_age>13 & child2_age<=19,0.9,
                             ifelse(child2_gender=="Male" & child2_age>19 & child2_age<=59,1.0,
                             ifelse(child2_gender=="Male" & child2_age>59,0.9,
                             ifelse(child2_gender=="Female" & child2_age<=1,0.3,
                             ifelse(child2_gender=="Female" & child2_age>1 & child2_age<=6,0.5,
                             ifelse(child2_gender=="Female" & child2_age>6 & child2_age<=13,0.7,
                             ifelse(child2_gender=="Female" & child2_age>13 & child2_age<=19,0.9,
                             ifelse(child2_gender=="Female" & child2_age>19 & child2_age<=59,0.9,
                             ifelse(child2_gender=="Female" & child2_age>59,0.7," ")))))))))))))

#Equivalence child 3
mydata<-mutate(mydata,equiv5=ifelse(child3_gender=="Male" & child3_age<=1, 0.3,
                             ifelse(child3_gender=="Male" & child3_age>1 & child3_age<=6,0.5,
                             ifelse(child3_gender=="Male" & child3_age>6 & child3_age<=13,0.7,
                             ifelse(child3_gender=="Male" & child3_age>13 & child3_age<=19,0.9,
                             ifelse(child3_gender=="Male" & child3_age>19 & child3_age<=59,1.0,
                             ifelse(child3_gender=="Male" & child3_age>59,0.9,
                             ifelse(child3_gender=="Female" & child3_age<=1,0.3,
                             ifelse(child3_gender=="Female" & child3_age>1 & child3_age<=6,0.5,
                             ifelse(child3_gender=="Female" & child3_age>6 & child3_age<=13,0.7,
                             ifelse(child3_gender=="Female" & child3_age>13 & child3_age<=19,0.9,
                             ifelse(child3_gender=="Female" & child3_age>19 & child3_age<=59,0.9,
                             ifelse(child3_gender=="Female" & child3_age>59,0.7," ")))))))))))))


#Equivalence child 4
mydata<-mutate(mydata,equiv6=ifelse(child4_gender=="Male" & child4_age<=1, 0.3,
                             ifelse(child4_gender=="Male" & child4_age>1 & child4_age<=6,0.5,
                             ifelse(child4_gender=="Male" & child4_age>6 & child4_age<=13,0.7,
                             ifelse(child4_gender=="Male" & child4_age>13 & child4_age<=19,0.9,
                             ifelse(child4_gender=="Male" & child4_age>19 & child4_age<=59,1.0,
                             ifelse(child4_gender=="Male" & child4_age>59,0.9,
                             ifelse(child4_gender=="Female" & child4_age<=1,0.3,
                             ifelse(child4_gender=="Female" & child4_age>1 & child4_age<=6,0.5,
                             ifelse(child4_gender=="Female" & child4_age>6 & child4_age<=13,0.7,
                             ifelse(child4_gender=="Female" & child4_age>13 & child4_age<=19,0.9,
                             ifelse(child4_gender=="Female" & child4_age>19 & child4_age<=59,0.9,
                             ifelse(child4_gender=="Female" & child4_age>59,0.7," ")))))))))))))
#Equivalence child 5

mydata<-mutate(mydata,equiv7=ifelse(child5_gender=="Male" & child5_age<=1, 0.3,
                             ifelse(child5_gender=="Male" & child5_age>1 & child5_age<=6,0.5,
                             ifelse(child5_gender=="Male" & child5_age>6 & child5_age<=13,0.7,
                             ifelse(child5_gender=="Male" & child5_age>13 & child5_age<=19,0.9,
                             ifelse(child5_gender=="Male" & child5_age>19 & child5_age<=59,1.0,
                             ifelse(child5_gender=="Male" & child5_age>59,0.9,
                             ifelse(child5_gender=="Female" & child5_age<=1,0.3,
                             ifelse(child5_gender=="Female" & child5_age>1 & child5_age<=6,0.5,
                             ifelse(child5_gender=="Female" & child5_age>6 & child5_age<=13,0.7,
                             ifelse(child5_gender=="Female" & child5_age>13 & child5_age<=19,0.9,
                             ifelse(child5_gender=="Female" & child5_age>19 & child5_age<=59,0.9,
                             ifelse(child5_gender=="Female" & child5_age>59,0.7," ")))))))))))))
#Equivalence child 6

mydata<-mutate(mydata,equiv7=ifelse(child6_gender=="Male" & child6_age<=1, 0.3,
                             ifelse(child6_gender=="Male" & child6_age>1 & child6_age<=6,0.5,
                             ifelse(child6_gender=="Male" & child6_age>6 & child6_age<=13,0.7,
                             ifelse(child6_gender=="Male" & child6_age>13 & child6_age<=19,0.9,
                             ifelse(child6_gender=="Male" & child6_age>19 & child6_age<=59,1.0,
                             ifelse(child6_gender=="Male" & child6_age>59,0.9,
                             ifelse(child6_gender=="Female" & child6_age<=1,0.3,
                             ifelse(child6_gender=="Female" & child6_age>1 & child6_age<=6,0.5,
                             ifelse(child6_gender=="Female" & child6_age>6 & child6_age<=13,0.7,
                             ifelse(child6_gender=="Female" & child6_age>13 & child6_age<=19,0.9,
                             ifelse(child6_gender=="Female" & child6_age>19 & child6_age<=59,0.9,
                             ifelse(child6_gender=="Female" & child6_age>59,0.7," ")))))))))))))

#We need to make the character variables into numeric variables
options(digits=4)
attach(mydata)
mydata$equiv1=as.numeric(equiv1)
mydata$equiv2=as.numeric(equiv2)
mydata$equiv3=as.numeric(equiv3)
mydata$equiv4=as.numeric(equiv4)
mydata$equiv5=as.numeric(equiv5)
mydata$equiv6=as.numeric(equiv6)
mydata$equiv7=as.numeric(equiv7)

##Sum adult equivalence
mydata$adult_equivalence <- rowSums(mydata[,c("equiv1", "equiv2","equiv3","equiv4","equiv5","equiv6","equiv7")], na.rm=TRUE)

##For consumption: Q7.1c to Q7.29c represent consumption of food-items in the past one week
##Q7.2.1 to Q7.2.18 represent consumption of food items in the past one week
##We need to get total consumption for one month, then divide by adult equivalence to get those who fall below poverty line
##We assume all hh are from rural areas

##Find monthly consumption on food and non-food item

mydata$monthly_consumption=rowSums(mydata[,19:67],na.rm=TRUE)*52/12

##Check outliers for each case through standardized values and use trimmed mean (5% above and below) if outliers present
##This could definitely have been done for each item

stdz<-scale(mydata$monthly_consumption)

#Finaly compute individual consumption i.e. monthlyconsumption/adult equivalence
#Then classify

mydata$individual_consumption=round(mydata$monthly_consumption/mydata$adult_equivalence,0)

##Poverty classification i.e. those above and below poverty line

mydata$poverty_line=cases(
  "Above Poverty"=(individual_consumption>3252),
  "Below Poverty"=(individual_consumption<=3252)
)

##You can then do crosstabulation & visualization using ggplots

prop.table(table(mydata$poverty_line))
CrossTable(Qa,mydata$poverty_line,prop.chisq = FALSE,prop.c=FALSE)

ggplot(mydata, aes(fill=poverty_line, y=individual_consumption, x=Qa)) + 
  geom_bar( stat="identity", position="fill")
