crime<-read.csv("/Users/madhurimapramod/Desktop/TemporaryFolder/Blog Data/crime.csv", sep=",", header = TRUE, na.strings=c(""," ","NA"))

#Remove rows with missing values
crime$BORO_NM <- as.character(crime$BORO_NM)
crime[is.na(crime$BORO_NM),] <- "Missing"
crime$BORO_NM <- as.factor(crime$BORO_NM)

crime <- crime[!crime$BORO_NM =="Missing", ]
crime$BORO_NM <- droplevels(crime$BORO_NM)


crime$LAW_CAT_CD <- as.character(crime$LAW_CAT_CD)
crime[is.na(crime$LAW_CAT_CD),] <- "Missing"
crime$LAW_CAT_CD <- as.factor(crime$LAW_CAT_CD)

crime <- crime[!crime$LAW_CAT_CD =="VIOLATION", ]
crime <- crime[!crime$LAW_CAT_CD =="Missing", ]
crime$LAW_CAT_CD <- droplevels(crime$LAW_CAT_CD)



#Plot Data

library(ggplot2)

ggplot(crime, aes(BORO_NM)) + geom_bar(stat = "count", aes(fill=LAW_CAT_CD), position = "dodge") +
  xlab("Borough") + ylab("Number of Crimes") +
  ggtitle("Crime Type by Borough") +
  labs(fill='Crime Type') 

#Run chi-square test for Hpothesis testing
library(epitools)

p<-table(crime$BORO_NM, crime$LAW_CAT_CD)
oddsratio(p)

chisq.test(crime$LAW_CAT_CD, crime$BORO_NM, correct=FALSE)
