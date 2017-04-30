#########################################################
# Developed by Elsayed Hemayed
# for Data Incubator Challenge
#########################################################

library(gtools)
library(data.table)

options(digits = 10)

setwd('C:/Downloads/Data/CollegeScorecard_Raw_Data')
# Importing the dataset
data=fread("MERGED2013_14_PP.csv", select = c("HIGHDEG", "SAT_AVG","UGDS"))

# get only the SAT_AVG and UGDS (enrollment) for Bachelor's degree
df=data[data$HIGHDEG==3,]

#convert factor to numeric
df=sapply(df,function(x) as.numeric(as.character(x)))

#replace NULL with na
is.na(df) <- df == "NULL"

# remove na from data
df<-df[!apply(is.na(df),1,any),]

#admitted = enrollment/4  
df[,3]=df[,3]/4

#average sat score = sum of total score / sum of admitted students
avgSat=sum(df[,2]*df[,3])/sum(df[,3])
avgSat

#############################
# Getting the enrollment after two years
df=fread("MERGED2013_14_PP.csv", select = c("SAT_AVG","ENRL_ORIG_YR2_RT"))
df=sapply(df,function(x) as.numeric(as.character(x)))
is.na(df) <- df == "NULL"
df<-df[!apply(is.na(df),1,any),]

#compute Pearson Correlation Coefficient
res <- cor.test(df[,1], df[,2], method = "pearson")
res

#############################
# getting income data
df=fread("MERGED2013_14_PP.csv", select = c("HIGHDEG","LO_INC_COMP_ORIG_YR4_RT","MD_INC_COMP_ORIG_YR4_RT","HI_INC_COMP_ORIG_YR4_RT"))
df=df[df$HIGHDEG==3,]
df=subset(df,select=-c(1))
df=sapply(df,function(x) as.numeric(as.character(x)))
is.na(df) <- df == "NULL"
df<-df[!apply(is.na(df),1,any),]

#Compute diffAvgPercent
diffAvgPercent=mean(df[,3])-mean(df[,1])
#Perform a two-sample t-test and 
#compute the log10 of the two-tailed p-value.
t.test(df[,3],df[,1])
log10(2*pt(16.297005,1242.3287-1,lower=FALSE))


#############################
# Getting the enrollment per ethnic group
df=fread("MERGED2013_14_PP.csv", select = (293:306))
df=sapply(df,function(x) as.numeric(as.character(x)))
is.na(df) <- df == "NULL"
df<-df[!apply(is.na(df),1,all),]

#compute diverse Metric
diffEthinc=apply(df,1,max,na.rm=TRUE)-apply(df,1,min,na.rm=TRUE)
diffEthincNoZeros=diffEthinc[diffEthinc!=0]
diverseEthinc=min(diffEthincNoZeros)
diverseEthinc

#############################
# Getting the enrollment per ethnic group

df=fread("MERGED2001_02_PP.csv", select = c("OPEID","UGDS_WOMEN"))
dt <- data.table(df, key = "OPEID") 

for (year in c(2002:2010)){
  
  if (year<2009)
    fname=paste0("MERGED",year,"_0",year-1999,"_PP.csv")
  else
    fname=paste0("MERGED",year,"_",year-1999,"_PP.csv")
  
  df1=fread(fname, select = c("OPEID","UGDS_WOMEN"))
  
  # join data tables based on OPEID
  dt1 <- data.table(df1, key = "OPEID") 
  dt <- dt[dt1]

}

dt=sapply(dt,function(x) as.numeric(as.character(x)))
is.na(dt) <- dt == "NULL"
dt<-dt[!apply(is.na(dt),1,any),]

#Compute the average of UGDD_WOMEN across 10 years
mean(dt[,-1])


#############################
# Getting the region and locale
df=fread("MERGED2014_15_PP.csv", select = c("REGION","LOCALE"))
df=sapply(df,function(x) as.numeric(as.character(x)))
is.na(df) <- df == "NULL"
df<-df[!apply(is.na(df),1,any),]

df1=df[df[,2]<20,]
df2=df[df[,2]<30 & df[,2]>20,]
df3=df[df[,2]<40 & df[,2]>30,]
df4=df[df[,2]>40,]

df1[,2]=1
df2[,2]=1
df3[,2]=1
df4[,2]=1

m1=aggregate(df1[,2], by=list(Region=df1[,1]), FUN=sum)
m2=aggregate(df2[,2], by=list(Region=df2[,1]), FUN=sum)
m3=aggregate(df3[,2], by=list(Region=df3[,1]), FUN=sum)
m4=aggregate(df4[,2], by=list(Region=df4[,1]), FUN=sum)

dm1 <- data.table(m1, key = "Region")
dm2 <- data.table(m2, key = "Region") 
dm3 <- data.table(m3, key = "Region") 
dm4 <- data.table(m4, key = "Region") 

dm1 <- dm1[dm2]
dm1 <- dm1[dm3]
dm1 <- dm1[dm4]

mydata=data.matrix(dm1)

# Compute Max Probablity 
maxProb=max(mydata[,2]/rowSums(mydata[,-1],dims = 1))
maxProb
