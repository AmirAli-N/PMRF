library(data.table)
library(e1071)
library(tidyr)
setwd("C:/Users/anasr/Desktop/AHCMT lab pc")

###read the file with the activity codes and the crew number, and change row names to activity codes
act_crew.df=fread(file="./activity+crew.csv", sep=",", header = TRUE)
act_crew.df=act_crew.df[!duplicated(act_crew.df),]
row.names(act_crew.df)=act_crew.df$Activity

file.names=list.files(path="./LEMO/", pattern = "*.csv") #creat a list of to be read files
files=lapply(file.names, function(x) fread(file=paste("./LEMO/", x, sep=""), header=TRUE)) #read the files in the file.names list

### .csv file for year 2017 has some discrepency, uncomment the two lines below
#colnames(files[[5]])=c(as.character(files[[5]][2,]))
#files[[5]]=files[[5]][-c(1,2),]

###for each data.frame in the files list, add a column of year, starting from 2013
for (i in 1:length(files)){
  files[[i]]=cbind(files[[i]], c(rep(2013+i-1, dim(files[[i]])[1])))
  colnames(files[[i]])[length(files[[i]])]="Year"
}

###merge all the data vertically
df=rbindlist(files, use.names = TRUE, fill = TRUE)
###order the data base by year and then by activity code, group the data by year and then by activity code and evaluate total sum,
                                                                                                                      #first quartile
                                                                                                                      #median
                                                                                                                      #average
                                                                                                                      #3rd quartile
                                                                                                                      #number of work orders
                                                                                                                      #skewness
                                                                                                                      #kurtosis
                                                                                                                      #number of outlier for the adjusted box and whiskers
skew_adjust.fun=function(Hours){
  ##hours are is a numeric vector
  medcouple=mc(Hours)
  ##An Adjusted Boxplot for Skewed Distributions, M. Hubert a,∗, E. Vandervieren b
  if (medcouple>=0){
    return c(exp(-4*medcouple), exp(3*medcouple))
  } else{
    return c(exp(-3*medcouple), exp(4*medcouple))
  }
}

outlier.fun=function(Hours){##evaluate upper and lower whiskers to identify potential outliers
  ##hours is a numeric vector
  upper_whisker=min(max(Hours), quantile(Hours)[4]+skew_adjust.fun(Hours)[2]*IQR(Hours))
  lower_whisker=max(min(Hours), quantile(Hours)[2]-skew_adjust.fun(Hours)[1]*IQR(Hours))
  upper_outliers=which(Hours>upper_whisker)
  lower_outliers=which(Hours<lower_whisker)
  return (length(upper_outliers)/length(Hours))
}

###group by year and then activity code, and then evaluate total sum, 1st quartile, median, mean, 3rd quatile, frequency, skewness, kurtosis, and outlier percentage
###order by year and activity
hours_by_YearActivity.df=df[order(Year, Activity), .(Hours.Sum=sum(as.numeric(Hours))
                                                     , Hours.1stQ=quantile(as.numeric(Hours))[2]
                                                     , Hours.median=median(as.numeric(Hours))
                                                     , Hours.mean=mean(as.numeric(Hours))
                                                     , Hours.3rdQ=quantile(as.numeric(Hours))[4]
                                                     , Freq=length(Hours)
                                                     , Skew=skewness(as.numeric(Hours))
                                                     , Kurtosis=kurtosis(as.numeric(Hours))
                                                     , Upper_outliers.percent=outlier.fun(as.numeric(Hours)), by=.(Year, Activity)]

###identify unique values of Activity codes, and remove the gibberish
unique(hours_by_YearActivity.df$Activity)
hours_by_YearActivity.df=hours_by_YearActivity.df[-c(which(hours_by_YearActivity.df$Activity=="")),]
hours_by_YearActivity.df=hours_by_YearActivity.df[-c(which(hours_by_YearActivity.df$Activity==" ")),]

###pivot tables describing each statistics, identified by each activity from 2013 to 2019 
H_Sum.df=pivot_wider(hours_by_YearActivity.df, id_cols=Activity, names_from = Year, values_from = Hours.Sum)
H_1stQ.df=pivot_wider(hours_by_YearActivity.df, id_cols=Activity, names_from = Year, values_from = Hours.1stQ)
H_Median.df=pivot_wider(hours_by_YearActivity.df, id_cols=Activity, names_from = Year, values_from = Hours.median)
H_Mean.df=pivot_wider(hours_by_YearActivity.df, id_cols=Activity, names_from = Year, values_from = Hours.mean)
H_3rdQ.df=pivot_wider(hours_by_YearActivity.df, id_cols=Activity, names_from = Year, values_from = Hours.3rdQ)
H_Freq.df=pivot_wider(hours_by_YearActivity.df, id_cols=Activity, names_from = Year, values_from = Freq)
H_Skew.df=pivot_wider(hours_by_YearActivity.df, id_cols=Activity, names_from = Year, values_from = Skew)
H_Kurt.df=pivot_wider(hours_by_YearActivity.df, id_cols=Activity, names_from = Year, values_from = Kurtosis)
###change row names to activity code
row.names(H_Skew.df)=H_Sum.df$Activity
row.names(H_Kurt.df)=H_Kurt.df$Activity
row.names(H_Sum.df)=H_Sum.df$Activity
row.names(H_1stQ.df)=H_1stQ.df$Activity
row.names(H_Median.df)=H_Median.df$Activity
row.names(H_3rdQ.df)=H_3rdQ.df$Activity
row.names(H_Freq.df)=H_Freq.df$Activity
row.names(H_Mean.df)=H_Mean.df$Activity

###print results, replace H_Sum.df with any privot tables above
write.csv(H_Sum.df, file="./IMMS/Activity_Hours.csv", append=FALSE, sep=",", row.names = TRUE, col.names = TRUE)
fwrite(df, file="./IMMS/LEMO_hours_budget.csv", append=FALSE, sep=",", col.names = TRUE)

###########################################
###########################################
###########################################
##Input the activity code and generate plots to see the distribution of work duration data, and different trends in statistics
A_code="H10140"

###histogram of work duration for each year
par(mfrow=c(3,2))
hist(as.numeric(unlist(df[which(df$Activity==A_code & df$Year=="2013"),"Hours"])), breaks=50, main="2013", xlab="")
axis(1, at=seq(0,1000,100), labels=TRUE)
hist(as.numeric(unlist(df[which(df$Activity==A_code & df$Year=="2014"),"Hours"])), breaks=50, main="2014", xlab="")
axis(1, at=seq(0,1000,100), labels=TRUE)
hist(as.numeric(unlist(df[which(df$Activity==A_code & df$Year=="2015"),"Hours"])), breaks=50, main="2015", xlab="")
axis(1, at=seq(0,1000,100), labels=TRUE)
hist(as.numeric(unlist(df[which(df$Activity==A_code & df$Year=="2016"),"Hours"])), breaks=50, main="2016", xlab="")
axis(1, at=seq(0,1000,100), labels=TRUE)
hist(as.numeric(unlist(df[which(df$Activity==A_code & df$Year=="2017"),"Hours"])), breaks=50, main="2017", xlab="")
axis(1, at=seq(0,1000,100), labels=TRUE)
hist(as.numeric(unlist(df[which(df$Activity==A_code & df$Year=="2018"),"Hours"])), breaks=50, main="2018", xlab="")
axis(1, at=seq(0,1000,100), labels=TRUE)
mtext(paste(A_code, act_crew.df[A_code, 2], sep=": "), side=3, line=-2, outer=TRUE)

###histogram of log(work duration) for each year
par(mfrow=c(3,2))
hist(log(as.numeric(unlist(df[which(df$Activity==A_code & df$Year=="2013"),"Hours"]))), breaks=30, main="2013", xlab="")
hist(log(as.numeric(unlist(df[which(df$Activity==A_code & df$Year=="2014"),"Hours"]))), breaks=30, main="2014", xlab="")
hist(log(as.numeric(unlist(df[which(df$Activity==A_code & df$Year=="2015"),"Hours"]))), breaks=30, main="2015", xlab="")
hist(log(as.numeric(unlist(df[which(df$Activity==A_code & df$Year=="2016"),"Hours"]))), breaks=30, main="2016", xlab="")
hist(log(as.numeric(unlist(df[which(df$Activity==A_code & df$Year=="2017"),"Hours"]))), breaks=30, main="2017", xlab="")
hist(log(as.numeric(unlist(df[which(df$Activity==A_code & df$Year=="2018"),"Hours"]))), breaks=30, main="2018", xlab="")
mtext(paste(A_code, act_crew.df[which(act_crew.df$Activity==A_code), 2], sep=": "), side=3, line=-2, outer=TRUE)

###box plot (and skew adjusted box plot) of work durations for each year,
par(mfrow=c(1,2))
box.plot=boxplot(as.numeric(Hours)~Year, data=df[which(df$Activity==A_code & df$Year!="2019"), ])
title(main=paste("Skewed ", A_code, act_crew.df[which(act_crew.df$Activity==A_code), 2], sep=": "))
skew_adjuste_box.plot=boxplot(as.numeric(Hours)~Year, data=df[which(df$Activity==A_code & df$Year!="2019"), ], 
                 range=exp(3.5*mc(unlist(df[which(df$Activity==A_code & df$Year!="2019"), "Hours"]))))
title(main=paste("Skew Adjusted ", A_code, act_crew.df[which(act_crew.df$Activity==A_code), 2], sep=": "))

###statistics trends
par(mfrow=c(4,3))
plot(unlist(act_crew.df[which(act_crew.df$Activity==A_code),3:8]), ylab="No. of crew", xaxt="n", xlab="")
axis(1, at=c(1,2,3,4,5,6), labels=c(2013, 2014, 2015, 2016, 2017, 2018))
lines(unlist(act_crew.df[which(act_crew.df$Activity==A_code),3:8]), col="blue")

plot(unlist(H_Sum.df[A_code, 2:7]), col="red", ylab="Total work duration", xaxt="n", xlab="")
axis(1, at=c(1,2,3,4,5,6), labels=c(2013, 2014, 2015, 2016, 2017, 2018))
lines(unlist(H_Sum.df[A_code, 2:7]), col="red")

plot(unlist(H_Sum.df[A_code, 2:7])%/%unlist(act_crew.df[which(act_crew.df$Activity==A_code),3:8]), col="red", ylab="Work duration per person", xaxt="n", xlab="")
axis(1, at=c(1,2,3,4,5,6), labels=c(2013, 2014, 2015, 2016, 2017, 2018))
lines(unlist(H_Sum.df[A_code, 2:7])%/%unlist(act_crew.df[which(act_crew.df$Activity==A_code),3:8]), col="red")

plot(unlist(H_1stQ.df[A_code, 2:7]), col="red", ylab="First quantile", xaxt="n", xlab="")
axis(1, at=c(1,2,3,4,5,6), labels=c(2013, 2014, 2015, 2016, 2017, 2018))
lines(unlist(H_1stQ.df[A_code, 2:7]), col="red")

plot(unlist(H_Median.df[A_code, 2:7]), col="red", ylab="Median", xaxt="n", xlab="")
axis(1, at=c(1,2,3,4,5,6), labels=c(2013, 2014, 2015, 2016, 2017, 2018))
lines(unlist(H_Median.df[A_code, 2:7]), col="red")

plot(unlist(H_Mean.df[A_code, 2:7]), col="red", ylab="Mean", xaxt="n", xlab="")
axis(1, at=c(1,2,3,4,5,6), labels=c(2013, 2014, 2015, 2016, 2017, 2018))
lines(unlist(H_Mean.df[A_code, 2:7]), col="red")

plot(unlist(H_3rdQ.df[A_code, 2:7]), col="red", ylab="Third quantile", xaxt="n", xlab="")
axis(1, at=c(1,2,3,4,5,6), labels=c(2013, 2014, 2015, 2016, 2017, 2018))
lines(unlist(H_3rdQ.df[A_code, 2:7]), col="red")

plot(unlist(H_Freq.df[A_code, 2:7]), col="red", ylab="Number of work orders", xaxt="n", xlab="")
axis(1, at=c(1,2,3,4,5,6), labels=c(2013, 2014, 2015, 2016, 2017, 2018))
lines(unlist(H_Freq.df[A_code, 2:7]), col="red")

plot(unlist(H_Skew.df[A_code, 2:7]), col="red", ylab="Skewness", xaxt="n", xlab="")
axis(1, at=c(1,2,3,4,5,6), labels=c(2013, 2014, 2015, 2016, 2017, 2018))
lines(unlist(H_Skew.df[A_code, 2:7]), col="red")

plot(unlist(H_Kurt.df[A_code, 2:7]), col="red", ylab="Kurtosis", xaxt="n", xlab="")
axis(1, at=c(1,2,3,4,5,6), labels=c(2013, 2014, 2015, 2016, 2017, 2018))
lines(unlist(H_Kurt.df[A_code, 2:7]), col="red")
mtext(paste(A_code, act_crew.df[which(act_crew.df$Activity==A_code), 2], sep=": "), side=3, line=-2, outer=TRUE)