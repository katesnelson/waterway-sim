

library(utils)
library(dplyr)

#setwd('C:/Users/tuan/Dropbox/Kate_Paul')
setwd('C:/Users/nelsonks/Dropbox/Kate_Paul/original data')
#setwd("C:/Users/nelsonks/Desktop/working papers/")

############
###inputs###
############
#the state, place(major city), and year for which you need the sunrise/sunset info for

State<-"PA"
Place<-"Pittsburgh"
Year<-"2013"
interval_time<-10 #(interval-time from NetLogo in minutes)

###########
###BUILD###
###########
#build the url to strip info from

baselink<-"http://aa.usno.navy.mil/cgi-bin/aa_rstablew.pl?ID=AA&"
year<-paste0("year=",Year)
state<-paste0("&task=0&state=", State)
place<-paste0("&place=", Place)
url<-paste0(baselink, year,state,place) #url for the time/place you need sunrise and sunset for

#build the dataset
w<-c(2,2,4,1,4,2,4,1,4,2,4,1,4,2,4,1,4,2,4,1,4,2,4,1,4,2,4,1,4,2,4,1,4,2,4,1,4,2,4,1,4,2,4,1,4,2,4,1,4) #widths of columns in html table
new<-read.fwf(url, w)                                   #read in and create a table of the html data
new<-new[35:65,]                                        #subset to the actual datetimes 


                   # date = as.Date(date, "%Y-%m-%d"),
                   # time =as.character(time, format = "%H:%M:%S"))

#Subset by month
Jan<-new[ ,c(1,3,5)]
colnames(Jan)<-c("Day", "Rise", "Set" )
Jan$Month<-"01"
Jan$Year<-Year
Jan = transform(Jan, 
                Day = as.character(Day),
                Rise = as.character(Rise),
                Set = as.character(Set))
Jan$RiseDatetime<-paste0(Jan$Month,"-", Jan$Day, "-", Jan$Year," ", Jan$Rise)
Jan$SetDatetime<-paste0(Jan$Month,"-", Jan$Day, "-", Jan$Year," ", Jan$Set)
Jan$RiseDatetime<-as.POSIXlt(Jan$RiseDatetime, format = "%m-%d-%Y %H%M", tz="GMT")
Jan$SetDatetime<-as.POSIXlt(Jan$SetDatetime, format = "%m-%d-%Y %H%M", tz="GMT")
Jan<-Jan[Jan$Rise!="    ",]
#
Feb<-new[ ,c(1, 7,9)]
colnames(Feb)<-c("Day", "Rise", "Set" )
Feb$Month<-"02"
Feb$Year<-Year
Feb = transform(Feb, 
                Day = as.character(Day),
                Rise = as.character(Rise),
                Set = as.character(Set))
Feb$RiseDatetime<-paste0(Feb$Month,"-", Feb$Day, "-", Feb$Year," ", Feb$Rise)
Feb$SetDatetime<-paste0(Feb$Month,"-", Feb$Day, "-", Feb$Year," ", Feb$Set)
Feb$RiseDatetime<-as.POSIXlt(Feb$RiseDatetime, format = "%m-%d-%Y %H%M", tz="GMT")
Feb$SetDatetime<-as.POSIXlt(Feb$SetDatetime, format = "%m-%d-%Y %H%M", tz="GMT")
Feb<-Feb[Feb$Rise!="    ",]
#
Mar<-new[ ,c(1, 11,13)]
colnames(Mar)<-c("Day", "Rise", "Set" )
Mar$Month<-"03"
Mar$Year<-Year
Mar = transform(Mar, 
                Day = as.character(Day),
                Rise = as.character(Rise),
                Set = as.character(Set))
Mar$RiseDatetime<-paste0(Mar$Month,"-", Mar$Day, "-", Mar$Year," ", Mar$Rise)
Mar$SetDatetime<-paste0(Mar$Month,"-", Mar$Day, "-", Mar$Year," ", Mar$Set)
Mar$RiseDatetime<-as.POSIXlt(Mar$RiseDatetime, format = "%m-%d-%Y %H%M", tz="GMT")
Mar$SetDatetime<-as.POSIXlt(Mar$SetDatetime, format = "%m-%d-%Y %H%M", tz="GMT")
Mar<-Mar[Mar$Rise!="    ",]
#
Apr<-new[ ,c(1, 15,17)]
colnames(Apr)<-c("Day", "Rise", "Set" )
Apr$Month<-"04"
Apr$Year<-Year
Apr = transform(Apr, 
                Day = as.character(Day),
                Rise = as.character(Rise),
                Set = as.character(Set))
Apr$RiseDatetime<-paste0(Apr$Month,"-", Apr$Day, "-", Apr$Year," ", Apr$Rise)
Apr$SetDatetime<-paste0(Apr$Month,"-", Apr$Day, "-", Apr$Year," ", Apr$Set)
Apr$RiseDatetime<-as.POSIXlt(Apr$RiseDatetime, format = "%m-%d-%Y %H%M", tz="GMT")
Apr$SetDatetime<-as.POSIXlt(Apr$SetDatetime, format = "%m-%d-%Y %H%M", tz="GMT")
Apr<-Apr[Apr$Rise!="    ",]
#
May<-new[ ,c(1, 19,21)]
colnames(May)<-c("Day", "Rise", "Set" )
May$Month<-"05"
May$Year<-Year
May = transform(May, 
                Day = as.character(Day),
                Rise = as.character(Rise),
                Set = as.character(Set))
May$RiseDatetime<-paste0(May$Month,"-", May$Day, "-", May$Year," ", May$Rise)
May$SetDatetime<-paste0(May$Month,"-", May$Day, "-", May$Year," ", May$Set)
May$RiseDatetime<-as.POSIXlt(May$RiseDatetime, format = "%m-%d-%Y %H%M", tz="GMT")
May$SetDatetime<-as.POSIXlt(May$SetDatetime, format = "%m-%d-%Y %H%M", tz="GMT")
May<-May[May$Rise!="    ",]
#
June<-new[ ,c(1, 23,25)]
colnames(June)<-c("Day", "Rise", "Set" )
June$Month<-"06"
June$Year<-Year
June = transform(June, 
                Day = as.character(Day),
                Rise = as.character(Rise),
                Set = as.character(Set))
June$RiseDatetime<-paste0(June$Month,"-", June$Day, "-", June$Year," ", June$Rise)
June$SetDatetime<-paste0(June$Month,"-", June$Day, "-", June$Year," ", June$Set)
June$RiseDatetime<-as.POSIXlt(June$RiseDatetime, format = "%m-%d-%Y %H%M", tz="GMT")
June$SetDatetime<-as.POSIXlt(June$SetDatetime, format = "%m-%d-%Y %H%M", tz="GMT")
June<-June[June$Rise!="    ",]
#
Jul<-new[ ,c(1, 27,29)]
colnames(Jul)<-c("Day", "Rise", "Set" )
Jul$Month<-"07"
Jul$Year<-Year
Jul = transform(Jul, 
                 Day = as.character(Day),
                 Rise = as.character(Rise),
                 Set = as.character(Set))
Jul$RiseDatetime<-paste0(Jul$Month,"-", Jul$Day, "-", Jul$Year," ", Jul$Rise)
Jul$SetDatetime<-paste0(Jul$Month,"-", Jul$Day, "-", Jul$Year," ", Jul$Set)
Jul$RiseDatetime<-as.POSIXlt(Jul$RiseDatetime, format = "%m-%d-%Y %H%M", tz="GMT")
Jul$SetDatetime<-as.POSIXlt(Jul$SetDatetime, format = "%m-%d-%Y %H%M", tz="GMT")
Jul<-Jul[Jul$Rise!="    ",]
#
Aug<-new[ ,c(1, 31,33)]
colnames(Aug)<-c("Day", "Rise", "Set" )
Aug$Month<-"08"
Aug$Year<-Year
Aug = transform(Aug, 
                Day = as.character(Day),
                Rise = as.character(Rise),
                Set = as.character(Set))
Aug$RiseDatetime<-paste0(Aug$Month,"-", Aug$Day, "-", Aug$Year," ", Aug$Rise)
Aug$SetDatetime<-paste0(Aug$Month,"-", Aug$Day, "-", Aug$Year," ", Aug$Set)
Aug$RiseDatetime<-as.POSIXlt(Aug$RiseDatetime, format = "%m-%d-%Y %H%M", tz="GMT")
Aug$SetDatetime<-as.POSIXlt(Aug$SetDatetime, format = "%m-%d-%Y %H%M", tz="GMT")
Aug<-Aug[Aug$Rise!="    ",]
#
Sept<-new[ ,c(1, 35,37)]
colnames(Sept)<-c("Day", "Rise", "Set" )
Sept$Month<-"09"
Sept$Year<-Year
Sept = transform(Sept, 
                Day = as.character(Day),
                Rise = as.character(Rise),
                Set = as.character(Set))
Sept$RiseDatetime<-paste0(Sept$Month,"-", Sept$Day, "-", Sept$Year," ", Sept$Rise)
Sept$SetDatetime<-paste0(Sept$Month,"-", Sept$Day, "-", Sept$Year," ", Sept$Set)
Sept$RiseDatetime<-as.POSIXlt(Sept$RiseDatetime, format = "%m-%d-%Y %H%M", tz="GMT")
Sept$SetDatetime<-as.POSIXlt(Sept$SetDatetime, format = "%m-%d-%Y %H%M", tz="GMT")
Sept<-Sept[Sept$Rise!="    ",]
#
Oct<-new[ ,c(1, 39,41)]
colnames(Oct)<-c("Day", "Rise", "Set" )
Oct$Month<-"10"
Oct$Year<-Year
Oct = transform(Oct, 
                 Day = as.character(Day),
                 Rise = as.character(Rise),
                 Set = as.character(Set))
Oct$RiseDatetime<-paste0(Oct$Month,"-", Oct$Day, "-", Oct$Year," ", Oct$Rise)
Oct$SetDatetime<-paste0(Oct$Month,"-", Oct$Day, "-", Oct$Year," ", Oct$Set)
Oct$RiseDatetime<-as.POSIXlt(Oct$RiseDatetime, format = "%m-%d-%Y %H%M", tz="GMT")
Oct$SetDatetime<-as.POSIXlt(Oct$SetDatetime, format = "%m-%d-%Y %H%M", tz="GMT")
Oct<-Oct[Oct$Rise!="    ",]
#
Nov<-new[ ,c(1, 43,45)]
colnames(Nov)<-c("Day", "Rise", "Set" )
Nov$Month<-"11"
Nov$Year<-Year
Nov = transform(Nov, 
                Day = as.character(Day),
                Rise = as.character(Rise),
                Set = as.character(Set))
Nov$RiseDatetime<-paste0(Nov$Month,"-", Nov$Day, "-", Nov$Year," ", Nov$Rise)
Nov$SetDatetime<-paste0(Nov$Month,"-", Nov$Day, "-", Nov$Year," ", Nov$Set)
Nov$RiseDatetime<-as.POSIXlt(Nov$RiseDatetime, format = "%m-%d-%Y %H%M", tz="GMT")
Nov$SetDatetime<-as.POSIXlt(Nov$SetDatetime, format = "%m-%d-%Y %H%M", tz="GMT")
Nov<-Nov[Nov$Rise!="    ",]
#
Dec<-new[ ,c(1, 47,49)]
colnames(Dec)<-c("Day", "Rise", "Set" )
Dec$Month<-"12"
Dec$Year<-Year
Dec = transform(Dec, 
                Day = as.character(Day),
                Rise = as.character(Rise),
                Set = as.character(Set))
Dec$RiseDatetime<-paste0(Dec$Month,"-", Dec$Day, "-", Dec$Year," ", Dec$Rise)
Dec$SetDatetime<-paste0(Dec$Month,"-", Dec$Day, "-", Dec$Year," ", Dec$Set)
Dec$RiseDatetime<-as.POSIXlt(Dec$RiseDatetime, format = "%m-%d-%Y %H%M", tz="GMT")
Dec$SetDatetime<-as.POSIXlt(Dec$SetDatetime, format = "%m-%d-%Y %H%M", tz="GMT")
Dec<-Dec[Dec$Rise!="    ",]

#Merge the month subsets
yrdat<-rbind(Jan, Feb, Mar, Apr, May, June, Jul, Aug, Sept, Oct, Nov, Dec)
yrdat<-yrdat[ ,6:7]
yrdat$RiseDatetime$min<-round(yrdat$RiseDatetime$min / interval_time) * interval_time   
yrdat$SetDatetime$min<-round(yrdat$SetDatetime$min / interval_time) * interval_time 
yrdat<-yrdat[!is.na(yrdat$SetDatetime),]

#create seperate datasets for sunrise and sunset
R<-as.data.frame(yrdat$RiseDatetime)
colnames(R)<-c("yrseq")
R$Rise<-"rise"

S<-as.data.frame(yrdat$SetDatetime)
colnames(S)<-c("yrseq")
S$Set<-"set"

#create a datetime sequence based on the year and interval-time given above
yrseq<-seq(yrdat$RiseDatetime[1], to=yrdat$SetDatetime[365], by = paste(interval_time, "mins"), tz ="GMT" )
yrseq<-as.data.frame(yrseq)


#merge the year sequence with the sunrise and sunset times
yrseq<-merge(yrseq, R, by ="yrseq", all.x=T )
yrseq<-merge(yrseq, S, by = "yrseq", all.x=T)

#designate times as night or day depending on the time of sunrise/sunset 
yrseq[is.na(yrseq)]<-""
yrseq$RorS<-paste0(yrseq$Rise,yrseq$Set)
yrseq$TOD<-"unknown"

for (i in 1:length(yrseq$yrseq))
{
  if (yrseq$RorS[i]=="rise") {
  yrseq$TOD[i] <- "day"}
  if (yrseq$RorS[i]=="set") {
    yrseq$TOD[i] <- "night"}
}
for (i in 2:(length(yrseq$yrseq)-1))
{
  if (yrseq$TOD[i]=="unknown") {
    if (yrseq$TOD[i-1]== "day") {
    yrseq$TOD[i] <- "day"}}
  if (yrseq$TOD[i]=="unknown") {
    if (yrseq$TOD[i-1]== "night") {
      yrseq$TOD[i] <- "night"}}
}

sunriseset<-yrseq[,c(1,5)]
colnames(sunriseset)<-c("timestamp","TOD")

###########
###WRITE###
###########

#write out the timeseries
write.csv(sunriseset, "sunriseset10.csv", row.names=FALSE) #IMPORTANT!!! open the csv files and remove the quotes from the column headers or they will not pull up correctly in NetLogo