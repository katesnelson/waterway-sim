library(dplyr)
library(data.table)

setwd('C:/Users/tuan/Dropbox/Kate_Paul/2013_clean/')
#setwd('C:/Users/nelsonks/Dropbox/Kate_Paul/2013_clean/')

interval_time<-30 #(interval-time from NetLogo in minutes)

##############################################
####READ AND PREP THE RAW TIME SERIES DATA####
##############################################

#read in the time series of AIS transit times with associated start and end time, for each link-direction
ts1d<-read.csv("link1-time-series-data-down.csv", header=F, na.strings= "") 
ts1u<-read.csv("link1-time-series-data-up.csv", header=F, na.strings= "")
ts2d<-read.csv("link2-time-series-data-down.csv", header=F, na.strings= "")
ts2u<-read.csv("link2-time-series-data-up.csv", header=F, na.strings= "")
ts3d<-read.csv("link3-time-series-data-down.csv", header=F, na.strings= "")
ts3u<-read.csv("link3-time-series-data-up.csv", header=F, na.strings= "")
ts4d<-read.csv("link4-time-series-data-down.csv", header=F, na.strings= "")
ts4u<-read.csv("link4-time-series-data-up.csv", header=F, na.strings= "")

#remove any unneeded NAs
ts1d<-ts1d[!is.na(ts1d$V1),]
ts2d<-ts2d[!is.na(ts2d$V1),]
ts3d<-ts3d[!is.na(ts3d$V1),]
ts4d<-ts4d[!is.na(ts4d$V1),]
ts1u<-ts1u[!is.na(ts1u$V1),]
ts2u<-ts2u[!is.na(ts2u$V1),]
ts3u<-ts3u[!is.na(ts3u$V1),]
ts4u<-ts4u[!is.na(ts4u$V1),]

#Convert to Datetime objects and build the table structure for each link-direction
ts<-ts1u
ts$V1 <- as.POSIXlt.factor(ts$V1, tz ="GMT", "%m/%d/%y %H:%M" ) #convert from factor to datetime format
ts$V2 <- as.POSIXlt.factor(ts$V2, tz ="GMT", "%m/%d/%y %H:%M" )
colnames(ts)<-c("arrival", "departure") #name the column timestamp for format recognition by Netlogo::time extension
link1u<-ts #rename to be link-direction specific
link1u$name<-NA #placeholders to be used later
link1u$link1tt<-difftime(link1u$departure,link1u$arrival,units= "mins") #calculate the transit time for this link
link1u$link2tt<-NA
link1u$link3tt<-NA
link1u$link4tt<-NA

ts<-ts2u
ts$V1 <- as.POSIXlt.factor(ts$V1, tz ="GMT", "%m/%d/%y %H:%M" ) #convert from factor to datetime format
ts$V2 <- as.POSIXlt.factor(ts$V2, tz ="GMT", "%m/%d/%y %H:%M" )
colnames(ts)<-c("arrival", "departure") #name the column timestamp for format recognition by Netlogo::time extension
link2u<-ts #rename to be link-direction specific
link2u$name<-NA
link2u$link1tt<-NA
link2u$link2tt<-difftime(link2u$departure,link2u$arrival,units= "mins") 
link2u$link3tt<-NA
link2u$link4tt<-NA

ts<-ts3u
ts$V1 <- as.POSIXlt.factor(ts$V1, tz ="GMT", "%m/%d/%y %H:%M" ) #convert from factor to datetime format
ts$V2 <- as.POSIXlt.factor(ts$V2, tz ="GMT", "%m/%d/%y %H:%M" )
colnames(ts)<-c("arrival", "departure") #name the column timestamp for format recognition by Netlogo::time extension
link3u<-ts #rename to be link-direction specific
link3u$name<-NA
link3u$link1tt<-NA
link3u$link2tt<-NA
link3u$link3tt<-difftime(link3u$departure,link3u$arrival,units= "mins") 
link3u$link4tt<-NA

ts<-ts4u
ts$V1 <- as.POSIXlt.factor(ts$V1, tz ="GMT", "%m/%d/%y %H:%M" ) #convert from factor to datetime format
ts$V2 <- as.POSIXlt.factor(ts$V2, tz ="GMT", "%m/%d/%y %H:%M" )
colnames(ts)<-c("arrival", "departure") #name the column timestamp for format recognition by Netlogo::time extension
link4u<-ts #rename to be link-direction specific
link4u$name<-NA
link4u$link1tt<-NA
link4u$link2tt<-NA
link4u$link3tt<-NA
link4u$link4tt<-difftime(link4u$departure,link4u$arrival,units= "mins")

ts<-ts1d
ts$V1 <- as.POSIXlt.factor(ts$V1, tz ="GMT", "%m/%d/%y %H:%M" ) #convert from factor to datetime format
ts$V2 <- as.POSIXlt.factor(ts$V2, tz ="GMT", "%m/%d/%y %H:%M" )
colnames(ts)<-c("arrival", "departure") #name the column timestamp for format recognition by Netlogo::time extension
link1d<-ts #rename to be link-direction specific
link1d$name<-NA
link1d$link1tt<-difftime(link1d$departure,link1d$arrival,units= "mins")
link1d$link2tt<-NA
link1d$link3tt<-NA
link1d$link4tt<-NA

ts<-ts2d
ts$V1 <- as.POSIXlt.factor(ts$V1, tz ="GMT", "%m/%d/%y %H:%M" ) #convert from factor to datetime format
ts$V2 <- as.POSIXlt.factor(ts$V2, tz ="GMT", "%m/%d/%y %H:%M" )
colnames(ts)<-c("arrival", "departure") #name the column timestamp for format recognition by Netlogo::time extension
link2d<-ts #rename to be link-direction specific
link2d<-ts #rename to be link-direction specific
link2d$name<-NA
link2d$link1tt<-NA
link2d$link2tt<-difftime(link2d$departure,link2d$arrival,units= "mins")
link2d$link3tt<-NA
link2d$link4tt<-NA

ts<-ts3d
ts$V1 <- as.POSIXlt.factor(ts$V1, tz ="GMT", "%m/%d/%y %H:%M" ) #convert from factor to datetime format
ts$V2 <- as.POSIXlt.factor(ts$V2, tz ="GMT", "%m/%d/%y %H:%M" )
colnames(ts)<-c("arrival", "departure") #name the column timestamp for format recognition by Netlogo::time extension
link3d<-ts #rename to be link-direction specific
link3d<-ts #rename to be link-direction specific
link3d$name<-NA
link3d$link1tt<-NA
link3d$link2tt<-NA
link3d$link3tt<-difftime(link3d$departure,link3d$arrival,units= "mins")
link3d$link4tt<-NA

ts<-ts4d
ts$V1 <- as.POSIXlt.factor(ts$V1, tz ="GMT", "%m/%d/%y %H:%M" ) #convert from factor to datetime format
ts$V2 <- as.POSIXlt.factor(ts$V2, tz ="GMT", "%m/%d/%y %H:%M" )
colnames(ts)<-c("arrival", "departure") #name the column timestamp for format recognition by Netlogo::time extension
link4d<-ts #rename to be link-direction specific
link4d<-ts #rename to be link-direction specific
link4d$name<-NA
link4d$link1tt<-NA
link4d$link2tt<-NA
link4d$link3tt<-NA
link4d$link4tt<-difftime(link4d$departure,link4d$arrival,units= "mins")


#######################
####SUBSET THE DATA####
#######################
#Need 4 things: 1) a list for tows that are departing the current link, but do not enter the following link, of: arrival times at the first link and ending river mile of the first link
#               2) a list for tows that have an arrival time for the following link, but no corresponding departure time for the current link (new arrivals), of: arrival times at the  
#                   following link and the starting river mile of the following link
#               3) a list for tows that traveled on the current link and continue traveling on the following link of:arrival, departures, and transit times
#               4) a list for tows traveling on a link of: arrival, departure, and transit times, and their point of origin


###UPSTREAM### 
#for upstream, direction of travel link 4-->3-->2-->1

#link 4 to 3
current4<-as.data.frame(link4u) #link currently being traveled, this will have the larger link id number
current4$name<-seq(1, length(current4$arrival)) #create the unique tow id for tows starting on link 4
entering4<-current4 #for the last link in the segment the tows the full record of those currently traveling it is the same as those that enter at that link

#identify records for tows that continue travel from the current link to the following link and add their transit times for the following link
following3<-as.data.frame(link3u) #link that follows that being currently traveled, this will have the smaller link id number
following3$adjustedarrival<-following3$arrival          #add 5 minutes to the following segment arrival time so that, for continuing tows...
following3$adjustedarrival$min<-following3$adjustedarrival$min +5 #the departure time from one link equals the arrival time at the following link
continuing4<-current4[(current4$departure %in% following3$adjustedarrival), ] #identifies tows that travel on both current and folowwing link
continuing4<-merge(continuing4,following3, by.x = "departure", by.y= "adjustedarrival",sort=F, all.x=T ) #add the link3 transit times for tows that travel both link 4 and link 3
continuing4[is.na(continuing4)]<-0 #convert NAs to zero 
continuing4$link1tt<-( continuing4$link1tt.x) #merge the transit time columns
continuing4$link2tt<-(continuing4$link2tt.x) #merge the transit time columns
continuing4$link3tt<-( continuing4$link3tt.y) #merge the transit time columns
continuing4$link4tt<-(continuing4$link4tt.x) #merge the transit time columns
continuing4<-continuing4[ ,-c(4:14)] #remove uncessesary info
continuing4<-continuing4[,c(2,1,3:7)] #flip back the reversed arrival and departure times
colnames(continuing4)<-c("arrival", "departure", "name","link1tt","link2tt", "link3tt", "link4tt")

#identify records for tows that are active on the current link but not on the following link
departing4<-current4[!(current4$departure %in% following3$adjustedarrival), ] 

#identify records for tows that are active on the following link but not on the current link
entering3<-following3[!(following3$adjustedarrival %in% current4$departure), ] 
entering3$name<-seq((max(current4$name)+1), (max(current4$name)+length(entering3$arrival)))#add unique names to the tows entering at link 3

#add a location for the action (tow enter or depart) based on the link and direction of the timeseries data (Pitts area: 0,10,20,27,37)
entering4$origin<-37
departing4$destination<-27
entering3$origin<-27
entering3$destination<-0

#add known destinations to the full list of tows entering at link 4
t4<-merge(entering4, departing4, by =c("arrival", "departure"), sort=F, all.x=T) 
t4<-t4[ ,-c(9:13)] #remove duplicate departing times  column
colnames(t4)<-c("arrival", "departure", "name","link1tt","link2tt", "link3tt", "link4tt","origin", "destination")

#now add transit times for tows continuing travle on next link
t4<-merge(t4,continuing4, by = c("arrival", "departure"),sort=F, all.x=T) 
t4<-t4[,-(10)]
t4[is.na(t4)]<-0#convert NAs to zero
t4$link1tt<-(t4$link1tt.x) #collapse the transit time columns
t4$link2tt<-(t4$link2tt.x) #collapse the transit time columns
t4$link3tt<-(t4$link3tt.y) #collapse the transit time columns
t4$link4tt<-(t4$link4tt.x) #collapse the transit time columns
t4<-t4[,-c(4:7,10:13)]
t4<-t4[,c(1:3,6:9,4,5)]
colnames(t4)<-c("arrival", "departure", "name","link1tt","link2tt", "link3tt", "link4tt","origin", "destination")

#combine the list of tows entering at link 4 and those entering at link 3
t3<-entering3[ ,-8]
t3<-rbind(t4,t3)       

# link 3 to 2
current3<-rbind(continuing4,entering3[ ,-c(8:10)]) #the full set of tows traveling on link 3 is equal to ...
                                                    #those continuing from the previous plus those that enter the waterway at the current link

#identify records for tows that continue travel from the current link to the following link and add their transit times for the following link
following2<-as.data.frame(link2u) #this will have the smaller link id number
following2$adjustedarrival<-following2$arrival          #add 5 minutes to the following segment arrival time so that, for continuing tows the departure time from one link equals the arrival time at the following link
following2$adjustedarrival$min<-following2$adjustedarrival$min +5
continuing3<-current3[(current3$departure %in% following2$adjustedarrival), ] #identifies records for tows that continue travel from the current link to the following link
continuing3<-merge(continuing3,following2, by.x = "departure", by.y= "adjustedarrival",sort=F, all.x=T ) #add the link3 transit times for tows that travel both link 4 and link 3
continuing3[is.na(continuing3)]<-0 #convert NAs to zero 
continuing3$link1tt<-(continuing3$link1tt.x) #condense the ttime  columns
continuing3$link2tt<-(continuing3$link2tt.y) #condense the ttime  columns
continuing3$link3tt<-(continuing3$link3tt.x) #condense the ttime columns
continuing3$link4tt<-(continuing3$link4tt.x) #condense the ttime columns
continuing3<-continuing3[ ,-c(4:14)]
continuing3<-continuing3[,c(2,1,3:7)] #flip back arrival and departure times
colnames(continuing3)<-c("arrival", "departure", "name","link1tt","link2tt", "link3tt", "link4tt")

#identifies records for tows that are active on the current link but not on the following link
departing3<-current3[!(current3$departure %in% following2$adjustedarrival), ] 

#identifies records for tows that are active on the following link but not on the current link
entering2<-following2[!(following2$adjustedarrival %in% current3$departure), ] 
entering2$name<-seq((max(current3$name)+1), (max(current3$name)+length(entering2$arrival)))

#add a location for the action (tow enter or depart) based on the link and direction of the timeseries data (0,10,20,27,37)
departing3$destination<-20
entering2$origin<-20
entering2$destination<-0

#add known destinations to the list of tows 
t3<-merge(t3, departing3, by =c("arrival", "departure"), sort=F, all.x=T)
t3<-t3[ ,-c(10:14)] #remove duplicate departing times  column
t3[is.na(t3)]<-0 #convert NAs to zero
t3$destination<-(t3$destination.x+ t3$destination.y) #merge the destination columns
t3<-t3[ ,-c(9:10)] #remove extraneous info
colnames(t3)<-c("arrival", "departure", "name","link1tt","link2tt", "link3tt", "link4tt","origin", "destination")

#now add transit times for tows continuing travle on next link
t3<-merge(t3,continuing3, by = c("arrival", "departure"),sort=F, all.x=T) 
t3<-t3[,-(10)]
t3[is.na(t3)]<-0#convert NAs to zero 
t3$link1tt<-( t3$link1tt.x) #condense the transit time columns
t3$link2tt<-( t3$link2tt.y) #condense the transit time columns
t3$link3tt<-(t3$link3tt.x) #condense the transit time columns
t3$link4tt<-(t3$link4tt.x) #condensethe transit time columns
t3<-t3[,-c(4:7,10:13)]
t3<-t3[,c(1:3,6:9,4,5)]
colnames(t3)<-c("arrival", "departure", "name","link1tt","link2tt", "link3tt", "link4tt","origin", "destination")

#combine the list of tows with those entering at link 2
t2<-entering2[ ,-8] 
t2<-rbind(t3,t2)       

# link 2 to 1
current2<-rbind(continuing3,entering2[ ,-c(8:10)])

#identify records for tows that continue travel from the current link to the following link
following1<-as.data.frame(link1u) #this will have the smaller link id number
following1$adjustedarrival<-following1$arrival          #add 5 minutes to the following segment arrival time so that, for continuing tows the departure time from one link equals the arrival time at the following link
following1$adjustedarrival$min<-following1$adjustedarrival$min +5
continuing2<-current2[(current2$departure %in% following1$adjustedarrival), ] #identifies records for tows that continue travel from the current link to the following link
continuing2<-merge(continuing2,following1, by.x = "departure", by.y= "adjustedarrival",sort=F, all.x=T ) #add the link3 transit times for tows that travel both link 4 and link 3
continuing2[is.na(continuing2)]<-0 #convert NAs to zero 
continuing2$link1tt<-(continuing2$link1tt.y) #condense the ttime  columns
continuing2$link2tt<-(continuing2$link2tt.x) #condense the ttime  columns
continuing2$link3tt<-(continuing2$link3tt.x) #condense the ttime columns
continuing2$link4tt<-(continuing2$link4tt.x) #condense the ttime columns
continuing2<-continuing2[ ,-c(4:14)]
continuing2<-continuing2[,c(2,1,3:7)] #flip back arrival and departure times
colnames(continuing2)<-c("arrival", "departure", "name","link1tt","link2tt", "link3tt", "link4tt")

#identifies records for tows that are active on the current link but not on the following link
departing2<-current2[!(current2$departure %in% following1$adjustedarrival), ] 

#identifies records for tows that are active on the following link but not on the current link
entering1<-following1[!(following1$adjustedarrival %in% current2$departure), ] 
entering1$name<-seq((max(current2$name)+1), (max(current2$name)+length(entering1$arrival)))

#add a location for the action (tow enter or depart) based on the link and direction of the timeseries data (0,10,20,27,37)
departing2$destination<-10
entering1$origin<-10
entering1$destination<-0

#add known destinations to the list of tows 
t2<-merge(t2, departing2, by =c("arrival", "departure"), sort=F, all.x=T) 
t2<-t2[ ,-c(10:14)] #remove duplicate departing times  column
t2[is.na(t2)]<-0 #convert NAs to zero
t2$destination<-(t2$destination.x+ t2$destination.y) #merge the destination columns
t2<-t2[ ,-c(9:10)] #remove extraneous info
colnames(t2)<-c("arrival", "departure", "name","link1tt","link2tt", "link3tt", "link4tt","origin", "destination")

#now add transit times for tows continuing travle on next link
t2<-merge(t2,continuing2, by = c("arrival", "departure"),sort=F, all.x=T) 
t2<-t2[,-(10)]
t2[is.na(t2)]<-0#convert NAs to zero
t2$link1tt<-( t2$link1tt.y) #condense the transit time columns
t2$link2tt<-( t2$link2tt.x) #condense the transit time columns
t2$link3tt<-(t2$link3tt.x) #condense the transit time columns
t2$link4tt<-(t2$link4tt.x) #condensethe transit time columns
t2<-t2[,-c(4:7,10:13)]
t2<-t2[,c(1:3,6:9,4,5)]
colnames(t2)<-c("arrival", "departure", "name","link1tt","link2tt", "link3tt", "link4tt","origin", "destination")

#combine the list of tows with those entering at link 1
t1<-entering1[ ,-8] 
t1<-rbind(t2,t1)       
t1[is.na(t1)]<-0 
if (any(duplicated(t1$name))== TRUE) #test to make sure you haven't duplicated any tows records
{t1<-unique(t1)} #if there are dups retain only the unique records

#build dataset for NetLogo
upstream<-t1
upstream$arrival_orig<-upstream$arrival
upstream$arrival$min<-round(upstream$arrival$min / interval_time) * interval_time  #round waterway entry times to the nearest tick increment (half hour)
upstream$timestamp<-upstream$arrival #create a new column named "timestamp" for format recognition by Netlogo::time extension 
upstream<-upstream[,c(11,1:10)] #rearrange so timestamp is first column (NetLogo formatting thing)

upstream<-upstream[order(upstream$timestamp), ] #this is our output


###DOWNSTREAM### 
#for downstream, direction of travel link 1-->2-->3-->4

#link 1 to 2
current1<-as.data.frame(link1d) #this will have the smaller link id number
current1$name<-seq((max(upstream$name)+1), (max(upstream$name)+length(current1$arrival))) #create the unique tow id for tows starting on link 1 and headed downstream
entering1<-current1


following2<-as.data.frame(link2d) #this will have the larger link id number
following2$adjustedarrival<-following2$arrival          #add 5 minutes to the following segment arrival time so that, for continuing tows the departure time from one link equals the arrival time at the following link
following2$adjustedarrival$min<-following2$adjustedarrival$min +5
continuing1<-current1[(current1$departure %in% following2$adjustedarrival), ] #identifies records for tows that continue travel from the current link to the following link
continuing1<-merge(continuing1,following2, by.x = "departure", by.y= "adjustedarrival",sort=F, all.x=T ) #add the link3 transit times for tows that travel both link 4 and link 3
continuing1[is.na(continuing1)]<-0 #convert NAs to zero 
continuing1$link1tt<-(continuing1$link1tt.x) #merge the transit time columns
continuing1$link2tt<-(continuing1$link2tt.y) #merge the transit time columns
continuing1$link3tt<-(continuing1$link3tt.x) #merge the transit time columns
continuing1$link4tt<-(continuing1$link4tt.x) #merge the transit time columns
continuing1<-continuing1[ ,-c(4:14)]
continuing1<-continuing1[,c(2,1,3:7)] #flip back arrival and departure times
colnames(continuing1)<-c("arrival", "departure", "name","link1tt","link2tt", "link3tt", "link4tt")

departing1<-current1[!(current1$departure %in% following2$adjustedarrival), ] #identifies records for tows that are active on the current link but not on the following link

entering2<-following2[!(following2$adjustedarrival %in% current1$departure), ] #identifies records for tows that are active on the following link but not on the current link
entering2$name<-seq((max(current1$name)+1), (max(current1$name)+length(entering2$arrival)))

#add a location for the action (tow enter or depart) based on the link and direction of the timeseries data (0,10,20,27,37)
entering1$origin<-0
departing1$destination<-10
entering2$origin<-10
entering2$destination<-0 #put zero for unknown destiantion placeholder

#add known destinations to the list of tows 
t1<-merge(entering1, departing1, by = c("arrival","departure"), sort=F, all.x=T) 
t1<-t1[ ,-c(9:13)] #remove duplicate departing times  column
colnames(t1)<-c("arrival", "departure", "name","link1tt","link2tt", "link3tt", "link4tt","origin", "destination")

#now add transit times for tows continuing travle on next link
t1<-merge(t1,continuing1, by = c("arrival", "departure"),sort=F, all.x=T) 
t1<-t1[,-(10)]
t1[is.na(t1)]<-0#convert NAs to zero
t1$link1tt<-( t1$link1tt.x) #condense the transit time columns
t1$link2tt<-( t1$link2tt.y) #condense the transit time columns
t1$link3tt<-(t1$link3tt.x) #condense the transit time columns
t1$link4tt<-(t1$link4tt.x) #condensethe transit time columns
t1<-t1[,-c(4:7,10:13)]
t1<-t1[,c(1:3,6:9,4,5)]
colnames(t1)<-c("arrival", "departure", "name","link1tt","link2tt", "link3tt", "link4tt","origin", "destination")

#combine the list of tows entering at link 1 and those entering at link 2
t2<-entering2[ ,-8] 
t2<-rbind(t1,t2)    

# link 2 to 3
current2<-rbind(continuing1,entering2[ ,-c(8:10)])

following3<-as.data.frame(link3d) #this will have the larger link id number
following3$adjustedarrival<-following3$arrival          #add 5 minutes to the following segment arrival time so that, for continuing tows the departure time from one link equals the arrival time at the following link
following3$adjustedarrival$min<-following3$adjustedarrival$min +5
continuing2<-current2[(current2$departure %in% following3$adjustedarrival), ] #identifies records for tows that continue travel from the current link to the following link
continuing2<-merge(continuing2,following3, by.x = "departure", by.y= "adjustedarrival",sort=F, all.x=T ) #add the link3 transit times for tows that travel both link 4 and link 3
continuing2[is.na(continuing2)]<-0 #convert NAs to zero 
continuing2$link1tt<-(continuing2$link1tt.x) #merge the transit time columns
continuing2$link2tt<-(continuing2$link2tt.x) #merge the transit time columns
continuing2$link3tt<-(continuing2$link3tt.y) #merge the transit time columns
continuing2$link4tt<-(continuing2$link4tt.x) #merge the transit time columns
continuing2<-continuing2[ ,-c(4:14)]
continuing2<-continuing2[,c(2,1,3:7)] #flip back arrival and departure times
colnames(continuing2)<-c("arrival", "departure", "name","link1tt","link2tt", "link3tt", "link4tt")

departing2<-current2[!(current2$departure %in% following3$adjustedarrival), ] #identifies records for tows that are active on the current link but not on the following link

entering3<-following3[!(following3$adjustedarrival %in% current2$departure), ] #identifies records for tows that are active on the following link but not on the current link
entering3$name<-seq((max(current2$name)+1), (max(current2$name)+length(entering3$arrival)))

#add a location for the action (tow enter or depart) based on the link and direction of the timeseries data (0,10,20,27,37)
departing2$destination<-20
entering3$origin<-20
entering3$destination<-0

#add known destinations to the list of tows 
t2<-merge(t2, departing2, by =c("arrival","departure"), sort=F, all.x=T) 
t2<-t2[ ,-c(10:14)] #remove duplicate departing times  column
t2[is.na(t2)]<-0 #convert NAs to zero
t2$destination<-(t2$destination.x+ t2$destination.y) #merge the destination columns #merge the destination columns
t2<-t2[ ,-c(9:10)] #remove extraneous info
colnames(t2)<-c("arrival", "departure", "name","link1tt","link2tt", "link3tt", "link4tt","origin", "destination")

#now add transit times for tows continuing travel on next link
t2<-merge(t2,continuing2, by = c("arrival", "departure"),sort=F, all.x=T) 
t2<-t2[,-(10)]
t2[is.na(t2)]<-0#convert NAs to zero
t2$link1tt<-( t2$link1tt.x) #condense the transit time columns
t2$link2tt<-( t2$link2tt.x) #condense the transit time columns
t2$link3tt<-(t2$link3tt.y) #condense the transit time columns
t2$link4tt<-(t2$link4tt.x) #condensethe transit time columns
t2<-t2[,-c(4:7,10:13)]
t2<-t2[,c(1:3,6:9,4,5)]
colnames(t2)<-c("arrival", "departure", "name","link1tt","link2tt", "link3tt", "link4tt","origin", "destination")

#combine the list of tows with those entering at link 2
t3<-entering3[ ,-8] 
t3<-rbind(t2,t3)       

# link 3 to 4
current3<-rbind(continuing2,entering3[ ,-c(8:10)])

following4<-as.data.frame(link4d) #this will have the larger link id number
following4$adjustedarrival<-following4$arrival          #add 5 minutes to the following segment arrival time so that, for continuing tows the departure time from one link equals the arrival time at the following link
following4$adjustedarrival$min<-following4$adjustedarrival$min +5
continuing3<-current3[(current3$departure %in% following4$adjustedarrival), ] #identifies records for tows that continue travel from the current link to the following link
continuing3<-merge(continuing3,following4, by.x = "departure", by.y= "adjustedarrival",sort=F, all.x=T ) #add the link3 transit times for tows that travel both link 4 and link 3
continuing3[is.na(continuing3)]<-0 #convert NAs to zero 
continuing3$link1tt<-(continuing3$link1tt.x) #merge the transit time columns
continuing3$link2tt<-(continuing3$link2tt.x) #merge the transit time columns
continuing3$link3tt<-(continuing3$link3tt.x) #merge the transit time columns
continuing3$link4tt<-(continuing3$link4tt.y) #merge the transit time columns
continuing3<-continuing3[ ,-c(4:14)]
continuing3<-continuing3[,c(2,1,3:7)] #flip back arrival and departure times
colnames(continuing3)<-c("arrival", "departure", "name","link1tt","link2tt", "link3tt", "link4tt")

departing3<-current3[!(current3$departure %in% following4$adjustedarrival), ] #identifies records for tows that are active on the current link but not on the following link

entering4<-following4[!(following4$adjustedarrival %in% current3$departure), ] #identifies records for tows that are active on the following link but not on the current link
entering4$name<-seq((max(current3$name)+1), (max(current3$name)+length(entering4$arrival)))

#add a location for the action (tow enter or depart) based on the link and direction of the timeseries data (0,10,20,27,37)
departing3$destination<-27
entering4$origin<-27
entering4$destination<-37

#add known destinations to the list of tows 
t3<-merge(t3, departing3, by = c("arrival","departure"), sort=F, all.x=T) 
t3<-t3[ ,-c(10:14)] #remove duplicate departing times  column
t3[is.na(t3)]<-0 #convert NAs to zero
t3$destination<-(t3$destination.x+ t3$destination.y) #merge the destination columns
t3<-t3[ ,-c(9:10)] #remove extraneous info
colnames(t3)<-c("arrival", "departure", "name","link1tt","link2tt", "link3tt", "link4tt","origin", "destination")

#now add transit times for tows continuing travle on next link
t3<-merge(t3,continuing3, by = c("arrival", "departure"),sort=F, all.x=T) 
t3<-t3[,-(10)]
t3[is.na(t3)]<-0#convert NAs to zero
t3$link1tt<-( t3$link1tt.x) #condense the transit time columns
t3$link2tt<-( t3$link2tt.x) #condense the transit time columns
t3$link3tt<-(t3$link3tt.x) #condense the transit time columns
t3$link4tt<-(t3$link4tt.y) #condensethe transit time columns
t3<-t3[,-c(4:7,10:13)]
t3<-t3[,c(1:3,6:9,4,5)]
colnames(t3)<-c("arrival", "departure", "name","link1tt","link2tt", "link3tt", "link4tt","origin", "destination")

#combine the list of tows with those entering at link 1
t4<-entering4[ ,-8] 
t4<-rbind(t3,t4)       
t4[is.na(t4)]<-0#convert NAs to zero
t4$destination[t4$destination==0]<-37 #convert zeros in destination (were there to simplify mergers) to the last possible destination river mile
if (any(duplicated(t4$name)) == TRUE)
{t4<-unique(t4)}

#build dataset for NetLogo
downstream<-t4
downstream$arrival_orig<-downstream$arrival
downstream$arrival$min<-round(downstream$arrival$min / interval_time) * interval_time           #round waterway entry times to the nearest tick increment (half hour)
downstream$timestamp<-downstream$arrival #create a new column named "timestamp" for format recognition by Netlogo::time extension 
downstream<-downstream[,c(11,1:10)] #rearrange so timestamp is first column (NetLogo formatting thing)
downstream<-downstream[order(downstream$timestamp), ] #this is our output


#####################
####FINAL PRODUCT####
#####################


write.csv(upstream, "upstreaming2.csv", row.names=FALSE)
write.csv(downstream, "downstreaming2.csv", row.names=FALSE) #IMPORTANT!!! open the csv files and remove the quotes from the column headers or they will not pull up correctly in NetLogo