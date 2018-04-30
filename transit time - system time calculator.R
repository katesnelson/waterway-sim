#1
#!!!IMPORTANT: Before you start check file name (line 19) and interval time (line 47)!!!#

library (dplyr)
library (tidyr)
library(splitstackshape)
library (stringr)

#setwd('C:/Users/nelsonks/Dropbox/Kate_Paul/paul_simulations/2013_AIS_runs_02')
#setwd('C:/Users/nelsonks/Dropbox/Kate_Paul/paul_simulations/2013_AIS_15min_jan_jun')
#setwd('C:/Users/nelsonks/Dropbox/Kate_Paul/paul_simulations/2013_AIS_15min_may_oct')
#setwd('C:/Users/nelsonks/Dropbox/Kate_Paul/paul_simulations/2013_AIS_15min_jun_aug')
#setwd('C:/Users/nelsonks/Dropbox/Kate_Paul/paul_simulations/2013_AIS_30min_jan_jun')
#setwd('C:/Users/nelsonks/Dropbox/Kate_Paul/paul_simulations/2013_AIS_30min_may_oct')
#setwd('C:/Users/nelsonks/Dropbox/Kate_Paul/paul_simulations/2013_AIS_30min_jun_aug')
setwd('C:/Users/nelsonks/Dropbox/Kate_Paul/paul_simulations/2013_AIS_DOE_01')

####################################
####READ IN THE SIMULATION DATA####
###################################
file<-"042018_D_DOE_junaug_30min_D.txt"
simname<-substr(file,1,9)
dat<-scan(paste(file), what=character(), sep =",", strip.white=T, blank.lines.skip=T) #scan in the simulation data
new <-  strsplit(as.character(dat),", ",fixed=TRUE) #break up single text line of data into rows of text 
d<-as.data.frame(new)
colnames(d)= c("col")
dnew<-as.data.frame(do.call('rbind', strsplit(as.character(d$col)," "))) #Break up the data in each row into different columns using a spcae delimiter
dnew<-dnew[ ,1:20] #select only the columns we need, then rename each column, and select the final dataset
colnames(dnew) <- c("who", "xcor", "heading", "speed",  "time.of.day",  "barges.delivered",  "status", "connected.barges", "transit.time","birthday", "deathday","origin","destination", "link1tt", "link2tt", "link3tt", "link4tt", "id", "date", "time")
simdat<-dplyr::select(dnew, who, xcor, heading, speed,  time.of.day,  barges.delivered,  status, connected.barges, transit.time, birthday, origin, destination, deathday, link1tt, link2tt, link3tt, link4tt, id, date, time)
head(simdat) #check the data layout then transform the data ine ach column to the desired format
simdat = transform(simdat, 
                  xcor = as.numeric(as.character(xcor)),
                  speed = as.numeric(as.character(speed)),
                  barges.delivered = as.numeric(as.character(barges.delivered)),
                  connected.barges = as.numeric(as.character(connected.barges)),
                  heading = as.numeric(as.character(heading)),
                  transit.time = as.numeric(as.character(transit.time)),
                  link1tt = as.numeric(as.character(link1tt))/60, #convert mins to hours
                  link2tt = as.numeric(as.character(link2tt))/60,
                  link3tt = as.numeric(as.character(link3tt))/60,
                  link4tt = as.numeric(as.character(link4tt))/60,
                  birthday = as.numeric(as.character(birthday)),
                  deathday = as.numeric(as.character(deathday)),
                  date = as.Date(date, "%Y-%m-%d"),
                  time =as.character(time, format = "%H:%M:%S"))
simdat$datetime <- with(simdat, as.POSIXct(paste(date, time), format="%Y-%m-%d %H:%M")) #combine the date and time columns

inttime<-30 #interval time for the simulation
#############################################################
####SETUP A LOOP TO CALCULATE TRANSIT TIMES FOR EACH TOW####
#############################################################

tows<-as.data.frame(unique(simdat$who))
colnames(tows)<-c("name")

#choose any two river miles, for simplicity make first location the smaller one

#for Pitts area Dams: 6, 13, 32 AND AISlinks: 0,10,20,27,37
max_pxcor<- 75 #max x coordinate in the model setup (check in 3D View settings)

#link through the segments that you want to calculate transit time for
links<-c("link1","link2","link3","link4")
n<-length(links)

for (i in 1:n)
{ if (links[i] == "link1"){
  loc1=0
  loc2=10
  linkname<-links[i]
}else{
  if (links[i]=="link2"){
    loc1=10
    loc2=20
    linkname<-links[i]
  }else{
    if (links[i]=="link3"){
      loc1=20
      loc2=27
      linkname<-links[i]
    }else{
      if (links[i]=="link4"){
        loc1=27
        loc2=37
        linkname<-links[i]
      }
    }
  }
}



# loc1<- 0
# loc2<- 10

loc1<-(max_pxcor -(loc1+ 10))
loc2<-(max_pxcor -(loc2 +10))

transittime<-tows #create a copy dataframe to add calculated transit times to
transittime$ttime<-NA

for (j in 1:length(tows$name))
  {

################################
####EXTRACT SINGLE TOW TRACK####
################################

towtrack<-simdat[simdat$who==tows$name[j], ]

#########################################################################################
####CALCULATE A TRANSIT TIME FOR SINGLE TOW AND WRITE TO THE EMPTY TRANSITTIME VECTOR####
##########################################################################################

subset1<-filter(towtrack, xcor<=loc1+1 & xcor>=loc1-1) #filter to records within x river miles of input locations
subset2<-filter(towtrack, xcor<=loc2+1 & xcor>=loc2-1)
subset1$diff<-abs(subset1$xcor-loc1)                   #calculate the difference between the selected record river mile and the input locations
subset2$diff<-abs(subset2$xcor-loc2)
if (length(subset1$datetime>1)){
subset1<-subset1[subset1$diff==min(subset1$diff), ]}   #if more than one record within 2 river miles of input location choose the record closest to input location
if (length(subset2$datetime>1)){
subset2<-subset2[subset2$diff==min(subset2$diff), ]}

if (length(subset1$datetime>0) & length(subset2$datetime>0)){
if (subset1$heading[1] == 270) {
  ttime<- difftime(max(subset2$datetime),min(subset1$datetime), units=c("hours"))#calculate time difference between time at loc1 and time at loc2
} else {
  ttime<-difftime(max(subset1$datetime),min(subset2$datetime), units=c("hours"))
}
transittime$ttime[j]<-as.difftime(ttime, units=c("hours")) #assign the transit time to the appropriate slot in the transit time dataframe
}
}

###################################################################################
####MERGE THE TRANSITTIMES CALCULATED WITH OTHER INFORMATION FROM THE SIMULATION###
###################################################################################

datsub<-simdat[ ,c(1,3,8,10,13, 11,12, 14:18, 21)] #select columns from sim data to join to transit time data
dat<-left_join(transittime, datsub, by = c("name"="who") )
dat<-dat[!is.na(dat$ttime),] #remove NAs for rows that don't operate within loc1 and loc2
dat<-unique(dat[,c(1:5,7:13)]) #remove duplicate records from join, keeps the last (or exit time)
# dat<-dat[dat$deathday!=0,]
#lefttime<-as.data.frame(simdat[ ,c(10,16)]) #identify datetimes associated with each tows birthday
#dat<-merge(dat, lefttime, by = "birthday", all.x=T)

#######################
####GENERATE OUTPUT####
#######################

# quantiles<-seq(0, 1, by= 0.05)
# 
uptt<-dat[dat$heading == 90,]
# upmean<-mean(uptt$ttime)
# upstd<-sd(uptt$ttime)
# uphist<-hist(uptt$ttime)
# upquants<-quantile(uptt$ttime, quantiles)
# plot(quantiles, upquants)


write.csv(uptt,paste0("uptt_",simname,"_",linkname,".csv"),row.names=FALSE)

downtt<-dat[dat$heading == 270,]
# downmean<-mean(downtt$ttime)
# downstd<-sd(downtt$ttime)
# downhist<-hist(downtt$ttime)
# downquants<-quantile(downtt$ttime, quantiles)
# plot(quantiles, downquants)

write.csv(downtt,paste0("dntt_",simname,"_",linkname,".csv"),row.names=FALSE)

####################################################################
#######Calculate and write total system time for each tow##########
##################################################################
st<-unique(simdat[,c(1,3,10:13,18)])
st<-st[order(st$who),]
st<-st[st$deathday!=0,]
st$systemtime<-(st$deathday-st$birthday)*inttime/60
st$systemtime<-as.difftime(st$systemtime, units = c("hours"))

write.csv(st,paste0("systime_",simname,".csv"),row.names=FALSE)

}