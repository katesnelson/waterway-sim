#4

#!!!IMPORTANT: Before running check the working directory, simulation datafile name (line 24), interval time (line 52), and ais datafiles (lines 115, 116) !!!#

library (dplyr)
library (tidyr)
library(splitstackshape)
library (stringr)
library(stats)
library(ggplot2)
library(bayesplot)
library(hms)
library(lubridate)
library(anytime)


#setwd('C:/Users/nelsonks/Dropbox/Kate_Paul/paul_simulations/2013_AIS_runs_02')
#setwd('C:/Users/nelsonks/Dropbox/Kate_Paul/paul_simulations/2013_AIS_30min_jan_jun')
#setwd('C:/Users/nelsonks/Dropbox/Kate_Paul/paul_simulations/2013_AIS_30min_may_oct')
#setwd('C:/Users/nelsonks/Dropbox/Kate_Paul/paul_simulations/2013_AIS_30min_jun_aug')
#setwd('C:/Users/nelsonks/Dropbox/Kate_Paul/paul_simulations/2013_AIS_15min_jan_jun')
#setwd('C:/Users/nelsonks/Dropbox/Kate_Paul/paul_simulations/2013_AIS_15min_may_oct')
setwd('C:/Users/nelsonks/Dropbox/Kate_Paul/paul_simulations/2013_AIS_15min_jun_aug/')

####################################
####READ IN THE SIMULATION DATA####
###################################
file<-"012018_09_15m_junaug_xx.txt"
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

    ##################################################
    ####Extract and Plot the Simulated Tow Tracks####
    #################################################
    
    tows<-as.data.frame(unique(simdat$who))
    colnames(tows)<-c("name")
    
    towtrack<-simdat[simdat$who==tows$name[1],c("who","heading","xcor","datetime", "id")]
    towtrack$date<-as.Date(towtrack$datetime)
    towtrack$time<-format(towtrack$datetime,"%H:%M:%S")
    
    for (j in 2:length(tows$name)){
      t<-simdat[simdat$who==tows$name[j],c("who","heading","xcor","datetime","id")]
      t$date<-as.Date(t$datetime)
      t$time<-format(t$datetime,"%H:%M:%S")
      towtrack<-rbind(towtrack,t)
    }
    
    #convert model locations to Ohio River Miles
    max_pxcor<- 75 #max x coordinate in the model setup (check in 3D View settings)
    towtrack$rm<-(max_pxcor -(towtrack$xcor+ 10))
    towtrack$id<-as.character(towtrack$id)
    towtrack<-arrange(towtrack,datetime)
    names<-unique(towtrack$id)
    
    #Plot tracks for upstream, grouping by tow
    up<-towtrack[towtrack$heading==90 & towtrack$id %in% names[1:30], ]
    up$time<-as.hms(up$time) #convert to hms class which plays nicely with ggplot
    
    p <- ggplot(up, aes(x = rm))
    p <- p + geom_line(aes(y = time, colour = id, group = id, size=1)) + 
      theme(legend.position="none") + xlab("River Mile") + ylab("Hour of the Day") + scale_x_reverse()
    p
    
    p <- ggplot(up, aes(x = datetime))
    p <- p + geom_line(aes(y = rm, colour = id, group = id)) + 
      theme(legend.position="none") + xlab("Date and Time") + ylab("River Mile")+ scale_y_reverse()
    p
    
    #Plot tracks for downstream, grouping by tow
    dwn<-towtrack[towtrack$heading==270 & towtrack$id %in% names[1:30], ]
    dwn<-arrange(dwn,desc(datetime)) #sort descending by time so plot lines connect properly
    dwn$time<-as.hms(dwn$time)
    
    p <- ggplot(dwn, aes(x = rm))
    p <- p + geom_line(aes(y = time, colour = id, group = id, size=1)) + 
      theme(legend.position="none") + xlab("River Mile") + ylab("Hour of the Day")+ scale_x_reverse()
    p
    
    p <- ggplot(dwn, aes(x = datetime))
    p <- p + geom_line(aes(y = rm, colour = id, group = id)) + 
      theme(legend.position="none") + xlab("Date and Time") + ylab("River Mile")+ scale_y_reverse()
    p
    
###########################################################################
############## Read in the AIS data build towtracks###########
###########################################################################

  #read in the associated ais data and combine the upstream and downstream files
  aisup<-read.csv("C:/Users/nelsonks/Dropbox/Kate_Paul/models/upstreaming2.csv", header=T, na.strings= "")
  aisdown<-read.csv("C:/Users/nelsonks/Dropbox/Kate_Paul/models/downstreaming2.csv", header=T, na.strings= "")
  aisup$heading<-90
  aisdown$heading<-270
  
  #upstream first
  names<-unique(aisup$name)
  aisup$link1tt[aisup$link1tt==0]<-NA
  aisup$link2tt[aisup$link2tt==0]<-NA
  aisup$link3tt[aisup$link3tt==0]<-NA
  aisup$link4tt[aisup$link4tt==0]<-NA
  
  ais_up_trk<-data.frame()
  l<-length(names)
  
    for (i in 1:l){
      record<-as.data.frame(aisup[i,])#extract the info for a single tow
      record$arrival_orig<-anytime(record$arrival_orig)
      record$departure<-anytime(record$departure)
      
      track<-data.frame(record$heading,record$name,record$arrival_orig) #build a dataframe to hold the track for a single tow, will ultimately have up to 5 records
      colnames(track)<-c("heading","id","datetime")
      track$rm[1]<-record$origin
      m<-as.data.frame(matrix(nrow=4,ncol=4)) #max number of obs
      colnames(m)<-c("heading","id","datetime","rm")
      track<-rbind(track,m)
      
      track$heading<-track$heading[1]
      track$id<-track$id[1]
      
      track$datetime<- as.POSIXct(track$datetime, format="%Y-%m-%d %H:%M:%S")
      record$departure <-as.POSIXct(record$departure, format="%Y-%m-%d %H:%M:%S")
      record$arrival_orig<-as.POSIXct(record$arrival_orig, format="%Y-%m-%d %H:%M:%S")
      
      if (record$origin == 10){
        track$rm[2]<-record$destination
        track$datetime[2]<-record$departure
      }else{
        if (record$origin == 20){
          track$rm[2]<-10
          track$datetime[2]<-record$arrival_orig + minutes(as.numeric(record$link2tt))
          track$rm[3]<-0
          track$datetime[3]<-record$arrival_orig + minutes(as.numeric(record$link2tt)) + minutes(as.numeric(record$link1tt))
        }else{
          if (record$origin == 27){
            track$rm[2]<-20
            track$datetime[2]<-record$arrival_orig + minutes(as.numeric(record$link3tt))
            track$rm[3]<-10
            track$datetime[3]<-record$arrival_orig + minutes(as.numeric(record$link3tt)) + minutes(as.numeric(record$link2tt))
            track$rm[4]<-0
            track$datetime[4]<-record$arrival_orig + minutes(as.numeric(record$link3tt)) + minutes(as.numeric(record$link2tt)) + minutes(as.numeric(record$link1tt))
          }else{
            if (record$origin == 37){
              track$rm[2]<-27
              track$datetime[2]<-record$arrival_orig + minutes(as.numeric(record$link4tt))
              track$rm[3]<-20
              track$datetime[3]<-record$arrival_orig + minutes(as.numeric(record$link4tt)) + minutes(as.numeric(record$link3tt))
              track$rm[4]<-10
              track$datetime[4]<-record$arrival_orig + minutes(as.numeric(record$link4tt)) + minutes(as.numeric(record$link3tt)) + minutes(as.numeric(record$link2tt))
              track$rm[5]<-0
              track$datetime[5]<-record$arrival_orig + minutes(as.numeric(record$link4tt)) + minutes(as.numeric(record$link3tt)) + minutes(as.numeric(record$link2tt)) + minutes(as.numeric(record$link1tt))
            }
          }
        }
      }
      ais_up_trk<-rbind(track,ais_up_trk)
    }
  
  ais_up_trk$time<-format(ais_up_trk$datetime,"%H:%M:%S")
  
  #now downstream
  names<-unique(aisdown$name)
  aisdown$link1tt[aisdown$link1tt==0]<-NA
  aisdown$link2tt[aisdown$link2tt==0]<-NA
  aisdown$link3tt[aisdown$link3tt==0]<-NA
  aisdown$link4tt[aisdown$link4tt==0]<-NA
  
  ais_dwn_trk<-data.frame()
  l<-length(names)
  i=1 #temp while testing script
  
  for (i in 1:l){
    record<-as.data.frame(aisdown[i,])#extract the info for a single tow
    record$arrival_orig<-anytime(record$arrival_orig)
    record$departure<-anytime(record$departure)
    
    track<-data.frame(record$heading,record$name,record$arrival_orig) #build a dataframe to hold the track for a single tow, will ultimately have up to 5 records
    colnames(track)<-c("heading","id","datetime")
    track$rm[1]<-record$origin
    m<-as.data.frame(matrix(nrow=4,ncol=4)) #max number of obs
    colnames(m)<-c("heading","id","datetime","rm")
    track<-rbind(track,m)
    
    track$heading<-track$heading[1]
    track$id<-track$id[1]
    
    track$datetime<- as.POSIXct(track$datetime, format="%Y-%m-%d %H:%M:%S")
    record$departure <-as.POSIXct(record$departure, format="%Y-%m-%d %H:%M:%S")
    record$arrival_orig<-as.POSIXct(record$arrival_orig, format="%Y-%m-%d %H:%M:%S")
    
    if (record$origin == 27){
      track$rm[2]<-record$destination
      track$datetime[2]<-record$departure
    }else{
      if (record$origin == 20){
        track$rm[2]<-27
        track$datetime[2]<-record$arrival_orig + minutes(as.numeric(record$link3tt))
        track$rm[3]<-37
        track$datetime[3]<-record$arrival_orig + minutes(as.numeric(record$link3tt)) + minutes(as.numeric(record$link4tt))
      }else{
        if (record$origin == 10){
          track$rm[2]<-20
          track$datetime[2]<-record$arrival_orig + minutes(as.numeric(record$link2tt))
          track$rm[3]<-27
          track$datetime[3]<-record$arrival_orig + minutes(as.numeric(record$link2tt)) + minutes(as.numeric(record$link3tt))
          track$rm[4]<-37
          track$datetime[4]<-record$arrival_orig + minutes(as.numeric(record$link2tt)) + minutes(as.numeric(record$link3tt)) + minutes(as.numeric(record$link4tt))
        }else{
          if (record$origin == 0){
            track$rm[2]<-10
            track$datetime[2]<-record$arrival_orig + minutes(as.numeric(record$link1tt))
            track$rm[3]<-20
            track$datetime[3]<-record$arrival_orig + minutes(as.numeric(record$link1tt)) + minutes(as.numeric(record$link2tt))
            track$rm[4]<-27
            track$datetime[4]<-record$arrival_orig + minutes(as.numeric(record$link1tt)) + minutes(as.numeric(record$link2tt)) + minutes(as.numeric(record$link3tt))
            track$rm[5]<-37
            track$datetime[5]<-record$arrival_orig + minutes(as.numeric(record$link1tt)) + minutes(as.numeric(record$link2tt)) + minutes(as.numeric(record$link3tt)) + minutes(as.numeric(record$link4tt))
          }
        }
      }
    }
    ais_dwn_trk<-rbind(track,ais_dwn_trk)
  } 
  
ais_dwn_trk$time<-format(ais_dwn_trk$datetime,"%H:%M:%S")
  
  aistrack<-rbind(ais_up_trk,ais_dwn_trk)
  #################################################
  #### Plot the AIS Tow Tracks####
  #################################################
  
  #clean data
  aistrack<-aistrack[!is.na(aistrack$datetime),] #remove null rows
  aistrack<-arrange(aistrack,datetime) #sort by date
  aistrack$id<-as.character(aistrack$id)
  names<-unique(aistrack$id)
  
  #Plot tracks for upstream, grouping by tow
  up<-aistrack[aistrack$heading==90 & aistrack$id %in% names[1:30], ] #change the names indexing to select different tows and associated time periods
  up$time<-as.hms(up$time) #convert to hms class which plays nicely with ggplot
  
  p <- ggplot(up, aes(x = rm))
  p <- p + geom_line(aes(y = time, colour = id, group = id, size=1)) + 
    theme(legend.position="none") + xlab("River Mile") + ylab("Hour of the Day") + scale_x_reverse()
  p
  
  p <- ggplot(up, aes(x = datetime))
  p <- p + geom_line(aes(y = rm, colour = id, group = id)) + 
    theme(legend.position="none") + xlab("Date and Time") + ylab("River Mile")+ scale_y_reverse()
  p
  
  #Plot tracks for downstream, grouping by tow
  dwn<-aistrack[aistrack$heading==270 & aistrack$id %in% names[1:30], ]
  dwn<-arrange(dwn,desc(datetime)) #sort descending by time so plot lines connect properly
  dwn$time<-as.hms(dwn$time)
  
  p <- ggplot(dwn, aes(x = rm))
  p <- p + geom_line(aes(y = time, colour = id, group = id, size=1)) + 
    theme(legend.position="none") + xlab("River Mile") + ylab("Hour of the Day")+ scale_x_reverse()
  p
  
  
  p <- ggplot(dwn, aes(x = datetime))
  p <- p + geom_line(aes(y = rm, colour = id, group = id)) + 
    theme(legend.position="none") + xlab("Date and Time") + ylab("River Mile")+ scale_y_reverse()
  p
  
  
  ##########################################################
  ###Now combine AIS and SIM data and plot tracks together
  ##########################################################
  
  #build a table linking sim tows and their match in the ais tows
  full_trk<-left_join(aistrack, towtrack, by = c("id"))
  colnames(full_trk)<-c("ais_head","id","ais_dt","ais_rm","ais_time","who","sim_head","xcor","sim_dt","sim_date","sim_time","sim_rm")
  full_trk<-full_trk[!is.na(full_trk$who),]
  names<-unique(full_trk$id)
  
  #Plot joint ais and sim tracks for upstream, grouping by tow [500:520]
  up<-full_trk[full_trk$ais_head==90 & full_trk$id %in% names[360:380], ] #jun 19-jun 22
  up$time<-as.hms(up$time) #convert to hms class which plays nicely with ggplot
  
  p <- ggplot(up)
  p <- p + geom_line(aes(y = ais_time, x=ais_rm, colour = id, group = id), size=1) + 
            geom_line(aes(y = sim_time, x=sim_rm, colour = id, group = id), size=2) +
            theme(legend.position="none") + xlab("River Mile") + ylab("Hour of the Day") + scale_x_reverse()
  p
  
  p <- ggplot(up)
  p <- p + geom_line(aes(y = ais_rm, x=ais_dt, colour = id, group = id)) + 
    geom_line(aes(y = sim_rm, x=sim_dt, colour = id, group = id), size=1.5) +
    theme(legend.position="none") + xlab("Date and Time") + ylab("River Mile")+ scale_y_reverse()
  p
  
  #Plot joint ais and sim tracks for downstream, grouping by tow
  dwn<-full_trk[full_trk$ais_head==270 & full_trk$id %in% names[330:350], ]
  dwn<-arrange(dwn,desc(datetime)) #sort descending by time so plot lines connect properly
  dwn$time<-as.hms(dwn$time) #convert to hms class which plays nicely with ggplot
  
  p <- ggplot(dwn)
  p <- p + geom_line(aes(y = ais_time, x=ais_rm, colour = id, group = id, size=1)) + 
    geom_line(aes(y = sim_time, x=sim_rm, colour = id, group = id), size=2) +
    theme(legend.position="none") + xlab("River Mile") + ylab("Hour of the Day") + scale_x_reverse()
  p
  
  p <- ggplot(dwn)
  p <- p + geom_line(aes(y = ais_rm, x=ais_dt, colour = id, group = id)) + 
    geom_line(aes(y = sim_rm, x=sim_dt, colour = id, group = id), size=1.5) +
    theme(legend.position="none") + xlab("Date and Time") + ylab("River Mile")+ scale_y_reverse()
  p
  
