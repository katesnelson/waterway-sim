#Tow Tracks

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



setwd('C:/yoursimulationdatafilelocation')

####################################
####READ IN THE SIMULATION DATA####
###################################
file<-"simulationdatafilename.txt"
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
    max_pxcor<- 85 #max x coordinate in the model setup (check in 3D View settings)
    towtrack$rm<-(max_pxcor -(towtrack$xcor+ 10))
    towtrack$id<-as.character(towtrack$id)
    towtrack<-arrange(towtrack,datetime)
    names<-unique(towtrack$id)
    
    #Plot tracks for upstream, grouping by tow
    up<-towtrack[towtrack$heading==90 & towtrack$id %in% names[20:100], ]
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
    dwn<-towtrack[towtrack$heading==270 & towtrack$id %in% names[20:100], ]
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
############## Read in the AIS data and build towtracks###########
###########################################################################

 
  ais<-readRDS("youraistowtracksfile")
    
  #upstream first
  names<-as.data.frame(unique(ais$tow_id))
  colnames(names)<-c("name")
  
  aistrack<-ais[ais$tow_id==names$name[1],]
  aistrack$date<-as.Date(aistrack$PositionTi)
  aistrack$time<-format(aistrack$PositionTi,"%H:%M:%S")
  
  
  for (j in 2:length(names$name)){
    t<-ais[ais$tow_id==names$name[j],]
    t$date<-as.Date(t$PositionTi)
    t$time<-format(t$PositionTi,"%H:%M:%S")
    aistrack<-rbind(aistrack,t)
  }
  
  aistrack<-aistrack[order(aistrack$PositionTi,aistrack$tow_id),]
  names<-as.data.frame(unique(aistrack$tow_id))
  
  #Plot tracks for upstream, grouping by tow
  aisup<-aistrack[aistrack$heading=="Upstream" & aistrack$tow_id %in% names[0:100,1], ]
  aisup$tow_id<-as.character(aisup$tow_id)
  aisup<-arrange(aisup,PositionTi)
  aisup$time<-as.hms(aisup$time) #convert to hms class which plays nicely with ggplot
  
  p <- ggplot(aisup, aes(x = RiverMile))
  p <- p + geom_line(aes(y = time, colour = tow_id, group = tow_id, size=1)) + 
    theme(legend.position="none") + xlab("River Mile") + ylab("Hour of the Day") + scale_x_reverse()
  p
  
  p <- ggplot(aisup, aes(x = PositionTi))
  p <- p + geom_line(aes(y = RiverMile, colour = tow_id, group = tow_id)) + 
    theme(legend.position="none") + xlab("Date and Time") + ylab("River Mile")+ scale_y_reverse()
  p
  
  #Plot tracks for downstream, grouping by tow
  aisdwn<-aistrack[aistrack$heading=="Downstream" & aistrack$tow_id %in% names[0:100,1], ]
  aisdwn$tow_id<-as.character(aisdwn$tow_id)
  aisdwn<-arrange(aisdwn,desc(PositionTi)) #sort descending by time so plot lines connect properly
  aisdwn$time<-as.hms(dwn$time)
  
  p <- ggplot(aisdwn, aes(x = RiverMile))
  p <- p + geom_line(aes(y = time, colour = tow_id, group = tow_id, size=1)) + 
    theme(legend.position="none") + xlab("River Mile") + ylab("Hour of the Day")+ scale_x_reverse()
  p
  
  p <- ggplot(aisdwn, aes(x = PositionTi))
  p <- p + geom_line(aes(y = RiverMile, colour = tow_id, group = tow_id)) + 
    theme(legend.position="none") + xlab("Date and Time") + ylab("River Mile")+ scale_y_reverse()
  p
  

  
  ##########################################################
  ###Now combine AIS and SIM data and plot tracks together
  ##########################################################
  
  #build a table linking sim tows and their match in the ais tows
  sim<-towtrack[ ,c("heading", "datetime", "id", "date", "time", "rm")]
  sim$type<-"sim"
  ais<-aistrack[ ,c("heading", "PositionTi", "tow_id", "date", "time", "RiverMile")]
  ais$type<-"ais"
  ais[ais$heading=="Upstream","heading"]<-90
  ais[ais$heading=="Downstream","heading"]<-270
  colnames(ais)<-colnames(sim)
  
  full_trk<-rbind(sim, ais)
  
  write.csv(full_trk, file="fulltracksname.csv") #use this file in Tableau or similar software for making nice plots, or use figure code below
  
  
 
   #Plot joint ais and sim tracks for upstream, grouping by tow 
  cutoff<-as.Date("2013-01-07 07:00:19")
  up<-full_trk[full_trk$heading==90 & full_trk$date < cutoff & full_trk$rm <= 40, ] #jun 19-jun 22
  up$time<-as.hms(up$time) #convert to hms class which plays nicely with ggplot
  up<-arrange(up,datetime) #sort  by time so plot lines connect properly
  
  p <- ggplot(up)
  p <- p + geom_line(aes(y = time, x=rm,  colour= type, group = id), size=1) +  
    theme(legend.position="none") + xlab("River Mile") + ylab("Hour of the Day") + scale_x_reverse()
           # geom_line(aes(y = sim_time, x=sim_rm, colour = id, group = id), size=2) +
           
  p
  
  p <- ggplot(up)
  p <- p + geom_line(aes(y = rm, x=datetime, colour = type, group = id)) + 
    xlab("Date and Time") + ylab("River Mile")+ scale_y_reverse()
  #geom_line(aes(y = sim_rm, x=sim_dt, colour = id, group = id), size=1.5) +
    
  p
  
  #Plot joint ais and sim tracks for downstream, grouping by tow
  cutoff<-as.Date("2013-01-07 07:00:19")
  dwn<-full_trk[full_trk$heading==270 & full_trk$date < cutoff & full_trk$rm <= 40, ]
  dwn<-arrange(dwn,desc(datetime)) #sort descending by time so plot lines connect properly
  dwn$time<-as.hms(dwn$time) #convert to hms class which plays nicely with ggplot
  
  p <- ggplot(dwn)
  p <- p + geom_line(aes(y = time, x=rm, colour = type, group = id, size=1)) + 
    theme(legend.position="none") + xlab("River Mile") + ylab("Hour of the Day") + scale_x_reverse()
  #geom_line(aes(y = sim_time, x=sim_rm, colour = id, group = id), size=2) +
  p
  
  p <- ggplot(dwn)
  p <- p + geom_line(aes(y = rm, x=datetime, colour = type, group = id)) + 
     xlab("Date and Time") + ylab("River Mile")+ scale_y_reverse()
  #geom_line(aes(y = sim_rm, x=sim_dt, colour = id, group = id), size=1.5) +
  p
  
  #######################################################
  #Plot joint ais and sim tracks for upstream, grouping by tow 
  cutoff<-as.Date("2013-01-07 07:00:19")
  cutoff2<-as.Date("2013-01-20 07:00:19")
  up<-full_trk[full_trk$heading==90 & full_trk$date > cutoff & full_trk$date<cutoff2 & full_trk$rm <= 40, ] #jun 19-jun 22
  up$time<-as.hms(up$time) #convert to hms class which plays nicely with ggplot
  up<-arrange(up,datetime) #sort  by time so plot lines connect properly
  
  p <- ggplot(up)
  p <- p + geom_line(aes(y = time, x=rm,  colour= type, group = id), size=1) +  
    theme(legend.position="none") + xlab("River Mile") + ylab("Hour of the Day") + scale_x_reverse()
  # geom_line(aes(y = sim_time, x=sim_rm, colour = id, group = id), size=2) +
  
  p
  
  p <- ggplot(up)
  p <- p + geom_line(aes(y = rm, x=datetime, colour = type, group = id)) + 
    xlab("Date and Time") + ylab("River Mile")+ scale_y_reverse()
  #geom_line(aes(y = sim_rm, x=sim_dt, colour = id, group = id), size=1.5) +
  
  p
  
  #Plot joint ais and sim tracks for downstream, grouping by tow
  dwn<-full_trk[full_trk$heading==270 & full_trk$date > cutoff & full_trk$date<cutoff2 & full_trk$rm <= 40, ]
  dwn<-arrange(dwn,desc(datetime)) #sort descending by time so plot lines connect properly
  dwn$time<-as.hms(dwn$time) #convert to hms class which plays nicely with ggplot
  
  p <- ggplot(dwn)
  p <- p + geom_line(aes(y = time, x=rm, colour = type, group = id, size=1)) + 
    theme(legend.position="none") + xlab("River Mile") + ylab("Hour of the Day") + scale_x_reverse()
  #geom_line(aes(y = sim_time, x=sim_rm, colour = id, group = id), size=2) +
  p
  
  p <- ggplot(dwn)
  p <- p + geom_line(aes(y = rm, x=datetime, colour = type, group = id)) + 
    theme(legend.position="none") + xlab("Date and Time") + ylab("River Mile")+ scale_y_reverse()
  #geom_line(aes(y = sim_rm, x=sim_dt, colour = id, group = id), size=1.5) +
  p
  
  