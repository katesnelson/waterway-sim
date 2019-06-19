#Simulation Metrics
#!!!IMPORTANT: Before you start check file names (line 16) and interval time (line 74) AND output file name (line 220) and all dates!!!#

library (dplyr)
library (tidyr)
library(splitstackshape)
library (stringr)


setwd('C:yoursimdatafilelocation')

####################################
####READ IN THE SIMULATION DATA####
###################################

files<- c("2013_c_1.txt","2013_c_2.txt","2013_c_3.txt","2013_c_4.txt","2013_c_5.txt",
          "2013_200_1.txt","2013_200_2.txt","2013_200_3.txt","2013_200_4.txt","2013_200_5.txt",
          "2013_150_1.txt","2013_150_2.txt","2013_150_3.txt","2013_150_4.txt","2013_150_5.txt",
          "2013_t10_1.txt","2013_t10_2.txt","2013_t10_3.txt","2013_t10_4.txt","2013_t10_5.txt",
          "2013_cas_1.txt","2013_cas_2.txt","2013_cas_3.txt","2013_cas_4.txt","2013_cas_5.txt",
          "2013_dock_1.txt","2013_dock_2.txt","2013_dock_3.txt","2013_dock_4.txt","2013_dock_5.txt",
          "2013_divert_1.txt","2013_divert_2.txt","2013_divert_3.txt","2013_divert_4.txt","2013_divert_5.txt",
          "2013_wfh_1.txt","2013_wfh_2.txt","2013_wfh_3.txt","2013_wfh_4.txt","2013_wfh_5.txt"
          ) #list of data file names

n<-length(files)
output<-data.frame(Scenario=as.character(seq(1,n)),Tows=as.numeric(seq(1,n)), Tows_Up=as.numeric(seq(1,n)),Tows_Down=as.numeric(seq(1,n)),
                   Barges=as.numeric(seq(1,n)),Barges_up=as.numeric(seq(1,n)),Barges_down=as.numeric(seq(1,n)), 
                   Transit_Time=as.numeric(seq(1,n)), Transit_Time_Up=as.numeric(seq(1,n)),Transit_Time_Down=as.numeric(seq(1,n)), 
                   Speed=as.numeric(seq(1,n)),Speed_up=as.numeric(seq(1,n)),Speed_down=as.numeric(seq(1,n)),
                   Delay_time =as.numeric(seq(1,n)),Delay_time_up=as.numeric(seq(1,n)),Delay_time_down=as.numeric(seq(1,n)),
                   Barges_delivered = as.numeric(seq(1,n)), Barge_delivery_rate =as.numeric(seq(1,n)),
                   Tows_Prior=as.numeric(seq(1,n)),Barges_Prior=as.numeric(seq(1,n)),Transit_Time_Prior=as.numeric(seq(1,n)),
                   Speed_Prior=as.numeric(seq(1,n)),Delay_time_Prior=as.numeric(seq(1,n)),Barges_delivered_prior = as.numeric(seq(1,n)), Barge_delivery_rate_prior=as.numeric(seq(1,n)),
                   Tows_CasualtyWeek=as.numeric(seq(1,n)),Barges_CasualtyWeek=as.numeric(seq(1,n)),Transit_Time_CasualtyWeek=as.numeric(seq(1,n)),
                   Speed_CasualtyWeek=as.numeric(seq(1,n)),Delay_time_CasualtyWeek=as.numeric(seq(1,n)),Barges_delivered_CasualtyWeek = as.numeric(seq(1,n)),Barge_delivery_rate_CasualtyWeek=as.numeric(seq(1,n)),
                   Tows_PostCasualtyWeek=as.numeric(seq(1,n)),Barges_PostCasualtyWeek=as.numeric(seq(1,n)),Transit_Time_PostCasualtyWeek=as.numeric(seq(1,n)),
                   Speed_PostCasualtyWeek=as.numeric(seq(1,n)),Delay_time_PostCasualtyWeek=as.numeric(seq(1,n)),Barges_delivered_PostCasualtyWeek = as.numeric(seq(1,n)), Barge_delivery_rate_PostCasualtyWeek=as.numeric(seq(1,n)),
                   Tows_PostCasualtyWeek2=as.numeric(seq(1,n)),Barges_PostCasualtyWeek2=as.numeric(seq(1,n)),Transit_Time_PostCasualtyWeek2=as.numeric(seq(1,n)),
                   Speed_PostCasualtyWeek2=as.numeric(seq(1,n)),Delay_time_PostCasualtyWeek2=as.numeric(seq(1,n)),Barges_delivered_PostCasualtyWeek2 = as.numeric(seq(1,n)), Barge_delivery_rate_PostCasualtyWeek2=as.numeric(seq(1,n)),
                   stringsAsFactors = F)

for (i in 1:n)
{

  file<-files[i]
  simname<-file
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
  
  ##########################################
  #######Calculate Full sim Metrics##########
  #########################################
  st<-unique(simdat[,c(1,3,4,6,7,8,10:13,18,21)])
  st<-st %>% group_by(who) %>% mutate(firsttime = min(datetime), lasttime=max(datetime)) %>% mutate(systemtime=difftime(lasttime,firsttime, units="hours"))
  st$id<-as.numeric(as.character(st$id))
  st<-as.data.frame(st)
  
    #aggregate metrics
    tows<-length(unique(st$who))
    barges<-sum(st[!duplicated(st$id),"connected.barges"], na.rm=T)
    transittime<-as.numeric(mean(st$systemtime, na.rm=T))
    speed<-mean(st$speed, na.rm=T)
    Delaytime<-length(st[st$status %in% "Lock-delay" | st$status %in% "Casualty-delay",1])*inttime/60
    barges_delivered<-max(st$barges.delivered, na.rm=T)
    barges_delivery_rate<-barges_delivered/as.numeric(sum(st[!duplicated(st$id), "systemtime"], na.rm=T))
    
    #upstream metrics
    up_st<-st[st$heading == 90,]
    towsup<-length(unique(up_st$who))
    bargesup<-sum(up_st[!duplicated(up_st$id),"connected.barges"], na.rm=T)
    transittimeup<-as.numeric(mean(up_st$systemtime, na.rm=T))
    speedup<-mean(up_st$speed, na.rm=T)
    Delaytimeup<-length(up_st[up_st$status %in% "Lock-delay" | up_st$status %in% "Casualty-delay",1])*inttime/60
    
    
    #downstream metrics
    dwn_st<-st[st$heading == 270,]
    towsdwn<-length(unique(dwn_st$who))
    bargesdwn<-sum(dwn_st[!duplicated(dwn_st$id),"connected.barges"], na.rm=T)
    transittimedwn<-as.numeric(mean(dwn_st$systemtime, na.rm=T))
    speeddwn<-mean(dwn_st$speed, na.rm=T)
    Delaytimedwn<-length(dwn_st[dwn_st$status %in% "Lock-delay" | dwn_st$status %in% "Casualty-delay",1])*inttime/60
    
  
  ####################################
  #Calucalte Metrics by Time Group
  #################################
    
  stprior<-st[st$datetime < "2013-01-08 08:30:00", ] #baseline period before casualty
    
    #aggregate metrics
    towsprior<-length(unique(stprior$who))
    bargesprior<-sum(stprior[!duplicated(stprior$id),"connected.barges"], na.rm=T)
    transittimeprior<-as.numeric(mean(stprior$systemtime, na.rm=T))
    speedprior<-mean(stprior$speed, na.rm=T)
    Delaytimeprior<-length(stprior[stprior$status %in% "Lock-delay" | stprior$status %in% "Casualty-delay",1])*inttime/60
    barges_deliveredprior<-max(stprior$barges.delivered, na.rm=T)
    barges_delivery_rateprior<-barges_deliveredprior/as.numeric(sum(stprior[!duplicated(stprior$id), "systemtime"], na.rm=T))
    
  stcaswk<-st[st$datetime >= "2013-01-08 08:30:00" & st$datetime < "2013-01-16 08:30:00", ] 
    
    #aggregate metrics
    towscaswk<-length(unique(stcaswk$who))
    bargescaswk<-sum(stcaswk[!duplicated(stcaswk$id),"connected.barges"], na.rm=T)
    transittimecaswk<-as.numeric(mean(stcaswk$systemtime, na.rm=T))
    speedcaswk<-mean(stcaswk$speed, na.rm=T)
    Delaytimecaswk<-length(stcaswk[stcaswk$status %in% "Lock-delay" | stcaswk$status %in% "Casualty-delay",1])*inttime/60
    barges_deliveredcaswk<-max(stcaswk$barges.delivered, na.rm=T)-barges_deliveredprior
    barges_delivery_ratecaswk<-barges_deliveredcaswk/as.numeric(sum(stcaswk[!duplicated(stcaswk$id), "systemtime"], na.rm=T))
 
  stpostwk<-st[st$datetime >= "2013-01-16 08:30:00" & st$datetime < "2013-01-23 08:30:00", ] 
    
    #aggregate metrics
    towspostwk<-length(unique(stpostwk$who))
    bargespostwk<-sum(stpostwk[!duplicated(stpostwk$id),"connected.barges"], na.rm=T)
    transittimepostwk<-as.numeric(mean(stpostwk$systemtime, na.rm=T))
    speedpostwk<-mean(stpostwk$speed, na.rm=T)
    Delaytimepostwk<-length(stpostwk[stpostwk$status %in% "Lock-delay" | stpostwk$status %in% "Casualty-delay",1])*inttime/60
    barges_deliveredpostwk<-max(stpostwk$barges.delivered, na.rm=T)-max(stcaswk$barges.delivered, na.rm=T)
    barges_delivery_ratepostwk<-barges_deliveredpostwk/as.numeric(sum(stpostwk[!duplicated(stpostwk$id), "systemtime"], na.rm=T))  
    
  stpostwk2<-st[st$datetime >= "2013-01-23 08:30:00" & st$datetime < "2013-01-30 08:30:00", ] 
    
    #aggregate metrics
    towspostwk2<-length(unique(stpostwk2$who))
    bargespostwk2<-sum(stpostwk2[!duplicated(stpostwk2$id),"connected.barges"], na.rm=T)
    transittimepostwk2<-as.numeric(mean(stpostwk2$systemtime, na.rm=T), na.rm=T)
    speedpostwk2<-mean(stpostwk2$speed, na.rm=T)
    Delaytimepostwk2<-length(stpostwk2[stpostwk2$status %in% "Lock-delay" | stpostwk2$status %in% "Casualty-delay",1])*inttime/60
    barges_deliveredpostwk2<-max(stpostwk2$barges.delivered, na.rm=T)-max(stpostwk$barges.delivered, na.rm=T)
    barges_delivery_ratepostwk2<-barges_deliveredpostwk2/as.numeric(sum(stpostwk2[!duplicated(stpostwk2$id), "systemtime"], na.rm=T))  
    
   #################################
  #Build a Table
  ###############################
  
  #fill in the ouput table
  output$Scenario[i]<-simname
  output$Tows[i]<-tows
  output$Tows_Up[i]<-towsup
  output$Tows_Down[i]<-towsdwn
  output$Barges[i]<-barges
  output$Barges_up[i]<-bargesup
  output$Barges_down[i]<-bargesdwn
  output$Transit_Time[i]<-transittime
  output$Transit_Time_Up[i]<-transittimeup
  output$Transit_Time_Down[i]<-transittimedwn
  output$Speed[i]<-speed
  output$Speed_up[i]<-speedup
  output$Speed_down[i]<-speeddwn
  output$Delay_time[i]<-Delaytime
  output$Delay_time_up[i]<-Delaytimeup
  output$Delay_time_down[i]<-Delaytimedwn
  output$Barges_delivered[i]<-barges_delivered
  output$Barge_delivery_rate[i]<-barges_delivery_rate
  
  
  output$Tows_Prior[i]<-towsprior
  output$Barges_Prior[i]<-bargesprior
  output$Transit_Time_Prior[i]<-transittimeprior
  output$Speed_Prior[i]<-speedprior
  output$Delay_time_Prior[i]<-Delaytimeprior
  output$Barges_delivered_prior[i]<-barges_deliveredprior 
  output$Barge_delivery_rate_prior[i]<-barges_delivery_rateprior
  
  output$Tows_CasualtyWeek[i]<-towscaswk
  output$Barges_CasualtyWeek[i]<-bargescaswk
  output$Transit_Time_CasualtyWeek[i]<-transittimecaswk
  output$Speed_CasualtyWeek[i]<-speedcaswk
  output$Delay_time_CasualtyWeek[i]<-Delaytimecaswk
  output$Barges_delivered_CasualtyWeek[i]<-barges_deliveredcaswk
  output$Barge_delivery_rate_CasualtyWeek[i]<-barges_delivery_ratecaswk
  
  output$Tows_PostCasualtyWeek[i]<-towspostwk
  output$Barges_PostCasualtyWeek[i]<-bargespostwk
  output$Transit_Time_PostCasualtyWeek[i]<-transittimepostwk
  output$Speed_PostCasualtyWeek[i]<-speedpostwk
  output$Delay_time_PostCasualtyWeek[i]<-Delaytimepostwk
  output$Barges_delivered_PostCasualtyWeek[i]<-barges_deliveredpostwk
  output$Barge_delivery_rate_PostCasualtyWeek[i]<-barges_delivery_ratepostwk
  
  output$Tows_PostCasualtyWeek2[i]<-towspostwk2
  output$Barges_PostCasualtyWeek2[i]<-bargespostwk2
  output$Transit_Time_PostCasualtyWeek2[i]<-transittimepostwk2
  output$Speed_PostCasualtyWeek2[i]<-speedpostwk2
  output$Delay_time_PostCasualtyWeek2[i]<-Delaytimepostwk2
  output$Barges_delivered_PostCasualtyWeek2[i]<-barges_deliveredpostwk2
  output$Barge_delivery_rate_PostCasualtyWeek2[i]<-barges_delivery_ratepostwk2
  
  
}

output<-t(output)
write.csv(output,"yourmetricsfilename.csv")

