#2

#!!!IMPORTANT: Before running check the list of transit time data (lines 23-75) and systime data (lines 235-248) 
#and the version of AIS data read in (lines 100-103 & 2696-267)!!!#

library (dplyr)
library (tidyr)
library(splitstackshape)
library (stringr)
library(stats)
library(ggplot2)
library(bayesplot)


#setwd('C:/Users/nelsonks/Dropbox/Kate_Paul/paul_simulations/2013_AIS_runs_02')
#setwd('C:/Users/nelsonks/Dropbox/Kate_Paul/paul_simulations/2013_AIS_15min_jan_jun')
#setwd('C:/Users/nelsonks/Dropbox/Kate_Paul/paul_simulations/2013_AIS_15min_jun_aug/')
setwd('C:/Users/nelsonks/Dropbox/Kate_Paul/paul_simulations/2013_AIS_15min_may_oct')
#setwd('C:/Users/nelsonks/Dropbox/Kate_Paul/paul_simulations/2013_AIS_30min_jan_jun')
#setwd('C:/Users/nelsonks/Dropbox/Kate_Paul/paul_simulations/2013_AIS_30min_may_oct')
#setwd('C:/Users/nelsonks/Dropbox/Kate_Paul/paul_simulations/2013_AIS_30min_jun_aug')

################################################
####READ IN THE SIMULATION TRANSIT TIME DATA####
################################################

#enter the list of files you want to analyze, order as upstream followed by downstream for a specific link
#files should use the naming convention used in the following examples:"uptt_02012017a_link2.csv","dntt_02012017a_link2.csv"
files<- c(
          "uptt_092017_01_link1.csv","dntt_092017_01_link1.csv",
          "uptt_092017_01_link2.csv","dntt_092017_01_link2.csv",
          "uptt_092017_01_link3.csv","dntt_092017_01_link3.csv",
          "uptt_092017_01_link4.csv","dntt_092017_01_link4.csv",
          "uptt_092017_02_link1.csv","dntt_092017_02_link1.csv",
          "uptt_092017_02_link2.csv","dntt_092017_02_link2.csv",
          "uptt_092017_02_link3.csv","dntt_092017_02_link3.csv",
          "uptt_092017_02_link4.csv","dntt_092017_02_link4.csv",
          "uptt_092017_03_link1.csv","dntt_092017_03_link1.csv",
          "uptt_092017_03_link2.csv","dntt_092017_03_link2.csv",
          "uptt_092017_03_link3.csv","dntt_092017_03_link3.csv",
          "uptt_092017_03_link4.csv","dntt_092017_03_link4.csv",
          "uptt_092017_04_link1.csv","dntt_092017_04_link1.csv",
          "uptt_092017_04_link2.csv","dntt_092017_04_link2.csv",
          "uptt_092017_04_link3.csv","dntt_092017_04_link3.csv",
          "uptt_092017_04_link4.csv","dntt_092017_04_link4.csv",
          "uptt_092017_05_link1.csv","dntt_092017_05_link1.csv",
          "uptt_092017_05_link2.csv","dntt_092017_05_link2.csv",
          "uptt_092017_05_link3.csv","dntt_092017_05_link3.csv",
          "uptt_092017_05_link4.csv","dntt_092017_05_link4.csv",
          "uptt_092017_06_link1.csv","dntt_092017_06_link1.csv",
          "uptt_092017_06_link2.csv","dntt_092017_06_link2.csv",
          "uptt_092017_06_link3.csv","dntt_092017_06_link3.csv",
          "uptt_092017_06_link4.csv","dntt_092017_06_link4.csv",
          "uptt_092017_07_link1.csv","dntt_092017_07_link1.csv",
          "uptt_092017_07_link2.csv","dntt_092017_07_link2.csv",
          "uptt_092017_07_link3.csv","dntt_092017_07_link3.csv",
          "uptt_092017_07_link4.csv","dntt_092017_07_link4.csv",
          "uptt_092017_08_link1.csv","dntt_092017_08_link1.csv",
          "uptt_092017_08_link2.csv","dntt_092017_08_link2.csv",
          "uptt_092017_08_link3.csv","dntt_092017_08_link3.csv",
          "uptt_092017_08_link4.csv","dntt_092017_08_link4.csv",
          "uptt_092017_09_link1.csv","dntt_092017_09_link1.csv",
          "uptt_092017_09_link2.csv","dntt_092017_09_link2.csv",
          "uptt_092017_09_link3.csv","dntt_092017_09_link3.csv",
         # "uptt_092017_09_link4.csv","dntt_092017_09_link4.csv",
          "uptt_092017_10_link1.csv","dntt_092017_10_link1.csv",
          "uptt_092017_10_link2.csv","dntt_092017_10_link2.csv",
          "uptt_092017_10_link3.csv","dntt_092017_10_link3.csv",
          "uptt_092017_10_link4.csv","dntt_092017_10_link4.csv",
          "uptt_092017_11_link1.csv","dntt_092017_11_link1.csv",
          "uptt_092017_11_link2.csv","dntt_092017_11_link2.csv",
          "uptt_092017_11_link3.csv","dntt_092017_11_link3.csv",
          "uptt_092017_11_link4.csv","dntt_092017_11_link4.csv",
          "uptt_092017_12_link1.csv","dntt_092017_12_link1.csv",
          "uptt_092017_12_link2.csv","dntt_092017_12_link2.csv",
          "uptt_092017_12_link3.csv","dntt_092017_12_link3.csv",
          "uptt_092017_12_link4.csv","dntt_092017_12_link4.csv",
          "uptt_092017_13_link1.csv","dntt_092017_13_link1.csv",
          "uptt_092017_13_link2.csv","dntt_092017_13_link2.csv",
          "uptt_092017_13_link3.csv","dntt_092017_13_link3.csv",
          "uptt_092017_13_link4.csv","dntt_092017_13_link4.csv"
          )


#Make an empty table to hold basic stats for comparisons of different link, heading , and simualtion results
n<-length(files)
output<-data.frame(Sim=as.character(seq(1,n)),Link=as.numeric(seq(1,n)),Heading=as.character(seq(1,n)),MeanTTSim=as.numeric(seq(1,n)), MeanTTAis=as.numeric(seq(1,n)), SdTTSim=as.numeric(seq(1,n)),SdTTAis=as.numeric(seq(1,n)), RMSE=as.numeric(seq(1,n)),KS=as.numeric(seq(1,n)),stringsAsFactors = F)
fulllinktime<-data.frame(matrix(ncol=n*2, nrow = 200))

for (i in 1:n)
{
simulation<-substr(files[i],6,14)
link<-substr(files[i],16,20 )
linkvar<-paste0(link,"tt")
heading<-substr(files[i],1,2)

#read in the post-processed simulation results for a specific link
sim<-read.csv(paste(files[i]), header=T, na.strings= "") 
# sim$heading<-"upstream"
# downstream.sim$heading<-"downstream"

##############################################################
####READ IN THE AIS DATA BUILT FOR SIMULATION TOW SPAWNING####
##############################################################

#read in the time series of AIS transit times with associated start and end time, for each link-direction
if (heading =="up"){
  ais<-read.csv("C:/Users/nelsonks/Dropbox/Kate_Paul/models/upstreaming2.csv", header=T, na.strings= "")
}else{
  ais<-read.csv("C:/Users/nelsonks/Dropbox/Kate_Paul/models/downstreaming2.csv", header=T, na.strings= "")}
# ais$heading<-"upstream"
# downstream.ais$heading<-"downstream"


####################################
###extract transit times to compare 
####################################

#create tables for the transit time comparisons
keepers<-c("id","ttime",paste(linkvar))
sim<-sim[ ,keepers] 
keepers<-c("arrival","arrival_orig","name",paste(linkvar))
ais<-ais[,keepers] 
ais[,paste(linkvar)]<-  ais[,paste(linkvar)]/60 #convert ais transit times from min to hours
ais<- ais[ais[paste(linkvar)]!=0,] #get rid of zeros for tows not traveling on the current link
ais<-distinct(ais)
ais_orig<-ais

#Remove outliers less than quantile 1 minus 1.5IQR and greater than quantile 3 plus 1.5IQR
ais_IQR<-IQR(ais[ ,paste(linkvar)], na.rm=TRUE, type=7)
Quantile_1<-quantile(ais[ ,paste(linkvar)], probs=c(0.25, 0.5, 0.75), na.rm=TRUE)
bottom<-Quantile_1[1]-ais_IQR
top<-Quantile_1[3]+ais_IQR
ais<-ais[ais[ ,paste(linkvar)] >= bottom & ais[ ,paste(linkvar)] <= top, ]

#basic plots
#  hist(sim$ttime, breaks=c(0,2,3,4,5,8,10, 100))
#  hist(ais[ ,paste(linkvar)],  breaks=c(0,2,3,4,5,8,10, 100))
#  plot(ecdf(sim$ttime))
#  plot(ecdf(ais[ ,paste(linkvar)]))
# 
# 
# qqplot(ais[,paste(linkvar)], sim$ttime)
# 

#build a table linking sim tows and their match in the ais tows
full<-left_join(ais, sim, by = c("name"="id"))
full<-full[!is.na(full$ttime),-c(8:9)] #reduce to records with matches
ais$arrival<-as.POSIXlt.factor(ais$arrival)
full$arrival<-as.POSIXlt.factor(full$arrival)

#kolmogorow-Smirnov test (non-parameteric) of CDFs where null-hyp: distr are equal
ais_sub<-ais[(ais$arrival <= max(full$arrival)& ais$arrival >= min(full$arrival)), paste(linkvar)] #subset of ais data bounded by simulated time frame
ks<-ks.test(ais_sub,sim$ttime, alternative = "two.sided") 
  #for n=489, alpha=0.05, p < 0.1208 (stats book table 7) --> which is greatly exceeded by D, so reject the null --> distr NOT equal

#kolmogorow-Smirnov test (non-parameteric) of CDFs where null-hyp: distr are equal
ks2<-ks.test(full[,paste0(linkvar,".x")],full$ttime, alternative = "two.sided") #ks for matching sim and ais records only
#for n=489, alpha=0.05, p < 0.1208 (stats book table 7) --> which is greatly exceeded by D, so reject the null --> distr NOT equal

#calculate the n for the ais and sim datasets being compared and the Critical D statitistic for the KS test
n_ais<-length(ais[(ais$arrival <= max(full$arrival)& ais$arrival >= min(full$arrival)),paste(linkvar)]) #ais[ , paste(linkvar)])
n_sim<-length(sim$ttime)
alpha=0.1
crit_D<-1.22*sqrt((n_ais + n_sim)/(n_ais*n_sim)) #for alpha = 0.10 (for alpha = 0.2 use 1.07 mulitplier, for alpha =0.15 use 1.14 multiplier for alpha = 0.05 multiplier is 1.358, for alpha = 0.01 multiplier is 1.63)
# crit_value<-sqrt(-0.5*log(alpha/2))*sqrt((n_ais + n_sim)/(n_ais*n_sim)) #another way to compute the critical D given an alpha
p_value<-exp(((ks$statistic/sqrt((n_ais + n_sim)/(n_ais*n_sim)))^2)/-0.5)*2 #compute the estiamted p-value for the crit_D and n #similar to estimate in ks.test

#summary stats
mean_sim<-mean(sim$ttime)
mean_ais<-mean(ais[(ais$arrival <= max(full$arrival)& ais$arrival >= min(full$arrival)),paste(linkvar)])
sd_sim<-sd(sim$ttime)
sd_ais<-sd(ais[(ais$arrival <= max(full$arrival)& ais$arrival >= min(full$arrival)),paste(linkvar)])

#one-to-one RMSE
full$resid<-full$ttime-full[,paste0(linkvar,".x")]
RMSE<-sqrt(mean(full$resid^2)) #25, 151
nRMSE<-RMSE/(max(ais[,paste(linkvar)])-min(ais[,paste(linkvar)])) #range normalized RMSE

#correlation calc and plots
spear_r<-cor(full[,paste0(linkvar,".x")],full$ttime, method="spearman")


# plot(full$ttime,full[,paste0(linkvar,".x")], ylab="AIS transit time", xlab="sim transit time", main = paste(simulation, link, heading), ylim=c(0,(mean_ais+2*sd_ais)))
# plot(as.Date(full$arrival_orig, tz="GMT"),full[,paste0(linkvar,".x")], col="green", ylim = c(0, max(full[,c(4,6)])), ylab="transit times", xlab="Date", main = paste(simulation, link, heading))
# points(as.Date(full$arrival_orig, tz="GMT"),full$ttime, col="red", cex=0.5) # --> should run several replicate simulations to get an RMSE for each time point

#mean relationship between sim and ais
data<-full
data$bins <- cut(data$ttime,breaks = 10)
g<-ggplot(data, aes(x = bins, y = data[,paste0(linkvar,".x")] )) +
  stat_summary(fun.y = "mean", geom = "point") + ggtitle (paste(simulation, link, heading, "functional relationship"))
print(g)

data$date<-strptime(data$arrival, "%Y-%m-%d")
g2<-ggplot(data, aes(x = date, y = data[,paste0(linkvar,".x")]) ) + geom_point(colour="blue") + 
  stat_smooth( method="loess",span = 0.028, na.rm=T, fill="blue", colour="blue") + ggtitle (paste(simulation, link, heading, "daily means")) + 
  geom_point(data = data, aes(x = date, y = ttime), colour = "red") + geom_smooth(data = data, aes(x = date, y = ttime), fill="red", colour = "red", method="loess",span = 0.028, na.rm=T)
print(g2)

#fill in the ouput table
output$Sim[i]<-simulation
output$Link[i]<-link
output$Heading[i]<-heading
output$MeanTTSim[i]<-mean_sim
output$MeanTTAis[i]<-mean_ais
output$delta_mean[i]<-mean_ais-mean_sim
output$SdTTSim[i]<-sd_sim
output$SdTTAis[i]<-sd_ais
output$delta_sd[i]<-sd_ais-sd_sim
output$n_ais[i]<-n_ais
output$n_sim[i]<-n_sim
output$KS[i]<-ks$statistic
output$crit_D[i]<-crit_D
output$RMSE[i]<-RMSE
output$nRMSE[i]<-nRMSE
output$spear_r[i]<-spear_r
output$ks_p[i]<-ks$p.value


#fullsystime<-as.data.frame(full_b$ais_systime)
fulllinktime[1:length(full$ttime),i]<-as.numeric(full$ttime)
fulllinktime[1:length(full[,6]),n+i]<-as.numeric(full[,6])

}
output<-output[,c(1:3,12,13,4,5,10,6,7,11,9,14,17,8,15,16)]

write.csv(output,"comparisontbl_links_outliers.csv")
write.csv(fulllinktime, "full_linktime_data.csv")

# data<-full
# data$bins <- cut(data$ttime,breaks = 10)
# # Points:
# ggplot(data, aes(x = bins, y = link4tt.x)) +
 # stat_summary(fun.y = "mean", geom = "point")


#########################################
###Comparison of Total Time in System ###
#########################################

  files2<- c("systime_092017_01.csv",
             "systime_092017_02.csv",
             "systime_092017_03.csv",
             "systime_092017_04.csv",
             "systime_092017_05.csv",
             "systime_092017_06.csv",
             "systime_092017_07.csv",
             "systime_092017_08.csv",
             "systime_092017_09.csv",
             "systime_092017_10.csv",
             "systime_092017_11.csv",
             "systime_092017_12.csv",
             "systime_092017_13.csv"
            )

n<-length(files2)
output2<-data.frame(Sim=as.character(seq(1,n)),MeanSTSim=as.numeric(seq(1,n)), MeanSTAis=as.numeric(seq(1,n)), SdSTSim=as.numeric(seq(1,n)),SdSTAis=as.numeric(seq(1,n)), stringsAsFactors = F)
fullsystime<-data.frame(matrix(ncol=n*2, nrow = 1260))

for (i in 1:n)
{
  simulation<-substr(files2[i],9,17)
  
  #read in the post-processed simulation results for a sim
  sim_all<-read.csv(paste(files2[i]), header=T, na.strings= "") 
  sim_all$id<-as.integer(as.character(sim_all$id))
  sim_all$systemtime<-as.numeric(as.character(sim_all$systemtime))
  sim_all<-sim_all[sim_all$systemtime <1000,] #remove bad data pieces
  sim_all<-sim_all[!is.na(sim_all$systemtime),] #remove bad data pieces
  
  #read in the associated ais data and combine the upstream and downstream files
  aisup<-read.csv("C:/Users/nelsonks/Dropbox/Kate_Paul/models/upstreaming2.csv", header=T, na.strings= "")
  aisdown<-read.csv("C:/Users/nelsonks/Dropbox/Kate_Paul/models/downstreaming2.csv", header=T, na.strings= "")
  aisup$heading<-90
  aisdown$heading<-270
  ais_all<-rbind(aisup,aisdown)
  
  
  #build a table linking sim tows and their match in the ais tows
  full_b<-left_join(ais_all, sim_all, by = c("name"="id"))
  full_b<-full_b[!is.na(full_b$who),] #reduce to records with matches
  full_b$arrival<-as.POSIXlt.factor(full_b$arrival)
  full_b$departure<-as.POSIXlt.factor(full_b$departure)
  full_b$ais_systime<-difftime(full_b$departure,full_b$arrival, units =c("hours"))
  
  #convert ais_all time values 
  ais_all$arrival<-as.POSIXlt.factor(ais_all$arrival)
  ais_all$departure<-as.POSIXlt.factor(ais_all$departure)
  ais_all$ais_systime<-difftime(ais_all$departure,ais_all$arrival, units =c("hours"))
  ais_all$ais_systime<-as.numeric(ais_all$ais_systime)
  
  #remove outliers
  full_b_orig <-full_b
  ais_IQR<-IQR(full_b$ais_systime, na.rm=TRUE, type=7)
  Quantile_1<-quantile(full_b$ais_systime, probs=c(0.25, 0.5, 0.75), na.rm=TRUE)
  bottom<-Quantile_1[1]-ais_IQR
  top<-Quantile_1[3]+ais_IQR
  full_b<-full_b[full_b$ais_systime >= bottom & full_b$ais_systime <= top, ]
  
  ais_all_orig<-ais_all
  ais_IQR<-IQR(ais_all$ais_systime, na.rm=TRUE, type=7)
  Quantile_1<-quantile(ais_all$ais_systime, probs=c(0.25, 0.5, 0.75), na.rm=TRUE)
  bottom<-Quantile_1[1]-ais_IQR
  top<-Quantile_1[3]+ais_IQR
  ais_all<-ais_all[ais_all$ais_systime >= bottom & ais_all$ais_systime <= top, ]
  
  #calculate the n for the ais and sim datasets being compared and the Critical D statitistic for the KS test
  sys_n_ais<-length(ais_all[(ais_all$arrival <= max(full_b$arrival) & ais_all$arrival >= min(full_b$arrival)) ,1]) #ais[ , paste(linkvar)])
  sys_n_sim<-length(sim_all[,1])
  alpha=0.1
  sys_crit_D<-1.22*sqrt((sys_n_ais + sys_n_sim)/(sys_n_ais*sys_n_sim)) #for alpha = 0.10 (for alpha = 0.2 use 1.07 mulitplier, for alpha =0.15 use 1.14 multiplier for alpha = 0.05 multiplier is 1.358, for alpha = 0.01 multiplier is 1.63)
  ais_all_sub<-ais_all[(ais_all$arrival <= max(full_b$arrival)& ais_all$arrival >= min(full_b$arrival)), "ais_systime" ] #subset of ais data bounded by simulated time frame
  ks<-ks.test(ais_all_sub,full_b$systemtime, alternative = "two.sided") 
  p_value<-exp(((ks$statistic/sqrt((sys_n_ais + sys_n_sim)/(sys_n_ais*sys_n_sim)))^2)/-0.5)*2 #compute the estiamted p-value for the crit_D and n #similar to estimate in ks.test

  #mean and sd stats
  mean_sim_all<-mean(sim_all$systemtime)
  mean_ais_all<-as.numeric(mean(ais_all[(ais_all$arrival <= max(full_b$arrival) & ais_all$arrival >= min(full_b$arrival)) ,"ais_systime"]))
  sd_sim_all<-sd(sim_all$systemtime)
  sd_ais_all<-as.numeric(sd(ais_all[(ais_all$arrival <= max(full_b$arrival) & ais_all$arrival >= min(full_b$arrival)) ,"ais_systime"]))
  
  
  #one-to-one RMSE
  full_b$resid<-as.numeric(full_b$ais_systime-full_b$systemtime)
  RMSE<-sqrt(mean(full_b$resid^2)) #25, 151
  nRMSE<-RMSE/as.numeric(max(ais_all[,"ais_systime"])-min(ais_all[,"ais_systime"])) #range normalized RMSE
  
  #correlation calc and plots
  spear_r<-cor(as.numeric(full_b[,"systemtime"]),as.numeric(full_b$ais_systime), method="spearman")
  
  # plot(full$ttime,full[,paste0(linkvar,".x")], ylab="AIS transit time", xlab="sim transit time", main = paste(simulation, link, heading), ylim=c(0,(mean_ais+2*sd_ais)))
  # plot(as.Date(full$arrival_orig, tz="GMT"),full[,paste0(linkvar,".x")], col="green", ylim = c(0, max(full[,c(4,6)])), ylab="transit times", xlab="Date", main = paste(simulation, link, heading))
  # points(as.Date(full$arrival_orig, tz="GMT"),full$ttime, col="red", cex=0.5) # --> should run several replicate simulations to get an RMSE for each time point
  
  plot(ecdf(full_b$ais_systime))
  plot(ecdf(full_b$systemtime))

  
  #fill in the ouput table
  output2$Sim[i]<-simulation
  output2$MeanSTSim[i]<-mean_sim_all
  output2$MeanSTAis[i]<-mean_ais_all
  output2$delta_mean_ST[i]<-mean_ais_all-mean_sim_all
  output2$SdSTSim[i]<-sd_sim_all
  output2$SdSTAis[i]<-sd_ais_all
  output2$delta_sd_ST[i]<-sd_ais_all-sd_sim_all
  output2$n_ais_all[i]<-sys_n_ais
  output2$n_sim_all[i]<-sys_n_sim
  output2$KS_all[i]<-ks$statistic
  output2$crit_D_all[i]<-sys_crit_D
  output2$RMSE_all[i]<-RMSE
  output2$nRMSE_all[i]<-nRMSE
  output2$spear_r_all[i]<-spear_r
  output2$ks_p_all[i]<-ks$p.value
  
  #fullsystime<-as.data.frame(full_b$ais_systime)
  fullsystime[1:length(full_b$systemtime),i]<-as.numeric(full_b$systemtime)
  fullsystime[1:length(full_b$ais_systime),n+i]<-as.numeric(full_b$ais_systime)
  
}

write.csv(output2,"comparisontbl_system_outlier.csv")
write.csv(fullsystime, "full_systime_data.csv")

#