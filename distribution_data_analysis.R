#3


library (bayesplot)
library(ggplot2)
library(dplyr)
library(stringr)
library(reshape2)

#setwd('C:/Users/nelsonks/Dropbox/Kate_Paul/paul_simulations/2013_AIS_runs_02')
#setwd('C:/Users/nelsonks/Dropbox/Kate_Paul/paul_simulations/2013_AIS_30min_jan_jun')
#setwd('C:/Users/nelsonks/Dropbox/Kate_Paul/paul_simulations/2013_AIS_30min_may_oct')
#setwd('C:/Users/nelsonks/Dropbox/Kate_Paul/paul_simulations/2013_AIS_30min_jun_aug')
#setwd('C:/Users/nelsonks/Dropbox/Kate_Paul/paul_simulations/2013_AIS_15min_jan_jun')
#setwd('C:/Users/nelsonks/Dropbox/Kate_Paul/paul_simulations/2013_AIS_15min_may_oct')
setwd('C:/Users/nelsonks/Dropbox/Kate_Paul/paul_simulations/2013_AIS_15min_jun_aug/')


d_sys<-read.csv("full_systime_data.csv", header=T, na.strings= "", stringsAsFactors = FALSE) 
d_sys<-d_sys[,2:length(d_sys)]
d_links<-read.csv("full_linktime_data.csv",header=T, na.strings="",stringsAsFactors = FALSE)
d_links<-d_links[2:length(d_links)]

#adjust the matrix for simualtion set that was missing records for link 4
# d_links$blnk1<-NA
# d_links$blnk2<-NA
# d_links$blnk3<-NA
# d_links$blnk4<-NA
# d_links<-d_links[,c(1:70,205,206,71:102,103:172,207,208,173:204)]

#######################################################################
##############Bayesian Style Posterior Predictive Checks###############
#######################################################################
n<-13 #number of simulations

####################
#system times
#################

d_sys[] <- lapply(d_sys, function(x) as.numeric(as.character(x)))
d_sys[is.na(d_sys) ]<-0
d_sys<-d_sys[apply(d_sys[,-1], 1, function(x) !all(x==0)),]
colnames(d_sys)<-c("Sim1","Sim2","Sim3","Sim4","Sim5","Sim6","Sim7","Sim8","Sim9","Sim10","Sim11","Sim12","Sim13",
                         "AIS1","AIS2","AIS3","AIS4","AIS5","AIS6","AIS7","AIS8","AIS9","AIS10","AIS11","AIS12","AIS13")
# c<-colSums(d_sys[,(n+1):(n+13)] != 0)
# m<-which.max(c)
# y<-as.numeric(d_sys[,n+m])
# yrep<-d_sys[,c(1:n)]
# yrep<-as.matrix(yrep)
# yrep<-t(yrep)
# 
# pp_check(y,yrep, fun="ecdf_overlay")
# pp_check(y,yrep, fun="boxplot", notch=FALSE)
# pp_check(y,yrep, fun="stat_2d", stat=c( "mean","sd"))
# pp_check(y,yrep, fun="hist")
# pp_check(y,yrep, ppc_dens_overlay)
# ppc_intervals(y,yrep, prob=0.9)
# ppc_ribbon(y, yrep, prob=0.9)
# ppc_error_scatter(y, yrep)
# ppc_error_scatter_avg(y, yrep)
# ppc_scatter(y, yrep)
# ppc_stat(y,yrep, stat="mean")


mydf_m<-melt(d_sys)
mydf_m$number<-mydf_m$variable
mydf_m$number<- mydf_m$number %>% str_match_all("[0-9]+") %>% unlist %>% as.character
mydf_m$type<-mydf_m$variable
mydf_m$type<- mydf_m$type %>% str_match_all("[[:alpha:]]+") %>% unlist %>% as.character
mydf_m<-mydf_m[mydf_m$value!=0,]

#boxplots
ggplot(mydf_m,aes(y = value, x = number,colour=type)) + geom_boxplot(aes(), size=1) + ylim(0,50) 

#empircal cumulative distribution fcn
ggplot(mydf_m,aes(x = value)) + stat_ecdf(geom="step",aes(colour = number, linetype=type), size=1) + xlim(0,10) 
ggplot(mydf_m[mydf_m$number %in% c("13","7","9","12","1"),],aes(x = value)) + stat_ecdf(geom="step",aes(colour = number, linetype=type), size=1) + xlim(0,10) #subset to just the sims that look good

#probability density fcn
ggplot(mydf_m,aes(x = value,colour = number,linetype=type)) + geom_density(aes(), size=1, adjust=1) + xlim(0,10) 
ggplot(mydf_m[mydf_m$number %in% c("13","7","9"),],aes(x = value,colour = number,linetype=type)) + geom_density(aes(), size=1) + xlim(0,10)  #subset to just the sims that look good


###########################
#LINKS
#########################
n2<-13*8 #number of simulations times number of files (4 links upstream and 4 links downstream)
d_links[] <- lapply(d_links, function(x) as.numeric(as.character(x)))
d_links[is.na(d_links) ]<-0

##link1 up##
    d1<-d_links[ ,c(seq(1,length(d_links), by=8))] #extract the columns for link1 upstream results, every 8th record
    colnames(d1)<-c("Sim1_1up","Sim2_1up","Sim3_1up","Sim4_1up","Sim5_1up","Sim6_1up","Sim7_1up","Sim8_1up",
                      "Sim9_1up","Sim10_1up","Sim11_1up","Sim12_1up","Sim13_1up",
                    "AIS1_1up","AIS2_1up","AIS3_1up","AIS4_1up","AIS5_1up","AIS6_1up","AIS7_1up","AIS8_1up",
                    "AIS9_1up","AIS10_1up","AIS11_1up","AIS12_1up","AIS13_1up")
    d1<-d1[apply(d1[,-1], 1, function(x) !all(x==0)),]
    # c<-colSums(d1[,(n+1):(n+13)] != 0)
    # m<-which.max(c)
    # y<-as.numeric(d1[,n+m])
    # yrep<-d1[,c(1:n)]
    # yrep<-as.matrix(yrep)
    # yrep<-t(yrep)
    # 
    # pp_check(y,yrep, fun="ecdf_overlay")
    # pp_check(y,yrep, fun="boxplot", notch=FALSE)
    # pp_check(y,yrep, fun="stat_2d", stat=c( "mean","sd"))
    # pp_check(y,yrep, fun="hist")
    # pp_check(y,yrep, ppc_dens_overlay)
    # ppc_intervals(y,yrep, prob=0.9)
    # ppc_ribbon(y, yrep, prob=0.9)
    # ppc_error_scatter(y, yrep)
    # ppc_error_scatter_avg(y, yrep)
    # ppc_scatter(y, yrep)
    # ppc_stat(y,yrep, stat="mean")
    
    colnames(d1)<-c("Sim1","Sim2","Sim3","Sim4","Sim5","Sim6","Sim7","Sim8",
                    "Sim9","Sim10","Sim11","Sim12","Sim13",
                    "AIS1","AIS2","AIS3","AIS4","AIS5","AIS6","AIS7","AIS8",
                    "AIS9","AIS10","AIS11","AIS12","AIS13")
    mydf_m<-melt(d1)
    mydf_m$number<-mydf_m$variable
    mydf_m$number<- mydf_m$number %>% str_match_all("[0-9]+") %>% unlist %>% as.character
    mydf_m$type<-mydf_m$variable
    mydf_m$type<- mydf_m$type %>% str_match_all("[[:alpha:]]+") %>% unlist %>% as.character
    mydf_m<-mydf_m[mydf_m$value!=0,]
    
    #boxplots
    ggplot(mydf_m,aes(y = value, x = number,colour=type)) + geom_boxplot(aes(), size=1) #+ ylim(0,50) 

    #empircal cumulative distribution fcn
    ggplot(mydf_m,aes(x = value)) + stat_ecdf(geom="step",aes(colour = number, linetype=type), size=1) + xlim(0,10) 
    ggplot(mydf_m[mydf_m$number %in% c("6","10","13"),],aes(x = value)) + stat_ecdf(geom="step",aes(colour = number, linetype=type), size=1) + xlim(0,10) #subset to just the sims that look good
    
    #probability density fcn
    ggplot(mydf_m,aes(x = value,colour = number,linetype=type)) + geom_density(aes(), size=1) + xlim(0,10) 
    ggplot(mydf_m[mydf_m$number %in% c("6","10"),],aes(x = value,colour = number,linetype=type)) + geom_density(aes(), size=1) + xlim(0,10)  #subset to just the sims that look good
    
    
##link1 down##
    d1<-d_links[ ,c(seq(2,length(d_links), by=8))] #extract the columns for link1 downstream results, every 8th record
    colnames(d1)<-c("Sim1_1dwn","Sim2_1dwn","Sim3_1dwn","Sim4_1dwn","Sim5_1dwn","Sim6_1dwn","Sim7_1dwn","Sim8_1dwn",
                    "Sim9_1dwn","Sim10_1dwn","Sim11_1dwn","Sim12_1dwn","Sim13_1dwn",
                    "AIS1_1dwn","AIS2_1dwn","AIS3_1dwn","AIS4_1dwn","AIS5_1dwn","AIS6_1dwn","AIS7_1dwn","AIS8_1dwn",
                    "AIS9_1dwn","AIS10_1dwn","AIS11_1dwn","AIS12_1dwn","AIS13_1dwn")
    d1<-d1[apply(d1[,-1], 1, function(x) !all(x==0)),]
    # c<-colSums(d1[,(n+1):(n+13)] != 0)
    # m<-which.max(c)
    # y<-as.numeric(d1[,n+m])
    # yrep<-d1[,c(1:n)]
    # yrep<-as.matrix(yrep)
    # yrep<-t(yrep)
    # 
    # pp_check(y,yrep, fun="ecdf_overlay")
    # pp_check(y,yrep, fun="boxplot", notch=FALSE)
    # pp_check(y,yrep, fun="stat_2d", stat=c( "mean","sd"))
    # pp_check(y,yrep, fun="hist")
    # pp_check(y,yrep, ppc_dens_overlay)
    # ppc_intervals(y,yrep, prob=0.9)
    # ppc_ribbon(y, yrep, prob=0.9)
    # ppc_error_scatter(y, yrep)
    # ppc_error_scatter_avg(y, yrep)
    # ppc_scatter(y, yrep)
    # ppc_stat(y,yrep, stat="mean")
    
    colnames(d1)<-c("Sim1","Sim2","Sim3","Sim4","Sim5","Sim6","Sim7","Sim8",
                    "Sim9","Sim10","Sim11","Sim12","Sim13",
                    "AIS1","AIS2","AIS3","AIS4","AIS5","AIS6","AIS7","AIS8",
                    "AIS9","AIS10","AIS11","AIS12","AIS13")
    mydf_m<-melt(d1)
    mydf_m$number<-mydf_m$variable
    mydf_m$number<- mydf_m$number %>% str_match_all("[0-9]+") %>% unlist %>% as.character
    mydf_m$type<-mydf_m$variable
    mydf_m$type<- mydf_m$type %>% str_match_all("[[:alpha:]]+") %>% unlist %>% as.character
    mydf_m<-mydf_m[mydf_m$value!=0,]
    
    #boxplots
    ggplot(mydf_m,aes(y = value, x = number,colour=type)) + geom_boxplot(aes(), size=1) #+ ylim(0,50) 
    
    #empircal cumulative distribution fcn
    ggplot(mydf_m,aes(x = value)) + stat_ecdf(geom="step",aes(colour = number, linetype=type), size=1) + xlim(0,10) 
    ggplot(mydf_m[mydf_m$number %in% c("11","8","7","6","10","9"),],aes(x = value)) + stat_ecdf(geom="step",aes(colour = number, linetype=type), size=1) + xlim(0,10) #subset to just the sims that look good
    
    #probability density fcn
    ggplot(mydf_m,aes(x = value,colour = number,linetype=type)) + geom_density(aes(), size=1) + xlim(0,10) 
    ggplot(mydf_m[mydf_m$number %in% c("8","11","13"),],aes(x = value,colour = number,linetype=type)) + geom_density(aes(), size=1) + xlim(0,10)  #subset to just the sims that look good
    
    

##link2 up##
    d1<-d_links[ ,c(seq(3,length(d_links), by=8))] #extract the columns for link-direction results, every 8th record
    colnames(d1)<-c("Sim1_4up","Sim2_4up","Sim3_4up","Sim4_4up","Sim5_4up","Sim6_4up","Sim7_4up","Sim8_4up",
                    "Sim9_4up","Sim10_4up","Sim11_4up","Sim12_4up","Sim13_4up",
                    "AIS1_4up","AIS2_4up","AIS3_4up","AIS4_4up","AIS5_4up","AIS6_4up","AIS7_4up","AIS8_4up",
                    "AIS9_4up","AIS10_4up","AIS11_4up","AIS12_4up","AIS13_4up")
    d1<-d1[apply(d1[,-1], 1, function(x) !all(x==0)),]
    # c<-colSums(d1[,(n+1):(n+13)] != 0)
    # m<-which.max(c)
    # y<-as.numeric(d1[,n+m])
    # yrep<-d1[,c(1:n)]
    # yrep<-as.matrix(yrep)
    # yrep<-t(yrep)
    # 
    # pp_check(y,yrep, fun="ecdf_overlay")
    # pp_check(y,yrep, fun="boxplot", notch=FALSE)
    # pp_check(y,yrep, fun="stat_2d", stat=c( "mean","sd"))
    # pp_check(y,yrep, fun="hist")
    # pp_check(y,yrep, ppc_dens_overlay)
    # ppc_intervals(y,yrep, prob=0.9)
    # ppc_ribbon(y, yrep, prob=0.9)
    # ppc_error_scatter(y, yrep)
    # ppc_error_scatter_avg(y, yrep)
    # ppc_scatter(y, yrep)
    # ppc_stat(y,yrep, stat="mean")
    
    colnames(d1)<-c("Sim1","Sim2","Sim3","Sim4","Sim5","Sim6","Sim7","Sim8",
                    "Sim9","Sim10","Sim11","Sim12","Sim13",
                    "AIS1","AIS2","AIS3","AIS4","AIS5","AIS6","AIS7","AIS8",
                    "AIS9","AIS10","AIS11","AIS12","AIS13")
    mydf_m<-melt(d1)
    mydf_m$number<-mydf_m$variable
    mydf_m$number<- mydf_m$number %>% str_match_all("[0-9]+") %>% unlist %>% as.character
    mydf_m$type<-mydf_m$variable
    mydf_m$type<- mydf_m$type %>% str_match_all("[[:alpha:]]+") %>% unlist %>% as.character
    mydf_m<-mydf_m[mydf_m$value!=0,]
    
    #boxplots
    ggplot(mydf_m,aes(y = value, x = number,colour=type)) + geom_boxplot(aes(), size=1) #+ ylim(0,50) 
    
    #empircal cumulative distribution fcn
    ggplot(mydf_m,aes(x = value)) + stat_ecdf(geom="step",aes(colour = number, linetype=type), size=1) + xlim(0,10) 
    ggplot(mydf_m[mydf_m$number %in% c("10","9","6"),],aes(x = value)) + stat_ecdf(geom="step",aes(colour = number, linetype=type), size=1) + xlim(0,10) #subset to just the sims that look good
    
    #probability density fcn
    ggplot(mydf_m,aes(x = value,colour = number,linetype=type)) + geom_density(aes(), size=1) + xlim(0,10) 
    ggplot(mydf_m[mydf_m$number %in% c("10","6","9"),],aes(x = value,colour = number,linetype=type)) + geom_density(aes(), size=1) + xlim(0,10)  #subset to just the sims that look good
    
  
#link2 down
    d1<-d_links[ ,c(seq(4,length(d_links), by=8))] #extract the columns for link-direction results, every 8th record
    colnames(d1)<-c("Sim1_2dwn","Sim2_2dwn","Sim3_2dwn","Sim4_2dwn","Sim5_2dwn","Sim6_2dwn","Sim7_2dwn","Sim8_2dwn",
                    "Sim9_2dwn","Sim10_2dwn","Sim11_2dwn","Sim12_2dwn","Sim13_2dwn",
                    "AIS1_2dwn","AIS2_2dwn","AIS3_2dwn","AIS4_2dwn","AIS5_2dwn","AIS6_2dwn","AIS7_2dwn","AIS8_2dwn",
                    "AIS9_2dwn","AIS10_2dwn","AIS11_2dwn","AIS12_2dwn","AIS13_2dwn")
    d1<-d1[apply(d1[,-1], 1, function(x) !all(x==0)),]
    # c<-colSums(d1[,(n+1):(n+13)] != 0)
    # m<-which.max(c)
    # y<-as.numeric(d1[,n+m])
    # yrep<-d1[,c(1:n)]
    # yrep<-as.matrix(yrep)
    # yrep<-t(yrep)
    # 
    # pp_check(y,yrep, fun="ecdf_overlay")
    # pp_check(y,yrep, fun="boxplot", notch=FALSE)
    # pp_check(y,yrep, fun="stat_2d", stat=c( "mean","sd"))
    # pp_check(y,yrep, fun="hist")
    # pp_check(y,yrep, ppc_dens_overlay)
    # ppc_intervals(y,yrep, prob=0.9)
    # ppc_ribbon(y, yrep, prob=0.9)
    # ppc_error_scatter(y, yrep)
    # ppc_error_scatter_avg(y, yrep)
    # ppc_scatter(y, yrep)
    # ppc_stat(y,yrep, stat="mean")
    
    colnames(d1)<-c("Sim1","Sim2","Sim3","Sim4","Sim5","Sim6","Sim7","Sim8",
                    "Sim9","Sim10","Sim11","Sim12","Sim13",
                    "AIS1","AIS2","AIS3","AIS4","AIS5","AIS6","AIS7","AIS8",
                    "AIS9","AIS10","AIS11","AIS12","AIS13")
    mydf_m<-melt(d1)
    mydf_m$number<-mydf_m$variable
    mydf_m$number<- mydf_m$number %>% str_match_all("[0-9]+") %>% unlist %>% as.character
    mydf_m$type<-mydf_m$variable
    mydf_m$type<- mydf_m$type %>% str_match_all("[[:alpha:]]+") %>% unlist %>% as.character
    mydf_m<-mydf_m[mydf_m$value!=0,]
    
    #boxplots
    ggplot(mydf_m,aes(y = value, x = number,colour=type)) + geom_boxplot(aes(), size=1) + ylim(0,50)
    
    #empircal cumulative distribution fcn
    ggplot(mydf_m,aes(x = value)) + stat_ecdf(geom="step",aes(colour = number, linetype=type), size=1) + xlim(0,10) 
    ggplot(mydf_m[mydf_m$number %in% c("1","2","5","6","7","10","11","12","13"),],aes(x = value)) + stat_ecdf(geom="step",aes(colour = number, linetype=type), size=1) + xlim(0,10) #subset to just the sims that look good
    
    #probability density fcn
    ggplot(mydf_m,aes(x = value,colour = number,linetype=type)) + geom_density(aes(), size=1) + xlim(0,10) +ylim(0,1)
    ggplot(mydf_m[mydf_m$number %in% c("11","13","6"),],aes(x = value,colour = number,linetype=type)) + geom_density(aes(), size=1) + xlim(0,10) +ylim(0,1) #subset to just the sims that look good
    

#link3 up
    d1<-d_links[ ,c(seq(5,length(d_links), by=8))] #extract the columns for link-direction results, every 8th record
    colnames(d1)<-c("Sim1_3up","Sim2_3up","Sim3_3up","Sim4_3up","Sim5_3up","Sim6_3up","Sim7_3up","Sim8_3up",
                    "Sim9_3up","Sim10_3up","Sim11_3up","Sim12_3up","Sim13_3up",
                    "AIS1_3up","AIS2_3up","AIS3_3up","AIS4_3up","AIS5_3up","AIS6_3up","AIS7_3up","AIS8_3up",
                    "AIS9_3up","AIS10_3up","AIS11_3up","AIS12_3up","AIS13_3up")
    d1<-d1[apply(d1[,-1], 1, function(x) !all(x==0)),]
    # c<-colSums(d1[,(n+1):(n+13)] != 0)
    # m<-which.max(c)
    # y<-as.numeric(d1[,n+m])
    # yrep<-d1[,c(1:n)]
    # yrep<-as.matrix(yrep)
    # yrep<-t(yrep)
    # 
    # pp_check(y,yrep, fun="ecdf_overlay")
    # pp_check(y,yrep, fun="boxplot", notch=FALSE)
    # pp_check(y,yrep, fun="stat_2d", stat=c( "mean","sd"))
    # pp_check(y,yrep, fun="hist")
    # pp_check(y,yrep, ppc_dens_overlay)
    # ppc_intervals(y,yrep, prob=0.9)
    # ppc_ribbon(y, yrep, prob=0.9)
    # ppc_error_scatter(y, yrep)
    # ppc_error_scatter_avg(y, yrep)
    # ppc_scatter(y, yrep)
    # ppc_stat(y,yrep, stat="mean")
    
    colnames(d1)<-c("Sim1","Sim2","Sim3","Sim4","Sim5","Sim6","Sim7","Sim8",
                    "Sim9","Sim10","Sim11","Sim12","Sim13",
                    "AIS1","AIS2","AIS3","AIS4","AIS5","AIS6","AIS7","AIS8",
                    "AIS9","AIS10","AIS11","AIS12","AIS13")
    mydf_m<-melt(d1)
    mydf_m$number<-mydf_m$variable
    mydf_m$number<- mydf_m$number %>% str_match_all("[0-9]+") %>% unlist %>% as.character
    mydf_m$type<-mydf_m$variable
    mydf_m$type<- mydf_m$type %>% str_match_all("[[:alpha:]]+") %>% unlist %>% as.character
    mydf_m<-mydf_m[mydf_m$value!=0,]
    
    #boxplots
    ggplot(mydf_m,aes(y = value, x = number,colour=type)) + geom_boxplot(aes(), size=1) #+ ylim(0,50)
    
    #empircal cumulative distribution fcn
    ggplot(mydf_m,aes(x = value)) + stat_ecdf(geom="step",aes(colour = number, linetype=type), size=1) + xlim(0,10) 
    ggplot(mydf_m[mydf_m$number %in% c("10","5","13","9","12","6"),],aes(x = value)) + stat_ecdf(geom="step",aes(colour = number, linetype=type), size=1) + xlim(0,10) #subset to just the sims that look good

    #probability density fcn
    ggplot(mydf_m,aes(x = value,colour = number,linetype=type)) + geom_density(aes(), size=1, adjust=2) + xlim(0,10) 
    ggplot(mydf_m[mydf_m$number %in% c("10","5","9","12","6","13","1"),],aes(x = value,colour = number,linetype=type)) + geom_density(aes(), size=1, adjust=2) + xlim(0,10)  #subset to just the sims that look good
    

##link3 down##
    d1<-d_links[ ,c(seq(6,length(d_links), by=8))] #extract the columns for link-direction results, every 8th record
    colnames(d1)<-c("Sim1_3dwn","Sim2_3dwn","Sim3_3dwn","Sim4_3dwn","Sim5_3dwn","Sim6_3dwn","Sim7_3dwn","Sim8_3dwn",
                    "Sim9_3dwn","Sim10_3dwn","Sim11_3dwn","Sim12_3dwn","Sim13_3dwn",
                    "AIS1_3dwn","AIS2_3dwn","AIS3_3dwn","AIS4_3dwn","AIS5_3dwn","AIS6_3dwn","AIS7_3dwn","AIS8_3dwn",
                    "AIS9_3dwn","AIS10_3dwn","AIS11_3dwn","AIS12_3dwn","AIS13_3dwn")
    d1<-d1[apply(d1[,-1], 1, function(x) !all(x==0)),]
    # c<-colSums(d1[,(n+1):(n+13)] != 0)
    # m<-which.max(c)
    # y<-as.numeric(d1[,n+m])
    # yrep<-d1[,c(1:n)]
    # yrep<-as.matrix(yrep)
    # yrep<-t(yrep)
    # 
    # pp_check(y,yrep, fun="ecdf_overlay")
    # pp_check(y,yrep, fun="boxplot", notch=FALSE)
    # pp_check(y,yrep, fun="stat_2d", stat=c( "mean","sd"))
    # pp_check(y,yrep, fun="hist")
    # pp_check(y,yrep, ppc_dens_overlay)
    # ppc_intervals(y,yrep, prob=0.9)
    # ppc_ribbon(y, yrep, prob=0.9)
    # ppc_error_scatter(y, yrep)
    # ppc_error_scatter_avg(y, yrep)
    # ppc_scatter(y, yrep)
    # ppc_stat(y,yrep, stat="mean")
    
    colnames(d1)<-c("Sim1","Sim2","Sim3","Sim4","Sim5","Sim6","Sim7","Sim8",
                    "Sim9","Sim10","Sim11","Sim12","Sim13",
                    "AIS1","AIS2","AIS3","AIS4","AIS5","AIS6","AIS7","AIS8",
                    "AIS9","AIS10","AIS11","AIS12","AIS13")
    mydf_m<-melt(d1)
    mydf_m$number<-mydf_m$variable
    mydf_m$number<- mydf_m$number %>% str_match_all("[0-9]+") %>% unlist %>% as.character
    mydf_m$type<-mydf_m$variable
    mydf_m$type<- mydf_m$type %>% str_match_all("[[:alpha:]]+") %>% unlist %>% as.character
    mydf_m<-mydf_m[mydf_m$value!=0,]
    
    #boxplots
    ggplot(mydf_m,aes(y = value, x = number,colour=type)) + geom_boxplot(aes(), size=1) + ylim(0,10)
    
    #empircal cumulative distribution fcn
    ggplot(mydf_m,aes(x = value)) + stat_ecdf(geom="step",aes(colour = number, linetype=type), size=1) + xlim(0,10) 
    ggplot(mydf_m[mydf_m$number %in% c("2","3","4","5","10","12","13"),],aes(x = value)) + stat_ecdf(geom="step",aes(colour = number, linetype=type), size=1) + xlim(0,5) #subset to just the sims that look good

    #probability density fcn
    ggplot(mydf_m,aes(x = value,colour = number,linetype=type)) + geom_density(aes(), size=1, adjust = 1) + xlim(0,10) 
    ggplot(mydf_m[mydf_m$number %in% c("2","4","5","10","12","13"),],aes(x = value,colour = number,linetype=type)) + geom_density(aes(), size=1,adjust = 2) + xlim(0,3)  #subset to just the sims that look good
    
##link4 up##
    d1<-d_links[ ,c(seq(7,length(d_links), by=8))] #extract the columns for link-direction results, every 8th record
    colnames(d1)<-c("Sim1_4up","Sim2_4up","Sim3_4up","Sim4_4up","Sim5_4up","Sim6_4up","Sim7_4up","Sim8_4up",
                    "Sim9_4up","Sim10_4up","Sim11_4up","Sim12_4up","Sim13_4up",
                    "AIS1_4up","AIS2_4up","AIS3_4up","AIS4_4up","AIS5_4up","AIS6_4up","AIS7_4up","AIS8_4up",
                    "AIS9_4up","AIS10_4up","AIS11_4up","AIS12_4up","AIS13_4up")
    d1<-d1[apply(d1[,-1], 1, function(x) !all(x==0)),]
    # c<-colSums(d1[,(n+1):(n+13)] != 0)
    # m<-which.max(c)
    # y<-as.numeric(d1[,n+m])
    # yrep<-d1[,c(1:n)]
    # yrep<-as.matrix(yrep)
    # yrep<-t(yrep)
    # 
    # pp_check(y,yrep, fun="ecdf_overlay")
    # pp_check(y,yrep, fun="boxplot", notch=FALSE)
    # pp_check(y,yrep, fun="stat_2d", stat=c( "mean","sd"))
    # pp_check(y,yrep, fun="hist")
    # pp_check(y,yrep, ppc_dens_overlay)
    # ppc_intervals(y,yrep, prob=0.9)
    # ppc_ribbon(y, yrep, prob=0.9)
    # ppc_error_scatter(y, yrep)
    # ppc_error_scatter_avg(y, yrep)
    # ppc_scatter(y, yrep)
    # ppc_stat(y,yrep, stat="mean")
    # 
    colnames(d1)<-c("Sim1","Sim2","Sim3","Sim4","Sim5","Sim6","Sim7","Sim8",
                    "Sim9","Sim10","Sim11","Sim12","Sim13",
                    "AIS1","AIS2","AIS3","AIS4","AIS5","AIS6","AIS7","AIS8",
                    "AIS9","AIS10","AIS11","AIS12","AIS13")
    mydf_m<-melt(d1)
    mydf_m$number<-mydf_m$variable
    mydf_m$number<- mydf_m$number %>% str_match_all("[0-9]+") %>% unlist %>% as.character
    mydf_m$type<-mydf_m$variable
    mydf_m$type<- mydf_m$type %>% str_match_all("[[:alpha:]]+") %>% unlist %>% as.character
    mydf_m<-mydf_m[mydf_m$value!=0,]
    
    #boxplots
    ggplot(mydf_m,aes(y = value, x = number,colour=type)) + geom_boxplot(aes(), size=1) #+ ylim(0,50)
    
    #empircal cumulative distribution fcn
    ggplot(mydf_m,aes(x = value)) + stat_ecdf(geom="step",aes(colour = number, linetype=type), size=1) + xlim(0,10) 
    ggplot(mydf_m[mydf_m$number %in% c("6","10","5","9"),],aes(x = value)) + stat_ecdf(geom="step",aes(colour = number, linetype=type), size=1) + xlim(0,10) #subset to just the sims that look good

    #probability density fcn
    ggplot(mydf_m,aes(x = value,colour = number,linetype=type)) + geom_density(aes(), size=1) + xlim(0,10) 
    ggplot(mydf_m[mydf_m$number %in% c("6","10","5","9"),],aes(x = value,colour = number,linetype=type)) + geom_density(aes(), size=1) + xlim(0,10)  #subset to just the sims that look good
    

##link4 down##
    d1<-d_links[ ,c(seq(8,length(d_links), by=8))] #extract the columns for link-direction results, every 8th record
    colnames(d1)<-c("Sim1_4dwn","Sim2_4dwn","Sim3_4dwn","Sim4_4dwn","Sim5_4dwn","Sim6_4dwn","Sim7_4dwn","Sim8_4dwn",
                    "Sim9_4dwn","Sim10_4dwn","Sim11_4dwn","Sim12_4dwn","Sim13_4dwn",
                    "AIS1_4dwn","AIS2_4dwn","AIS3_4dwn","AIS4_4dwn","AIS5_4dwn","AIS6_4dwn","AIS7_4dwn","AIS8_4dwn",
                    "AIS9_4dwn","AIS10_4dwn","AIS11_4dwn","AIS12_4dwn","AIS13_4dwn")
    d1<-d1[apply(d1[,-1], 1, function(x) !all(x==0)),]
    # c<-colSums(d1[,(n+1):(n+13)] != 0)
    # m<-which.max(c)
    # y<-as.numeric(d1[,n+m])
    # yrep<-d1[,c(1:n)]
    # yrep<-as.matrix(yrep)
    # yrep<-t(yrep)
    # 
    # pp_check(y,yrep, fun="ecdf_overlay")
    # pp_check(y,yrep, fun="boxplot", notch=FALSE)
    # pp_check(y,yrep, fun="stat_2d", stat=c( "mean","sd"))
    # pp_check(y,yrep, fun="hist")
    # pp_check(y,yrep, ppc_dens_overlay)
    # ppc_intervals(y,yrep, prob=0.9)
    # ppc_ribbon(y, yrep, prob=0.9)
    # ppc_error_scatter(y, yrep)
    # ppc_error_scatter_avg(y, yrep)
    # ppc_scatter(y, yrep)
    # ppc_stat(y,yrep, stat="mean")
    
    colnames(d1)<-c("Sim1","Sim2","Sim3","Sim4","Sim5","Sim6","Sim7","Sim8",
                    "Sim9","Sim10","Sim11","Sim12","Sim13",
                    "AIS1","AIS2","AIS3","AIS4","AIS5","AIS6","AIS7","AIS8",
                    "AIS9","AIS10","AIS11","AIS12","AIS13")
    mydf_m<-melt(d1)
    mydf_m$number<-mydf_m$variable
    mydf_m$number<- mydf_m$number %>% str_match_all("[0-9]+") %>% unlist %>% as.character
    mydf_m$type<-mydf_m$variable
    mydf_m$type<- mydf_m$type %>% str_match_all("[[:alpha:]]+") %>% unlist %>% as.character
    mydf_m<-mydf_m[mydf_m$value!=0,]
    
    #boxplots
    ggplot(mydf_m,aes(y = value, x = number,colour=type)) + geom_boxplot(aes(), size=1) #+ ylim(0,50)
    
    #empircal cumulative distribution fcn
    ggplot(mydf_m,aes(x = value)) + stat_ecdf(geom="step",aes(colour = number, linetype=type), size=1) + xlim(0,10) 
    ggplot(mydf_m[mydf_m$number %in% c("11","12","7","6"),],aes(x = value)) + stat_ecdf(geom="step",aes(colour = number, linetype=type), size=1) + xlim(0,10) #subset to just the sims that look good
    
    #probability density fcn
    ggplot(mydf_m,aes(x = value,colour = number,linetype=type)) + geom_density(aes(), size=1) + xlim(0,10) 
    ggplot(mydf_m[mydf_m$number %in% c("11","12","7","6"),],aes(x = value,colour = number,linetype=type)) + geom_density(aes(), size=1, adjust=2) + xlim(0,10)  #subset to just the sims that look good
    
    
    