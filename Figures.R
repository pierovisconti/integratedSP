#####PACKAGES#####
library(data.table)
library(ggplot2)
library(dplyr)
library(reshape2)
library(extrafont)
library(scales)

#####_____READ DATA_____#####
setwd("output")
thirtyperc_mv<-list.files("./",pattern = 'GDNH_mv0')
thirtypercILUP_mv<-list.files("./",pattern = 'GDNS_mv0')
ILUP_mv<-list.files("./",pattern = 'ILUP_mv0')
thirtyperc<-data.frame()
thirtypercILUP<-data.frame()
ilup<-data.frame()

aggregatefiles <- function(final, input){
  for (i in 1:length(input)){
    intermediate<-fread(input[[i]])
    intermediate<-intermediate[,c(1:8)]
    intermediate$runnumber<-i
    final<-rbind(final,intermediate,fill=TRUE)
  }
  return(final)
}
ilup<-aggregatefiles(ilup,ILUP_mv); thirtyperc<-aggregatefiles(thirtyperc,thirtyperc_mv)

ilup <- ilup %>% mutate(shortfall=pmax(0,Target-`Contributing Amount Held`))
thirtyperc <- thirtyperc %>% mutate(shortfall=pmax(0,Target-`Contributing Amount Held`))
thirtypercILUP <- thirtypercILUP %>% mutate(shortfall=pmax(0,Target-`Contributing Amount Held`))

#####_____FIGURE 2 - TRADE-OFFS_____######

#####Mammals#####
mammals<-fread('feat_mammals.dat')

#ILUP
ilup_mammals<-subset(ilup,ilup$Feature %in% mammals$id)
ilup_mamext<-aggregate(targetmet~runnumber,ilup_mammals,table)
ilup_mamext<-data.table(ilup_mamext)
ilup_mamext$perc<-ilup_mamext$targetmet.0/ilup_mamext$targetmet.1*100
#30%+ILUP
thirtypercILUP_mammals<-subset(thirtypercILUP,thirtypercILUP$Feature %in% mammals$id)
thirtypercILUP_mamext<-aggregate(targetmet~runnumber,thirtypercILUP_mammals,table)
thirtypercILUP_mamext<-data.table(thirtypercILUP_mamext)
thirtypercILUP_mamext$perc<-thirtypercILUP_mamext$targetmet.0/thirtypercILUP_mamext$targetmet.1*100
#30%
thirtyperc_mammals<-subset(thirtyperc,thirtyperc$Feature %in% mammals$id)
thirtyperc_mamext<-aggregate(targetmet~runnumber,thirtyperc_mammals,table)
thirtyperc_mamext<-data.table(thirtyperc_mamext)
thirtyperc_mamext$perc<-thirtyperc_mamext$targetmet.0/thirtyperc_mamext$targetmet.1*100

#mammal percentages
mamext<-data.table(cbind(ilup_mamext$perc,thirtypercILUP_mamext$perc,
                         thirtyperc_mamext$perc,thirtyperc_mamext$runnumber))
names(mamext)<-c('ILUP','30%+ILUP','30%','runnumber')
mamext$runnumber<-as.factor(mamext$runnumber)
mamext$feat<-'Mammals'
mamext<-melt(mamext,id.vars = c('feat','runnumber'))


#####Birds#####
birds<-ilup[!ilup$Feature %in% mammals$id,]
birds<-ilup[!ilup$Feature %in% c(1:78),]

#ILUP
ilup_birds<-subset(ilup,ilup$Feature %in% birds$Feature)
ilup_birdsext<-aggregate(targetmet~runnumber,ilup_birds,table)
ilup_birdsext<-data.table(ilup_birdsext)
ilup_birdsext$perc<-ilup_birdsext$targetmet.0/ilup_birdsext$targetmet.1*100
#30% + ILUP
thirtypercILUP_birds<-subset(thirtypercILUP,thirtypercILUP$Feature %in% birds$Feature)
thirtypercILUP_birdsext<-aggregate(targetmet~runnumber,thirtypercILUP_birds,table)
thirtypercILUP_birdsext<-data.table(thirtypercILUP_birdsext)
thirtypercILUP_birdsext$perc<-thirtypercILUP_birdsext$targetmet.0/thirtypercILUP_birdsext$targetmet.1*100
#30%
thirtyperc_birds<-subset(thirtyperc,thirtyperc$Feature %in% birds$Feature)
thirtyperc_birdsext<-aggregate(targetmet~runnumber,thirtyperc_birds,table)
thirtyperc_birdsext<-data.table(thirtyperc_birdsext)
thirtyperc_birdsext$perc<-thirtyperc_birdsext$targetmet.0/thirtyperc_birdsext$targetmet.1*100

#birds percentages
birdsext<-data.table(cbind(ilup_birdsext$perc,thirtypercILUP_birdsext$perc,
                         thirtyperc_birdsext$perc,thirtyperc_birdsext$runnumber))
names(birdsext)<-c('ILUP','30%+ILUP','30%','runnumber')
birdsext$runnumber<-as.factor(birdsext$runnumber)
birdsext$feat<-'Birds'
birdsext<-melt(birdsext,id.vars = c('feat','runnumber'))


#####Pastures#####
pastures<-c(27:52)

#ILUP
ilup_pas<-subset(ilup,ilup$Feature %in% pastures)
targetpastbl<-subset(ilup_pas,ilup_pas$runnumber==10)
targetpas<-sum(targetpastbl$target)
ilup_pas<-subset(ilup_pas,ilup_pas$targetmet==0)
ilup_pasext<-aggregate(shortfall~runnumber,ilup_pas,sum)
ilup_pasext$perc<-ilup_pasext$shortfall/targetpas*100
#30%+ILUP
thirtypercILUP_pas<-subset(thirtypercILUP,thirtypercILUP$Feature %in% pastures)
thirtypercILUP_pas<-subset(thirtypercILUP_pas,thirtypercILUP_pas$targetmet==0)
thirtypercILUP_pasext<-aggregate(shortfall~runnumber,thirtypercILUP_pas,sum)
thirtypercILUP_pasext$perc<-thirtypercILUP_pasext$shortfall/targetpas*100
#30% - must calculate the targets manually
thirtyperc_pas<-subset(thirtyperc,thirtyperc$Feature %in% pastures)
thirtyperc_pas<-subset(thirtyperc_pas,thirtyperc_pas$targetmet==0)
thirtyperc_pasext<-aggregate(shortfall~runnumber,thirtyperc_pas,sum)
thirtyperc_pasext$perc<-thirtyperc_pasext$shortfall/targetpas*100

#pastures percentages
pasext<-data.table(cbind(ilup_pasext$perc,thirtypercILUP_pasext$perc,
                           thirtyperc_pasext$perc,thirtypercILUP_pasext$runnumber))
names(pasext)<-c('ILUP','30%+ILUP','30%','runnumber')
pasext$runnumber<-as.factor(pasext$runnumber)
pasext$feat<-'Pastures'
pasext<-melt(pasext,id.vars = c('feat','runnumber'))

#####Crops#####
crops<-c(1:26)

#ILUP
ilup_crops<-subset(ilup,ilup$Feature %in% crops)
targetcropstbl<-subset(ilup_crops,ilup_crops$runnumber==10)
targetcrops<-sum(targetcropstbl$target)
ilup_crops<-subset(ilup_crops,ilup_crops$targetmet==0)
ilup_cropsext<-aggregate(shortfall~runnumber,ilup_crops,sum)
ilup_cropsext$perc<-ilup_cropsext$shortfall/targetcrops*100
#30%+ILUP
thirtypercILUP_crops<-subset(thirtypercILUP,thirtypercILUP$Feature %in% crops)
thirtypercILUP_crops<-subset(thirtypercILUP_crops,thirtypercILUP_crops$targetmet==0)
thirtypercILUP_cropsext<-aggregate(shortfall~runnumber,thirtypercILUP_crops,sum)
thirtypercILUP_cropsext$perc<-thirtypercILUP_cropsext$shortfall/targetcrops*100
#30% - must calculate the targets manually
thirtyperc_crops<-subset(thirtyperc,thirtyperc$Feature %in% crops)
thirtyperc_crops<-subset(thirtyperc_crops,thirtyperc_crops$targetmet==0)
thirtyperc_cropsext<-aggregate(shortfall~runnumber,thirtyperc_crops,sum)
thirtyperc_cropsext$perc<-thirtyperc_cropsext$shortfall/targetcrops*100

#crops percentages
cropsext<-data.table(cbind(ilup_cropsext$perc,thirtypercILUP_cropsext$perc,
                         thirtyperc_cropsext$perc,thirtypercILUP_cropsext$runnumber))
names(cropsext)<-c('ILUP','30%+ILUP','30%','runnumber')
cropsext$runnumber<-as.factor(cropsext$runnumber)
cropsext$feat<-'Crops'
cropsext<-melt(cropsext,id.vars = c('feat','runnumber'))


#####Final tables#####
mampas<-rbind(mamext,pasext)
meanmampas<-aggregate(value~variable+feat,mampas,mean)
meanmampas$type<-'mean'
maxmampas<-aggregate(value~variable+feat,mampas,max)
maxmampas$type<-'max'
minmampas<-aggregate(value~variable+feat,mampas,min)
minmampas$type<-'min'
mampas<-rbind(meanmampas,maxmampas,minmampas)
mampas<-dcast(mampas,...~feat+type,value.var='value')

birdcrops<-rbind(birdsext,cropsext)
meanbirdcrops<-aggregate(value~variable+feat,birdcrops,mean)
meanbirdcrops$type<-'mean'
maxbirdcrops<-aggregate(value~variable+feat,birdcrops,max)
maxbirdcrops$type<-'max'
minbirdcrops<-aggregate(value~variable+feat,birdcrops,min)
minbirdcrops$type<-'min'
birdcrops<-rbind(meanbirdcrops,maxbirdcrops,minbirdcrops)
birdcrops<-dcast(birdcrops,...~feat+type,value.var='value')


#####PLOTS#####
#loadfonts(device="win")
cbp1 <- c("#D55E00","#999999","#0072B2")

#####Mammals and pastures#####
mp<-ggplot(mampas,aes(Mammals_mean,Pastures_mean,col=variable))+
  geom_point(size=4)+
  geom_point(colour = "grey90", size = 2)+
  scale_colour_manual(values = cbp1)+
  xlim(0,20)+
  ylim(0,10)+
  xlab('Mammal species at risk of extinction (%)')+
  ylab('Shortfall pasture area (%)')+
  theme(text=element_text(size=12, family="Arial Rounded MT Bold"),legend.title=element_blank())+
  geom_line() + 
  geom_linerange(aes(ymin = Pastures_min, ymax=  Pastures_max), colour = "black") + 
  geom_linerange(aes(xmin = Mammals_min, xmax=  Mammals_max), colour = "black") 
mp

ggsave(filename = "mampas.jpg", mp,
       width = 5, height = 4, dpi = 300, units = "in", device='jpeg')

#####Birds and crops#####
#Plot birds and crops
bc<-ggplot(birdcrops,aes(Birds_mean,Crops_mean,col=variable))+
  geom_point(size=4)+
  geom_point(colour = "grey90", size = 2)+
  scale_colour_manual(values = cbp1)+
  xlim(0,40)+
  #ylim(0,5)+
  scale_y_continuous(limits=c(0,5),labels = comma)+
  xlab('Bird species at risk of extinction (%)')+
  ylab('Shortfall crop production (%)')+
  theme(text=element_text(size=12, family="Arial Rounded MT Bold"),legend.title=element_blank())+
  geom_line() + 
  geom_linerange(aes(ymin=Crops_min, ymax=Crops_max),colour = "black") + 
  geom_linerange(aes(xmin = Birds_min, xmax=  Birds_max), colour = "black")
bc
ggsave(filename = "birdscrops.jpg", bc,
       width = 5, height = 4, dpi = 300, units = "in", device='jpeg')