#LOAD Libraries of functions needed:============
library(tidyverse)
library(egg)
library(lme4)
library(lmerTest)
library(doBy)
library(ggpmisc)
library(readxl)


#OLD DATA Updated with MD data=========
#Add MD (Midday Potential) data in to heat
heat <-read.csv("Heat_Freeze_CSM_DATA.csv") %>% filter(Block !=8)#- these data contained Block 8 data that has been cut out from Master as the Block 8 heater has died.
dim(heat)#127  31 

h <-  heat %>% select(SampleID, PD_MD, WaterPotential_bar, WaterPotential_Mpa)
h2join <- heat %>% select(-WaterPotential_bar, -WaterPotential_Mpa, -PD_MD)
names(h2join)

PD_MD <- read.csv("HeatWaveData.csv")

p <- PD_MD %>% select(SampleID, PD_MD, WaterPotential_bar) %>% #Select columns to row-bind with h2join
  filter(PD_MD == "MD") %>%
  mutate(WaterPotential_Mpa = WaterPotential_bar/10)

ph <- rbind(h,p)


heat <- left_join(ph, h2join,  by = "SampleID")
dim(heat)#217  34
#Columns explained:
#CSM - cumulative stress measure
#PD = PreDawn Potential and MD = MidDay Potential,
#RWC = relative water content = (Fresh.wt - Dry.wt) / (Turgid wt - Dry.wt) * 100
#LDMC = Leaf dry matter content = Dry wt / Turgid wt
unique(heat$Genus) #6 focus plants were: #"Aciphylla"     "Carex"         "Grevillea"     "Leptorhynchos" "Pimelea"       "Poa"  

#Create Life Form column and turn dots into shapes based on Life Formn:
unique(heat$Genus)#"Aciphylla" and Leptorhynchos = Forbs |   "Carex"&,"Poa" = Grass        "Pimelea" ,"Grevillea" = shrub
heat$Forms <- ifelse(heat$Genus == "Aciphylla" | heat$Genus == "Leptorhynchos", "Forb", heat$Genus  )
heat$Form2 <- ifelse(heat$Forms == "Carex"| heat$Genus == "Poa", "Grass", heat$Forms  )
heat$Form <- ifelse(heat$Form2 == "Pimelea" | heat$Genus == "Grevillea", "Shrub", heat$Form2  )
heat$Form <- as.factor(heat$Form)
heat$LT50_Freeze <- as.numeric(as.character((heat$LT50_Freeze)))

#write.csv(heat,"Update_MASTER_HeatWave_Data.csv" , row.names = F )
#dim(heat)#217  34

#FINAL DATA:==========
heat <- read_excel("MASTER_HeatWave_Data.xlsx", sheet = "Heat_Freeze_CSM_DATA") #Get clean CSM
dim(heat) #217  34



#Plot MD+PD by Day==============
PD_MD <- read.csv("HeatWaveData.csv")
PD_MD$WaterPotential_Mpa = PD_MD$WaterPotential_bar/10
f.sum<- summaryBy(WaterPotential_Mpa~ Treatment+Day+PD_MD, data=PD_MD,FUN=c(mean,sd,length))
f.sum$sem <- f.sum$WaterPotential_Mpa.sd/sqrt(f.sum$WaterPotential_Mpa.length)
#write.table(f.sum, file="SEM.csv")

ggplot(data = f.sum, aes(x=factor(Treatment), y=WaterPotential_Mpa.mean, group = as.factor(PD_MD), color=as.factor(Day)))  +
  geom_line( aes(colour=as.factor(Day)), size = 1.5) +
  geom_errorbar( aes(ymin=WaterPotential_Mpa.mean-sem, ymax=WaterPotential_Mpa.mean+sem),  width=.03) +
  geom_point(aes(shape=as.factor(Day)), size = 5)+
  facet_grid(Day~PD_MD)+
  labs(x="", y = "Water Potential (MPa)")+
  #scale_x_discrete( labels=c("1" = "Control", "2" = "Heatwave")) +
  #scale_color_manual(values = c("tomato", "cornflowerblue")) +
  theme_bw() + # remove grey background 
  theme(axis.text.y=element_text(size=20 , color = "black"),
        axis.text.x = element_text(size = 20,  color = "black"),
        axis.title.y=element_text(size=24),
        axis.title.x=element_text(size=24),
        legend.position = "none",
    strip.text = element_text(size=24 , color = "black"),
    strip.background = element_rect(fill = "white"))

ggsave(file= "HeatWave_WaterPotential_MPa_MD_PD_Day036.jpg", width = 12, height = 7)


#PLOT PD+MD By Species =============
PD_MD <- read.csv("HeatWaveData.csv")
PD_MD$WaterPotential_Mpa = PD_MD$WaterPotential_bar/10
f.sum<- summaryBy(WaterPotential_Mpa ~ Treatment+Day+PD_MD+Genus, data=PD_MD,FUN=c(mean,sd,length))
f.sum$sem <- f.sum$WaterPotential_Mpa.sd/sqrt(f.sum$WaterPotential_Mpa.length)

#PLOT:
ggplot(data = f.sum, aes(x=Treatment, y=WaterPotential_Mpa.mean, group = as.factor(PD_MD), color=as.factor(Treatment)))  +
  geom_line(color="grey50",linetype="dashed") +
  geom_errorbar( aes(ymin=WaterPotential_Mpa.mean-sem, ymax=WaterPotential_Mpa.mean+sem),  width=.03) +
  geom_point(aes(shape=as.factor(Day)), size = 5)+
  facet_grid(Day~PD_MD+Genus)+
  labs(x="", y = "Water Potential (MPa)")+
  #scale_x_discrete( labels=c("1" = "Control", "2" = "Heatwave")) +
  scale_color_manual(values = c( "cornflowerblue","tomato")) +
  theme_bw() + # remove grey background 
  theme(axis.text.y=element_text(size=16 , color = "black"),
        axis.text.x = element_text(size = 16,  color = "black",vjust=0.4,angle = 45),
        axis.title.y=element_text(size=24),
        axis.title.x=element_text(size=24),
        legend.position = "bottom",
    strip.text = element_text(size=12 , color = "black"),
    strip.background = element_rect(fill = "white"))

ggsave(file= "HeatWave_WaterPotential_MPa_AllData_BySpecies.jpg", width = 16, height = 7)


#By Species on Day 6 =============
PD_MD$WaterPotential_MPa = PD_MD$WaterPotential_bar/10
PD_MD_Day6 <- PD_MD %>% filter( Day == "6")

f.sum<- summaryBy(WaterPotential_MPa ~ Treatment+PD_MD+Genus, data=PD_MD,FUN=c(mean,sd,length))
f.sum$sem <- f.sum$WaterPotential_MPa.sd/sqrt(f.sum$WaterPotential_MPa.length)
#write.table(f.sum, file="SEM.csv")

ggplot(data = f.sum, aes(x=factor(Treatment), y=WaterPotential_MPa.mean, group = as.factor(PD_MD), color=as.factor(Genus)))  +
  geom_line( aes(colour=as.factor(Genus)), size = 1) +
  geom_errorbar( aes(ymin=WaterPotential_MPa.mean-sem, ymax=WaterPotential_MPa.mean+sem),  width=.03) +
  geom_point(aes(shape=as.factor(PD_MD)), size = 1)+
  facet_grid(Genus~PD_MD)+
  labs(x="Day 6", y = "Water Potential (MPa)")+
  #scale_x_discrete( labels=c("1" = "Control", "2" = "Heatwave")) +
  #scale_color_manual(values = c("tomato", "cornflowerblue")) +
  theme_classic() + # remove grey background 
  theme(axis.text.y=element_text(size=20 , color = "black"),
        axis.text.x = element_text(size = 20,  color = "black"),
        axis.title.y=element_text(size=24),
        axis.title.x=element_text(size=24),
        legend.position = "none",
    strip.text = element_text(size=16 , color = "black",face="italic" ),
    strip.background = element_rect(fill = "white"))

ggsave(file= "HeatWave_WaterPotential_MPa_Day6.jpg", width = 9, height = 12)

#Pre-Dawn vs LT50_Heat:============
heat3 <- subset (heat, Day == "6" & PD_MD == "PD")

ggplot(data = heat3, aes(x= WaterPotential_Mpa, y=LT50_Heat )) + #, color= AreaType
  geom_point(size=3,aes( color = Genus,  shape=Form))+
  stat_smooth(method = "lm", col = "grey50")+
  
  facet_grid(Day~Treatment)+
  labs(x = "Pre-Dawn Water Potential (MPa)",y = "LT50 Heat Tolerance", color = "Genus")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "right",               #c(0.2, 0.5)
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 17, face = "bold", vjust = 0.5))+
  
  stat_fit_glance(method = "lm",
                  label.x =  c(0.5,0),
                  method.args = list(formula = y ~ x),
                  mapping = aes(label = sprintf('R^2~"="~%.3f~~italic(P)~"="~%.2g',
                                                stat(r.squared), stat(p.value))),parse = TRUE)

ggsave(file= "HeatWave_PD_Day6_Genus_Regression.jpg", width = 12, height = 8)

#Mid-Day v LT50_Heat:===============
heat3 <- heat2 %>%
  filter(PD_MD == "MD")  

ggplot(data = heat3, aes(x= WaterPotential_Mpa, y=LT50_Heat )) + #, color= AreaType
  geom_point(size=3,aes( color = Genus, shape=Form))+
  stat_smooth(method = "lm", col = "grey50")+
  
  facet_grid(Day~Treatment)+
  labs(x = "Mid-Day Water Potential (MPa)",y = "LT50 Heat Tolerance", color = "Genus")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "right",               #c(0.2, 0.5)
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 17, face = "bold", vjust = 0.5))+
  
  stat_fit_glance(method = "lm",
                  label.x =  c(0.5,0),
                  method.args = list(formula = y ~ x),
                  mapping = aes(label = sprintf('R^2~"="~%.3f~~italic(P)~"="~%.2g',
                                                stat(r.squared), stat(p.value))),parse = TRUE)

#Nice PD_MD vs Soil Moisture Correlation Plot========
# Quick display of two cabapilities of GGally, to assess the distribution and correlation of variables 
library(GGally)
heat2 <- heat  %>% filter(Day != 0) #Filtering out Day 0 as there were no heat treatment applied on this day.

data <- heat2 %>% select( SampleID, PD_MD, WaterPotential_Mpa, SoilMoisture_percent, Form) %>%
  rename( Moisture = SoilMoisture_percent) %>%
  spread(PD_MD, WaterPotential_Mpa) %>%
  filter( MD !="NA") %>%
  select(-SampleID) %>% na.omit()

data$Moisture <- as.numeric(data$Moisture)

LT50_CorPlot<- ggpairs(data, columns = c(1,3,4), ggplot2::aes(colour=Form)) +
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=22),
                 axis.text.y=element_text(size=22),
                 axis.title.x=element_text(size=20),
                 axis.title.y=element_text(size=22),
                 panel.grid.minor.x = element_blank(),
                 strip.text=element_text(size=22))
LT50_CorPlot
ggsave(LT50_CorPlot , dpi=600, width = 12, height = 7, filename = "Heatwave_MD_PD_SoilMoisture_CorrelationPlot.png")
