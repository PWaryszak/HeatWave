#LOAD Hotham heat  DATA (Heat Wave Experiment):=========
library(tidyverse)
library(lme4)
library(lmerTest)
library(sjPlot)
library(sjstats)
library(sjmisc)
library(readxl)
library(corrplot)
library(gridExtra)
library(ggpmisc)

#LOAD DATA Updated with MD data=========
# MD (Midday Potential) data added to heat master spreadsheet. Wrangled in HeatWave_LMER (line 0-51) and then moved to "MASTER_HeatWave_Data.xlsx", sheet = "Heat_Freeze_CSM_DATA") 
heat <- read_excel("MASTER_HeatWave_Data.xlsx", sheet = "Heat_Freeze_CSM_DATA") #Get clean CSM
dim(heat)#228  33
heat2 <- heat  %>% filter(Day != 0) #Filtering out Day-0 as there were no heat treatment applied on this day.
dim(heat) - dim(heat2) #Losing 60  rows of DAY-0 data
heat$SoilMoisture_percent <- as.numeric(as.character(heat$SoilMoisture_percent))


#soil moisture=============
#STATS TABLE SPLIT between PD and MD:
Model_RWC_PD_only <-( lmer(SoilMoisture_percent ~ RWC  + Treatment+ (1|Genus)+(1|Block), data=heat[ heat$PD_MD == "PD",] ))
tab_model(Model_RWC_PD_only,show.icc = FALSE,show.stat =T,show.re.var=F, title = "PD ")

Model_RWC_MD_only <-( lmer(SoilMoisture_percent ~ RWC   + Treatment+ (1|Genus)+(1|Block), data=heat[ heat$PD_MD == "MD",] ))
tab_model(Model_RWC_MD_only,show.icc = FALSE,show.stat =T,show.re.var=F,  title = "MD ")

#Combine Soil moisture under MD and PD together
tab_model(Model_RWC_MD_only,Model_RWC_PD_only,show.icc = FALSE,show.stat =T,show.re.var=F, 
  title = "||||||||||||||||||||||Mid-day Water Potential     ||||||||||||||||||||||                Pre-dawn Water Potential||||||||||||||||||||||")
  
  
  
#Stats Table without Day 0 and PD+MD analysed together:
Model_SoilMoisture_percent <-( lmer(SoilMoisture_percent ~ Treatment * as.factor(Day)  +(1|Genus)+(1|Block), data=heat2))
tab_model(Model_SoilMoisture_percent)
summary(Model_SoilMoisture_percent)
#                          Estimate Std. Error df    t value Pr(>|t|)    
#(Intercept)               38.379      1.126  12.768  34.078 6.42e-14 ***
# Heat_TreatmentTreatment   -5.168     1.159 171.160  -4.458 1.49e-05 ***

#Genus Effect on LT50===========
#Heat:
mod4 <-( lmer(LT50_Heat ~  Treatment + Genus + (1|Block), data=heat2))
tab_model(mod4)
summary(mod4)
  
#Freeze:
heat2$LT50_Freeze <- as.numeric(as.character(heat2$LT50_Freeze))
mod5 <-( lmer(LT50_Freeze ~  Treatment + Form + (1|Block), data=heat2))
tab_model(mod5)
summary(mod4)





#Focus on Poa and soil moisture:
ggplot(data = heat, aes(x= as.factor(Day), y=SoilMoisture_percent )) + #, color= AreaType
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(size=4,aes( color = Genus, shape =Treatment))+
  facet_grid(.~PD_MD)+
  labs(x = "Field Heat Wave (Days)",y = "Soil Moisture (%)", color = "Genus: ")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position =  "top" , #c(.5,.8),               #c(0.2, 0.5)
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.key = element_rect( fill = "white", color = "black"),
        legend.box.background = element_rect(fill = "grey90"),
        legend.box.margin = margin(6, 6, 6, 6),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 17, face = "bold", vjust = 0.5))

ggsave( filename = "SoilMoisture_PDvsMD_Block8in.jpg", width = 8000, height = 5437, units = "px", dpi = 600)

#Focus on Poa and PD
heat$Poa <- ifelse(heat$Genus == "Poa", "Poa", "Shrub")

ggplot(data = heat, aes(x= as.factor(Day), y=WaterPotential_Mpa )) + #, color= AreaType
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(size=2,aes( color = Poa))+
  #facet_grid(Genus~.)+
  labs(x = "Day",y = "PD (MPa)", color = "Plant:  ")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "right",               #c(0.2, 0.5)
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 17, face = "bold", vjust = 0.5))


#PLOT Soil moisture versus WP=========
ggplot(data = heat2, aes(x= -WaterPotential_bar, y=SoilMoisture_percent )) + #, color= AreaType
  geom_point(size=2,aes( color =Genus))+
  stat_smooth(method = "lm", col = "black")+
  
  facet_grid(Day~Treatment)+
  #labs(x = "Water Potential",y = "Soil Moisture (%)", color = "Genus name:  ")+
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


#Soil moisture versus RWC
ggplot(data = heat2, aes(x= RWC, y=SoilMoisture_percent )) + #, color= AreaType
  geom_point(size=2,aes( color =Genus))+
  stat_smooth(method = "lm", col = "black")+
  
  facet_grid(Day~Treatment)+
  #labs(x = "Water Potential",y = "Soil Moisture (%)", color = "Genus name:  ")+
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



#Effect of Treatment on Soil Moisture:==========
#Stats Table without Day 0 and PD+MD analysed together:
Model_SoilMoisture1 <-( lmer(SoilMoisture_percent ~ Treatment +(1|Genus)+(1|Block), data=heat2))
tab_model(Model_SoilMoisture1)
summary(Model_SoilMoisture1)
#                  Estimate Std. Error      df t value Pr(>|t|)    
#(Intercept)         37.190      1.303   8.915  28.533 4.53e-10 ***
#Treatmentheatwave   -3.891      1.119 154.435  -3.477  0.00066 ***

#Effect of Treatment on RWC:==========
#Stats Table without Day 0 and PD+MD analysed together:
Model_RWC1 <-( lmer(RWC ~ Treatment +(1|Genus)+(1|Block), data=heat2))
tab_model(Model_RWC1)
summary(Model_RWC1)
#                  Estimate Std. Error      df t value Pr(>|t|)    
#(Intercept)         93.381      3.694   6.978  25.279 4.03e-08 ***
#Treatmentheatwave   -6.911      2.687 154.214  -2.572   0.0111 *  


#Effect of Treatment on WaterPotential_bar:==========
#Stats Table without Day 0 and PD+MD analysed together:
Model_bar1 <-( lmer(WaterPotential_bar ~ Treatment +(1|Genus)+(1|Block), data=heat2))
tab_model(Model_bar1)
summary(Model_bar1)
#                  Estimate Std. Error       df t value Pr(>|t|)
#(Intercept)         2.3382     0.5416   7.5472   4.317  0.00293
#Treatmentheatwave   3.1893     0.4155 154.9592   7.676 1.72e-12

