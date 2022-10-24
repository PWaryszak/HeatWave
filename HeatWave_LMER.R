#LOAD snow DATA and LIBRARIES for Heat Wave Experiment:=========
library(tidyverse)
library(lme4)
library(lmerTest)
library(broom)
library(sjPlot)
library(sjstats)
library(sjmisc)
library(ggpmisc)

#Load snow prepared and cured in CombineData.R file:
#setwd("C:/Users/BlueCarbon/Documents/00DeakinUni/R/SusannaVenn/HeatWave")
heat <- read.csv("HeatWaveData.csv") 
unique(heat$PreDawn_MidDay)
names(heat)
#Columns explained:
#RWC = relative water content = (Fresh.wt - Dry.wt) / (Turgid wt - Dry.wt) * 100

summary(heat$RWC)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.   
#-21.21   85.45   94.48   90.41  100.00  162.50 

unique(heat$Species) #6 focus plants were: 
#"Aciphylla"     "Carex"         "Grevillea"     "Leptorhynchos" "Pimelea"       "Poa"  

#Entire Data (Day 0, 3, 6) =====
summary( lmer(RWC ~ Heat_Treatment  +(1|Species)+(1|Block), data=heat))
##############Estimate Std. Error  df          t value Pr(>|t|)    
#(Intercept)               95.020      3.118   9.213  30.471 1.45e-10 ***
# Heat_Treatment  -7.385      2.565 233.000  -2.879  0.00436 ** 

#Stats Table:
mod1 <-( lmer(RWC ~ Heat_Treatment  +(1|Species)+(1|Block), data=heat))
tab_model(mod1)

#Plot PD/MD versus Relative Water Content (%):
heat$PreDawn_MidDay <- as.factor(as.character(heat$PreDawn_MidDay))
heat$PreDawn_MidDay <- factor(heat$PreDawn_MidDay, levels = c("PD ", "MD"))

ggplot(data = heat, aes(x= PreDawn_MidDay, y=RWC )) + #, color= AreaType
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(size=2,aes( color =Species))+
  facet_grid(Day~Heat_Treatment)+
  labs(x = "PreDawn vs MidDay",y = "Relative Water Content (%)") + #, color = "Dominant Life Form: "
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



#plot Species versus Day and RWC (%):
ggplot(data = heat, aes(x= Species, y=RWC )) + #, color= AreaType
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(size=2,aes( color =Species))+
  facet_grid(Day~Heat_Treatment)+
  labs(x = "PreDawn vs MidDay",y = "Relative Water Content (%)") + #, color = "Dominant Life Form: "
  theme_bw()+
  theme(axis.text.x = element_text(size = 12, angle=45),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "none",               #c(0.2, 0.5)
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 17, face = "bold", vjust = 0.5))





#Day 3 and 6 only Data =====
heat2 <- heat %>% filter(Day != 0)

summary( lmer(RWC ~ Heat_Treatment  +(1|Species)+(1|Block), data=heat2))
##############Estimate Std. Error  df          t value Pr(>|t|)    
#(Intercept)               95.020      3.118   9.213  30.471 1.45e-10 ***
# Heat_Treatment  -7.385      2.565 233.000  -2.879  0.00436 ** 

#Stats Table:
mod2 <-( lmer(RWC ~ Heat_Treatment  +(1|Species)+(1|Block), data=heat2))
tab_model(mod2)

#plot:
heat2$PreDawn_MidDay <- as.factor(as.character(heat2$PreDawn_MidDay))
heat2$PreDawn_MidDay <- factor(heat2$PreDawn_MidDay, levels = c("PD ", "MD"))

ggplot(data = heat2, aes(x= PreDawn_MidDay, y=RWC )) + #, color= AreaType
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(size=2,aes( color =Species))+
  facet_grid(Day~Heat_Treatment)+
  labs(x = "PreDawn vs MidDay",y = "Relative Water Content (%)") + #, color = "Dominant Life Form: "
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



#Soil Moisture =========
#Stats Table:
mod3 <-( lmer(SoilMoisture_percent ~ Heat_Treatment  +(1|Species)+(1|Block), data=heat2))
tab_model(mod3)
summary(mod3)
#                          Estimate Std. Error df    t value Pr(>|t|)    
#(Intercept)               38.379      1.126  12.768  34.078 6.42e-14 ***
# Heat_TreatmentTreatment   -5.168     1.159 171.160  -4.458 1.49e-05 ***
  
  
ggplot(data = heat, aes(x= PreDawn_MidDay, y=SoilMoisture_percent )) + #, color= AreaType
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(size=2,aes( color =Species))+
  facet_grid(Day~Heat_Treatment)+
  labs(x = "PreDawn vs MidDay",y = "Soil Moisture (%)", color = "Genus name:  ")+
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


#Soil moisture versus WP
ggplot(data = heat, aes(x= WaterPotential_bar, y=SoilMoisture_percent )) + #, color= AreaType
  geom_point(size=2,aes( color =Species))+
  stat_smooth(method = "lm", col = "black")+
  
  facet_grid(Day~Heat_Treatment)+
  labs(x = "Water Potential",y = "Soil Moisture (%)", color = "Genus name:  ")+
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


