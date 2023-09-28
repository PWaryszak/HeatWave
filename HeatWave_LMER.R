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

#FINAL DATA=========
heat <-read.csv("Heat_Freeze_CSM_DATA.csv")
dim(heat)#141  31 - these data contain Block 8 data that has been cut out from Master as Temp-Loggers malfunctioned.
#OR - USE THE FOLLOWING FILE:
heat <- read_excel("MASTER_HeatWave_Data.xlsx", sheet = "Heat_Freeze_CSM_DATA") #Get clean CSM
dim(heat) #127  31

names(heat)
#Columns explained:
#CSM - cumulative stress measure
#PD = PreDawn Potential and MD = MidDay Potential,
unique(heat$PD_MD)  #NOTE: MD is missing because the heat-essays were carried out after PD only.
#RWC = relative water content = (Fresh.wt - Dry.wt) / (Turgid wt - Dry.wt) * 100
#LDMC = Leaf dry matter content = Dry wt / Turgid wt
unique(heat$Genus) #6 focus plants were: #"Aciphylla"     "Carex"         "Grevillea"     "Leptorhynchos" "Pimelea"       "Poa"  

#Create Life Form column and turn dots into shapes based on Life Formn:
unique(heat$Genus)#"Aciphylla" and Leptorynchos = Forbs |   "Carex"&,"Poa" = Grass        "Pimelea" ,"Grevillea" = shrub
heat$Forms <- ifelse(heat$Genus == "Aciphylla" | heat$Genus == "Leptorynchos", "Forb", heat$Genus  )
heat$Form2 <- ifelse(heat$Forms == "Carex"| heat$Genus == "Poa", "Grass", heat$Forms  )
heat$Form <- ifelse(heat$Form2 == "Pimelea" | heat$Genus == "Grevillea", "Shrub", heat$Form2  )
heat$Form <- as.factor(heat$Form)
heat$LT50_Freeze <- as.numeric(as.character((heat$LT50_Freeze)))

#Filtering out Day 0 as there were no heat treatment applied on this day.
heat2 <- heat  %>% filter(Day != 0)




#Stats Table LT50_Heat & LT50_Freeze :==========
mod2 <-( lmer(LT50_Heat ~ Treatment+ as.factor(Day) + (1|Genus)+(1|Block), data=heat2))
tab_model(mod2,show.se = TRUE)
tab_model(mod2,show.icc = FALSE,show.stat =T,show.re.var=F) #Simpler table

mod3 <- lmer(as.numeric(LT50_Freeze) ~ Treatment + as.factor(Day)  + (1|Genus) + (1|Block), data=heat2,na.action = na.omit)
tab_model(mod3,show.se = TRUE)
tab_model(mod3,show.icc = FALSE,show.stat =T,show.re.var=F) #Simpler table

#Stats Table of LT50s:
tab_model(mod2,mod3)

plot(resid(mod2)) #nice homogeneity
plot(resid(mod3)) # one outlier but otherwise nice nice homogeneity


#PLOT Table LT50_Heat & LT50_Freeze :==========

h <- ggplot(data = heat[ heat$Day != 0,], aes(x = as.factor(Day), y = LT50_Heat, fill=Treatment)) +
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(aes(shape = Form),size=3)+
  scale_fill_manual(values = c('#fee0d2','#de2d26'))+
  labs (shape = "Life Form: ",fill= "Treatment: ", x = "Heat Day", y = (bquote(LT50~Heat ~Tolerance~("\u00B0"~C)))) +
  ggtitle("a)")+
  theme_bw()+
  theme(axis.text.y=element_text(size=24 , color = "black"),
        axis.text.x = element_text(size = 24,  color = "black"),
        axis.title.y=element_text(size=24),
        axis.title.x=element_text(size=24),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        panel.grid.minor.x = element_blank())

h


f<-ggplot(data = heat[ heat$Day != 0,], aes(x = as.factor(Day), y = as.numeric(LT50_Freeze), fill=Treatment)) +
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(aes(shape = Form),size=3)+
  scale_fill_manual(values = c('#deebf7','#3182bd'))+
  labs (shape = "Life Form: ",fill= "Treatment: ", x = "Heat Day", y = (bquote(LT50~Heat ~Tolerance~("\u00B0"~C)))) +
  ggtitle("b)")+
  # guides(shape="none")+
  theme_bw()+
  theme(axis.text.y=element_text(size=24 , color = "black"),
        axis.text.x = element_text(size = 24,  color = "black"),
        axis.title.y=element_text(size=24),
        axis.title.x=element_text(size=24),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        panel.grid.minor.x = element_blank())

f

plot2 <- grid.arrange(h,f, ncol = 2)
plot2
#ggsave(plot2 , dpi=600, width = 18, height = 12, filename = "Heatwave_Heat_Freeze2.png")

max_freeze <- heat2 [which.min(heat2$LT50_Freeze),c("SampleID", "LT50_Freeze")]
max_freeze


#Stats Table LT50_Heat +Form :========
heat2$Form <- factor(heat2$Form, levels = c("Grass","Shrub","Forb"))
mod3 <- lmer(LT50_Heat ~ Treatment +  as.factor(Day) + Form +(1|Genus)+(1|Block), data=heat2)
tab_model(mod3)
tab_model(mod3,show.icc = FALSE,show.stat =T,show.re.var=F) #Simpler table

#Stats Table LT50_Freeze +Form :
mod3 <- lmer(LT50_Freeze ~ Treatment + as.factor(Day) + (1|Genus) + (1|Block), data=heat2,na.action = na.omit)
tab_model(mod3,show.icc = FALSE,show.stat =T,show.re.var=F) #Simpler table

mod3 <- lmer(LT50_Freeze ~ Treatment +as.factor(Day) + Form + (1|Genus) + (1|Block), data=heat2,na.action = na.omit)
tab_model(mod3)
tab_model(mod3,show.icc = FALSE,show.stat =T,show.re.var=F) #Simpler table


#RWC Stats:===========
#The heat treatment was applied on Day 0 after Virginia took  midday readings -
#we waited until then before switching on the heating. 
#So the Day 0 has no matching response in heat treatment- we decided to remove  it prior analysis:
#hence: data = heat[ heat$Day != 0,])

#Stats Table - 
Model_RWC <-( lmer(RWC ~ Treatment + as.factor(Day) + (1|Genus)+(1|Block), data=heat[ heat$Day != 0,] ))
tab_model(Model_RWC,show.icc = FALSE,show.stat =T,show.re.var=F)
plot(resid(Model_RWC)) #test for homogeneity of residuals.  good.
summary(Model_RWC)
#            Estimate  Std. Error   df       t value Pr(>|t|) 
#Intercept) 48.580898   0.725846 16.686674  66.930   <2e-16 ***
#RWC         -0.008223   0.005547 94.005676  -1.482    0.142 

#Plot Day/Genus versus Relative Water Content (%):
ggplot(data = heat[ heat$Day != 0,], aes(x= as.factor(Day),y=RWC, fill=Treatment )) + #, color= AreaType
  geom_boxplot(outlier.shape = NA)+
  facet_wrap(.~Genus)+
  labs(x = "Day",y = "Relative Water Content", fill= "Treatment: ")+
  scale_fill_manual(values = c('#fee0d2','#de2d26'))+
  theme_classic()+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "top",               #c(0.2, 0.5)
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text=element_text(size=16,face = "italic"))

#ggsave(file= "IAVS2023_RWC_Genus_Plot.jpg", width = 8, height = 6)

#LDMC Stats:===========
Model_LDMC <-( lmer(LDMC ~ Treatment + as.factor(Day) + (1|Genus)+(1|Block), data=heat[ heat$Day != 0,] ))
tab_model(Model_LDMC,show.icc = FALSE,show.stat =T,show.re.var=F)
plot(resid(Model_LDMC)) #test for homogeneity of residuals.  good.
summary(Model_LDMC)


#Table LDMC + RWC  for paper: =====
tab_model(Model_RWC, Model_LDMC)

#Soil Moisture =========
#Stats Table:
Model_SoilMoisture_percent <-( lmer(SoilMoisture_percent ~ Treatment * as.factor(Day)  +(1|Genus)+(1|Block), data=heat2))
tab_model(Model_SoilMoisture_percent)
summary(Model_SoilMoisture_percent)
#                          Estimate Std. Error df    t value Pr(>|t|)    
#(Intercept)               38.379      1.126  12.768  34.078 6.42e-14 ***
# Heat_TreatmentTreatment   -5.168     1.159 171.160  -4.458 1.49e-05 ***

mod4 <-( lmer(LT50_Heat ~  as.factor(Day) +Genus+(1|Block), data=heat2))
tab_model(mod4)
Genus [Carex]	3.12
Genus [Poa]	2.45
Genus [Grevillea]	2.31
Genus [Pimelea]	1.02
Aciphylla (Intercept)	46.90
Genus [Leptorynchos]	-0.07

summary(lm(SoilMoisture_percent ~ as.factor(Day), data = heat))
  
#Focus on Poa and soil moisture:
ggplot(data = heat, aes(x= as.factor(Day), y=SoilMoisture_percent )) + #, color= AreaType
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(size=2,aes( color = Genus))+
  #facet_grid(Genus~.)+
  labs(x = "Field Heat Wave (Days)",y = "Soil Moisture (%)", color = "Genus: ")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = c(.5,.8),               #c(0.2, 0.5)
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.key = element_rect( fill = "white", color = "black"),
        legend.box.background = element_rect(fill = "yellow"),
        legend.box.margin = margin(6, 6, 6, 6),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 17, face = "bold", vjust = 0.5))


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


#Soil moisture versus WP
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



#Heat DATA======
#Filtering out Daty 0
heat2 <- heat  %>% filter(Day != 0)
heat2$LT50_Freeze <- as.numeric(as.character(heat2$LT50_Freeze))

#LT50_versus_RWC PLOT=========
heat$RelativeWaterContent <- ((heat$FreshWeight_g - heat $DryWeight_g)/ (heat $TurgidWeight_g-heat$DryWeight_g)) *100
heat2 <- heat %>% filter(Day != 0)

#For James King:
##RelativeWaterContent differs from RWC computed by Virginia 
#CONFIRM WITH VIRGINIA !!!!!!!!!!
summary( lmer(LT50_Heat ~ SoilMoisture_percent +(1|Genus)+(1|Block), data=heat2))#-0.05517 
summary( lmer(LT50_Freeze ~ SoilMoisture_percent +(1|Genus)+(1|Block), data=heat2))#-0.05517 


summary( lmer(LT50_Heat ~ LDMC +(1|Genus)+(1|Block), data=heat2)) #No!
summary( lmer(LT50_Heat ~ WaterPotential_Mpa +(1|Genus)+(1|Block), data=heat2)) #No!
summary( lmer(LT50_Heat ~ WaterPotential_bar +(1|Genus)+(1|Block), data=heat2)) #No!
summary( lmer(LT50_Heat ~ RWC +(1|Genus)+(1|Block), data=heat2))#No


summary( lmer(LT50_Heat ~ RelativeWaterContent +(1|Genus)+(1|Block), data=heat))
RWC_Model <-  lmer(LT50_Heat ~ RelativeWaterContent +(1|Genus)+(1|Block), data=heat)
tab_model(RWC_Model)

summary( lmer(LT50_Heat ~ RWC                *as.factor(Day)  +(1|Genus)+(1|Block), data=heat2))
summary( lmer(LT50_Heat ~ WaterPotential_Mpa *as.factor(Day)  +(1|Genus)+(1|Block), data=heat2))

library(ggpmisc)

ggplot(data = heat2, aes(x=RelativeWaterContent , y=LT50_Heat  )) + #, color= AreaType
  geom_point(size=2,aes( color =Genus))+
  stat_smooth(method = "lm", col = "black")+
  
  facet_grid(Day~Treatment)+
  labs(x = "RWC",y = "LT50", color = "Genus:  ")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "bottom",               #c(0.2, 0.5)
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 17, face = "bold", vjust = 0.5))+
  
  stat_fit_glance(method = "lm",
                  label.x =  c(0.5,0),
                  method.args = list(formula = y ~ x),
                  mapping = aes(label = sprintf('R^2~"="~%.3f~~italic(P)~"="~%.2g',
                                                stat(r.squared), stat(p.value))),parse = TRUE)


#Water Potential Stats =====
heat2 <- heat %>% filter(Day != 0)

summary( lmer(-WaterPotential_bar ~ Treatment+ as.factor(Day)+SoilMoisture_percent +(1|Genus)+(1|Block), data=heat2))
##############Estimate Std. Error  df          t value Pr(>|t|)    
#(Intercept)               95.020      3.118   9.213  30.471 1.45e-10 ***
# Heat_Treatment  -7.385      2.565 233.000  -2.879  0.00436 ** 

#Stats Table:
model_WP <-( lmer(-WaterPotential_Mpa ~ Treatment  + as.factor(Day) + (1|Genus)+(1|Block), data=heat2))
tab_model(model_WP)
tab_model(model_WP,show.icc = FALSE,show.stat =T,show.re.var=F) #Simpler table


#Plot Day/Genus versus Water Potential (%):
ggplot(data = heat[ heat$Day != 0,], aes(x= as.factor(Day),y=-WaterPotential_Mpa, fill=Treatment )) + #, color= AreaType
  geom_boxplot()+
  facet_wrap(.~Genus)+
  labs(x = "Day",y = "Water Potential (MPa)", fill= "Treatment: ")+
    scale_fill_manual(values = c('#fee0d2','#de2d26'))+

  theme_classic()+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "top",               #c(0.2, 0.5)
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 17, face = "bold", vjust = 0.5))

#ggsave(file= "IAVS2023_WaterPotential_Heat_Genus_Plot.jpg", width = 8, height = 6)


#Plot Day/Genus versus Lethal Temperature 50 (%):
ggplot(data = heat[ heat$Day != 0,], aes(x= as.factor(Day),y=LT50_Heat, fill=Treatment )) + #, color= AreaType
  geom_boxplot()+
  facet_wrap(.~Genus)+
  labs(x = "Day",y = "LT50 Heat Tolerance", fill= "Treatment: ")+
    scale_fill_manual(values = c('#fee0d2','#de2d26'))+

  theme_classic()+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "top",               #c(0.2, 0.5)
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 17, face = "bold", vjust = 0.5))

#ggsave(file= "IAVS2023_LT50_Heat_Genus_Plot.jpg", width = 8, height = 6)

#LT50 Stats =====
heat2 <- heat %>% filter(Day != 0)
heat2$WP <- -heat2$WaterPotential_bar

summary( lmer(LT50 ~ Treatment *  as.factor(Day) +(1|Genus)+(1|Block), data=heat2))
##############Estimate Std. Error  df          t value Pr(>|t|)    
#(Intercept)               95.020      3.118   9.213  30.471 1.45e-10 ***
# Heat_Treatment  -7.385      2.565 233.000  -2.879  0.00436 ** 

#Stats Table:
mod2 <-( lmer(LT50 ~ Treatment+ as.factor(Day) + WaterPotential_bar+ (1|Genus)+(1|Block), data=heat2))
tab_model(mod2)
tab_model(mod2,show.icc = FALSE,show.stat =T,show.re.var=F) #Simpler table

#Plot Day/Genus versus Lethal Temperature 50 (%):
ggplot(data = heat[ heat$Day != 0,], aes(x= as.factor(Day),y=LT50_Heat, fill=Treatment )) + #, color= AreaType
  geom_boxplot()+
  #facet_wrap(.~Genus)+
  labs(x = "Day",y = "LT50", fill= "Treatment: ")+
  scale_fill_manual(values = c("#2ca25f", "red"))+
  theme_classic()+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "top",               #c(0.2, 0.5)
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 17, face = "bold", vjust = 0.5))



#LT50 versus PreDawn Water Potential PLOT=========
heat3 <- heat2 %>%
  filter(PD_MD == "PD")

#Plot Day/Genus versus  WaterPotential_Mpa:
ggplot(data = heat[ heat$Day != 0,], aes(x= as.factor(Day),y=-WaterPotential_Mpa, fill=Treatment )) + #, color= AreaType
  geom_boxplot(outlier.shape = NA)+
  facet_wrap(.~Genus)+
  labs(x = "Day",y = "Water Potential (MPa)", fill= "Treatment: ")+
  scale_fill_manual(values = c("darkgreen", "pink"))+
  theme_classic()+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "top",               #c(0.2, 0.5)
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 17, face = "bold", vjust = 0.5))

ggsave(file= "WP_Genus_Plot.jpg", width = 8, height = 6)


ggplot(data = heat3, aes(x= -WaterPotential_bar, y=LT50 )) + #, color= AreaType
  geom_point(size=2,aes( color =Genus))+
  stat_smooth(method = "lm", col = "black")+
  
  facet_grid(Day~Treatment)+
  labs(x = "Water Potential",y = "LT50", color = "Genus name:  ")+
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


#LT50 versus LDMC PLOT=========
ggplot(data = heat2, aes(x= WaterPotential_bar, y=LDMC )) + #, color= AreaType
  geom_point(size=2,aes( color =Genus))+
  stat_smooth(method = "lm", col = "black")+
  
  facet_grid(Day~Treatment)+
  labs(x = "Water Potential",y = "LDMC", color = "Genus name:  ")+
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


#CSM Model =========
#Stats Table:
names(heat)
unique(heat$Treatment)#"control"  "heatwave"

csm_model <-( lmer(LT50 ~ Treatment + da_con_csm +as.factor(Day_CSM)+(1|Genus)+(1|Block), data=heat))
tab_model(csm_model)

H1 <- filter(heat, Treatment == "heatwave")
summary(heat$da_con_csm)
summary(H1$da_con_csm)

csm_model_heat <-( lm(LT50 ~  da_con_csm, data=H1))
tab_model(csm_model_heat)

csm_model_heat <-( lm( da_con_csm~ LT50 , data=H1))
tab_model(csm_model_heat)
plot(csm_model_heat)


mod4 <-( lm(LT50 ~  da_con_csm, data=heat[heat$Day==6,]))
tab_model(mod4)
plot(heat$da_con_csm, heat$Day)

mod5 <-( lm(da_con_csm ~  as.factor(Day_CSM), data=heat))
tab_model(mod5)
summary(mod5)

mod6 <-( lm(da_con_csm ~  as.factor(Day_CSM), data=heat))
tab_model(mod5)


#PLOT LT50 for IAVS 2023==========
ggplot(heat, aes(x = as.factor(Day), y = LT50, color=as.factor(Day))) +
  geom_boxplot()+
  geom_jitter()+
  scale_y_continuous(limits = c(40,60))+
  labs (x = "Heat Wave (days)", y = "LT50") +
  ggtitle("Field Heat Wave")+
  facet_grid(.~Treatment)+
  
  theme_bw()+
  theme(axis.text.y=element_text(size=24 , color = "black"),
        axis.text.x = element_text(size = 24,  color = "black"),
        axis.title.y=element_text(size=24),
        axis.title.x=element_text(size=24),
        legend.position = "none",
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(color = "black", size = 24, face = "bold",hjust=0.5),
        plot.subtitle = element_text(color = "red"),
        strip.text=element_text(size=20))

#PLOT CSM for IAVS 2023==========
names(heat)

ggplot(heat[heat$Day != 0,], aes(x = as.factor(Day), y = da_con_csm)) +
  geom_boxplot()+
  geom_jitter(aes(color = ifelse(Day == 6, "red", "blue"),size=2)) +
  scale_color_identity()+
  labs (x = "Days", y = "Comulative Strees Measure") +
  ggtitle("Field Heatwave Experiment")+
  facet_grid(.~Treatment)+
  
  theme_classic()+
  theme(axis.text.y=element_text(size=4 , color = "black"),
        axis.text.x = element_text(size = 24,  color = "black"),
        axis.title.y=element_text(size=24),
        axis.title.x=element_text(size=24),
        legend.position = "none",
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(color = "black", size = 24, face = "bold",hjust=0.5),
        plot.subtitle = element_text(color = "red"),
        strip.text=element_text(size=20))


#PLOT CSM~LT50 for IAVS 2023==========
names(heat)
pairs(~ LT50 + da_con_csm + Day, data = heat)


panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  Cor <- abs(cor(x, y)) # Remove abs function if desired
  txt <- paste0(prefix, format(c(Cor, 0.123456789), digits = digits)[1])
  if(missing(cex.cor)) {
    cex.cor <- 0.4 / strwidth(txt)
  }
  text(0.5, 0.5, txt,
       cex = 1 + cex.cor * Cor) # Resize the text by level of correlation
}

# PLOT correlation matrix of LT50~CSM=========
heat$CSM <- heat$da_con_csm
heat$SM <- heat$SoilMoisture_percent

H3<- select(heat, LT50 , CSM)
pairs(H3, 
      upper.panel = panel.cor,    # Correlation panel
      lower.panel = panel.smooth) # Background color of the symbol (pch 21 to 25)


# PLOT correlation matrix of LT50~LDMC=========
H4<- select(heat, LT50_Heat , LT50_Freeze)
pairs(H4,
      upper.panel = panel.cor,    # Correlation panel
      lower.panel = panel.smooth) # Background color of the symbol (pch 21 to 25)

# PLOT correlation matrix of LT50~SoilMoisture_percent=========
H5<- select(heat2, LT50_Heat,SoilMoisture_percent)
pairs(H5,
      upper.panel = panel.cor,    # Correlation panel
      lower.panel = panel.smooth) # Background color of the symbol (pch 21 to 25)


# PLOT correlation matrix of WaterPotential_Mpa~SoilMoisture_percent=========
H6<- select(heat, WaterPotential_Mpa, SoilMoisture_percent)
pairs(H6,
      upper.panel = panel.cor,    # Correlation panel
      lower.panel = panel.smooth) # Background color of the symbol (pch 21 to 25)



# Correlation in absolute terms:============
#install.packages("corrplot")


data <- heat2 %>% select( LT50_Heat, LDMC,RWC,SoilMoisture_percent) %>%
  rename(SM= SoilMoisture_percent)

corrplot.mixed(cor(data), 
               upper.col = c("blue","red"),
               lower.col = c("blue","red"), 
               lower = "number", 
               upper = "circle",
               tl.col = "black")
?cor.test
cor.test(~ LT50_Heat + RWC, data = data)
cor.test(~ LT50_Heat +LDMC, data = data)#p-value = 0.000968
cor.test(~ LT50_Heat + SoilMoisture_percent, data = data)#p-value = 0.0001201
cor.test(~ LT50_Heat + da_con_csm, data = heat_good)#p-value = 0.01617
cor.test(~ SoilMoisture_percent +LDMC, data = data)#p-value = 0.000968


heat_good <- filter(heat, Block < 8) #Block 8 did not work 
heat_good $ da_con_csm <- as.numeric(heat_good $ da_con_csm)
summary(lm(LT50_Heat~LDMC, data = heat_good))
summary(lm(LT50_Heat~Treatment, data = heat_good))
summary(lm(LT50_Heat~RWC, data = heat_good))
summary(lm(SoilMoisture_percent~LDMC, data = heat_good))

#Nice Correlation Plot========
# Quick display of two cabapilities of GGally, to assess the distribution and correlation of variables 
library(GGally)

data <- heat2 %>% select( LT50_Heat, LT50_Freeze,SoilMoisture_percent, Form) %>%
  rename( Soil_Moisture = SoilMoisture_percent)

LT50_CorPlot<- ggpairs(data, columns = 1:3, ggplot2::aes(colour=Form, shape=Form)) +
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=12),
                 axis.text.y=element_text(size=12),
                 axis.title.x=element_text(size=20),
                 axis.title.y=element_text(size=20),
                 panel.grid.minor.x = element_blank(),
                 strip.text=element_text(size=16))

#ggsave(LT50_CorPlot , dpi=600, width = 13, height = 12, filename = "Heatwave_Heat_Freeze_SoilMoisture_CorrPlot.png")
#WEB: https://r-graph-gallery.com/199-correlation-matrix-with-ggally.htmlhttps://r-graph-gallery.com/199-correlation-matrix-with-ggally.html