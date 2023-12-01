#### libraries needed ####
library(ggplot2)
library(egg)
library(lme4)
library(lmerTest)
library(doBy)
##########################


ld.df <- read.csv("HeatWaveData.csv")
names(ld.df)

#### plot ####

f.sum<- summaryBy(WaterPotential_bar ~ Heat_Treatment+Day+PD_MD, data=ld.df,FUN=c(mean,sd,length))
f.sum$sem <- f.sum$WaterPotential_bar.sd/sqrt(f.sum$WaterPotential_bar.length)


ggplot(data = f.sum, aes(x=factor(Heat_Treatment), y=WaterPotential_bar.mean, group = as.factor(PD_MD), color=as.factor(Day)))  +
  geom_line( aes(colour=as.factor(Day)), size = 1.5) +
  geom_errorbar( aes(ymin=WaterPotential_bar.mean-sem, ymax=WaterPotential_bar.mean+sem),  width=.03) +
  geom_point(aes(shape=as.factor(Day)), size = 5)+
  facet_grid(Day~PD_MD)+
  labs(x="", y = "Water Potential (bar)")+
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


HW2





HW1 <- ggplot(data = f.sum.hw1, aes(x=factor(treat), y=WaterPotential_bar.mean, group = Bin, color=Bin))  +
  geom_line( aes(colour=Bin), size = 1.5) +
  geom_errorbar( aes(ymin=WaterPotential_bar.mean-sem, ymax=WaterPotential_bar.mean+sem),  width=.03) +
  geom_point(aes(shape=Bin), size = 5) +
  scale_shape_manual(values=c(15,16, 15,16))+
  scale_x_discrete(limits=c('2','1'), labels=c("2" = "Moderate (40ºC)", "1" = "Extreme (46ºC)")) +
  scale_color_manual(values = c("tomato", "cornflowerblue")) +
  theme_bw() + # remove grey background 
  ylim(0.4,1.6) +
  theme(panel.grid.major = element_blank()) 
HW1
HW2 <- ggplot(data = f.sum.hw2, aes(x=factor(treat), y=WaterPotential_bar.mean, group = Bin, color=Bin))  +
  geom_line( aes(colour=Bin), size = 1.5) +
  geom_errorbar( aes(ymin=WaterPotential_bar.mean-sem, ymax=WaterPotential_bar.mean+sem),  width=.03) +
  geom_point(aes(shape=Bin), size = 5) +
  scale_shape_manual(values=c(15,16, 15,16))+
  scale_x_discrete(limits=c('2','1'), labels=c("3" = "Moderate (40ºC)", "2" = "Extreme (46ºC)")) +
  scale_color_manual(values = c("tomato", "cornflowerblue")) +
  theme_bw() + # remove grey background 
  ylim(0.4,1.6) +
  theme(panel.grid.major = element_blank()) 
HW2

grid.arrange(HW1,HW2,ncol=2)