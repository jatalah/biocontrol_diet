library(tidyverse)
library(cowplot)
library(lme4)
library(Rt)
library(lmerTest)
library(lattice)
library(DHARMa)
library(readxl)

### Import data#### skips columns at end where Grant has done some analysis in excel sheet
Dry_weights_datasheet <- read_excel("Dry weights datasheet.xlsx", col_types = c("numeric", "numeric", "numeric", 
                                                                                "numeric", "text", "numeric", "numeric", 
                                                                                "numeric", "numeric", "numeric", 
                                                                                "numeric", "numeric", "numeric", 
                                                                                "numeric", "numeric", "numeric", 
                                                                                "numeric", "numeric", "numeric", 
                                                                                "numeric", "numeric", "numeric", 
                                                                                "skip", "skip", "skip", "skip", "skip", 
                                                                                "skip", "skip", "skip", "skip", "skip", 
                                                                                "skip", "skip", "skip"), n_max = 72)

View(Dry_weights_datasheet)

###Data exploration

supp.labs <- c("High Growth", "Low Growth")
names(supp.labs) <- c("1", "2")

Dry_weights_datasheet%>%
  ggplot(.,aes(y=`protein g/m2`, x= as.factor(Age)))+
  geom_boxplot(aes (fill= as.factor(Location)))+
  facet_grid(~Season, labeller = labeller(Season = supp.labs))+
  theme_cowplot()+
  scale_x_discrete(name = "Age (days)", labels = c("13","32","54"))+
  scale_fill_discrete(name = "Location", labels = c("Berth C", "Berth G"))+
  labs(y= bquote('Protein g / m'^2))


ggsave("protein_m2_box.png", dpi =200, bg= "White")

####Data analyis####

##Assumption testing 

hist(Dry_weights_datasheet$`protein g/m2`)
hist(log(Dry_weights_datasheet$`protein g/m2`))


log_lmer_p_m2_num1<-lmer(log(`protein g/m2`)~as.factor(Season)*as.factor(Age)+(1|Location/Round), data= Dry_weights_datasheet)  
log_lmer_p_m2_num2<-lmer(log(`protein g/m2`)~as.factor(Season)*Age2+(1|Location/Round), data= Dry_weights_datasheet)  


summary(log_lmer_p_m2_num1)

anova(log_lmer_p_m2_num1,log_lmer_p_m2_num2)

plot(log_lmer_p_m2_num2) 


###model testing 
###overdisperision func

plot(simulateResiduals(log_lmer_p_m2_num1))
testDispersion(simulateResiduals(log_lmer_p_m2_num1))

dryweight.Study<-data.frame(Model.F.Res = residuals(log_lmer_dry)) #extracts the residuals and places them in a new column in our original data table
dryweight.Study$Abs.Model.F.Res <-abs(dryweight.Study$Model.F.Res) #creates a new column with the absolute value of the residuals
dryweight.Study$Model.F.Res2 <- dryweight.Study$Abs.Model.F.Res^2 #squares the absolute values of the residuals to provide the more robust estimate
dryweight.Study$Round<-Dry_weights_datasheet$Round

Levene.Model.F <- lm(Model.F.Res2 ~ Round, data=dryweight.Study) #ANOVA of the squared residuals
anova(Levene.Model.F)

qqmath(log_lmer_fat_m2,id=0.05)

###

interaction.plot(x.factor = Dry_weights_datasheet$Age2, #x-axis variable
                 trace.factor = Dry_weights_datasheet$Season, #variable for lines
                 response = Dry_weights_datasheet$`protein g/m2`, #y-axis variable
                 fun = mean, #metric to plot
                 ylab = "Protein g/m2",
                 xlab = "Age",
                 col = c("pink", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Season")

###Emmeans comparision 


summary(lmer_fat_m2_numeric2)

emms_p<-emmeans(log_lmer_p_m2_num1, ~as.factor(Season) * as.factor(Age))

contrast(emms_p, interaction = TRUE)

pairs(emms)



####


delta.table<-Dry_weights_datasheet%>%
  pivot_longer(cols=c(6:22), names_to="Analysis")%>%
  filter(Analysis == "protein g/m2")%>%
  group_by(Season,Round, Location,Replicate)%>%
  mutate(delta=value-lag(value))

#repalce first 6 NAs
  delta.table[c(1:6),8]<-delta.table[c(1:6),7]
  
  delta.table%>%
    ggplot(.,aes(y=`delta`, x= as.factor(Age)))+
    geom_boxplot(aes (fill= as.factor(Location)))+
    facet_grid(~Season, labeller = labeller(Season = supp.labs))+
    theme_cowplot()+
    scale_x_discrete(name = "Age (days)", labels = c("13","32","54"))+
    scale_fill_discrete(name = "Location", labels = c("Berth C", "Berth G"))+
    labs(y= bquote('delta Protein g / m'^2))
  
  ggsave("protein_delta_box.png", dpi =200, bg= "White")
  
  
  Dry_weights_datasheet%>%
      ggplot(.,aes(y=`average daily protein accumulation (g/m2/day)`, x= as.factor(Age)))+
       geom_boxplot(aes (fill= as.factor(Location)))+
       facet_grid(~Season, labeller = labeller(Season = supp.labs))+
       theme_cowplot()+
       scale_x_discrete(name = "Age (days)", labels = c("13","32","54"))+
       scale_fill_discrete(name = "Location", labels = c("Berth C", "Berth G"))+
       labs(y= bquote('accumulation of Protein g / m'^2))

  ggsave("protein_accumulation_box.png", dpi =200, bg= "White")
  