library(tidyverse)
library(cowplot)
library(lme4)
#library(Rt)
library(lmerTest)
library(lattice)
library(DHARMa)
library(car)

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

Dry_weights_datasheet$Season<-as.factor(Dry_weights_datasheet$Season)


###New variable for numeric value of ages
Dry_weights_datasheet<-
  Dry_weights_datasheet%>%
    mutate(Age2= recode(Age, "1" =13, "2" =32, "3"=54))

Dry_weights_datasheet<-Dry_weights_datasheet%>%
  mutate(
    Round2 =case_when(Dry_weights_datasheet$Season == 1 ~ Dry_weights_datasheet$Round,
                      Dry_weights_datasheet$Season ==2 ~ Dry_weights_datasheet$Round + Dry_weights_datasheet$Season,
                      TRUE ~ 0))

Dry_weights_datasheet$Age2<-as.factor(Dry_weights_datasheet$Age2)

###Data exploration

supp.labs <- c("High Growth", "Low Growth")
names(supp.labs) <- c("1", "2")

Dry_weights_datasheet%>%
  ggplot(.,aes(y=`Dry weight (g)`, x= as.factor(Age)))+
        geom_boxplot(aes (fill= as.factor(Location)))+
        facet_grid(~Season, labeller = labeller(Season = supp.labs))+
        theme_cowplot()+
        scale_x_discrete(name = "Age (days)", labels = c("13","32","54"))+
        scale_fill_discrete(name = "Location", labels = c("Berth C", "Berth G"))


ggsave("Dry_weight_box.png", dpi =200, bg= "White")

####Data analyis####

##Assumption testing 

hist(log(Dry_weights_datasheet$`Dry weight (g)`)) # log transformed for normality



log_lmer_dry<-lmer(log(`Dry weight (g)`)~Season*Age2+(1|Location/Round), data= Dry_weights_datasheet)  
log_lmer_dry2<-lmer(log(`Dry weight (g)`)~as.factor(Season)*as.factor(Age2)+(1|Location/Round2), data= Dry_weights_datasheet)

summary(log_lmer_dry)
anova(log_lmer_dry,log_lmer_dry2) 


plot(simulateResiduals(log_lmer_dry))
testDispersion(simulateResiduals(log_lmer_dry))

###model testing 

  
dryweight.Study<-data.frame(Model.F.Res = residuals(log_lmer_dry)) #extracts the residuals and places them in a new column in our original data table
dryweight.Study$Abs.Model.F.Res <-abs(dryweight.Study$Model.F.Res) #creates a new column with the absolute value of the residuals
dryweight.Study$Model.F.Res2 <- dryweight.Study$Abs.Model.F.Res^2 #squares the absolute values of the residuals to provide the more robust estimate
dryweight.Study$Round<-Dry_weights_datasheet$Round

Levene.Model.F <- lm(Model.F.Res2 ~ Round, data=dryweight.Study) #ANOVA of the squared residuals
anova(Levene.Model.F)

qqmath(log_lmer_dry,id=0.05)

citation("DHARMa")
citation("lme4")

###interaction plot
install.packages("interactions")
library(jtools)
library(interactions)
library(vctrs)


cat_plot(log_lmer_dry, pred = as.factor(Season), modx =as.factor(Age2),data = as.data.frame(Dry_weights_datasheet))

interaction.plot(x.factor = Dry_weights_datasheet$Age, #x-axis variable
                 trace.factor = Dry_weights_datasheet$Season, #variable for lines
                 response = Dry_weights_datasheet$`Dry weight (g)`, #y-axis variable
                 fun = mean, #metric to plot
                 ylab = "Dry Weight (g)",
                 xlab = "Season",
                 col = c("pink", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Season")


##### EMMEANS pairwise testing 

summary(log_lmer_dry)

emms_dry<-emmeans(log_lmer_dry, ~as.factor(Age2)|as.factor(Season))

contrast(emms_dry, interaction = TRUE)

pairs(emms_dry)
eff_size(emms_dry, sigma = sigma(log_lmer_dry), edf = df.residual(log_lmer_dry))


?emmeans
