library(tidyverse)
library(cowplot)
library(lme4)
library(lmerTest)
library(lattice)
library(DHARMa)
library(emmeans)

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

  

View(test)

 
 
###Data exploration

supp.labs <- c("High Growth", "Low Growth")
names(supp.labs) <- c("1", "2")

Dry_weights_datasheet%>%
  ggplot(.,aes(y=`fat g/m2`, x= as.factor(Age)))+
  geom_boxplot(aes (fill= as.factor(Location)))+
  facet_grid(~Season, labeller = labeller(Season = supp.labs))+
  theme_cowplot()+
  scale_x_discrete(name = "Age (days)", labels = c("13","32","54"))+
  scale_fill_discrete(name = "Location", labels = c("Berth C", "Berth G"))+
  labs(y= bquote('Fat g / m'^2))


ggsave("fat_m2_box.png", dpi =200, bg= "White")



Dry_weights_datasheet%>%
  ggplot(.,aes(y=`fat g/m2`, x= as.factor(Age)))+
  geom_point(aes (colour= as.factor(Location)))+
  facet_grid(~Season, labeller = labeller(Season = supp.labs))+
  theme_cowplot()+
  scale_x_discrete(name = "Age (days)", labels = c("13","32","54"))+
  scale_fill_discrete(name = "Location", labels = c("Berth C", "Berth G"))+
  labs(y= bquote('Fat g / m'^2))


####Data analyis####

##Assumption testing 

shapiro.test((Dry_weights_datasheet$`fat g/m2`))

Dry_weights_datasheet$Location<-as.factor(Dry_weights_datasheet$Location)

sqrt_lmer_fat_m2_numeric1<-lmer(sqrt(`fat g/m2`)~as.factor(Season)*Age2+(1|Round) +(1|Location), data= Dry_weights_datasheet)  


lmer_fat_m2_numeric1<-lmer(`fat g/m2`~as.factor(Season)*Age2+(1|Round) +(1|Location), data= Dry_weights_datasheet)  

lmer_fat_m2_numeric2<-lmer(log(`fat g/m2`)~as.factor(Season)*as.factor(Age2)+(1|Location/Round2), data= Dry_weights_datasheet)  

anova(lmer_fat_m2_numeric1,lmer_fat_m2_numeric2)

warnings()


plot(simulateResiduals(lmer_fat_m2_numeric2))
testDispersion(simulateResiduals(lmer_fat_m2_numeric2))

summary(lmer_fat_m2_numeric2)

plot(lmer_fat_m2_numeric)

plot(log_lmer_fat_m2_numeric1)




###model testing 


plot(simulateResiduals(sqrt_lmer_fat_m2_numeric1))

E1 <- resid(log_lmer_fat_m2_numeric, type = "pearson")
N  <- nrow(Dry_weights_datasheet)
p  <- length(fixef(log_lmer_fat_m2)) + 1
sum(E1^2) / (N - p)


# Apply model validation
E1 <- resid(log_lmer_fat_m2_numeric, type = "pearson") 
F1 <- fitted(log_lmer_fat_m2)


par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = F1,
     y = E1,
     xlab = "Fitted values",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2) 

F1

###overdisperision func

fatweight.Study<-data.frame(Model.F.Res = residuals(log_lmer_fat_m2_numeric)) #extracts the residuals and places them in a new column in our original data table
fatweight.Study$Abs.Model.F.Res <-abs(fatweight.Study$Model.F.Res) #creates a new column with the absolute value of the residuals
fatweight.Study$Model.F.Res2 <- fatweight.Study$Abs.Model.F.Res^2 #squares the absolute values of the residuals to provide the more robust estimate
fatweight.Study$Round<-Dry_weights_datasheet$Round

Levene.Model.F <- lm(Model.F.Res2 ~ Round, data=dryweight.Study) #ANOVA of the squared residuals
anova(Levene.Model.F)

qqmath(lmer_fat_m2_numeric)
qqmath(log_lmer_fat_m2)

plot(fitted(lme1), residuals(lme1),
     xlab = “Fitted Values”, ylab = “Residuals”)
abline(h=0, lty=2)
lines(smooth.spline(fitted(lme1), residuals(lme1)))

###interaction plot
interaction.plot(x.factor = Dry_weights_datasheet$Age2, #x-axis variable
                 trace.factor = Dry_weights_datasheet$Season, #variable for lines
                 response = Dry_weights_datasheet$`fat g/m2`, #y-axis variable
                 fun = mean, #metric to plot
                 ylab = "Fat g/m2",
                 xlab = "Age",
                 col = c("pink", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Season")

##### EMMEANS pairwise testing 

summary(lmer_fat_m2_numeric2)

emms<-emmeans(lmer_fat_m2_numeric2, ~as.factor(Season) * as.factor(Age2))

contrast(emms, interaction = TRUE)

pairs(emms)

?emmeans


######Accumulation


summary(dw.acc.mod)

ggplot(Dry_weights_datasheet, aes(x=Age2, y=`fat g/m2`, colour=as.factor(Season)))+
  geom_point()+
  geom_smooth(aes(Age2, `fat g/m2`),
              method = "glm", method.args = list(family = "gaussian"), se=F)+
  theme_cowplot()
