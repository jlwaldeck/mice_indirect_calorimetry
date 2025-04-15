################June 2023 Sable ALRC DAT-BMAL1fx/fx Female Analysis
###########################################Author: Nathan Waldeck
#####################################################June 8, 2023
#################################################################

rm(list = ls())

detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}

detachAllPackages()

#again this thoroughly removes all packages and loaded namespaces except for base packages "base" and "utils" (which is highly not recommended).

rm(list = ls())

library(multcompView)
library(multcomp)
library(plyr)
library(car)
library(emmeans)
library(nlme)
library(ggplot2)
library(rstatix)
library(tidyverse)
library(broom)
library(dplyr)
library(lme4)

superpose.eb <- function (x, y, ebl, ebu = ebl, length = 0.08, ...)
  arrows(x, y + ebu, x, y - ebl, angle = 90, code = 3,
         length = length, ...)

se <- function(x)
  sd(x, na.rm = TRUE)/sqrt(length(x))

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      se = se(x[[col]]))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- plyr::rename(data_sum, c("mean" = varname))
  return(data_sum)
}

# setwd("~/Desktop/OneDrive - Northwestern University/202305_Sable_DATBFX_Females_n16_AL_RC/")
# data <- read.csv("./202305_DATBFX_Females_RC_AL_Sable_n16.csv")

data <- read.csv("C:/Users/nwald/OneDrive/Desktop/202305_Sable_DATBFX_Females_n16_AL_RC/202305_DATBFX_Females_RC_AL_Sable_n16.csv")

data <-
  data %>%
  mutate(Genotype = case_when(Animal == "1" ~ 'BKO',
                             Animal == "2" ~ 'Ctrl',
                             Animal == "3" ~ 'BKO',
                             Animal == "4" ~ 'Ctrl',
                             Animal == "5" ~ 'BKO',
                             Animal == "6" ~ 'Ctrl',
                             Animal == "7" ~ 'BKO',
                             Animal == "8" ~ 'Ctrl',
                             Animal == "9" ~ 'BKO',
                             Animal == "10" ~ 'Ctrl',
                             Animal == "11" ~ 'BKO',
                             Animal == "12" ~ 'Ctrl',
                             Animal == "13" ~ 'BKO',
                             Animal == "14" ~ 'Ctrl',
                             Animal == "15" ~ 'BKO',
                             Animal == "16" ~ 'Ctrl',
                              TRUE ~ 'Low')) 
data <-
  data %>%
  mutate(four_hours = case_when(Cycle >= 40 ~ 'ZT20_ZT24',
                                Cycle >= 32 ~ 'ZT16_ZT20',
                                Cycle >= 24 ~ 'ZT12_ZT16',
                                Cycle >= 16 ~ 'ZT8_ZT12',
                                Cycle >= 8 ~ 'ZT4_ZT8',
                                Cycle >= 1 ~ 'ZT0_ZT4',
                                TRUE ~ 'Low')) 



data <-
  data %>%
  mutate(two_hours = case_when(Cycle >= 44 ~ 'ZT22_ZT24',
                               Cycle >= 40 ~ 'ZT20_ZT22',
                               Cycle >= 36 ~ 'ZT18_ZT20',
                               Cycle >= 32 ~ 'ZT16_ZT18',
                               Cycle >= 28 ~ 'ZT14_ZT16',
                               Cycle >= 24 ~ 'ZT12_ZT14',
                               Cycle >= 20 ~ 'ZT10_ZT12',
                               Cycle >= 16 ~ 'ZT8_ZT10',
                               Cycle >= 12 ~ 'ZT6_ZT8',
                               Cycle >= 8 ~ 'ZT4_ZT6',
                               Cycle >= 4 ~ 'ZT2_ZT4',
                               Cycle >= 1 ~ 'ZT0_ZT2',
                               TRUE ~ 'Low')) 

data <-
  data %>%
  mutate(one_hour = case_when(Cycle >= 47 ~ 'ZT23',
                              Cycle >= 45 ~ 'ZT22',
                              Cycle >= 43 ~ 'ZT21',
                              Cycle >= 41 ~ 'ZT20',
                              Cycle >= 39 ~ 'ZT19',
                              Cycle >= 37 ~ 'ZT18',
                              Cycle >= 35 ~ 'ZT17',
                              Cycle >= 33 ~ 'ZT16',
                              Cycle >= 31 ~ 'ZT15',
                              Cycle >= 29 ~ 'ZT14',
                              Cycle >= 27 ~ 'ZT13',
                              Cycle >= 25 ~ 'ZT12',
                              Cycle >= 23 ~ 'ZT11',
                              Cycle >= 21 ~ 'ZT10',
                              Cycle >= 19 ~ 'ZT9',
                              Cycle >= 17 ~ 'ZT8',
                              Cycle >= 15 ~ 'ZT7',
                              Cycle >= 13 ~ 'ZT6',
                              Cycle >= 11 ~ 'ZT5',
                              Cycle >= 9 ~ 'ZT4',
                              Cycle >= 7 ~ 'ZT3',
                              Cycle >= 5 ~ 'ZT2',
                              Cycle >= 3 ~ 'ZT1',
                              Cycle >= 1 ~ 'ZT0',
                              TRUE ~ 'Low')) 

data <-
  data %>%
  mutate(day_night = case_when(Cycle >= 25 ~ 'Dark',
                               Cycle >= 1 ~ 'Light',
                               TRUE ~ 'Low')) 

cols_1hr <- c("Animal","StartDate","one_hour")

data<- 
  data %>% 
  group_by(across(all_of(cols_1hr))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(UptakeASum_1hr = sum(UptakeA_Sum)) %>%
  ungroup

cols_2hrs <- c("Animal","StartDate","two_hours")

data<- 
  data %>% 
  group_by(across(all_of(cols_2hrs))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(UptakeASum_2hrs = sum(UptakeA_Sum)) %>%
  ungroup

cols_4hrs <- c("Animal","StartDate","four_hours")

data<- 
  data %>% 
  group_by(across(all_of(cols_4hrs))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(UptakeASum_4hrs = sum(UptakeA_Sum)) %>%
  ungroup

cols_LD <- c("Animal","StartDate","day_night")

data<- 
  data %>% 
  group_by(across(all_of(cols_LD))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(UptakeASum_LD = sum(UptakeA_Sum)) %>%
  ungroup

cols_Daily <- c("Animal","StartDate")

data<- 
  data %>% 
  group_by(across(all_of(cols_Daily))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(UptakeASum_Daily = sum(UptakeA_Sum)) %>%
  ungroup

data<- 
  data %>% 
  group_by(across(all_of(cols_1hr))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(AllMeters_1hr = sum(AllMeters)) %>%
  ungroup

data<- 
  data %>% 
  group_by(across(all_of(cols_2hrs))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(AllMeters_2hrs = sum(AllMeters)) %>%
  ungroup

data<- 
  data %>% 
  group_by(across(all_of(cols_4hrs))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(AllMeters_4hrs = sum(AllMeters)) %>%
  ungroup

data<- 
  data %>% 
  group_by(across(all_of(cols_LD))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(AllMeters_LD = sum(AllMeters)) %>%
  ungroup

data<- 
  data %>% 
  group_by(across(all_of(cols_Daily))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(AllMeters_Daily = sum(AllMeters)) %>%
  ungroup

  data<- 
  data %>% 
  group_by(across(all_of(cols_1hr))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(Sleep_pct_1hr = mean(Sleep_pct)) %>%
  ungroup

data<- 
  data %>% 
  group_by(across(all_of(cols_2hrs))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(Sleep_pct_2hrs = mean(Sleep_pct)) %>%
  ungroup

data<-
  data %>%
  group_by(across(all_of(cols_4hrs))) %>%
  mutate(index = cur_group_id()) %>%
  mutate(Sleep_pct_4hrs = mean(Sleep_pct)) %>%
  ungroup

data<-
  data %>%
  group_by(across(all_of(cols_LD))) %>%
  mutate(index = cur_group_id()) %>%
  mutate(Sleep_pct_LD = mean(Sleep_pct)) %>%
  ungroup

data<-
  data %>%
  group_by(across(all_of(cols_Daily))) %>%
  mutate(index = cur_group_id()) %>%
  mutate(Sleep_pct_Daily = mean(Sleep_pct)) %>%
  ungroup

  data<- 
  data %>% 
  group_by(across(all_of(cols_1hr))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(Avg_VO2_1hr = mean(Avg_VO2)) %>%
  ungroup

data<- 
  data %>% 
  group_by(across(all_of(cols_2hrs))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(Avg_VO2_2hrs = mean(Avg_VO2)) %>%
  ungroup

data<- 
  data %>% 
  group_by(across(all_of(cols_4hrs))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(Avg_VO2_4hrs = mean(Avg_VO2)) %>%
  ungroup

data<- 
  data %>% 
  group_by(across(all_of(cols_LD))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(Avg_VO2_LD = mean(Avg_VO2)) %>%
  ungroup

data<- 
  data %>% 
  group_by(across(all_of(cols_Daily))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(Avg_VO2_Daily = mean(Avg_VO2)) %>%
  ungroup

data<- 
  data %>% 
  group_by(across(all_of(cols_1hr))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(Avg_RQ_1hr = mean(Avg_RQ)) %>%
  ungroup

data<- 
  data %>% 
  group_by(across(all_of(cols_2hrs))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(Avg_RQ_2hrs = mean(Avg_RQ)) %>%
  ungroup

data<- 
  data %>% 
  group_by(across(all_of(cols_4hrs))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(Avg_RQ_4hrs = mean(Avg_RQ)) %>%
  ungroup

data<- 
  data %>% 
  group_by(across(all_of(cols_LD))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(Avg_RQ_LD = mean(Avg_RQ)) %>%
  ungroup

data<- 
  data %>% 
  group_by(across(all_of(cols_Daily))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(Avg_RQ_Daily = mean(Avg_RQ)) %>%
  ungroup

data<- 
  data %>% 
  group_by(across(all_of(cols_1hr))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(Avg_EE_1hr = mean(Avg_EE)) %>%
  ungroup

data<- 
  data %>% 
  group_by(across(all_of(cols_2hrs))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(Avg_EE_2hrs = mean(Avg_EE)) %>%
  ungroup

data<- 
  data %>% 
  group_by(across(all_of(cols_4hrs))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(Avg_EE_4hrs = mean(Avg_EE)) %>%
  ungroup

data<- 
  data %>% 
  group_by(across(all_of(cols_LD))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(Avg_EE_LD = mean(Avg_EE)) %>%
  ungroup

data<- 
  data %>% 
  group_by(across(all_of(cols_Daily))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(Avg_EE_Daily = mean(Avg_EE)) %>%
  ungroup

  data<- 
  data %>% 
  group_by(across(all_of(cols_1hr))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(Tot_EE_1hr = sum(Tot_EE)) %>%
  ungroup

data<- 
  data %>% 
  group_by(across(all_of(cols_2hrs))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(Tot_EE_2hrs = sum(Tot_EE)) %>%
  ungroup

data<- 
  data %>% 
  group_by(across(all_of(cols_4hrs))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(Tot_EE_4hrs = sum(Tot_EE)) %>%
  ungroup

data<- 
  data %>% 
  group_by(across(all_of(cols_LD))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(Tot_EE_LD = sum(Tot_EE)) %>%
  ungroup

data<- 
  data %>% 
  group_by(across(all_of(cols_Daily))) %>%
  mutate(index = cur_group_id()) %>% 
  mutate(Tot_EE_Daily = sum(Tot_EE)) %>%
  ungroup

## Run Outlier Check of Animals and Recording Dates Check for Chosen 7 Variables of Interest

FoodIntake_Daily_Means <- aggregate(UptakeASum_Daily ~ Animal:Genotype:StartDate, data = data, FUN = mean)
Activity_Daily_Means <- aggregate(AllMeters_Daily ~ Animal:Genotype:StartDate, data = data, FUN = mean)
Sleep_pct_Daily_Means <- aggregate(Sleep_pct_Daily ~ Animal:Genotype:StartDate, data = data, FUN = mean)
Avg_VO2_Daily_Means <- aggregate(Avg_VO2_Daily ~ Animal:Genotype:StartDate, data = data, FUN = mean)
Avg_RQ_Daily_Means <- aggregate(Avg_RQ_Daily ~ Animal:Genotype:StartDate, data = data, FUN = mean)
Avg_EE_Daily_Means <- aggregate(Avg_EE_Daily ~ Animal:Genotype:StartDate, data = data, FUN = mean)
Tot_EE_Daily_Means <- aggregate(Tot_EE_Daily ~ Animal:Genotype:StartDate, data = data, FUN = mean)

Daily_Means_Matrix <- cbind(FoodIntake_Daily_Means, Activity_Daily_Means[,4], Sleep_pct_Daily_Means[,4], Avg_VO2_Daily_Means[,4], Avg_RQ_Daily_Means[,4], Avg_EE_Daily_Means[,4], Tot_EE_Daily_Means[,4])
colnames(Daily_Means_Matrix) <- c("Animal","Genotype","StartDate","UptakeASum_Daily","AllMeters_Daily","Sleep_pct_Daily","Avg_VO2_Daily","Avg_RQ_Daily","Avg_EE_Daily","Tot_EE_Daily")
write.table(Daily_Means_Matrix, file="./output/Daily_Animal_Means_Matrix.csv", sep=",", row.names = FALSE)



## Exclude acclimation time (User defined)

unique(data$StartDate)

acclimation <- c("5/22/23","5/23/23","5/24/23","5/25/24","5/30/23")

data2 <- data[-which(data$StartDate %in% acclimation),]

data2$Cycle <- as.factor(data2$Cycle)

##############################################################
##### Food Intake Analysis (30 min)####
##############################################################

data_FoodIntake_30mins <- data_summary(data2, varname = "UptakeA_Sum", groupnames = c("Cycle", "Genotype")) 

  

pdf(file = "./output/202305_DATBFX_Females_LD_Sable_RC_AL_FoodIntake_30mins_Line_Plot.pdf", height = 5, width = 7) 

  

opar <- theme_update(panel.grid.major = element_blank(), 

                     panel.grid.minor = element_blank(), 

                     panel.background = element_rect(colour = "black")) 

gp <- ggplot(data_FoodIntake_30mins, aes(x=Cycle, y=UptakeA_Sum, colour=Genotype, group=Genotype)) 

gp + geom_line(aes(linetype=Genotype), size=.6) +  

  geom_point(aes(shape=Genotype), size=3) +  

  geom_errorbar(aes(ymax=UptakeA_Sum+se, ymin=UptakeA_Sum-se), width=.1) + ggtitle("FoodIntake") 

theme_set(opar)   

dev.off()

##############################################################
##### Food Intake Analysis (1hr)####
##############################################################

FoodIntake_1hr <- lmer(UptakeASum_1hr ~ Genotype*one_hour + (1|Animal) + (1|StartDate), data=data2, REML = FALSE)

Anova(FoodIntake_1hr)

Food_Intake_emmeans_1hr <- emmeans(FoodIntake_1hr, list(pairwise ~ Genotype:one_hour), adjust = "fdr")

Food_Intake_lsmeans_1hr <- Food_Intake_emmeans_1hr$`emmeans of Genotype, one_hour`

Food_Intake_lsmeans_1hr <- as.data.frame(Food_Intake_lsmeans_1hr)

Food_Intake_contrasts_1hr <- as.data.frame(Food_Intake_emmeans_1hr$`pairwise differences of Genotype, one_hour`)

print(Food_Intake_lsmeans_1hr)

Food_Intake_lsmeans_1hr$emmean <- Food_Intake_lsmeans_1hr$emmean
Food_Intake_lsmeans_1hr$SE <- Food_Intake_lsmeans_1hr$SE
Food_Intake_lsmeans_1hr <- Food_Intake_lsmeans_1hr[with(Food_Intake_lsmeans_1hr, order(one_hour, Genotype)), ]
write.table(Food_Intake_contrasts_1hr, file = "./output/202305_DATBFX_Females_Sable_RC_AL_LD_Food_Intake_contrasts_1hr.csv", sep = ",", row.names = FALSE)

Food_Intake_lsmeans_1hr <- Food_Intake_lsmeans_1hr[order(factor(Food_Intake_lsmeans_1hr$one_hour, levels = c('ZT0', 'ZT1', 'ZT2', 'ZT3', 'ZT4', 'ZT5', 'ZT6', 'ZT7', 'ZT8', 'ZT9', 'ZT10', 'ZT11', 'ZT12', 'ZT13', 'ZT14', 'ZT15', 'ZT16', 'ZT17', 'ZT18', 'ZT19', 'ZT20', 'ZT21', 'ZT22', 'ZT23'))),]

Food_Intake_lsmeans_1hr$zt <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14, 15, 15, 16, 16, 17, 17, 18, 18, 19, 19, 20, 20, 21, 21, 22, 22, 23, 23, 24, 24)

print(Food_Intake_lsmeans_1hr)

pdf(file = "./output/202305_DATBFX_Females_Sable_RC_AL_FoodIntake_1hr_Line_Plot.pdf", height = 5, width = 7)

opar <- theme_update(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_rect(colour = "black"))
gp <- ggplot(Food_Intake_lsmeans_1hr, aes(x=zt, y=emmean, colour=Genotype, group=Genotype))
gp + geom_line(aes(linetype=Genotype), size=.6) + 
  geom_point(aes(shape=Genotype), size=3) + 
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE), width=.1) + ggtitle("Food Intake (1hr)")
theme_set(opar)  
dev.off()

##############################################################
##### Food Intake Analysis (2hrs)####
##############################################################

FoodIntake_2hrs <- lmer(UptakeASum_2hrs ~ Genotype*two_hours + (1|Animal) + (1|StartDate), data=data2, REML = FALSE)

Anova(FoodIntake_2hrs)

Food_Intake_emmeans_2hrs <- emmeans(FoodIntake_2hrs, list(pairwise ~ Genotype:two_hours), adjust = "fdr")

Food_Intake_lsmeans_2hrs <- Food_Intake_emmeans_2hrs$`emmeans of Genotype, two_hours`

Food_Intake_lsmeans_2hrs <- as.data.frame(Food_Intake_lsmeans_2hrs)
write.table(Food_Intake_lsmeans_2hrs, file = "./output/emms_master_output.csv", sep = ",", row.names = FALSE)

Food_Intake_contrasts_2hrs <- as.data.frame(Food_Intake_emmeans_2hrs$`pairwise differences of Genotype, two_hours`)

print(Food_Intake_lsmeans_2hrs)

Food_Intake_lsmeans_2hrs$emmean <- Food_Intake_lsmeans_2hrs$emmean
Food_Intake_lsmeans_2hrs$SE <- Food_Intake_lsmeans_2hrs$SE
Food_Intake_lsmeans_2hrs <- Food_Intake_lsmeans_2hrs[with(Food_Intake_lsmeans_2hrs, order(two_hours, Genotype)), ]
write.table(Food_Intake_contrasts_2hrs, file = ".output/202305_DBFX_Females_Sable_RC_AL_LD_Food_Intake_contrasts_2hrs.csv")

Food_Intake_lsmeans_2hrs <- Food_Intake_lsmeans_2hrs[order(factor(Food_Intake_lsmeans_2hrs$two_hours, levels = c('ZT0_ZT2', 'ZT2_ZT4', 'ZT4_ZT6', 'ZT6_ZT8', 'ZT8_ZT10', 'ZT10_ZT12', 'ZT12_ZT14', 'ZT14_Z1T6', 'ZT16_ZT18', 'ZT18_ZT20', 'ZT20_Z22', 'ZT22_ZT24'))),]

Food_Intake_lsmeans_2hrs$zt <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12)

print(Food_Intake_lsmeans_2hrs)

pdf(file = "./output/202305_DBFX_Females_Sable_RC_AL_FoodIntake_2hrs_Line_Plot.pdf", height = 5, width = 7)

opar <- theme_update(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_rect(colour = "black"))
gp <- ggplot(Food_Intake_lsmeans_2hrs, aes(x=zt, y=emmean, colour=Genotype, group=Genotype))
gp + geom_line(aes(linetype=Genotype), size=.6) + 
  geom_point(aes(shape=Genotype), size=3) + 
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE), width=.1) + ggtitle("Food Intake (2hrs)")
theme_set(opar)  
dev.off()



##############################################################
##### Food Intake Analysis (4hrs)####
##############################################################

# FoodIntake_4hrs <- glm(UptakeA_Sum ~ Genotype + four_hours, data=data2)
FoodIntake_4hrs <- lmer(UptakeASum_4hrs ~ Genotype*four_hours + (1|Animal) + (1|StartDate), data=data2, REML = FALSE)

Anova(FoodIntake_4hrs)

Food_Intake_emmeans_4hrs <- emmeans(FoodIntake_4hrs, list(pairwise ~ Genotype:four_hours), adjust = "fdr")

Food_Intake_lsmeans_4hrs <- Food_Intake_emmeans_4hrs$`emmeans of Genotype, four_hours`

Food_Intake_lsmeans_4hrs <- as.data.frame(Food_Intake_lsmeans_4hrs)

Food_Intake_contrasts_4hrs <- as.data.frame(Food_Intake_emmeans_4hrs$`pairwise differences of Genotype, four_hours`)

print(Food_Intake_lsmeans_4hrs)

Food_Intake_lsmeans_4hrs$emmean <- Food_Intake_lsmeans_4hrs$emmean
Food_Intake_lsmeans_4hrs$SE <- Food_Intake_lsmeans_4hrs$SE
Food_Intake_lsmeans_4hrs <- Food_Intake_lsmeans_4hrs[with(Food_Intake_lsmeans_4hrs, order(four_hours, Genotype)), ]
write.table(Food_Intake_contrasts_4hrs, file = "./output/202305_DBFX_Females_Sable_RC_AL_LD_Food_Intake_contrasts_4hrs.csv")

Food_Intake_lsmeans_4hrs <- Food_Intake_lsmeans_4hrs[order(factor(Food_Intake_lsmeans_4hrs$four_hours, levels = c('ZT0_ZT4', 'ZT4_ZT8', 'ZT8_ZT12', 'ZT12_ZT16', 'ZT16_ZT20', 'ZT20_ZT24'))),]

Food_Intake_lsmeans_4hrs$zt <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6)

print(Food_Intake_lsmeans_4hrs)

pdf(file = "./output/202305_DBFX_Females_Sable_RC_AL_FoodIntake_4hrs_Line_Plot.pdf", height = 5, width = 7)

opar <- theme_update(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_rect(colour = "black"))
gp <- ggplot(Food_Intake_lsmeans_4hrs, aes(x=zt, y=emmean, colour=Genotype, group=Genotype))
gp + geom_line(aes(linetype=Genotype), size=.6) + 
  geom_point(aes(shape=Genotype), size=3) + 
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE), width=.1) + ggtitle("Food Intake (4hrs)")
theme_set(opar)  
dev.off()


##############################################################
##### Food Intake Analysis (Day vs Night)####
##############################################################
FoodIntake_LD <- lmer(UptakeASum_LD ~ Genotype*day_night + (1|Animal) + (1|StartDate), data=data2, REML = FALSE)

Anova(FoodIntake_LD)

Food_Intake_emmeans_LD <- emmeans(FoodIntake_LD, list(pairwise ~ Genotype:day_night), adjust = "fdr")

Food_Intake_lsmeans_LD <- Food_Intake_emmeans_LD$`emmeans of Genotype, day_night`

Food_Intake_lsmeans_LD <- as.data.frame(Food_Intake_lsmeans_LD)

Food_Intake_contrasts_LD <- as.data.frame(Food_Intake_emmeans_LD$`pairwise differences of Genotype, day_night`)

print(Food_Intake_lsmeans_LD)

Food_Intake_lsmeans_LD$emmean <- Food_Intake_lsmeans_LD$emmean
Food_Intake_lsmeans_LD$SE <- Food_Intake_lsmeans_LD$SE
Food_Intake_lsmeans_LD <- Food_Intake_lsmeans_LD[with(Food_Intake_lsmeans_LD, order(day_night, Genotype)), ]
write.table(Food_Intake_contrasts_LD, file = "./output/202305_DBFX_Females_Sable_RC_AL_LD_Food_Intake_contrasts_LD.csv")
###  FDR-adjusted comparisons

print(Food_Intake_lsmeans_LD)

pdf(file = "./output/202305_DBFX_Females_Sable_RC_AL_LD_Feeding_Amount.pdf", height = 4, width = 6)

predicted_values_Food_Intake_LD <- data2 %>% 

  group_by(data2$Animal, data2$Genotype, data2$day_night) %>% 

  summarise(Predicted_Value = mean(predict(FoodIntake_LD, newdata = cur_data(), re.form = ~(1 | Animal))), .groups = 'drop')  

colnames(predicted_values_Food_Intake_LD) <- c("Animal", "Genotype", "day_night", "Predicted_value") 

   

# Create the plot 

ggplot() + 

  geom_col(data = Food_Intake_lsmeans_LD, aes(x = interaction(day_night, Genotype), y = emmean, fill = Genotype), position = position_dodge(), alpha = 0.7) + 

  geom_errorbar(data = Food_Intake_lsmeans_LD, aes(x = interaction(day_night, Genotype), y = emmean, ymin = emmean - SE, ymax = emmean + SE, group = Genotype), width = 0.2, position = position_dodge(.9)) + 

  geom_jitter(data = predicted_values_Food_Intake_LD, aes(x = interaction(day_night, Genotype), y = Predicted_value, color = Genotype), width = 0.2, size = 2, alpha = 0.7) + 

  theme_minimal() + 

  labs(y = "Food Consumed (g)", x = "day_night:Genotype", title = "Light and Dark Food Intake") 

  

dev.off() 

##############################################################
## Total Daily Food Intake (FoodIntake_Daily)
##############################################################

FoodIntake_Daily <- lmer(UptakeASum_Daily ~ Genotype + (1|Animal) + (1|StartDate), data=data2, REML=FALSE)

Anova(FoodIntake_Daily)

Food_Intake_emmeans_Daily <- emmeans(FoodIntake_Daily, list(pairwise ~ Genotype), adjust = "fdr")

Food_Intake_lsmeans_Daily <- Food_Intake_emmeans_Daily$`emmeans of Genotype`

Food_Intake_lsmeans_Daily <- as.data.frame(Food_Intake_lsmeans_Daily)

Food_Intake_contrasts_Daily <- as.data.frame(Food_Intake_emmeans_Daily$`pairwise differences of Genotype`)

print(Food_Intake_lsmeans_Daily)

Food_Intake_lsmeans_Daily$emmean <- Food_Intake_lsmeans_Daily$emmean
Food_Intake_lsmeans_Daily$SE <- Food_Intake_lsmeans_Daily$SE
Food_Intake_lsmeans_Daily <- Food_Intake_lsmeans_Daily[with(Food_Intake_lsmeans_Daily, order(Genotype)), ]
write.table(Food_Intake_contrasts_Daily, file = "./output/202305_DBFX_Females_Sable_RC_AL_Daily_Food_Intake_contrasts_Daily.csv")
###  FDR-adjusted comparisons

pdf(file = "./output/202305_DBFX_Females_Sable_RC_AL_Daily_Feeding_Amount.pdf", height = 4, width = 6)

predicted_values_Food_Intake_Daily <- data2 %>% 

  group_by(data2$Animal, data2$Genotype) %>% 

  summarise(Predicted_Value = mean(predict(FoodIntake_Daily, newdata = cur_data(), re.form = ~(1 | Animal))), .groups = 'drop')  

colnames(predicted_values_Food_Intake_Daily) <- c("Animal", "Genotype", "Predicted_value") 

   

# Create the plot 

ggplot() + 

  geom_col(data = Food_Intake_lsmeans_Daily, aes(x = Genotype, y = emmean, fill = Genotype), position = position_dodge(), alpha = 0.7) + 

  geom_errorbar(data = Food_Intake_lsmeans_Daily, aes(x = Genotype, y = emmean, ymin = emmean - SE, ymax = emmean + SE, group = Genotype), width = 0.2, position = position_dodge(.9)) + 

  geom_jitter(data = predicted_values_Food_Intake_Daily, aes(x = Genotype, y = Predicted_value, color = Genotype), width = 0.2, size = 2, alpha = 0.7) + 

  theme_minimal() + 

  labs(y = "Food Consumed (g)", x = "Genotype", title = "Daily Food Intake") 

  

dev.off()

##############################################################
##### Activity Analysis (30 min)####
##############################################################

data_Activity_30mins <- data_summary(data2, varname = "AllMeters", groupnames = c("Cycle", "Genotype"))

pdf(file = "./output/202305_DATBFX_Females_LD_Sable_RC_AL_Activity_30mins_Line_Plot.pdf", height = 5, width = 7) 

  

opar <- theme_update(panel.grid.major = element_blank(), 

                     panel.grid.minor = element_blank(), 

                     panel.background = element_rect(colour = "black")) 

gp <- ggplot(data_Activity_30mins, aes(x=Cycle, y=AllMeters, colour=Genotype, group=Genotype)) 

gp + geom_line(aes(linetype=Genotype), size=.6) +  

  geom_point(aes(shape=Genotype), size=3) +  

  geom_errorbar(aes(ymax=AllMeters+se, ymin=AllMeters-se), width=.1) + ggtitle("Activity") 

theme_set(opar)   

dev.off()

##############################################################
##### Activity Analysis (1hr)####
##############################################################

Activity_1hr <- lmer(AllMeters_1hr ~ Genotype*one_hour + (1|Animal) + (1|StartDate), data=data2, REML = FALSE)

Anova(Activity_1hr)

Activity_emmeans_1hr <- emmeans(Activity_1hr, list(pairwise ~ Genotype:one_hour), adjust = "fdr")

Activity_lsmeans_1hr <- Activity_emmeans_1hr$`emmeans of Genotype, one_hour`

Activity_lsmeans_1hr <- as.data.frame(Activity_lsmeans_1hr)

Activity_contrasts_1hr <- as.data.frame(Activity_emmeans_1hr$`pairwise differences of Genotype, one_hour`)

print(Activity_lsmeans_1hr)

Activity_lsmeans_1hr$emmean <- Activity_lsmeans_1hr$emmean
Activity_lsmeans_1hr$SE <- Activity_lsmeans_1hr$SE
Activity_lsmeans_1hr <- Activity_lsmeans_1hr[with(Activity_lsmeans_1hr, order(one_hour, Genotype)), ]
write.table(Activity_contrasts_1hr, file = "./output/202305_DBFX_Females_Sable_RC_AL_LD_Activity_contrasts_1hr.csv")

Activity_lsmeans_1hr <- Activity_lsmeans_1hr[order(factor(Activity_lsmeans_1hr$one_hour, levels = c('ZT0', 'ZT1', 'ZT2', 'ZT3', 'ZT4', 'ZT5', 'ZT6', 'ZT7', 'ZT8', 'ZT9', 'ZT10', 'ZT11', 'ZT12', 'ZT13', 'ZT14', 'ZT15', 'ZT16', 'ZT17', 'ZT18', 'ZT19', 'ZT20', 'ZT21', 'ZT22', 'ZT23'))),]

Activity_lsmeans_1hr$zt <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14, 15, 15, 16, 16, 17, 17, 18, 18, 19, 19, 20, 20, 21, 21, 22, 22, 23, 23, 24, 24)

print(Activity_lsmeans_1hr)

pdf(file = "./output/202305_DBFX_Females_Sable_RC_AL_Activity_1hr_Line_Plot.pdf", height = 5, width = 7)

opar <- theme_update(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_rect(colour = "black"))
gp <- ggplot(Activity_lsmeans_1hr, aes(x=zt, y=emmean, colour=Genotype, group=Genotype))
gp + geom_line(aes(linetype=Genotype), size=.6) + 
  geom_point(aes(shape=Genotype), size=3) + 
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE), width=.1) + ggtitle("Activity (1hr)")
theme_set(opar)  
dev.off()

##############################################################
##### Activity Analysis (2hrs)####
##############################################################

Activity_2hrs <- lmer(AllMeters_2hrs ~ Genotype*two_hours + (1|Animal) + (1|StartDate), data=data2, REML = FALSE)

Anova(Activity_2hrs)

Activity_emmeans_2hrs <- emmeans(Activity_2hrs, list(pairwise ~ Genotype:two_hours), adjust = "fdr")

Activity_lsmeans_2hrs <- Activity_emmeans_2hrs$`emmeans of Genotype, two_hours`

Activity_lsmeans_2hrs <- as.data.frame(Activity_lsmeans_2hrs)

Activity_contrasts_2hrs <- as.data.frame(Activity_emmeans_2hrs$`pairwise differences of Genotype, two_hours`)

print(Activity_lsmeans_2hrs)

Activity_lsmeans_2hrs$emmean <- Activity_lsmeans_2hrs$emmean
Activity_lsmeans_2hrs$SE <- Activity_lsmeans_2hrs$SE
Activity_lsmeans_2hrs <- Activity_lsmeans_2hrs[with(Activity_lsmeans_2hrs, order(two_hours, Genotype)), ]
write.table(Activity_contrasts_2hrs, file = "./output/202305_DBFX_Females_Sable_RC_AL_LD_Activity_contrasts_2hrs.csv")

Activity_lsmeans_2hrs <- Activity_lsmeans_2hrs[order(factor(Activity_lsmeans_2hrs$two_hours, levels = c('ZT0_ZT2', 'ZT2_ZT4', 'ZT4_ZT6', 'ZT6_ZT8', 'ZT8_ZT10', 'ZT10_ZT12', 'ZT12_ZT14', 'ZT14_Z1T6', 'ZT16_ZT18', 'ZT18_ZT20', 'ZT20_Z22', 'ZT22_ZT24'))),]

Activity_lsmeans_2hrs$zt <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12)

print(Activity_lsmeans_2hrs)

pdf(file = "./output/202305_DBFX_Females_Sable_RC_AL_Activity_2hrs_Line_Plot.pdf", height = 5, width = 7)

opar <- theme_update(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_rect(colour = "black"))
gp <- ggplot(Activity_lsmeans_2hrs, aes(x=zt, y=emmean, colour=Genotype, group=Genotype))
gp + geom_line(aes(linetype=Genotype), size=.6) + 
  geom_point(aes(shape=Genotype), size=3) + 
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE), width=.1) + ggtitle("Activity (2hrs)")
theme_set(opar)  
dev.off()

##############################################################
##### Activity Analysis (4hrs)####
##############################################################

Activity_4hrs <- lmer(AllMeters_4hrs ~ Genotype*four_hours + (1|Animal) + (1|StartDate), data=data2, REML = FALSE)

Anova(Activity_4hrs)

Activity_emmeans_4hrs <- emmeans(Activity_4hrs, list(pairwise ~ Genotype:four_hours), adjust = "fdr")

Activity_lsmeans_4hrs <- Activity_emmeans_4hrs$`emmeans of Genotype, four_hours`

Activity_lsmeans_4hrs <- as.data.frame(Activity_lsmeans_4hrs)

Activity_contrasts_4hrs <- as.data.frame(Activity_emmeans_4hrs$`pairwise differences of Genotype, four_hours`)

print(Activity_lsmeans_4hrs)

Activity_lsmeans_4hrs$emmean <- Activity_lsmeans_4hrs$emmean
Activity_lsmeans_4hrs$SE <- Activity_lsmeans_4hrs$SE
Activity_lsmeans_4hrs <- Activity_lsmeans_4hrs[with(Activity_lsmeans_4hrs, order(four_hours, Genotype)), ]
write.table(Activity_contrasts_4hrs, file = "./output/202305_DBFX_Females_Sable_RC_AL_LD_Activity_contrasts_4hrs.csv")

Activity_lsmeans_4hrs <- Activity_lsmeans_4hrs[order(factor(Activity_lsmeans_4hrs$four_hours, levels = c('ZT0_ZT4', 'ZT4_ZT8', 'ZT8_ZT12', 'ZT12_ZT16', 'ZT16_ZT20', 'ZT20_ZT24'))),]

Activity_lsmeans_4hrs$zt <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6)

print(Activity_lsmeans_4hrs)

pdf(file = "./output/202305_DBFX_Females_Sable_RC_AL_Activity_4hrs_Line_Plot.pdf", height = 5, width = 7)

opar <- theme_update(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_rect(colour = "black"))
gp <- ggplot(Activity_lsmeans_4hrs, aes(x=zt, y=emmean, colour=Genotype, group=Genotype))
gp + geom_line(aes(linetype=Genotype), size=.6) + 
  geom_point(aes(shape=Genotype), size=3) + 
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE), width=.1) + ggtitle("Activity (4hrs)")
theme_set(opar)  
dev.off()


##############################################################
##### Activity Analysis (Day vs Night)####
##############################################################
Activity_LD <- lmer(AllMeters_LD ~ Genotype*day_night + (1|Animal) + (1|StartDate), data=data2, REML = FALSE)

Anova(Activity_LD)

Activity_emmeans_LD <- emmeans(Activity_LD, list(pairwise ~ Genotype:day_night), adjust = "fdr")

Activity_lsmeans_LD <- Activity_emmeans_LD$`emmeans of Genotype, day_night`

Activity_lsmeans_LD <- as.data.frame(Activity_lsmeans_LD)

Activity_contrasts_LD <- as.data.frame(Activity_emmeans_LD$`pairwise differences of Genotype, day_night`)

print(Activity_lsmeans_LD)

Activity_lsmeans_LD$emmean <- Activity_lsmeans_LD$emmean
Activity_lsmeans_LD$SE <- Activity_lsmeans_LD$SE
Activity_lsmeans_LD <- Activity_lsmeans_LD[with(Activity_lsmeans_LD, order(day_night, Genotype)), ]
write.table(Activity_contrasts_LD, file = "./output/202305_DBFX_Females_Sable_RC_AL_LD_Activity_contrasts_LD.csv")
###  FDR-adjusted comparisons

print(Activity_lsmeans_LD)

pdf(file = "./output/202305_DBFX_Females_Sable_RC_AL_LD_Activity_Amount.pdf", height = 4, width = 6)

predicted_values_Activity_LD <- data2 %>% 

  group_by(data2$Animal, data2$Genotype, data2$day_night) %>% 

  summarise(Predicted_Value = mean(predict(Activity_LD, newdata = cur_data(), re.form = ~(1 | Animal))), .groups = 'drop')  

colnames(predicted_values_Activity_LD) <- c("Animal", "Genotype", "day_night", "Predicted_value") 

   

# Create the plot 

ggplot() + 

  geom_col(data = Activity_lsmeans_LD, aes(x = interaction(day_night, Genotype), y = emmean, fill = Genotype), position = position_dodge(), alpha = 0.7) + 

  geom_errorbar(data = Activity_lsmeans_LD, aes(x = interaction(day_night, Genotype), y = emmean, ymin = emmean - SE, ymax = emmean + SE, group = Genotype), width = 0.2, position = position_dodge(.9)) + 

  geom_jitter(data = predicted_values_Activity_LD, aes(x = interaction(day_night, Genotype), y = Predicted_value, color = Genotype), width = 0.2, size = 2, alpha = 0.7) + 

  theme_minimal() + 

  labs(y = "IR Beam Breaks", x = "day_night:Genotype", title = "Light and Dark Activity") 

  

dev.off() 

##############################################################
## Total Daily Activity (Activity_Daily)
##############################################################

Activity_Daily <- lmer(AllMeters_Daily ~ Genotype + (1|Animal) + (1|StartDate), data=data2, REML=FALSE)

Anova(Activity_Daily)

Activity_emmeans_Daily <- emmeans(Activity_Daily, list(pairwise ~ Genotype), adjust = "fdr")

Activity_lsmeans_Daily <- Activity_emmeans_Daily$`emmeans of Genotype`

Activity_lsmeans_Daily <- as.data.frame(Activity_lsmeans_Daily)

Activity_contrasts_Daily <- as.data.frame(Activity_emmeans_Daily$`pairwise differences of Genotype`)

print(Activity_lsmeans_Daily)

Activity_lsmeans_Daily$emmean <- Activity_lsmeans_Daily$emmean
Activity_lsmeans_Daily$SE <- Activity_lsmeans_Daily$SE
Activity_lsmeans_Daily <- Activity_lsmeans_Daily[with(Activity_lsmeans_Daily, order(Genotype)), ]
write.table(Activity_contrasts_Daily, file = "./output/202305_DBFX_Females_Sable_RC_AL_Daily_Activity_contrasts_Daily.csv")
###  FDR-adjusted comparisons

pdf(file = "./output/202305_DBFX_Females_Sable_RC_AL_Daily_Activity_Amount.pdf", height = 4, width = 6)

predicted_values_Activity_Daily <- data2 %>% 

  group_by(data2$Animal, data2$Genotype) %>% 

  summarise(Predicted_Value = mean(predict(Activity_Daily, newdata = cur_data(), re.form = ~(1 | Animal))), .groups = 'drop')  

colnames(predicted_values_Activity_Daily) <- c("Animal", "Genotype", "Predicted_value") 

   

# Create the plot 

ggplot() + 

  geom_col(data = Activity_lsmeans_Daily, aes(x = Genotype, y = emmean, fill = Genotype), position = position_dodge(), alpha = 0.7) + 

  geom_errorbar(data = Activity_lsmeans_Daily, aes(x = Genotype, y = emmean, ymin = emmean - SE, ymax = emmean + SE, group = Genotype), width = 0.2, position = position_dodge(.9)) + 

  geom_jitter(data = predicted_values_Activity_LD, aes(x = Genotype, y = Predicted_value, color = Genotype), width = 0.2, size = 2, alpha = 0.7) + 

  theme_minimal() + 

  labs(y = "IR Beam Breaks", x = "Genotype", title = "Daily Activity") 

  

dev.off()


##############################################################
##### Sleep_pct Analysis (30 min)####
##############################################################

data_Sleep_pct_30mins <- data_summary(data2, varname = "Sleep_pct", groupnames = c("Cycle", "Genotype"))

pdf(file = "./output/202305_DATBFX_Females_LD_Sable_RC_AL_Sleep_pct_30mins_Line_Plot.pdf", height = 5, width = 7) 

  

opar <- theme_update(panel.grid.major = element_blank(), 

                     panel.grid.minor = element_blank(), 

                     panel.background = element_rect(colour = "black")) 

gp <- ggplot(data_Sleep_pct_30mins, aes(x=Cycle, y=Sleep_pct, colour=Genotype, group=Genotype)) 

gp + geom_line(aes(linetype=Genotype), size=.6) +  

  geom_point(aes(shape=Genotype), size=3) +  

  geom_errorbar(aes(ymax=Sleep_pct+se, ymin=Sleep_pct-se), width=.1) + ggtitle("Sleep_pct") 

theme_set(opar)   

dev.off()

##############################################################
##### Sleep_pct Analysis (1hr)####
##############################################################

Sleep_pct_1hr <- lmer(Sleep_pct_1hr ~ Genotype*one_hour + (1|Animal) + (1|StartDate), data=data2, REML = FALSE)

Anova(Sleep_pct_1hr)

Sleep_pct_emmeans_1hr <- emmeans(Sleep_pct_1hr, list(pairwise ~ Genotype:one_hour), adjust = "fdr")

Sleep_pct_lsmeans_1hr <- Sleep_pct_emmeans_1hr$`emmeans of Genotype, one_hour`

Sleep_pct_lsmeans_1hr <- as.data.frame(Sleep_pct_lsmeans_1hr)

Sleep_pct_contrasts_1hr <- as.data.frame(Sleep_pct_emmeans_1hr$`pairwise differences of Genotype, one_hour`)

print(Sleep_pct_lsmeans_1hr)

Sleep_pct_lsmeans_1hr$emmean <- Sleep_pct_lsmeans_1hr$emmean
Sleep_pct_lsmeans_1hr$SE <- Sleep_pct_lsmeans_1hr$SE
Sleep_pct_lsmeans_1hr <- Sleep_pct_lsmeans_1hr[with(Sleep_pct_lsmeans_1hr, order(one_hour, Genotype)), ]
write.table(Sleep_pct_contrasts_1hr, file = "./output/202305_DBFX_Females_Sable_RC_AL_LD_Sleep_pct_contrasts_1hr.csv")

Sleep_pct_lsmeans_1hr <- Sleep_pct_lsmeans_1hr[order(factor(Sleep_pct_lsmeans_1hr$one_hour, levels = c('ZT0', 'ZT1', 'ZT2', 'ZT3', 'ZT4', 'ZT5', 'ZT6', 'ZT7', 'ZT8', 'ZT9', 'ZT10', 'ZT11', 'ZT12', 'ZT13', 'ZT14', 'ZT15', 'ZT16', 'ZT17', 'ZT18', 'ZT19', 'ZT20', 'ZT21', 'ZT22', 'ZT23'))),]

Sleep_pct_lsmeans_1hr$zt <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14, 15, 15, 16, 16, 17, 17, 18, 18, 19, 19, 20, 20, 21, 21, 22, 22, 23, 23, 24, 24)

print(Sleep_pct_lsmeans_1hr)

pdf(file = "./output/202305_DBFX_Females_Sable_RC_AL_Sleep_pct_1hr_Line_Plot.pdf", height = 5, width = 7)

opar <- theme_update(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_rect(colour = "black"))
gp <- ggplot(Sleep_pct_lsmeans_1hr, aes(x=zt, y=emmean, colour=Genotype, group=Genotype))
gp + geom_line(aes(linetype=Genotype), size=.6) + 
  geom_point(aes(shape=Genotype), size=3) + 
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE), width=.1) + ggtitle("Sleep_pct (1hr)")
theme_set(opar)  
dev.off()

##############################################################
##### Sleep_pct Analysis (2hrs)####
##############################################################

Sleep_pct_2hrs <- lmer(Sleep_pct_2hrs ~ Genotype*two_hours + (1|Animal) + (1|StartDate), data=data2, REML = FALSE)

Anova(Sleep_pct_2hrs)

Sleep_pct_emmeans_2hrs <- emmeans(Sleep_pct_2hrs, list(pairwise ~ Genotype:two_hours), adjust = "fdr")

Sleep_pct_lsmeans_2hrs <- Sleep_pct_emmeans_2hrs$`emmeans of Genotype, two_hours`

Sleep_pct_lsmeans_2hrs <- as.data.frame(Sleep_pct_lsmeans_2hrs)

Sleep_pct_contrasts_2hrs <- as.data.frame(Sleep_pct_emmeans_2hrs$`pairwise differences of Genotype, two_hours`)

print(Sleep_pct_lsmeans_2hrs)

Sleep_pct_lsmeans_2hrs$emmean <- Sleep_pct_lsmeans_2hrs$emmean
Sleep_pct_lsmeans_2hrs$SE <- Sleep_pct_lsmeans_2hrs$SE
Sleep_pct_lsmeans_2hrs <- Sleep_pct_lsmeans_2hrs[with(Sleep_pct_lsmeans_2hrs, order(two_hours, Genotype)), ]
write.table(Sleep_pct_contrasts_2hrs, file = "./output/202305_DBFX_Females_Sable_RC_AL_LD_Sleep_pct_contrasts_2hrs.csv")

Sleep_pct_lsmeans_2hrs <- Sleep_pct_lsmeans_2hrs[order(factor(Sleep_pct_lsmeans_2hrs$two_hours, levels = c('ZT0_ZT2', 'ZT2_ZT4', 'ZT4_ZT6', 'ZT6_ZT8', 'ZT8_ZT10', 'ZT10_ZT12', 'ZT12_ZT14', 'ZT14_Z1T6', 'ZT16_ZT18', 'ZT18_ZT20', 'ZT20_Z22', 'ZT22_ZT24'))),]

Sleep_pct_lsmeans_2hrs$zt <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12)

print(Sleep_pct_lsmeans_2hrs)

pdf(file = "./output/202305_DBFX_Females_Sable_RC_AL_Sleep_pct_2hrs_Line_Plot.pdf", height = 5, width = 7)

opar <- theme_update(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_rect(colour = "black"))
gp <- ggplot(Sleep_pct_lsmeans_2hrs, aes(x=zt, y=emmean, colour=Genotype, group=Genotype))
gp + geom_line(aes(linetype=Genotype), size=.6) + 
  geom_point(aes(shape=Genotype), size=3) + 
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE), width=.1) + ggtitle("Sleep_pct (2hrs)")
theme_set(opar)  
dev.off()

##############################################################
##### Sleep_pct Analysis (4hrs)####
##############################################################

Sleep_pct_4hrs <- lmer(Sleep_pct_4hrs ~ Genotype*four_hours + (1|Animal) + (1|StartDate), data=data2, REML = FALSE)

Anova(Sleep_pct_4hrs)

Sleep_pct_emmeans_4hrs <- emmeans(Sleep_pct_4hrs, list(pairwise ~ Genotype:four_hours), adjust = "fdr")

Sleep_pct_lsmeans_4hrs <- Sleep_pct_emmeans_4hrs$`emmeans of Genotype, four_hours`

Sleep_pct_lsmeans_4hrs <- as.data.frame(Sleep_pct_lsmeans_4hrs)

Sleep_pct_contrasts_4hrs <- as.data.frame(Sleep_pct_emmeans_4hrs$`pairwise differences of Genotype, four_hours`)

print(Sleep_pct_lsmeans_4hrs)

Sleep_pct_lsmeans_4hrs$emmean <- Sleep_pct_lsmeans_4hrs$emmean
Sleep_pct_lsmeans_4hrs$SE <- Sleep_pct_lsmeans_4hrs$SE
Sleep_pct_lsmeans_4hrs <- Sleep_pct_lsmeans_4hrs[with(Sleep_pct_lsmeans_4hrs, order(four_hours, Genotype)), ]
write.table(Sleep_pct_contrasts_4hrs, file = "./output/202305_DBFX_Females_Sable_RC_AL_LD_Sleep_pct_contrasts_4hrs.csv")

Sleep_pct_lsmeans_4hrs <- Sleep_pct_lsmeans_4hrs[order(factor(Sleep_pct_lsmeans_4hrs$four_hours, levels = c('ZT0_ZT4', 'ZT4_ZT8', 'ZT8_ZT12', 'ZT12_ZT16', 'ZT16_ZT20', 'ZT20_ZT24'))),]

Sleep_pct_lsmeans_4hrs$zt <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6)

print(Sleep_pct_lsmeans_4hrs)

pdf(file = "./output/202305_DBFX_Females_Sable_RC_AL_Sleep_pct_4hrs_Line_Plot.pdf", height = 5, width = 7)

opar <- theme_update(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_rect(colour = "black"))
gp <- ggplot(Sleep_pct_lsmeans_4hrs, aes(x=zt, y=emmean, colour=Genotype, group=Genotype))
gp + geom_line(aes(linetype=Genotype), size=.6) + 
  geom_point(aes(shape=Genotype), size=3) + 
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE), width=.1) + ggtitle("Sleep_pct (4hrs)")
theme_set(opar)  
dev.off()


##############################################################
##### Sleep_pct Analysis (Day vs Night)####
##############################################################
Sleep_Percent_LD <- lmer(Sleep_pct_LD ~ Genotype*day_night + (1|StartDate) + (1|Animal), data=data2, REML = FALSE)

Anova(Sleep_Percent_LD)

Sleep_Percent_emmeans_LD <- emmeans(Sleep_Percent_LD, list(pairwise ~ Genotype:day_night), adjust = "fdr")

Sleep_Percent_emmeans_LD

Sleep_Percent_lsmeans_LD <- Sleep_Percent_emmeans_LD$`emmeans of Genotype, day_night`

Sleep_Percent_lsmeans_LD <- as.data.frame(Sleep_Percent_lsmeans_LD)

Sleep_Percent_contrasts_LD <- as.data.frame(Sleep_Percent_emmeans_LD$`pairwise differences of Genotype, day_night`)

print(Sleep_Percent_lsmeans_LD)

Sleep_Percent_lsmeans_LD$emmean <- Sleep_Percent_lsmeans_LD$emmean
Sleep_Percent_lsmeans_LD$SE <- Sleep_Percent_lsmeans_LD$SE
Sleep_Percent_lsmeans_LD <- Sleep_Percent_lsmeans_LD[with(Sleep_Percent_lsmeans_LD, order(day_night, Genotype)), ]
write.table(Sleep_Percent_contrasts_LD, file = "./output/202111_DATBFX_Sable_HFD_LD_Sleep_Percent_contrasts_LD.csv")

pdf(file = "./output/202305_DATBFX_Females_Sable_RC_AL_Sleep_Percent_LD.pdf", height = 4, width = 6)

predicted_values <- data2 %>%
  group_by(data2$Animal, data2$Genotype, data2$day_night) %>%
  summarise(Predicted_Value = mean(predict(Sleep_Percent_LD, newdata = cur_data(), re.form = ~(1 | Animal))), .groups = 'drop')

colnames(predicted_values) <- c("Animal", "Genotype", "day_night", "Predicted_value")

# Create the plot
ggplot() +
  geom_col(data = Sleep_Percent_lsmeans_LD, aes(x = interaction(day_night, Genotype), y = emmean, fill = Genotype), position = position_dodge(), alpha = 0.7) +
  scale_y_continuous(limits=c(0,100)) +
  geom_errorbar(data = Sleep_Percent_lsmeans_LD, aes(x = interaction(day_night, Genotype), y = emmean, ymin = emmean - SE, ymax = emmean + SE, group = Genotype), width = 0.2, position = position_dodge(.9)) +
  geom_jitter(data = predicted_values, aes(x = interaction(day_night, Genotype), y = Predicted_value, color = Genotype), width = 0.2, size = 2, alpha = 0.7) +
  theme_minimal() +
  labs(y = "Sleep Percentage (%)", x = "day_night:Genotype", title = "Sleep Percentage (Light and Dark)")

dev.off()
##############################################################
## Total Daily Sleep_pct (Sleep_pct_Daily)
##############################################################

aggregate(Sleep_pct_Daily ~ Animal:StartDate, data = data2, FUN = mean)

Sleep_Percent_Daily <- lmer(Sleep_pct_Daily ~ Genotype*day_night + (1|StartDate) + (1|Animal), data=data2, REML = FALSE)

Anova(Sleep_Percent_Daily)

Sleep_Percent_emmeans_Daily <- emmeans(Sleep_Percent_Daily, list(pairwise ~ Genotype:day_night), adjust = "fdr")

Sleep_Percent_emmeans_Daily

Sleep_Percent_lsmeans_Daily <- Sleep_Percent_emmeans_Daily$`emmeans of Genotype, day_night`

Sleep_Percent_lsmeans_Daily <- as.data.frame(Sleep_Percent_lsmeans_Daily)

Sleep_Percent_contrasts_Daily <- as.data.frame(Sleep_Percent_emmeans_Daily$`pairwise differences of Genotype, day_night`)

print(Sleep_Percent_lsmeans_Daily)

Sleep_Percent_lsmeans_Daily$emmean <- Sleep_Percent_lsmeans_Daily$emmean
Sleep_Percent_lsmeans_Daily$SE <- Sleep_Percent_lsmeans_Daily$SE
Sleep_Percent_lsmeans_Daily <- Sleep_Percent_lsmeans_Daily[with(Sleep_Percent_lsmeans_Daily, order(day_night, Genotype)), ]
write.table(Sleep_Percent_contrasts_Daily, file = "./output/202111_DATBFX_Sable_HFD_Daily_Sleep_Percent_contrasts_Daily.csv")

pdf(file = "./output/202305_DATBFX_Females_Sable_RC_AL_Sleep_Percent_Daily.pdf", height = 4, width = 6)

predicted_values <- data2 %>%
  group_by(data2$Animal, data2$Genotype) %>%
  summarise(Predicted_Value = mean(predict(Sleep_Percent_Daily, newdata = cur_data(), re.form = ~(1 | Animal))), .groups = 'drop')

colnames(predicted_values) <- c("Animal", "Genotype", "Predicted_value")

# Create the plot
ggplot() +
  geom_col(data = Sleep_Percent_lsmeans_Daily, aes(x = Genotype, y = emmean, fill = Genotype), position = position_dodge(), alpha = 0.7) +
  scale_y_continuous(limits=c(0,100)) +
  geom_errorbar(data = Sleep_Percent_lsmeans_Daily, aes(x = Genotype, y = emmean, ymin = emmean - SE, ymax = emmean + SE, group = Genotype), width = 0.2, position = position_dodge(.9)) +
  geom_jitter(data = predicted_values, aes(x = Genotype, y = Predicted_value, color = Genotype), width = 0.2, size = 2, alpha = 0.7) +
  theme_minimal() +
  labs(y = "Sleep Percentage (%)", x = "Genotype", title = "Daily Sleep Percentage")

dev.off()


##############################################################
##### Avg_VO2 Analysis (30 min)####
##############################################################

data_Avg_VO2_30mins <- data_summary(data2, varname = "Avg_VO2", groupnames = c("Cycle", "Genotype"))

pdf(file = "./output/202305_DATBFX_Females_LD_Sable_RC_AL_Avg_VO2_30mins_Line_Plot.pdf", height = 5, width = 7) 

  

opar <- theme_update(panel.grid.major = element_blank(), 

                     panel.grid.minor = element_blank(), 

                     panel.background = element_rect(colour = "black")) 

gp <- ggplot(data_Avg_VO2_30mins, aes(x=Cycle, y=Avg_VO2, colour=Genotype, group=Genotype)) 

gp + geom_line(aes(linetype=Genotype), size=.6) +  

  geom_point(aes(shape=Genotype), size=3) +  

  geom_errorbar(aes(ymax=Avg_VO2+se, ymin=Avg_VO2-se), width=.1) + ggtitle("Avg_VO2") 

theme_set(opar)   

dev.off()

##############################################################
##### Avg_VO2 Analysis (1hr)####
##############################################################

Avg_VO2_1hr <- lmer(Avg_VO2_1hr ~ Genotype*one_hour + BodyMass_g + (1|Animal) + (1|StartDate), data=data2, REML = FALSE)

Anova(Avg_VO2_1hr)

Avg_VO2_emmeans_1hr <- emmeans(Avg_VO2_1hr, list(pairwise ~ Genotype:one_hour), adjust = "fdr")

Avg_VO2_lsmeans_1hr <- Avg_VO2_emmeans_1hr$`emmeans of Genotype, one_hour`

Avg_VO2_lsmeans_1hr <- as.data.frame(Avg_VO2_lsmeans_1hr)

Avg_VO2_contrasts_1hr <- as.data.frame(Avg_VO2_emmeans_1hr$`pairwise differences of Genotype, one_hour`)

print(Avg_VO2_lsmeans_1hr)

Avg_VO2_lsmeans_1hr$emmean <- Avg_VO2_lsmeans_1hr$emmean
Avg_VO2_lsmeans_1hr$SE <- Avg_VO2_lsmeans_1hr$SE
Avg_VO2_lsmeans_1hr <- Avg_VO2_lsmeans_1hr[with(Avg_VO2_lsmeans_1hr, order(one_hour, Genotype)), ]
write.table(Avg_VO2_contrasts_1hr, file = "./output/202305_DBFX_Females_Sable_RC_AL_LD_Avg_VO2_contrasts_1hr.csv")

Avg_VO2_lsmeans_1hr <- Avg_VO2_lsmeans_1hr[order(factor(Avg_VO2_lsmeans_1hr$one_hour, levels = c('ZT0', 'ZT1', 'ZT2', 'ZT3', 'ZT4', 'ZT5', 'ZT6', 'ZT7', 'ZT8', 'ZT9', 'ZT10', 'ZT11', 'ZT12', 'ZT13', 'ZT14', 'ZT15', 'ZT16', 'ZT17', 'ZT18', 'ZT19', 'ZT20', 'ZT21', 'ZT22', 'ZT23'))),]

Avg_VO2_lsmeans_1hr$zt <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14, 15, 15, 16, 16, 17, 17, 18, 18, 19, 19, 20, 20, 21, 21, 22, 22, 23, 23, 24, 24)

print(Avg_VO2_lsmeans_1hr)

pdf(file = "./output/202305_DBFX_Females_Sable_RC_AL_Avg_VO2_1hr_Line_Plot.pdf", height = 5, width = 7)

opar <- theme_update(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_rect(colour = "black"))
gp <- ggplot(Avg_VO2_lsmeans_1hr, aes(x=zt, y=emmean, colour=Genotype, group=Genotype))
gp + geom_line(aes(linetype=Genotype), size=.6) + 
  geom_point(aes(shape=Genotype), size=3) + 
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE), width=.1) + ggtitle("Avg_VO2 (1hr)")
theme_set(opar)  
dev.off()

##############################################################
##### Avg_VO2 Analysis (2hrs)####
##############################################################

Avg_VO2_2hrs <- lmer(Avg_VO2_2hrs ~ Genotype*two_hours + BodyMass_g + (1|Animal) + (1|StartDate), data=data2, REML = FALSE)

Anova(Avg_VO2_2hrs)

Avg_VO2_emmeans_2hrs <- emmeans(Avg_VO2_2hrs, list(pairwise ~ Genotype:two_hours), adjust = "fdr")

Avg_VO2_lsmeans_2hrs <- Avg_VO2_emmeans_2hrs$`emmeans of Genotype, two_hours`

Avg_VO2_lsmeans_2hrs <- as.data.frame(Avg_VO2_lsmeans_2hrs)

Avg_VO2_contrasts_2hrs <- as.data.frame(Avg_VO2_emmeans_2hrs$`pairwise differences of Genotype, two_hours`)

print(Avg_VO2_lsmeans_2hrs)

Avg_VO2_lsmeans_2hrs$emmean <- Avg_VO2_lsmeans_2hrs$emmean
Avg_VO2_lsmeans_2hrs$SE <- Avg_VO2_lsmeans_2hrs$SE
Avg_VO2_lsmeans_2hrs <- Avg_VO2_lsmeans_2hrs[with(Avg_VO2_lsmeans_2hrs, order(two_hours, Genotype)), ]
write.table(Avg_VO2_contrasts_2hrs, file = "./output/202305_DBFX_Females_Sable_RC_AL_LD_Avg_VO2_contrasts_2hrs.csv")

Avg_VO2_lsmeans_2hrs <- Avg_VO2_lsmeans_2hrs[order(factor(Avg_VO2_lsmeans_2hrs$two_hours, levels = c('ZT0_ZT2', 'ZT2_ZT4', 'ZT4_ZT6', 'ZT6_ZT8', 'ZT8_ZT10', 'ZT10_ZT12', 'ZT12_ZT14', 'ZT14_Z1T6', 'ZT16_ZT18', 'ZT18_ZT20', 'ZT20_Z22', 'ZT22_ZT24'))),]

Avg_VO2_lsmeans_2hrs$zt <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12)

print(Avg_VO2_lsmeans_2hrs)

pdf(file = "./output/202305_DBFX_Females_Sable_RC_AL_Avg_VO2_2hrs_Line_Plot.pdf", height = 5, width = 7)

opar <- theme_update(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_rect(colour = "black"))
gp <- ggplot(Avg_VO2_lsmeans_2hrs, aes(x=zt, y=emmean, colour=Genotype, group=Genotype))
gp + geom_line(aes(linetype=Genotype), size=.6) + 
  geom_point(aes(shape=Genotype), size=3) + 
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE), width=.1) + ggtitle("Avg_VO2 (2hrs)")
theme_set(opar)  
dev.off()

##############################################################
##### Avg_VO2 Analysis (4hrs)####
##############################################################

Avg_VO2_4hrs_fit <- lmer(Avg_VO2_4hrs ~ Genotype*four_hours + BodyMass_g + (1|Animal) + (1|StartDate), data=data2, REML = FALSE)

Anova(Avg_VO2_4hrs_fit)

Avg_VO2_emmeans_4hrs <- emmeans(Avg_VO2_4hrs_fit, list(pairwise ~ Genotype:four_hours), adjust = "fdr")

Avg_VO2_lsmeans_4hrs <- Avg_VO2_emmeans_4hrs$`emmeans of Genotype, four_hours`

Avg_VO2_lsmeans_4hrs <- as.data.frame(Avg_VO2_lsmeans_4hrs)

Avg_VO2_contrasts_4hrs <- as.data.frame(Avg_VO2_emmeans_4hrs$`pairwise differences of Genotype, four_hours`)

print(Avg_VO2_lsmeans_4hrs)

Avg_VO2_lsmeans_4hrs$emmean <- Avg_VO2_lsmeans_4hrs$emmean
Avg_VO2_lsmeans_4hrs$SE <- Avg_VO2_lsmeans_4hrs$SE
Avg_VO2_lsmeans_4hrs <- Avg_VO2_lsmeans_4hrs[with(Avg_VO2_lsmeans_4hrs, order(four_hours, Genotype)), ]
write.table(Avg_VO2_contrasts_4hrs, file = "./output/202305_DBFX_Females_Sable_RC_AL_LD_Avg_VO2_contrasts_4hrs.csv")

Avg_VO2_lsmeans_4hrs <- Avg_VO2_lsmeans_4hrs[order(factor(Avg_VO2_lsmeans_4hrs$four_hours, levels = c('ZT0_ZT4', 'ZT4_ZT8', 'ZT8_ZT12', 'ZT12_ZT16', 'ZT16_ZT20', 'ZT20_ZT24'))),]

Avg_VO2_lsmeans_4hrs$zt <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6)

print(Avg_VO2_lsmeans_4hrs)

pdf(file = "./output/202305_DBFX_Females_Sable_RC_AL_Avg_VO2_4hrs_Line_Plot.pdf", height = 5, width = 7)

opar <- theme_update(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_rect(colour = "black"))
gp <- ggplot(Avg_VO2_lsmeans_4hrs, aes(x=zt, y=emmean, colour=Genotype, group=Genotype))
gp + geom_line(aes(linetype=Genotype), size=.6) + 
  geom_point(aes(shape=Genotype), size=3) + 
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE), width=.1) + ggtitle("Avg_VO2 (4hrs)")
theme_set(opar)  
dev.off()


##############################################################
##### Avg_VO2 Analysis (Day vs Night)####
##############################################################
Avg_VO2_LD_fit <- lmer(Avg_VO2_LD ~ Genotype*day_night + (1|StartDate) + (1|Animal), data=data2, REML = FALSE)

Anova(Avg_VO2_LD_fit)

Avg_VO2_emmeans_LD <- emmeans(Avg_VO2_LD_fit, list(pairwise ~ Genotype:day_night), adjust = "fdr")

Avg_VO2_emmeans_LD

Avg_VO2_lsmeans_LD <- Avg_VO2_emmeans_LD$`emmeans of Genotype, day_night`

Avg_VO2_lsmeans_LD <- as.data.frame(Avg_VO2_lsmeans_LD)

Avg_VO2_contrasts_LD <- as.data.frame(Avg_VO2_emmeans_LD$`pairwise differences of Genotype, day_night`)

print(Avg_VO2_lsmeans_LD)

Avg_VO2_lsmeans_LD$emmean <- Avg_VO2_lsmeans_LD$emmean
Avg_VO2_lsmeans_LD$SE <- Avg_VO2_lsmeans_LD$SE
Avg_VO2_lsmeans_LD <- Avg_VO2_lsmeans_LD[with(Avg_VO2_lsmeans_LD, order(day_night, Genotype)), ]
write.table(Avg_VO2_contrasts_LD, file = "./output/202305_DATBFX_Females_Sable_RC_AL_LD_Avg_VO2_contrasts_LD.csv")

pdf(file = "./output/202305_DATBFX_Females_Sable_RC_AL_Avg_VO2_LD.pdf", height = 4, width = 6)

predicted_values <- data2 %>%
  group_by(data2$Animal, data2$Genotype, data2$day_night) %>%
  summarise(Predicted_Value = mean(predict(Avg_VO2_LD_fit, newdata = cur_data(), re.form = ~(1 | Animal))), .groups = 'drop')

colnames(predicted_values) <- c("Animal", "Genotype", "day_night", "Predicted_value")

# Create the plot
ggplot() +
  geom_col(data = Avg_VO2_lsmeans_LD, aes(x = interaction(day_night, Genotype), y = emmean, fill = Genotype), position = position_dodge(), alpha = 0.7) +
  geom_errorbar(data = Avg_VO2_lsmeans_LD, aes(x = interaction(day_night, Genotype), y = emmean, ymin = emmean - SE, ymax = emmean + SE, group = Genotype), width = 0.2, position = position_dodge(.9)) +
  geom_jitter(data = predicted_values, aes(x = interaction(day_night, Genotype), y = Predicted_value, color = Genotype), width = 0.2, size = 2, alpha = 0.7) +
  theme_minimal() +
  labs(y = "Oxygen Consumption (mL/min)", x = "day_night:Genotype", title = "Average VO2 Consumption (Light and Dark)")

dev.off()

##############################################################
## Daily Average VO2 Consumption (Avg_VO2_Daily)
##############################################################

Avg_VO2_Daily_fit <- lmer(Avg_VO2_Daily ~ Genotype*day_night + (1|StartDate) + (1|Animal), data=data2, REML = FALSE)

Anova(Avg_VO2_Daily_fit)

Avg_VO2_emmeans_Daily <- emmeans(Avg_VO2_Daily_fit, list(pairwise ~ Genotype:day_night), adjust = "fdr")

Avg_VO2_emmeans_Daily

Avg_VO2_lsmeans_Daily <- Avg_VO2_emmeans_Daily$`emmeans of Genotype, day_night`

Avg_VO2_lsmeans_Daily <- as.data.frame(Avg_VO2_lsmeans_Daily)

Avg_VO2_contrasts_Daily <- as.data.frame(Avg_VO2_emmeans_Daily$`pairwise differences of Genotype, day_night`)

print(Avg_VO2_lsmeans_Daily)

Avg_VO2_lsmeans_Daily$emmean <- Avg_VO2_lsmeans_Daily$emmean
Avg_VO2_lsmeans_Daily$SE <- Avg_VO2_lsmeans_Daily$SE
Avg_VO2_lsmeans_Daily <- Avg_VO2_lsmeans_Daily[with(Avg_VO2_lsmeans_Daily, order(day_night, Genotype)), ]
write.table(Avg_VO2_contrasts_Daily, file = "./output/202305_DATBFX_Females_Sable_RC_AL_Daily_Avg_VO2_contrasts_Daily.csv")

pdf(file = "./output/202305_DATBFX_Females_Sable_RC_AL_Avg_VO2_Daily.pdf", height = 4, width = 6)

predicted_values <- data2 %>%
  group_by(data2$Animal, data2$Genotype) %>%
  summarise(Predicted_Value = mean(predict(Avg_VO2_Daily_fit, newdata = cur_data(), re.form = ~(1 | Animal))), .groups = 'drop')

colnames(predicted_values) <- c("Animal", "Genotype", "Predicted_value")

# Create the plot
ggplot() +
  geom_col(data = Avg_VO2_lsmeans_Daily, aes(x = Genotype, y = emmean, fill = Genotype), position = position_dodge(), alpha = 0.7) +
  geom_errorbar(data = Avg_VO2_lsmeans_Daily, aes(x = Genotype, y = emmean, ymin = emmean - SE, ymax = emmean + SE, group = Genotype), width = 0.2, position = position_dodge(.9)) +
  geom_jitter(data = predicted_values, aes(x = Genotype, y = Predicted_value, color = Genotype), width = 0.2, size = 2, alpha = 0.7) +
  theme_minimal() +
  labs(y = "Oxygen Consumption (mL/min)", x = "Genotype", title = "Daily VO2 Consumption")

dev.off()

##############################################################
##### Avg_RQ Analysis (30 min)####
##############################################################

data_Avg_RQ_30mins <- data_summary(data2, varname = "Avg_RQ", groupnames = c("Cycle", "Genotype"))

pdf(file = "./output/202305_DATBFX_Females_LD_Sable_RC_AL_Avg_RQ_30mins_Line_Plot.pdf", height = 5, width = 7) 

opar <- theme_update(panel.grid.major = element_blank(), 

                     panel.grid.minor = element_blank(), 

                     panel.background = element_rect(colour = "black")) 

gp <- ggplot(data_Avg_RQ_30mins, aes(x=Cycle, y=Avg_RQ, colour=Genotype, group=Genotype)) 

gp + geom_line(aes(linetype=Genotype), size=.6) +  

  geom_point(aes(shape=Genotype), size=3) +  

  geom_errorbar(aes(ymax=Avg_RQ+se, ymin=Avg_RQ-se), width=.1) + ggtitle("Avg_RQ") 

theme_set(opar)   

dev.off()

##############################################################
##### Avg_RQ Analysis (1hr)####
##############################################################

Avg_RQ_1hr <- lmer(Avg_RQ_1hr ~ Genotype*one_hour + (1|Animal) + (1|StartDate), data=data2, REML = FALSE)

Anova(Avg_RQ_1hr)

Avg_RQ_emmeans_1hr <- emmeans(Avg_RQ_1hr, list(pairwise ~ Genotype:one_hour), adjust = "fdr")

Avg_RQ_lsmeans_1hr <- Avg_RQ_emmeans_1hr$`emmeans of Genotype, one_hour`

Avg_RQ_lsmeans_1hr <- as.data.frame(Avg_RQ_lsmeans_1hr)

Avg_RQ_contrasts_1hr <- as.data.frame(Avg_RQ_emmeans_1hr$`pairwise differences of Genotype, one_hour`)

print(Avg_RQ_lsmeans_1hr)

Avg_RQ_lsmeans_1hr$emmean <- Avg_RQ_lsmeans_1hr$emmean
Avg_RQ_lsmeans_1hr$SE <- Avg_RQ_lsmeans_1hr$SE
Avg_RQ_lsmeans_1hr <- Avg_RQ_lsmeans_1hr[with(Avg_RQ_lsmeans_1hr, order(one_hour, Genotype)), ]
write.table(Avg_RQ_contrasts_1hr, file = "./output/202305_DBFX_Females_Sable_RC_AL_LD_Avg_RQ_contrasts_1hr.csv")

Avg_RQ_lsmeans_1hr <- Avg_RQ_lsmeans_1hr[order(factor(Avg_RQ_lsmeans_1hr$one_hour, levels = c('ZT0', 'ZT1', 'ZT2', 'ZT3', 'ZT4', 'ZT5', 'ZT6', 'ZT7', 'ZT8', 'ZT9', 'ZT10', 'ZT11', 'ZT12', 'ZT13', 'ZT14', 'ZT15', 'ZT16', 'ZT17', 'ZT18', 'ZT19', 'ZT20', 'ZT21', 'ZT22', 'ZT23'))),]

Avg_RQ_lsmeans_1hr$zt <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14, 15, 15, 16, 16, 17, 17, 18, 18, 19, 19, 20, 20, 21, 21, 22, 22, 23, 23, 24, 24)

print(Avg_RQ_lsmeans_1hr)

pdf(file = "./output/202305_DBFX_Females_Sable_RC_AL_Avg_RQ_1hr_Line_Plot.pdf", height = 5, width = 7)

opar <- theme_update(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_rect(colour = "black"))
gp <- ggplot(Avg_RQ_lsmeans_1hr, aes(x=zt, y=emmean, colour=Genotype, group=Genotype))
gp + geom_line(aes(linetype=Genotype), size=.6) + 
  geom_point(aes(shape=Genotype), size=3) + 
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE), width=.1) + ggtitle("Avg_RQ (1hr)")
theme_set(opar)  
dev.off()

##############################################################
##### Avg_RQ Analysis (2hrs)####
##############################################################

Avg_RQ_2hrs <- lmer(Avg_RQ_2hrs ~ Genotype*two_hours + (1|Animal) + (1|StartDate), data=data2, REML = FALSE)

Anova(Avg_RQ_2hrs)

Avg_RQ_emmeans_2hrs <- emmeans(Avg_RQ_2hrs, list(pairwise ~ Genotype:two_hours), adjust = "fdr")

Avg_RQ_lsmeans_2hrs <- Avg_RQ_emmeans_2hrs$`emmeans of Genotype, two_hours`

Avg_RQ_lsmeans_2hrs <- as.data.frame(Avg_RQ_lsmeans_2hrs)

Avg_RQ_contrasts_2hrs <- as.data.frame(Avg_RQ_emmeans_2hrs$`pairwise differences of Genotype, two_hours`)

print(Avg_RQ_lsmeans_2hrs)

Avg_RQ_lsmeans_2hrs$emmean <- Avg_RQ_lsmeans_2hrs$emmean
Avg_RQ_lsmeans_2hrs$SE <- Avg_RQ_lsmeans_2hrs$SE
Avg_RQ_lsmeans_2hrs <- Avg_RQ_lsmeans_2hrs[with(Avg_RQ_lsmeans_2hrs, order(two_hours, Genotype)), ]
write.table(Avg_RQ_contrasts_2hrs, file = "./output/202305_DBFX_Females_Sable_RC_AL_LD_Avg_RQ_contrasts_2hrs.csv")

Avg_RQ_lsmeans_2hrs <- Avg_RQ_lsmeans_2hrs[order(factor(Avg_RQ_lsmeans_2hrs$two_hours, levels = c('ZT0_ZT2', 'ZT2_ZT4', 'ZT4_ZT6', 'ZT6_ZT8', 'ZT8_ZT10', 'ZT10_ZT12', 'ZT12_ZT14', 'ZT14_Z1T6', 'ZT16_ZT18', 'ZT18_ZT20', 'ZT20_Z22', 'ZT22_ZT24'))),]

Avg_RQ_lsmeans_2hrs$zt <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12)

print(Avg_RQ_lsmeans_2hrs)

pdf(file = "./output/202305_DBFX_Females_Sable_RC_AL_Avg_RQ_2hrs_Line_Plot.pdf", height = 5, width = 7)

opar <- theme_update(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_rect(colour = "black"))
gp <- ggplot(Avg_RQ_lsmeans_2hrs, aes(x=zt, y=emmean, colour=Genotype, group=Genotype))
gp + geom_line(aes(linetype=Genotype), size=.6) + 
  geom_point(aes(shape=Genotype), size=3) + 
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE), width=.1) + ggtitle("Avg_RQ (2hrs)")
theme_set(opar)  
dev.off()

##############################################################
##### Avg_RQ Analysis (4hrs)####
##############################################################

Avg_RQ_4hrs <- lmer(Avg_RQ_4hrs ~ Genotype*four_hours + (1|Animal) + (1|StartDate), data=data2, REML = FALSE)

Anova(Avg_RQ_4hrs)

Avg_RQ_emmeans_4hrs <- emmeans(Avg_RQ_4hrs, list(pairwise ~ Genotype:four_hours), adjust = "fdr")

Avg_RQ_lsmeans_4hrs <- Avg_RQ_emmeans_4hrs$`emmeans of Genotype, four_hours`

Avg_RQ_lsmeans_4hrs <- as.data.frame(Avg_RQ_lsmeans_4hrs)

Avg_RQ_contrasts_4hrs <- as.data.frame(Avg_RQ_emmeans_4hrs$`pairwise differences of Genotype, four_hours`)

print(Avg_RQ_lsmeans_4hrs)

Avg_RQ_lsmeans_4hrs$emmean <- Avg_RQ_lsmeans_4hrs$emmean
Avg_RQ_lsmeans_4hrs$SE <- Avg_RQ_lsmeans_4hrs$SE
Avg_RQ_lsmeans_4hrs <- Avg_RQ_lsmeans_4hrs[with(Avg_RQ_lsmeans_4hrs, order(four_hours, Genotype)), ]
write.table(Avg_RQ_contrasts_4hrs, file = "./output/202305_DBFX_Females_Sable_RC_AL_LD_Avg_RQ_contrasts_4hrs.csv")

Avg_RQ_lsmeans_4hrs <- Avg_RQ_lsmeans_4hrs[order(factor(Avg_RQ_lsmeans_4hrs$four_hours, levels = c('ZT0_ZT4', 'ZT4_ZT8', 'ZT8_ZT12', 'ZT12_ZT16', 'ZT16_ZT20', 'ZT20_ZT24'))),]

Avg_RQ_lsmeans_4hrs$zt <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6)

print(Avg_RQ_lsmeans_4hrs)

pdf(file = "./output/202305_DBFX_Females_Sable_RC_AL_Avg_RQ_4hrs_Line_Plot.pdf", height = 5, width = 7)

opar <- theme_update(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_rect(colour = "black"))
gp <- ggplot(Avg_RQ_lsmeans_4hrs, aes(x=zt, y=emmean, colour=Genotype, group=Genotype))
gp + geom_line(aes(linetype=Genotype), size=.6) + 
  geom_point(aes(shape=Genotype), size=3) + 
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE), width=.1) + ggtitle("Avg_RQ (4hrs)")
theme_set(opar)  
dev.off()


##############################################################
##### Avg_RQ Analysis (Day vs Night)####
##############################################################
RER_LD <- lmer(Avg_RQ_LD ~ Genotype*day_night + (1|StartDate) + (1|Animal), data=data2, REML = FALSE)

Anova(RER_LD)

RER_emmeans_LD <- emmeans(RER_LD, list(pairwise ~ Genotype:day_night), adjust = "fdr")

RER_lsmeans_LD <- RER_emmeans_LD$`emmeans of Genotype, day_night`

RER_lsmeans_LD <- as.data.frame(RER_lsmeans_LD)

RER_contrasts_LD <- as.data.frame(RER_emmeans_LD$`pairwise differences of Genotype, day_night`)

print(RER_lsmeans_LD)

RER_lsmeans_LD$emmean <- RER_lsmeans_LD$emmean
RER_lsmeans_LD$SE <- RER_lsmeans_LD$SE
RER_lsmeans_LD <- RER_lsmeans_LD[with(RER_lsmeans_LD, order(day_night, Genotype)), ]
write.table(RER_contrasts_LD, file = "./output/202305_DATBFX_Females_Sable_RC_AL_LD_RER_contrasts_LD.csv")

pdf(file = "./output/202305_DATBFX_Females_Sable_RC_AL_RER_LD.pdf", height = 4, width = 6)

predicted_values_RER_LD <- data2 %>%
  group_by(data2$Animal, data2$Genotype, data2$day_night) %>%
  summarise(Predicted_Value = mean(predict(RER_LD, newdata = cur_data(), re.form = ~(1 | Animal))), .groups = 'drop')

  colnames(predicted_values_RER_LD) <- c("Animal", "Genotype", "day_night", "Predicted_value")

   

# Create the plot 

ggplot() + 

  geom_col(data = RER_lsmeans_LD, aes(x = interaction(day_night, Genotype), y = emmean, fill = Genotype), position = position_dodge(), alpha = 0.7) + 

  geom_errorbar(data = RER_lsmeans_LD, aes(x = interaction(day_night, Genotype), y = emmean, ymin = emmean - SE, ymax = emmean + SE, group = Genotype), width = 0.2, position = position_dodge(.9)) + 

  geom_jitter(data = predicted_values_RER_LD, aes(x = interaction(day_night, Genotype), y = Predicted_value, color = Genotype), width = 0.2, size = 2, alpha = 0.7) + 

  theme_minimal() + 

  labs(y = "Average RER (VCO2/VO2)", x = "day_night:Genotype", title = "Light and Dark Average RER") 

dev.off() 

##############################################################
## Total Daily Avg_RQ (Avg_RQ_Daily)
##############################################################

RER_Daily <- lmer(Avg_RQ_Daily ~ Genotype + (1|Animal) + (1|StartDate), data=data2, REML=FALSE)

Anova(RER_Daily)

Avg_RQ_emmeans_Daily <- emmeans(RER_Daily, list(pairwise ~ Genotype), adjust = "fdr")

Avg_RQ_lsmeans_Daily <- Avg_RQ_emmeans_Daily$`emmeans of Genotype`

Avg_RQ_lsmeans_Daily <- as.data.frame(Avg_RQ_lsmeans_Daily)

Avg_RQ_contrasts_Daily <- as.data.frame(Avg_RQ_emmeans_Daily$`pairwise differences of Genotype`)

print(Avg_RQ_lsmeans_Daily)

Avg_RQ_lsmeans_Daily$emmean <- Avg_RQ_lsmeans_Daily$emmean
Avg_RQ_lsmeans_Daily$SE <- Avg_RQ_lsmeans_Daily$SE
Avg_RQ_lsmeans_Daily <- Avg_RQ_lsmeans_Daily[with(Avg_RQ_lsmeans_Daily, order(Genotype)), ]
write.table(Avg_RQ_contrasts_Daily, file = "./output/202305_DBFX_Females_Sable_RC_AL_Daily_Avg_RQ_contrasts_Daily.csv")
###  FDR-adjusted comparisons

pdf(file = "./output/202305_DBFX_Females_Sable_RC_AL_Daily_Average_RER.pdf", height = 4, width = 6)

predicted_values_Avg_RER_Daily <- data2 %>% 

  group_by(data2$Animal, data2$Genotype) %>% 

  summarise(Predicted_Value = mean(predict(RER_Daily, newdata = cur_data(), re.form = ~(1 | Animal))), .groups = 'drop')  

colnames(predicted_values_Avg_RER_Daily) <- c("Animal", "Genotype", "Predicted_value") 

   

# Create the plot 

ggplot() + 

  geom_col(data = Avg_RQ_lsmeans_Daily, aes(x = Genotype, y = emmean, fill = Genotype), position = position_dodge(), alpha = 0.7) + 

  geom_errorbar(data = Avg_RQ_lsmeans_Daily, aes(x = Genotype, y = emmean, ymin = emmean - SE, ymax = emmean + SE, group = Genotype), width = 0.2, position = position_dodge(.9)) + 

  geom_jitter(data = predicted_values_Avg_RER_Daily, aes(x = Genotype, y = Predicted_value, color = Genotype), width = 0.2, size = 2, alpha = 0.7) + 

  theme_minimal() + 

  labs(y = "RER (VCO2/VO2)", x = "Genotype", title = "Average Daily RER") 

  

dev.off()

##############################################################
##### Avg_EE Analysis (30 min)####
##############################################################

data_Avg_EE_30mins <- data_summary(data2, varname = "Avg_EE", groupnames = c("Cycle", "Genotype"))

pdf(file = "./output/202305_DATBFX_Females_LD_Sable_RC_AL_Avg_EE_30mins_Line_Plot.pdf", height = 5, width = 7)   

opar <- theme_update(panel.grid.major = element_blank(), 

                     panel.grid.minor = element_blank(), 

                     panel.background = element_rect(colour = "black")) 

gp <- ggplot(data_Avg_EE_30mins, aes(x=Cycle, y=Avg_EE, colour=Genotype, group=Genotype)) 

gp + geom_line(aes(linetype=Genotype), size=.6) +  

  geom_point(aes(shape=Genotype), size=3) +  

  geom_errorbar(aes(ymax=Avg_EE+se, ymin=Avg_EE-se), width=.1) + ggtitle("Avg_EE") 

theme_set(opar)   

dev.off()

##############################################################
##### Avg_EE Analysis (1hr)####
##############################################################

Avg_EE_1hr <- lmer(Avg_EE_1hr ~ Genotype*one_hour + BodyMass_g + (1|Animal) + (1|StartDate), data=data2, REML = FALSE)

Anova(Avg_EE_1hr)

Avg_EE_emmeans_1hr <- emmeans(Avg_EE_1hr, list(pairwise ~ Genotype:one_hour), adjust = "fdr")

Avg_EE_lsmeans_1hr <- Avg_EE_emmeans_1hr$`emmeans of Genotype, one_hour`

Avg_EE_lsmeans_1hr <- as.data.frame(Avg_EE_lsmeans_1hr)

Avg_EE_contrasts_1hr <- as.data.frame(Avg_EE_emmeans_1hr$`pairwise differences of Genotype, one_hour`)

print(Avg_EE_lsmeans_1hr)

Avg_EE_lsmeans_1hr$emmean <- Avg_EE_lsmeans_1hr$emmean
Avg_EE_lsmeans_1hr$SE <- Avg_EE_lsmeans_1hr$SE
Avg_EE_lsmeans_1hr <- Avg_EE_lsmeans_1hr[with(Avg_EE_lsmeans_1hr, order(one_hour, Genotype)), ]
write.table(Avg_EE_contrasts_1hr, file = "./output/202305_DBFX_Females_Sable_RC_AL_LD_Avg_EE_contrasts_1hr.csv")

Avg_EE_lsmeans_1hr <- Avg_EE_lsmeans_1hr[order(factor(Avg_EE_lsmeans_1hr$one_hour, levels = c('ZT0', 'ZT1', 'ZT2', 'ZT3', 'ZT4', 'ZT5', 'ZT6', 'ZT7', 'ZT8', 'ZT9', 'ZT10', 'ZT11', 'ZT12', 'ZT13', 'ZT14', 'ZT15', 'ZT16', 'ZT17', 'ZT18', 'ZT19', 'ZT20', 'ZT21', 'ZT22', 'ZT23'))),]

Avg_EE_lsmeans_1hr$zt <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14, 15, 15, 16, 16, 17, 17, 18, 18, 19, 19, 20, 20, 21, 21, 22, 22, 23, 23, 24, 24)

print(Avg_EE_lsmeans_1hr)

pdf(file = "./output/202305_DBFX_Females_Sable_RC_AL_Avg_EE_1hr_Line_Plot.pdf", height = 5, width = 7)

opar <- theme_update(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_rect(colour = "black"))
gp <- ggplot(Avg_EE_lsmeans_1hr, aes(x=zt, y=emmean, colour=Genotype, group=Genotype))
gp + geom_line(aes(linetype=Genotype), size=.6) + 
  geom_point(aes(shape=Genotype), size=3) + 
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE), width=.1) + ggtitle("Avg_EE (1hr)")
theme_set(opar)  
dev.off()

##############################################################
##### Avg_EE Analysis (2hrs)####
##############################################################

Avg_EE_2hrs <- lmer(Avg_EE_2hrs ~ Genotype*two_hours + BodyMass_g + (1|Animal) + (1|StartDate), data=data2, REML = FALSE)

Anova(Avg_EE_2hrs)

Avg_EE_emmeans_2hrs <- emmeans(Avg_EE_2hrs, list(pairwise ~ Genotype:two_hours), adjust = "fdr")

Avg_EE_lsmeans_2hrs <- Avg_EE_emmeans_2hrs$`emmeans of Genotype, two_hours`

Avg_EE_lsmeans_2hrs <- as.data.frame(Avg_EE_lsmeans_2hrs)

Avg_EE_contrasts_2hrs <- as.data.frame(Avg_EE_emmeans_2hrs$`pairwise differences of Genotype, two_hours`)

print(Avg_EE_lsmeans_2hrs)

Avg_EE_lsmeans_2hrs$emmean <- Avg_EE_lsmeans_2hrs$emmean
Avg_EE_lsmeans_2hrs$SE <- Avg_EE_lsmeans_2hrs$SE
Avg_EE_lsmeans_2hrs <- Avg_EE_lsmeans_2hrs[with(Avg_EE_lsmeans_2hrs, order(two_hours, Genotype)), ]
write.table(Avg_EE_contrasts_2hrs, file = "./output/202305_DBFX_Females_Sable_RC_AL_LD_Avg_EE_contrasts_2hrs.csv")

Avg_EE_lsmeans_2hrs <- Avg_EE_lsmeans_2hrs[order(factor(Avg_EE_lsmeans_2hrs$two_hours, levels = c('ZT0_ZT2', 'ZT2_ZT4', 'ZT4_ZT6', 'ZT6_ZT8', 'ZT8_ZT10', 'ZT10_ZT12', 'ZT12_ZT14', 'ZT14_Z1T6', 'ZT16_ZT18', 'ZT18_ZT20', 'ZT20_Z22', 'ZT22_ZT24'))),]

Avg_EE_lsmeans_2hrs$zt <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12)

print(Avg_EE_lsmeans_2hrs)

pdf(file = "./output/202305_DBFX_Females_Sable_RC_AL_Avg_EE_2hrs_Line_Plot.pdf", height = 5, width = 7)

opar <- theme_update(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_rect(colour = "black"))
gp <- ggplot(Avg_EE_lsmeans_2hrs, aes(x=zt, y=emmean, colour=Genotype, group=Genotype))
gp + geom_line(aes(linetype=Genotype), size=.6) + 
  geom_point(aes(shape=Genotype), size=3) + 
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE), width=.1) + ggtitle("Avg_EE (2hrs)")
theme_set(opar)  
dev.off()

##############################################################
##### Avg_EE Analysis (4hrs)####
##############################################################

Avg_EE_4hrs <- lmer(Avg_EE_4hrs ~ Genotype*four_hours + BodyMass_g + (1|Animal) + (1|StartDate), data=data2, REML = FALSE)

Anova(Avg_EE_4hrs)

Avg_EE_emmeans_4hrs <- emmeans(Avg_EE_4hrs, list(pairwise ~ Genotype:four_hours), adjust = "fdr")

Avg_EE_lsmeans_4hrs <- Avg_EE_emmeans_4hrs$`emmeans of Genotype, four_hours`

Avg_EE_lsmeans_4hrs <- as.data.frame(Avg_EE_lsmeans_4hrs)

Avg_EE_contrasts_4hrs <- as.data.frame(Avg_EE_emmeans_4hrs$`pairwise differences of Genotype, four_hours`)

print(Avg_EE_lsmeans_4hrs)

Avg_EE_lsmeans_4hrs$emmean <- Avg_EE_lsmeans_4hrs$emmean
Avg_EE_lsmeans_4hrs$SE <- Avg_EE_lsmeans_4hrs$SE
Avg_EE_lsmeans_4hrs <- Avg_EE_lsmeans_4hrs[with(Avg_EE_lsmeans_4hrs, order(four_hours, Genotype)), ]
write.table(Avg_EE_contrasts_4hrs, file = "./output/202305_DBFX_Females_Sable_RC_AL_LD_Avg_EE_contrasts_4hrs.csv")

Avg_EE_lsmeans_4hrs <- Avg_EE_lsmeans_4hrs[order(factor(Avg_EE_lsmeans_4hrs$four_hours, levels = c('ZT0_ZT4', 'ZT4_ZT8', 'ZT8_ZT12', 'ZT12_ZT16', 'ZT16_ZT20', 'ZT20_ZT24'))),]

Avg_EE_lsmeans_4hrs$zt <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6)

print(Avg_EE_lsmeans_4hrs)

pdf(file = "./output/202305_DBFX_Females_Sable_RC_AL_Avg_EE_4hrs_Line_Plot.pdf", height = 5, width = 7)

opar <- theme_update(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_rect(colour = "black"))
gp <- ggplot(Avg_EE_lsmeans_4hrs, aes(x=zt, y=emmean, colour=Genotype, group=Genotype))
gp + geom_line(aes(linetype=Genotype), size=.6) + 
  geom_point(aes(shape=Genotype), size=3) + 
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE), width=.1) + ggtitle("Avg_EE (4hrs)")
theme_set(opar)  
dev.off()


##############################################################
##### Avg_EE Analysis (Day vs Night)####
##############################################################

Avg_Energy_Expenditure_LD <- lmer(Avg_EE_LD ~ Genotype*day_night + BodyMass_g + (1|Animal) + (1|StartDate), data=data2, REML = FALSE)

Anova(Avg_Energy_Expenditure_LD)

Avg_EE_emmeans_LD <- emmeans(Avg_Energy_Expenditure_LD, list(pairwise ~ Genotype:day_night), adjust = "fdr")

Avg_EE_lsmeans_LD <- Avg_EE_emmeans_LD$`emmeans of Genotype, day_night`

Avg_EE_lsmeans_LD <- as.data.frame(Avg_EE_lsmeans_LD)

Avg_EE_contrasts_LD <- as.data.frame(Avg_EE_emmeans_LD$`pairwise differences of Genotype, day_night`)

print(Avg_EE_lsmeans_LD)

Avg_EE_lsmeans_LD$emmean <- Avg_EE_lsmeans_LD$emmean
Avg_EE_lsmeans_LD$SE <- Avg_EE_lsmeans_LD$SE
Avg_EE_lsmeans_LD <- Avg_EE_lsmeans_LD[with(Avg_EE_lsmeans_LD, order(day_night, Genotype)), ]
write.table(Avg_EE_contrasts_LD, file = "./output/202305_DBFX_Females_Sable_RC_AL_LD_Avg_EE_contrasts_LD.csv")
###  FDR-adjusted comparisons

print(Avg_EE_lsmeans_LD)

pdf(file = "./output/202305_DBFX_Females_Sable_RC_AL_LD_Average_Energy_Expenditure.pdf", height = 4, width = 6)

predicted_values_Avg_EE_LD <- data2 %>% 

  group_by(data2$Animal, data2$Genotype, data2$day_night) %>% 

  summarise(Predicted_Value = mean(predict(Avg_Energy_Expenditure_LD, newdata = cur_data(), re.form = ~(1 | Animal))), .groups = 'drop')  

colnames(predicted_values_Avg_EE_LD) <- c("Animal", "Genotype", "day_night", "Predicted_value") 

   

# Create the plot 

ggplot() + 

  geom_col(data = Avg_EE_lsmeans_LD, aes(x = interaction(day_night, Genotype), y = emmean, fill = Genotype), position = position_dodge(), alpha = 0.7) + 

  geom_errorbar(data = Avg_EE_lsmeans_LD, aes(x = interaction(day_night, Genotype), y = emmean, ymin = emmean - SE, ymax = emmean + SE, group = Genotype), width = 0.2, position = position_dodge(.9)) + 

  geom_jitter(data = predicted_values_Avg_EE_LD, aes(x = interaction(day_night, Genotype), y = Predicted_value, color = Genotype), width = 0.2, size = 2, alpha = 0.7) + 

  theme_minimal() + 

  labs(y = "Energy Expenditure (kcal/hr)", x = "day_night:Genotype", title = "Light and Dark Average Energy Expenditure") 

  

dev.off() 

##############################################################
## Total Daily Avg_EE (Avg_EE_Daily)
##############################################################

Avg_Energy_Expenditure_Daily <- lmer(Avg_EE_Daily ~ Genotype + BodyMass_g + (1|Animal) + (1|StartDate), data=data2, REML=FALSE)

Anova(Avg_Energy_Expenditure_Daily)

Avg_EE_emmeans_Daily <- emmeans(Avg_Energy_Expenditure_Daily, list(pairwise ~ Genotype), adjust = "fdr")

Avg_EE_lsmeans_Daily <- Avg_EE_emmeans_Daily$`emmeans of Genotype`

Avg_EE_lsmeans_Daily <- as.data.frame(Avg_EE_lsmeans_Daily)

Avg_EE_contrasts_Daily <- as.data.frame(Avg_EE_emmeans_Daily$`pairwise differences of Genotype`)

print(Avg_EE_lsmeans_Daily)

Avg_EE_lsmeans_Daily$emmean <- Avg_EE_lsmeans_Daily$emmean
Avg_EE_lsmeans_Daily$SE <- Avg_EE_lsmeans_Daily$SE
Avg_EE_lsmeans_Daily <- Avg_EE_lsmeans_Daily[with(Avg_EE_lsmeans_Daily, order(Genotype)), ]
write.table(Avg_EE_contrasts_Daily, file = "./output/202305_DBFX_Females_Sable_RC_AL_Daily_Avg_EE_contrasts_Daily.csv")
###  FDR-adjusted comparisons

pdf(file = "./output/202305_DBFX_Females_Sable_RC_AL_Daily_Average_Energy_Expenditure.pdf", height = 4, width = 6)

predicted_values_Avg_EE_Daily <- data2 %>% 

  group_by(data2$Animal, data2$Genotype) %>% 

  summarise(Predicted_Value = mean(predict(Avg_Energy_Expenditure_Daily, newdata = cur_data(), re.form = ~(1 | Animal))), .groups = 'drop')  

colnames(predicted_values_Avg_EE_Daily) <- c("Animal", "Genotype", "Predicted_value") 

   

# Create the plot 

ggplot() + 

  geom_col(data = Avg_EE_lsmeans_Daily, aes(x = Genotype, y = emmean, fill = Genotype), position = position_dodge(), alpha = 0.7) + 

  geom_errorbar(data = Avg_EE_lsmeans_Daily, aes(x = Genotype, y = emmean, ymin = emmean - SE, ymax = emmean + SE, group = Genotype), width = 0.2, position = position_dodge(.9)) + 

  geom_jitter(data = predicted_values_Avg_EE_LD, aes(x = Genotype, y = Predicted_value, color = Genotype), width = 0.2, size = 2, alpha = 0.7) + 

  theme_minimal() + 

  labs(y = "Energy Expenditure (kcal/hr)", x = "Genotype", title = "Daily Average Energy Expenditure") 

  

dev.off()

##############################################################
##### Tot_EE Analysis (30 min)####
##############################################################

data_Tot_EE_30mins <- data_summary(data2, varname = "Tot_EE", groupnames = c("Cycle", "Genotype"))

pdf(file = "./output/202305_DATBFX_Females_LD_Sable_RC_AL_Tot_EE_30mins_Line_Plot.pdf", height = 5, width = 7) 

opar <- theme_update(panel.grid.major = element_blank(), 

                     panel.grid.minor = element_blank(), 

                     panel.background = element_rect(colour = "black")) 

gp <- ggplot(data_Tot_EE_30mins, aes(x=Cycle, y=Tot_EE, colour=Genotype, group=Genotype)) 

gp + geom_line(aes(linetype=Genotype), size=.6) +  

  geom_point(aes(shape=Genotype), size=3) +  

  geom_errorbar(aes(ymax=Tot_EE+se, ymin=Tot_EE-se), width=.1) + ggtitle("Tot_EE") 

theme_set(opar)   

dev.off()

##############################################################
##### Tot_EE Analysis (1hr)####
##############################################################

Tot_EE_1hr <- lmer(Tot_EE_1hr ~ Genotype*one_hour + BodyMass_g + (1|Animal) + (1|StartDate), data=data2, REML = FALSE)

Anova(Tot_EE_1hr)

Tot_EE_emmeans_1hr <- emmeans(Tot_EE_1hr, list(pairwise ~ Genotype:one_hour), adjust = "fdr")

Tot_EE_lsmeans_1hr <- Tot_EE_emmeans_1hr$`emmeans of Genotype, one_hour`

Tot_EE_lsmeans_1hr <- as.data.frame(Tot_EE_lsmeans_1hr)

Tot_EE_contrasts_1hr <- as.data.frame(Tot_EE_emmeans_1hr$`pairwise differences of Genotype, one_hour`)

print(Tot_EE_lsmeans_1hr)

Tot_EE_lsmeans_1hr$emmean <- Tot_EE_lsmeans_1hr$emmean
Tot_EE_lsmeans_1hr$SE <- Tot_EE_lsmeans_1hr$SE
Tot_EE_lsmeans_1hr <- Tot_EE_lsmeans_1hr[with(Tot_EE_lsmeans_1hr, order(one_hour, Genotype)), ]
write.table(Tot_EE_contrasts_1hr, file = "./output/202305_DBFX_Females_Sable_RC_AL_LD_Tot_EE_contrasts_1hr.csv")

Tot_EE_lsmeans_1hr <- Tot_EE_lsmeans_1hr[order(factor(Tot_EE_lsmeans_1hr$one_hour, levels = c('ZT0', 'ZT1', 'ZT2', 'ZT3', 'ZT4', 'ZT5', 'ZT6', 'ZT7', 'ZT8', 'ZT9', 'ZT10', 'ZT11', 'ZT12', 'ZT13', 'ZT14', 'ZT15', 'ZT16', 'ZT17', 'ZT18', 'ZT19', 'ZT20', 'ZT21', 'ZT22', 'ZT23'))),]

Tot_EE_lsmeans_1hr$zt <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14, 15, 15, 16, 16, 17, 17, 18, 18, 19, 19, 20, 20, 21, 21, 22, 22, 23, 23, 24, 24)

print(Tot_EE_lsmeans_1hr)

pdf(file = "./output/202305_DBFX_Females_Sable_RC_AL_Tot_EE_1hr_Line_Plot.pdf", height = 5, width = 7)

opar <- theme_update(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_rect(colour = "black"))
gp <- ggplot(Tot_EE_lsmeans_1hr, aes(x=zt, y=emmean, colour=Genotype, group=Genotype))
gp + geom_line(aes(linetype=Genotype), size=.6) + 
  geom_point(aes(shape=Genotype), size=3) + 
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE), width=.1) + ggtitle("Tot_EE (1hr)")
theme_set(opar)  
dev.off()

##############################################################
##### Tot_EE Analysis (2hrs)####
##############################################################

Tot_EE_2hrs <- lmer(Tot_EE_2hrs ~ Genotype*two_hours + BodyMass_g + (1|Animal) + (1|StartDate), data=data2, REML = FALSE)

Anova(Tot_EE_2hrs)

Tot_EE_emmeans_2hrs <- emmeans(Tot_EE_2hrs, list(pairwise ~ Genotype:two_hours), adjust = "fdr")

Tot_EE_lsmeans_2hrs <- Tot_EE_emmeans_2hrs$`emmeans of Genotype, two_hours`

Tot_EE_lsmeans_2hrs <- as.data.frame(Tot_EE_lsmeans_2hrs)

Tot_EE_contrasts_2hrs <- as.data.frame(Tot_EE_emmeans_2hrs$`pairwise differences of Genotype, two_hours`)

print(Tot_EE_lsmeans_2hrs)

Tot_EE_lsmeans_2hrs$emmean <- Tot_EE_lsmeans_2hrs$emmean
Tot_EE_lsmeans_2hrs$SE <- Tot_EE_lsmeans_2hrs$SE
Tot_EE_lsmeans_2hrs <- Tot_EE_lsmeans_2hrs[with(Tot_EE_lsmeans_2hrs, order(two_hours, Genotype)), ]
write.table(Tot_EE_contrasts_2hrs, file = "./output/202305_DBFX_Females_Sable_RC_AL_LD_Tot_EE_contrasts_2hrs.csv")

Tot_EE_lsmeans_2hrs <- Tot_EE_lsmeans_2hrs[order(factor(Tot_EE_lsmeans_2hrs$two_hours, levels = c('ZT0_ZT2', 'ZT2_ZT4', 'ZT4_ZT6', 'ZT6_ZT8', 'ZT8_ZT10', 'ZT10_ZT12', 'ZT12_ZT14', 'ZT14_Z1T6', 'ZT16_ZT18', 'ZT18_ZT20', 'ZT20_Z22', 'ZT22_ZT24'))),]

Tot_EE_lsmeans_2hrs$zt <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12)

print(Tot_EE_lsmeans_2hrs)

pdf(file = "./output/202305_DBFX_Females_Sable_RC_AL_Tot_EE_2hrs_Line_Plot.pdf", height = 5, width = 7)

opar <- theme_update(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_rect(colour = "black"))
gp <- ggplot(Tot_EE_lsmeans_2hrs, aes(x=zt, y=emmean, colour=Genotype, group=Genotype))
gp + geom_line(aes(linetype=Genotype), size=.6) + 
  geom_point(aes(shape=Genotype), size=3) + 
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE), width=.1) + ggtitle("Tot_EE (2hrs)")
theme_set(opar)  
dev.off()

##############################################################
##### Tot_EE Analysis (4hrs)####
##############################################################

Tot_EE_4hrs <- lmer(Tot_EE_4hrs ~ Genotype*four_hours + BodyMass_g + (1|Animal) + (1|StartDate), data=data2, REML = FALSE)

Anova(Tot_EE_4hrs)

Tot_EE_emmeans_4hrs <- emmeans(Tot_EE_4hrs, list(pairwise ~ Genotype:four_hours), adjust = "fdr")

Tot_EE_lsmeans_4hrs <- Tot_EE_emmeans_4hrs$`emmeans of Genotype, four_hours`

Tot_EE_lsmeans_4hrs <- as.data.frame(Tot_EE_lsmeans_4hrs)

Tot_EE_contrasts_4hrs <- as.data.frame(Tot_EE_emmeans_4hrs$`pairwise differences of Genotype, four_hours`)

print(Tot_EE_lsmeans_4hrs)

Tot_EE_lsmeans_4hrs$emmean <- Tot_EE_lsmeans_4hrs$emmean
Tot_EE_lsmeans_4hrs$SE <- Tot_EE_lsmeans_4hrs$SE
Tot_EE_lsmeans_4hrs <- Tot_EE_lsmeans_4hrs[with(Tot_EE_lsmeans_4hrs, order(four_hours, Genotype)), ]
write.table(Tot_EE_contrasts_4hrs, file = "./output/202305_DBFX_Females_Sable_RC_AL_LD_Tot_EE_contrasts_4hrs.csv")

Tot_EE_lsmeans_4hrs <- Tot_EE_lsmeans_4hrs[order(factor(Tot_EE_lsmeans_4hrs$four_hours, levels = c('ZT0_ZT4', 'ZT4_ZT8', 'ZT8_ZT12', 'ZT12_ZT16', 'ZT16_ZT20', 'ZT20_ZT24'))),]

Tot_EE_lsmeans_4hrs$zt <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6)

print(Tot_EE_lsmeans_4hrs)

pdf(file = "./output/202305_DBFX_Females_Sable_RC_AL_Tot_EE_4hrs_Line_Plot.pdf", height = 5, width = 7)

opar <- theme_update(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_rect(colour = "black"))
gp <- ggplot(Tot_EE_lsmeans_4hrs, aes(x=zt, y=emmean, colour=Genotype, group=Genotype))
gp + geom_line(aes(linetype=Genotype), size=.6) + 
  geom_point(aes(shape=Genotype), size=3) + 
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE), width=.1) + ggtitle("Tot_EE (4hrs)")
theme_set(opar)  
dev.off()


##############################################################
##### Tot_EE Analysis (Day vs Night)####
##############################################################
Tot_Energy_Expenditure_LD <- lmer(Tot_EE_LD ~ Genotype*day_night + BodyMass_g + (1|Animal) + (1|StartDate), data=data2, REML = FALSE)

Anova(Tot_Energy_Expenditure_LD)

Tot_EE_emmeans_LD <- emmeans(Tot_Energy_Expenditure_LD, list(pairwise ~ Genotype:day_night), adjust = "fdr")

Tot_EE_lsmeans_LD <- Tot_EE_emmeans_LD$`emmeans of Genotype, day_night`

Tot_EE_lsmeans_LD <- as.data.frame(Tot_EE_lsmeans_LD)

Tot_EE_contrasts_LD <- as.data.frame(Tot_EE_emmeans_LD$`pairwise differences of Genotype, day_night`)

print(Tot_EE_lsmeans_LD)

Tot_EE_lsmeans_LD$emmean <- Tot_EE_lsmeans_LD$emmean
Tot_EE_lsmeans_LD$SE <- Tot_EE_lsmeans_LD$SE
Tot_EE_lsmeans_LD <- Tot_EE_lsmeans_LD[with(Tot_EE_lsmeans_LD, order(day_night, Genotype)), ]
write.table(Tot_EE_contrasts_LD, file = "./output/202305_DBFX_Females_Sable_RC_AL_LD_Tot_EE_contrasts_LD.csv")
###  FDR-adjusted comparisons

print(Tot_EE_lsmeans_LD)

pdf(file = "./output/202305_DBFX_Females_Sable_RC_AL_LD_Total_Energy_Expenditure.pdf", height = 4, width = 6)

predicted_values_Tot_EE_LD <- data2 %>% 

  group_by(data2$Animal, data2$Genotype, data2$day_night) %>% 

  summarise(Predicted_Value = mean(predict(Tot_Energy_Expenditure_LD, newdata = cur_data(), re.form = ~(1 | Animal))), .groups = 'drop')  

colnames(predicted_values_Tot_EE_LD) <- c("Animal", "Genotype", "day_night", "Predicted_value") 

   

# Create the plot 

ggplot() + 

  geom_col(data = Tot_EE_lsmeans_LD, aes(x = interaction(day_night, Genotype), y = emmean, fill = Genotype), position = position_dodge(), alpha = 0.7) + 

  geom_errorbar(data = Tot_EE_lsmeans_LD, aes(x = interaction(day_night, Genotype), y = emmean, ymin = emmean - SE, ymax = emmean + SE, group = Genotype), width = 0.2, position = position_dodge(.9)) + 

  geom_jitter(data = predicted_values_Tot_EE_LD, aes(x = interaction(day_night, Genotype), y = Predicted_value, color = Genotype), width = 0.2, size = 2, alpha = 0.7) + 

  theme_minimal() + 

  labs(y = "Energy Expenditure (kcal/hr)", x = "day_night:Genotype", title = "Light and Dark Cumulative Energy Expenditure") 

  

dev.off() 

##############################################################
## Total Daily Tot_EE (Tot_EE_Daily)
##############################################################

Tot_Energy_Expenditure_Daily <- lmer(Tot_EE_Daily ~ Genotype + BodyMass_g + (1|Animal) + (1|StartDate), data=data2, REML=FALSE)

Anova(Tot_Energy_Expenditure_Daily)

Tot_EE_emmeans_Daily <- emmeans(Tot_Energy_Expenditure_Daily, list(pairwise ~ Genotype), adjust = "fdr")

Tot_EE_lsmeans_Daily <- Tot_EE_emmeans_Daily$`emmeans of Genotype`

Tot_EE_lsmeans_Daily <- as.data.frame(Tot_EE_lsmeans_Daily)

Tot_EE_contrasts_Daily <- as.data.frame(Tot_EE_emmeans_Daily$`pairwise differences of Genotype`)

print(Tot_EE_lsmeans_Daily)

Tot_EE_lsmeans_Daily$emmean <- Tot_EE_lsmeans_Daily$emmean
Tot_EE_lsmeans_Daily$SE <- Tot_EE_lsmeans_Daily$SE
Tot_EE_lsmeans_Daily <- Tot_EE_lsmeans_Daily[with(Tot_EE_lsmeans_Daily, order(Genotype)), ]
write.table(Tot_EE_contrasts_Daily, file = "./output/202305_DBFX_Females_Sable_RC_AL_Daily_Tot_EE_contrasts_Daily.csv")
###  FDR-adjusted comparisons

pdf(file = "./output/202305_DBFX_Females_Sable_RC_AL_Daily_Average_Energy_Expenditure.pdf", height = 4, width = 6)

predicted_values_Tot_EE_Daily <- data2 %>% 

  group_by(data2$Animal, data2$Genotype) %>% 

  summarise(Predicted_Value = mean(predict(Tot_Energy_Expenditure_Daily, newdata = cur_data(), re.form = ~(1 | Animal))), .groups = 'drop')  

colnames(predicted_values_Tot_EE_Daily) <- c("Animal", "Genotype", "Predicted_value") 

   

# Create the plot 

ggplot() + 

  geom_col(data = Tot_EE_lsmeans_Daily, aes(x = Genotype, y = emmean, fill = Genotype), position = position_dodge(), alpha = 0.7) + 

  geom_errorbar(data = Tot_EE_lsmeans_Daily, aes(x = Genotype, y = emmean, ymin = emmean - SE, ymax = emmean + SE, group = Genotype), width = 0.2, position = position_dodge(.9)) + 

  geom_jitter(data = predicted_values_Tot_EE_LD, aes(x = Genotype, y = Predicted_value, color = Genotype), width = 0.2, size = 2, alpha = 0.7) + 

  theme_minimal() + 

  labs(y = "Energy Expenditure (kcal)", x = "Genotype", title = "Total Daily Energy Expenditure") 

  

dev.off()


##################End of Code##################