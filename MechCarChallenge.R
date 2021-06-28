library(dplyr)
library(tidyverse)

MechaCar <- read.csv('../Documents/Ranalysis Mod 15/MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
MechaData <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=MechaCar) 

summary(MechaData)

Suspension_table <- read.csv('../Documents/Ranalysis Mod 15/Suspension_Coil.csv',check.names=F,stringsAsFactors = F)

sum_total <- Suspension_table %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI)) 
sum_lot <- Suspension_table %>% group_by(Manufacturing_Lot)%>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI))

t.test(Suspension_table$PSI,mu = 1500)

t.test(subset(Suspension_table,Manufacturing_Lot=="Lot1")$PSI,mu = 1500)
t.test(subset(Suspension_table,Manufacturing_Lot=="Lot2")$PSI,mu = 1500)
t.test(subset(Suspension_table,Manufacturing_Lot=="Lot3")$PSI,mu = 1500)
