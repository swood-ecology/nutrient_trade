setwd("/Volumes/My Passport for Mac/Data/Nutrition")

prod <- read.csv("Food Balance Sheets/FoodBalanceSheets_FORMAT_5_20002011AVG_Food_Production_Trade_Edible_Nutrients.csv",header=T) 
prod <- prod[,-1]
rni <- read.csv("Current Nutrient Databases/RNI/RNI_co.csv",header=T)
colnames(rni)[3] <- "Country"

together <- merge(prod,rni,by="Country") 

rni.final <- data.frame(together[,1:2])
rni.final$protein <- (together$Protein_g/365)/together$Protein..g.d.
rni.final$energy <- (together$Energy_kcal/365)/together$Energy..kcal.d.
rni.final$zinc <- (together$Zn_mg/365)/together$Zinc..mg.d.
rni.final$calcium <- (together$Ca_mg/365)/together$Calcium..mg.d.
rni.final$iron <- (together$Fe_mg/365)/together$Iron..mg.d.
rni.final$VitB12 <- (together$VitB12_ug/365)/together$Vitamin.B12..mcg.d.
rni.final$folate <- (together$Folate_ug/365)/together$Folate..mcg.DFE.d.
rni.final$VitA <- (together$VitA_ug/365)/together$Vitamin.A..mcg.RE.day.
rni.final$VitD <- (together$VitD_ug/365)/together$Vitamin.D..mcg.d.

write.csv(rni.final,"Food Balance Sheets/FoodBalanceSheets_FORMAT_6_20002011AVG_Food_Production_Trade_RNI.csv")

popn <- read.csv("Population.csv")
rni.popn <- merge(rni.final,popn,by="Country")
rni.popn$protein <- rni.popn$protein/rni.popn$AVG.POPN.2007.2011
rni.popn$energy <- rni.popn$energy/rni.popn$AVG.POPN.2007.2011
rni.popn$zinc <- rni.popn$zinc/rni.popn$AVG.POPN.2007.2011
rni.popn$calcium <- rni.popn$calcium/rni.popn$AVG.POPN.2007.2011
rni.popn$iron <- rni.popn$iron/rni.popn$AVG.POPN.2007.2011
rni.popn$VitB12 <- rni.popn$VitB12/rni.popn$AVG.POPN.2007.2011
rni.popn$folate <- rni.popn$folate/rni.popn$AVG.POPN.2007.2011
rni.popn$VitA <- rni.popn$VitA/rni.popn$AVG.POPN.2007.2011
rni.popn$VitD <- rni.popn$VitD/rni.popn$AVG.POPN.2007.2011

write.csv(rni.popn[,c(1:11)],"Food Balance Sheets/FoodBalanceSheets_FORMAT_7_20002011AVG_Food_Production_Trade_RNIperCapita.csv")






# Do for other elements

prod <- read.csv("Food Balance Sheets/FoodBalanceSheets_FORMAT_5_ALT_20002011AVG_ALL_ELEMENTS_Edible_Nutrients.csv",header=T) 
prod <- prod[,-1]
rni <- read.csv("Current Nutrient Databases/RNI/RNI_co.csv",header=T)
colnames(rni)[3] <- "Country"

together <- merge(prod,rni,by="Country") 

rni.final <- data.frame(together[,1:2])
rni.final$protein <- (together$Protein_g/365)/together$Protein..g.d.
rni.final$energy <- (together$Energy_kcal/365)/together$Energy..kcal.d.
rni.final$zinc <- (together$Zn_mg/365)/together$Zinc..mg.d.
rni.final$calcium <- (together$Ca_mg/365)/together$Calcium..mg.d.
rni.final$iron <- (together$Fe_mg/365)/together$Iron..mg.d.
rni.final$VitB12 <- (together$VitB12_ug/365)/together$Vitamin.B12..mcg.d.
rni.final$folate <- (together$Folate_ug/365)/together$Folate..mcg.DFE.d.
rni.final$VitA <- (together$VitA_ug/365)/together$Vitamin.A..mcg.RE.day.
rni.final$VitD <- (together$VitD_ug/365)/together$Vitamin.D..mcg.d.

write.csv(rni.final,"Food Balance Sheets/FoodBalanceSheets_FORMAT_6_ALT_20002011AVG_ALL_ELEMENTS_RNI.csv")

popn <- read.csv("Population.csv")
rni.popn <- merge(rni.final,popn,by="Country")
rni.popn$protein <- rni.popn$protein/rni.popn$AVG.POPN.2007.2011
rni.popn$energy <- rni.popn$energy/rni.popn$AVG.POPN.2007.2011
rni.popn$zinc <- rni.popn$zinc/rni.popn$AVG.POPN.2007.2011
rni.popn$calcium <- rni.popn$calcium/rni.popn$AVG.POPN.2007.2011
rni.popn$iron <- rni.popn$iron/rni.popn$AVG.POPN.2007.2011
rni.popn$VitB12 <- rni.popn$VitB12/rni.popn$AVG.POPN.2007.2011
rni.popn$folate <- rni.popn$folate/rni.popn$AVG.POPN.2007.2011
rni.popn$VitA <- rni.popn$VitA/rni.popn$AVG.POPN.2007.2011
rni.popn$VitD <- rni.popn$VitD/rni.popn$AVG.POPN.2007.2011

write.csv(rni.popn[,c(1:11)],"Food Balance Sheets/FoodBalanceSheets_FORMAT_7_ALT_20002011AVG_ALL_ELEMENTS_RNIperCapita.csv")
