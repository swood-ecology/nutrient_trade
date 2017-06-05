# ----------------------------
# Bar charts comparing trade and waste + other uses
# Stephen Wood
# ----------------------------

# read nutrient production data per capita
rni.cap <- read.csv("/Volumes/My Passport for Mac/Data/Nutrition/Food Balance Sheets/FoodBalanceSheets_FORMAT_7_ALT_20002011AVG_ALL_ELEMENTS_RNIperCapita.csv",header=T)
rni.cap <- rni.cap[,-1]

# read nutrient production data
rni <- read.csv("/Volumes/My Passport for Mac/Data/Nutrition/Food Balance Sheets/FoodBalanceSheets_FORMAT_6_ALT_20002011AVG_ALL_ELEMENTS_RNI.csv",header=T)
rni <- rni[,-1]

# retain trade, waste, and other data
rni.cap <- rni.cap[rni.cap$Element == "Waste" | rni.cap$Element == "Other uses" | rni.cap$Element == "Import Quantity" | rni.cap$Element == 'ExportQuantity', ]
rni <- rni[rni$Element == "Waste" | rni$Element == "Other uses" | rni$Element == "Import Quantity" | rni$Element == 'ExportQuantity', ]

# expand data
library(reshape2)
rni.cap.wide <- dcast(melt(rni.cap,id.vars=c("Country","Element")),Country~Element+variable)
rni.wide <- dcast(melt(rni,id.vars=c("Country","Element")),Country~Element+variable)

# calculate aggregate variables
rni.cap.wide$protein_waste_other <- rni.cap.wide$Waste_protein + rni.cap.wide$`Other uses_protein`
rni.cap.wide$protein_trade <- rni.cap.wide$`Import Quantity_protein` - rni.cap.wide$ExportQuantity_protein
rni.cap.wide$energy_waste_other <- rni.cap.wide$Waste_energy + rni.cap.wide$`Other uses_energy`
rni.cap.wide$energy_trade <- rni.cap.wide$`Import Quantity_energy` - rni.cap.wide$ExportQuantity_energy
rni.cap.wide$zinc_waste_other <- rni.cap.wide$Waste_zinc + rni.cap.wide$`Other uses_zinc`
rni.cap.wide$zinc_trade <- rni.cap.wide$`Import Quantity_zinc` - rni.cap.wide$ExportQuantity_zinc
rni.cap.wide$calcium_waste_other <- rni.cap.wide$Waste_calcium + rni.cap.wide$`Other uses_calcium`
rni.cap.wide$calcium_trade <- rni.cap.wide$`Import Quantity_calcium` - rni.cap.wide$ExportQuantity_calcium
rni.cap.wide$iron_waste_other <- rni.cap.wide$Waste_iron + rni.cap.wide$`Other uses_iron`
rni.cap.wide$iron_trade <- rni.cap.wide$`Import Quantity_iron` - rni.cap.wide$ExportQuantity_iron
rni.cap.wide$VitB12_waste_other <- rni.cap.wide$Waste_VitB12 + rni.cap.wide$`Other uses_VitB12`
rni.cap.wide$VitB12_trade <- rni.cap.wide$`Import Quantity_VitB12` - rni.cap.wide$ExportQuantity_VitB12
rni.cap.wide$VitA_waste_other <- rni.cap.wide$Waste_VitA + rni.cap.wide$`Other uses_VitA`
rni.cap.wide$VitA_trade <- rni.cap.wide$`Import Quantity_VitA` - rni.cap.wide$ExportQuantity_VitA
rni.cap.wide$folate_waste_other <- rni.cap.wide$Waste_folate + rni.cap.wide$`Other uses_folate`
rni.cap.wide$folate_trade <- rni.cap.wide$`Import Quantity_folate` - rni.cap.wide$ExportQuantity_folate

rni.wide$protein_waste_other <- rni.wide$Waste_protein + rni.wide$`Other uses_protein`
rni.wide$protein_trade <- rni.wide$`Import Quantity_protein` - rni.wide$ExportQuantity_protein
rni.wide$energy_waste_other <- rni.wide$Waste_energy + rni.wide$`Other uses_energy`
rni.wide$energy_trade <- rni.wide$`Import Quantity_energy` - rni.wide$ExportQuantity_energy
rni.wide$zinc_waste_other <- rni.wide$Waste_zinc + rni.wide$`Other uses_zinc`
rni.wide$zinc_trade <- rni.wide$`Import Quantity_zinc` - rni.wide$ExportQuantity_zinc
rni.wide$calcium_waste_other <- rni.wide$Waste_calcium + rni.wide$`Other uses_calcium`
rni.wide$calcium_trade <- rni.wide$`Import Quantity_calcium` - rni.wide$ExportQuantity_calcium
rni.wide$iron_waste_other <- rni.wide$Waste_iron + rni.wide$`Other uses_iron`
rni.wide$iron_trade <- rni.wide$`Import Quantity_iron` - rni.wide$ExportQuantity_iron
rni.wide$VitB12_waste_other <- rni.wide$Waste_VitB12 + rni.wide$`Other uses_VitB12`
rni.wide$VitB12_trade <- rni.wide$`Import Quantity_VitB12` - rni.wide$ExportQuantity_VitB12
rni.wide$VitA_waste_other <- rni.wide$Waste_VitA + rni.wide$`Other uses_VitA`
rni.wide$VitA_trade <- rni.wide$`Import Quantity_VitA` - rni.wide$ExportQuantity_VitA
rni.wide$folate_waste_other <- rni.wide$Waste_folate + rni.wide$`Other uses_folate`
rni.wide$folate_trade <- rni.wide$`Import Quantity_folate` - rni.wide$ExportQuantity_folate

# remove original variables and collapse data
rni.cap.wide <- rni.cap.wide[,-c(2:37)]
rni.wide <- rni.wide[,-c(2:37)]

# make bar plot for waste and other per capita
library(ggplot2)

waste_other <- rni.cap.wide[,c(1,2,4,6,8,10,12,14,16)]
waste.other.plot <- data.frame(apply(waste_other[,-1], 2, mean, na.rm=T),
                               apply(waste_other[,-1], 2, sd, na.rm=T),c("Protein","Energy","Zinc",
                                                                        "Calcium","Iron",
                                                                        "Vitamin B12","Vitamin A",
                                                                        "Folate"))
names(waste.other.plot) <- c("Average","SD","Nutrient")
waste.other.plot$ymax <- waste.other.plot$Average + waste.other.plot$SD
waste.other.plot$ymin <- waste.other.plot$Average - waste.other.plot$SD

ggplot(waste.other.plot,aes(x=Nutrient,y=Average)) + 
  geom_bar(stat="identity",fill="grey80") + 
  geom_errorbar(aes(ymax=ymax,ymin=ymin),position = position_dodge(0.9),width = 0.25) +
  ylab("Fraction of population potentially nourished") + theme_bw()

# make bar plot for trade per capita
trade <- rni.cap.wide[,c(1,3,5,7,9,11,13,15,17)]
trade.plot <- data.frame(apply(trade[,-1], 2, mean, na.rm=T),
                               apply(trade[,-1], 2, sd, na.rm=T),c("Protein","Energy","Zinc",
                                                                        "Calcium","Iron",
                                                                        "Vitamin B12","Vitamin A",
                                                                        "Folate"))
names(trade.plot) <- c("Average","SD","Nutrient")
trade.plot$ymax <- trade.plot$Average + trade.plot$SD
trade.plot$ymin <- trade.plot$Average - trade.plot$SD

ggplot(trade.plot,aes(x=Nutrient,y=Average)) + 
  geom_bar(stat="identity",fill="grey80") + 
  geom_errorbar(aes(ymax=ymax,ymin=ymin),position = position_dodge(0.9),width = 0.25) +
  ylab("Fraction of population potentially nourished") + theme_bw()

# make bar plot for waste and other
waste_other <- rni.wide[,c(1,2,4,6,8,10,12,14,16)]
waste.other.plot <- data.frame((apply(waste_other[,-1], 2, sum, na.rm=T)/1000000000),c("Protein","Energy","Zinc",
                                                                         "Calcium","Iron",
                                                                         "Vitamin B12","Vitamin A",
                                                                         "Folate"))
names(waste.other.plot) <- c("Total_billions","Nutrient")

ggplot(waste.other.plot,aes(x=Nutrient,y=Total_billions)) + 
  geom_bar(stat="identity",fill="grey80") + 
  ylab("Billions of people potentially nourished") + theme_bw()

# make bar plot for trade
trade <- rni.wide[,c(1,3,5,7,9,11,13,15,17)]
trade.plot <- data.frame((apply(trade[,-1], 2, mean, na.rm=T)/1000000000),
                         c("Protein","Energy","Zinc","Calcium","Iron","Vitamin B12","Vitamin A",
                           "Folate"))
names(trade.plot) <- c("Total_billions","Nutrient")

ggplot(trade.plot,aes(x=Nutrient,y=Total_billions)) + 
  geom_bar(stat="identity",fill="grey80") + 
  ylab("Billions of people potentially nourished") + theme_bw()
