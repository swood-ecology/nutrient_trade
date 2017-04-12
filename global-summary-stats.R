# ---------------------
# Calculate summary statistics of nutrient sufficiency to the global level
# Steve Wood
# ---------------------

rni <- read.csv("/Volumes/My Passport for Mac/Data/Nutrition/Food Balance Sheets/FoodBalanceSheets_FORMAT_6_ALT_20002011AVG_ALL_ELEMENTS_RNI.csv",header=T)
popn <- read.csv("/Volumes/My Passport for Mac/Data/Nutrition/Population.csv")

popn <- popn[-c(18,37,38,49,134,114),]

world.popn <- sum(as.numeric(popn$AVG.POPN.2007.2011),na.rm=T)

global.sum <- aggregate(.~Element,FUN=sum,data=rni[,-c(1,2)])
global.sum[,2:10] <- global.sum[,2:10]/world.popn
global.sum

write.csv(global.sum,"~/Documents/Work/Projects/Manuscripts/Unsubmitted/Nutrient Trade/Tables/GlobalStats.csv")
