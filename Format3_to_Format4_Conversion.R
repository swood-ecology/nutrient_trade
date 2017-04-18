# --------------------
# Expanding data from long to wide
# Calculating a trade balance number (Production + Imports - Exports)
# Collapsing back to long
# Outputting the file as FORMAT_4
# --------------------

library(tidyr)

setwd("/Volumes/My Passport for Mac/Data/Nutrition/Food Balance Sheets")

data <- read.csv("FoodBalanceSheets_FORMAT_3_20002011AVG_Food_Production_Trade.csv")
data <- data[,c(2,3,4,6,7,8)]
data2 <- spread(data,Element,Y2000_to_2011_AVG,fill=0)
data2$ProdTrade <- data2$Production + data2$`Import Quantity` - data2$`Export Quantity`

data3 <- data2[,c(1:4,6,8,9)]

data4 <- gather(data3,Element,Y2000_to_2011_AVG,Food:ProdTrade,factor_key="TRUE")

write.csv(data4,"FoodBalanceSheets_FORMAT_4_20002011AVG_Food_Production_Trade.csv")


# Do the same for all food items

data <- read.csv("FoodBalanceSheets_FORMAT_2_20002011AVG.csv")
data <- data[,c(2,3,4,6,7,8)]
data2 <- spread(data,Element,Y2000_to_2011_AVG,fill=0)
data2$ProdTrade <-data2$Production + data2$`Import Quantity` - data2$`Export Quantity`
names(data2)[5] <- 'ExportQuantity' 

data3 <- gather(data2,Element,Y2000_to_2011_AVG,ExportQuantity:ProdTrade,factor_key="TRUE")

write.csv(data3,"FoodBalanceSheets_FORMAT_4_ALT_20002011AVG_ALL_ELEMENTS.csv")
