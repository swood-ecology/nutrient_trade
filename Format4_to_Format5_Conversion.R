# ---------------------------------------------
# Add nutrient data to production spreadsheet
# Adjust production, food, and production_trade data for edible portions
# Calculate amount of nutrients produced
# Export as FORMAT_5
# ---------------------------------------------

library(xlsx)

setwd("/Volumes/My Passport for Mac/Data/Nutrition")

fao_data <- read.csv("Food Balance Sheets/FoodBalanceSheets_FORMAT_4_20002011AVG_Food_Production_Trade.csv")
fao_data <- fao_data[,-1]

usda_nutr <- read.xlsx("Current Nutrient Databases/USDA_Nutrients.xlsx", sheetName="Aggregated Nutrients")
colnames(usda_nutr)[2] <- colnames(fao_data)[2]
usda_nutr <- usda_nutr[,-3]

new <- merge(fao_data,usda_nutr,by="Item.Code")

new$Y2000_to_2011_AVG_Edible <- new$Y2000_to_2011_AVG - (new$Y2000_to_2011_AVG*new$Refuse)

# convert production to tonnes
new$Edible_Tonnes <- new$Y2000_to_2011_AVG_Edible*1000

# convert nutrients (X per 100g) to X / tonne
new[,c(9:31)] <- new[,c(9:31)]*10000

# multiply by production to get total amount produced
new$Energy_kcal <- new$Energy..kcal.*new$Edible_Tonnes
new$Protein_g <- new$Protein..g.*new$Edible_Tonnes
new$Lipid_g <- new$Total.Lipid..g.*new$Edible_Tonnes
new$Carbs_g <- new$Carbs..g.*new$Edible_Tonnes
new$Fiber_g <- new$Fiber..g.*new$Edible_Tonnes
new$Sugars_g <- new$Sugars..g.*new$Edible_Tonnes
new$Ca_mg <- new$Ca..mg.*new$Edible_Tonnes
new$Fe_mg <- new$Fe..mg.*new$Edible_Tonnes
new$Mg_mg <- new$Mg..mg.*new$Edible_Tonnes
new$P_mg <- new$P..mg.*new$Edible_Tonnes
new$K_mg <- new$K..mg.*new$Edible_Tonnes
new$Na_mg <- new$Na..mg.*new$Edible_Tonnes
new$Zn_mg <- new$Zn..mg.*new$Edible_Tonnes
new$VitC_mg <- new$Vitamin.C..mg.*new$Edible_Tonnes
new$Thiamin_mg <- new$Thiamin..mg.*new$Edible_Tonnes
new$Riboflavin_mg <- new$Riboflavin..mg.*new$Edible_Tonnes
new$Niacin_mg <- new$Niacin..mg.*new$Edible_Tonnes
new$VitB6_mg <- new$Vitamin.B.6..mg.*new$Edible_Tonnes
new$Folate_ug <- new$Folate..ug.*new$Edible_Tonnes
new$VitB12_ug <- new$Vitamin.B.12..ug.*new$Edible_Tonnes
new$VitA_ug <- new$Vitamin.A..RAE..ug.*new$Edible_Tonnes
new$VitE_mg <- new$Vitamin.E..mg.*new$Edible_Tonnes
new$VitD_ug <- new$Vitamin.D..ug.*new$Edible_Tonnes

final <- new[,c(2,5,34:56)]
final2 <- aggregate(.~Country+Element,data=final,FUN=sum,na.rm=T)

write.csv(final2,"Food Balance Sheets/FoodBalanceSheets_FORMAT_5_20002011AVG_Food_Production_Trade_Edible_Nutrients.csv")






# Do for other elements

fao_data <- read.csv("Food Balance Sheets/FoodBalanceSheets_FORMAT_4_ALT_20002011AVG_ALL_ELEMENTS.csv")
fao_data <- fao_data[,-1]

usda_nutr <- read.xlsx("Current Nutrient Databases/USDA_Nutrients.xlsx", sheetName="Aggregated Nutrients")
colnames(usda_nutr)[2] <- colnames(fao_data)[2]
usda_nutr <- usda_nutr[,-3]

new <- merge(fao_data,usda_nutr,by="Item.Code")

new$Y2000_to_2011_AVG_Edible <- new$Y2000_to_2011_AVG - (new$Y2000_to_2011_AVG*new$Refuse)

# convert production to tonnes
new$Edible_Tonnes <- new$Y2000_to_2011_AVG_Edible*1000

# convert nutrients (X per 100g) to X / tonne
new[,c(9:31)] <- new[,c(9:31)]*10000

# multiply by production to get total amount produced
new$Energy_kcal <- new$Energy..kcal.*new$Edible_Tonnes
new$Protein_g <- new$Protein..g.*new$Edible_Tonnes
new$Lipid_g <- new$Total.Lipid..g.*new$Edible_Tonnes
new$Carbs_g <- new$Carbs..g.*new$Edible_Tonnes
new$Fiber_g <- new$Fiber..g.*new$Edible_Tonnes
new$Sugars_g <- new$Sugars..g.*new$Edible_Tonnes
new$Ca_mg <- new$Ca..mg.*new$Edible_Tonnes
new$Fe_mg <- new$Fe..mg.*new$Edible_Tonnes
new$Mg_mg <- new$Mg..mg.*new$Edible_Tonnes
new$P_mg <- new$P..mg.*new$Edible_Tonnes
new$K_mg <- new$K..mg.*new$Edible_Tonnes
new$Na_mg <- new$Na..mg.*new$Edible_Tonnes
new$Zn_mg <- new$Zn..mg.*new$Edible_Tonnes
new$VitC_mg <- new$Vitamin.C..mg.*new$Edible_Tonnes
new$Thiamin_mg <- new$Thiamin..mg.*new$Edible_Tonnes
new$Riboflavin_mg <- new$Riboflavin..mg.*new$Edible_Tonnes
new$Niacin_mg <- new$Niacin..mg.*new$Edible_Tonnes
new$VitB6_mg <- new$Vitamin.B.6..mg.*new$Edible_Tonnes
new$Folate_ug <- new$Folate..ug.*new$Edible_Tonnes
new$VitB12_ug <- new$Vitamin.B.12..ug.*new$Edible_Tonnes
new$VitA_ug <- new$Vitamin.A..RAE..ug.*new$Edible_Tonnes
new$VitE_mg <- new$Vitamin.E..mg.*new$Edible_Tonnes
new$VitD_ug <- new$Vitamin.D..ug.*new$Edible_Tonnes

final <- new[,c(2,5,34:56)]
final2 <- aggregate(.~Country+Element,data=final,FUN=sum,na.rm=T)

write.csv(final2,"Food Balance Sheets/FoodBalanceSheets_FORMAT_5_ALT_20002011AVG_ALL_ELEMENTS_Edible_Nutrients.csv")
