library(ineq)
library(GiniWegNeg)
library(ggplot2)

rni <- read.csv("/Volumes/My Passport for Mac/Data/Nutrition/Food Balance Sheets/FoodBalanceSheets_FORMAT_8_ALT_20002011AVG_ALL_ELEMENTS_RNIperCapita_Wide.csv",header=T)
gni <- read.csv("~/Box Sync/Work/Writing/Manuscripts/Unsubmitted/Nutrient Trade/Data/GNI Per Capita.csv",header=T)
merged <- merge(rni,gni,by="ISO3")

protein.gini <- merged[-c(62,94,112,129),]
protein.gini$Food_no_trade_protein <- protein.gini$Food_protein - (protein.gini$Import.Quantity_protein - protein.gini$ExportQuantity_protein)

# Create variables for food supply, minus trade balance
merged$Food_no_trade_energy <- merged$Food_energy - (merged$Import.Quantity_energy-merged$ExportQuantity_energy)
merged$Food_no_trade_protein <- merged$Food_protein - (merged$Import.Quantity_protein-merged$ExportQuantity_protein)
merged$Food_no_trade_zinc <- merged$Food_zinc - (merged$Import.Quantity_zinc-merged$ExportQuantity_zinc)
merged$Food_no_trade_iron <- merged$Food_iron - (merged$Import.Quantity_iron-merged$ExportQuantity_iron)
merged$Food_no_trade_VitB12 <- merged$Food_VitB12 - (merged$Import.Quantity_VitB12-merged$ExportQuantity_VitB12)
merged$Food_no_trade_VitA <- merged$Food_VitA - (merged$Import.Quantity_VitA-merged$ExportQuantity_VitA)
merged$Food_no_trade_folate <- merged$Food_folate - (merged$Import.Quantity_folate-merged$ExportQuantity_folate)
merged$Food_no_trade_calcium <- merged$Food_calcium - (merged$Import.Quantity_calcium-merged$ExportQuantity_calcium)

# Define equal weights
w <- rep(1,nrow(merged))

# equality (higher more unequal)
trade.gini <- unlist(c(Gini_RSV(rni$Food_protein[!is.na(rni$Food_protein)]),
                Gini(rni$Food_energy),
                Gini(rni$Food_zinc),
                Gini(rni$Food_iron),
                Gini(rni$Food_VitB12),
                Gini(rni$Food_folate),
                Gini(rni$Food_VitA),
                Gini(rni$Food_calcium)))
notrade.gini <- unlist(c(Gini_RSV(merged$Food_no_trade_protein[!is.na(merged$Food_no_trade_protein)],w),
                  Gini_RSV(merged$Food_no_trade_energy[!is.na(merged$Food_no_trade_energy)],w),
                  Gini_RSV(merged$Food_no_trade_zinc[!is.na(merged$Food_no_trade_zinc)],w),
                  Gini_RSV(merged$Food_no_trade_iron[!is.na(merged$Food_no_trade_iron)],w),
                  Gini_RSV(merged$Food_no_trade_VitB12[!is.na(merged$Food_no_trade_VitB12)],w),
                  Gini_RSV(merged$Food_no_trade_folate[!is.na(merged$Food_no_trade_folate)],w),
                  Gini_RSV(merged$Food_no_trade_VitA[!is.na(merged$Food_no_trade_VitA)],w),
                  Gini_RSV(merged$Food_no_trade_calcium[!is.na(merged$Food_no_trade_calcium)],w)))
Nutrients <- c("Protein","Energy","Zinc","Iron","Vitamin B12","Folate","Vitamin A","Calcium")
gini.global <- data.frame(Nutrients, cbind(trade.gini,notrade.gini))


# make bar plot
plot_theme <- theme(
  panel.grid=element_blank(),
  axis.title.x = element_blank(),
  axis.text.x = element_text(color="black",angle=90,vjust=0.5,hjust=1),
  legend.position = "none",
  plot.title = element_text(hjust=0.5,face="bold"),
  plot.subtitle = element_text(hjust=0.5,face="italic",vjust=1),
  panel.background=element_blank()
)

bar.gini.trade <- ggplot(data=gini.global,aes(y=as.numeric(trade.gini),x=Nutrients)) + 
  geom_bar(stat="identity") + geom_text(aes(label=sprintf("%0.2f", round(trade.gini, digits = 2))),vjust=-1) +
  ylab("Gini coefficient (0-1)") + 
  ylim(c(0,1)) + xlab("") + theme_bw() + plot_theme
bar.gini.trade

bar.gini.notrade <- ggplot(data=gini.global,aes(y=as.numeric(notrade.gini),x=Nutrients)) + 
  geom_bar(stat="identity") + geom_text(aes(label=sprintf("%0.2f", round(notrade.gini, digits = 2))),vjust=-1) +
  ylab("Gini coefficient (0-1)") + 
  ylim(c(0,1)) + xlab("") + theme_bw() + plot_theme
bar.gini.notrade

## Write figures to .pdf files to be merged into single figure in Illustrator
plotlist = list()
plotlist[[1]] <- bar.gini.trade
plotlist[[2]] <- bar.gini.notrade

setwd("~/Box Sync/Work/Writing/Manuscripts/Unsubmitted/Nutrient Trade/Figures/Gini")
for(i in 1:2){
  ggsave(plot=plotlist[[i]],file=paste("file",i,".pdf",sep=""))
}

# aggregate for country groups

aggregate(protein.gini$Food_no_trade_protein~protein.gini$INCOME.GROUP,FUN=Gini)
aggregate(Food_no_trade_energy~INCOME.GROUP,FUN=Gini_RSV,data=merged[!is.na(merged$Food_energy),])
aggregate(merged$Food_no_trade_iron~merged$INCOME.GROUP,FUN=Gini_RSV)
aggregate(merged$Food_no_trade_zinc~merged$INCOME.GROUP,FUN=Gini)
aggregate(merged$Food_no_trade_VitA~merged$INCOME.GROUP,FUN=Gini)
aggregate(merged$Food_no_trade_folate~merged$INCOME.GROUP,FUN=Gini)
aggregate(merged$Food_no_trade_VitB12~merged$INCOME.GROUP,FUN=Gini)
aggregate(merged$Food_no_trade_calcium~merged$INCOME.GROUP,FUN=Gini)

aggregate(Food_protein~INCOME.GROUP,FUN=Gini,data=merged[!is.na(merged$Food_protein),])
aggregate(Food_energy~INCOME.GROUP,FUN=Gini,data=merged[!is.na(merged$Food_energy),])
aggregate(merged$Food_iron~merged$INCOME.GROUP,FUN=Gini_RSV)
aggregate(merged$Food_zinc~merged$INCOME.GROUP,FUN=Gini_RSV)
aggregate(merged$Food_VitA~merged$INCOME.GROUP,FUN=Gini_RSV)
aggregate(merged$Food_folate~merged$INCOME.GROUP,FUN=Gini_RSV)
aggregate(merged$Food_VitB12~merged$INCOME.GROUP,FUN=Gini)
aggregate(merged$Food_calcium~merged$INCOME.GROUP,FUN=Gini)
