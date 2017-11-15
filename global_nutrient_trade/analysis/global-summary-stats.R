# ---------------------
# Calculate summary statistics of nutrient sufficiency to the global level
# Steve Wood
# ---------------------

## calulate summary statistics and export
rni <- read.csv("/Volumes/My Passport for Mac/Data/Nutrition/Food Balance Sheets/FoodBalanceSheets_FORMAT_6_ALT_20002011AVG_ALL_ELEMENTS_RNI.csv",header=T)
popn <- read.csv("/Volumes/My Passport for Mac/Data/Nutrition/Population.csv")

popn <- popn[-c(18,37,38,49,134,114),]

world.popn <- sum(as.numeric(popn$AVG.POPN.2007.2011),na.rm=T)

global.sum <- aggregate(.~Element,FUN=sum,data=rni[,-c(1,2)])
global.sum[,2:10] <- global.sum[,2:10]/world.popn
global.sum

write.csv(global.sum,"~/Documents/Work/Projects/Manuscripts/Unsubmitted/Nutrient Trade/Tables/GlobalStats.csv")


## create figure of people potentially nourished at global level by trade
library(ggplot2)

plot_theme <- theme(
  panel.grid=element_blank(),
  axis.title.x = element_blank(),
  axis.text.x = element_text(color="black",angle=90,vjust=0.5,hjust=1),
  legend.position = "none",
  plot.title = element_text(hjust=0.5,face="bold"),
  plot.subtitle = element_text(hjust=0.5,face="italic",vjust=1),
  panel.background=element_blank()
)

global.nourished <- aggregate(.~Element,FUN=sum,data=rni[,-c(1,2)])

# number of extra people could be fed by redistributing food, focusing on aggregation
# exclude vitamin D
redistribute.potential <- ((global.nourished[3,-1] - world.popn)/1000000000)[-9]
names(redistribute.potential) <- c("Protein","Energy","Zinc","Calcium",
                                   "Iron","Vitamin B12","Folate",
                                   "Vitamin A")

redistribute.potential <- data.frame(t(redistribute.potential),rownames(t(redistribute.potential)))
names(redistribute.potential) <- c('People','Nutrient')
rownames(redistribute.potential) <- seq(1:nrow(redistribute.potential))

ggplot(data=redistribute.potential,aes(y=People,x=Nutrient)) + 
  geom_bar(stat="identity") + geom_text(aes(label=sprintf("%0.2f", round(People, digits = 2))),vjust=-1) +
  ylab("Additional people potentially nourished (billions)") + 
  ylim(c(0,20)) + xlab("") + theme_bw() + plot_theme


## do the same as above, but plot total people potentially nourished and show line for current population
# # number of people could be fed by redistributing food
# redistribute.potential.2 <- (global.nourished[3,-1])/1000000000
# 
# redistribute.potential.2 <- data.frame(t(redistribute.potential.2),rownames(t(redistribute.potential.2)))
# names(redistribute.potential.2) <- c('People','Nutrient')
# rownames(redistribute.potential.2) <- seq(1:nrow(redistribute.potential.2))
# 
# ggplot(data=redistribute.potential.2,aes(y=People,x=Nutrient)) + 
#   geom_bar(stat="identity") + geom_hline(yintercept=world.popn/1000000000)