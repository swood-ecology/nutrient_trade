# -------------------------------------------
# Country ranking of nutritional potential
# Plotting
# Stephen Wood
# -------------------------------------------

## Load needed libraries
library(ggplot2)    # For plotting
library(reshape2)   # For transforming data frame from long to wide
library(tidyverse)

## Read in data
data <- read.csv("/Volumes/My Passport for Mac/Data/Nutrition/Food Balance Sheets/FoodBalanceSheets_FORMAT_7_ALT_20002011AVG_ALL_ELEMENTS_RNIperCapita.csv",header=T)

# Country short names
countryLookUp <- read_csv("~/Box Sync/Work/Writing/Manuscripts/Unsubmitted/Nutrient Trade/Data/countryLookUp.csv", 
                          col_types = cols(FAOContinent = col_skip(), 
                          FAOEconomicGroup = col_skip(), FAOFood1 = col_skip(), 
                          FAOFood2 = col_skip(), FAOLandlocked = col_skip(), 
                          FAOSTAT = col_skip(), FAOSubRegion = col_skip(), 
                          ISOCountryLC = col_skip(), X1 = col_skip()))

names(data)[2] <- 'FAOCountryLC'

data.merged <- merge(data,countryLookUp,by='FAOCountryLC')
data.merged <- data.merged[,-c(1,2,13,14)]

## Cast data wide
data.wide <- melt(data.merged,id.vars=c("ISO3","Element"))
data.wide <- dcast(data.wide,ISO3~Element+variable)


## Define plotting theme
plot_theme <- theme(
  panel.grid=element_blank(),
  axis.title.x = element_blank(),
  axis.text.x = element_text(size=11,angle=90,vjust=0.5,hjust=1),
  legend.position = "none",
  plot.title = element_text(hjust=0.5,
                            margin = margin(t=10,b = -15)),
  panel.background=element_blank()
)

colors <- c("#a50026","#006837")
names(colors) <- c("TRUE","FALSE")
  

## Country rank plotting, with trade

## Protein
data.food.protein <- data.wide[,c(1,20)]
# Lowest 20
data.food.protein.low <- data.food.protein[order(data.food.protein$Food_protein),][1:20,]
# Highest 20
data.food.protein.high <- data.food.protein[order(data.food.protein$Food_protein,decreasing=T),][1:20,]
# Plots
low.food.protein <- ggplot(data=data.food.protein.low,
                           aes(x=reorder(ISO3,-Food_protein),
                               y=Food_protein,label=sprintf("%0.1f", round(Food_protein, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=Food_protein < 1),width=.7) + 
  geom_text(size=3,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.food.protein$Food_protein,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.food.protein$Food_protein,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements in food supply",
       title="Protein: lowest 20") +
  theme_bw() + plot_theme

high.food.protein <- ggplot(data=data.food.protein.high,
                            aes(x=reorder(ISO3,-Food_protein),
                                y=Food_protein,label=sprintf("%0.1f", round(Food_protein, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=Food_protein < 1),width=.7) + 
  geom_text(size=3,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.food.protein$Food_protein,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.food.protein$Food_protein,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements in food supply",
       title="Protein: highest 20") +
  theme_bw() + plot_theme

## Energy
data.food.energy <- data.wide[,c(1,21)]
# Lowest 20
data.food.energy.low <- data.food.energy[order(data.food.energy$Food_energy),][1:20,]
# Highest 20
data.food.energy.high <- data.food.energy[order(data.food.energy$Food_energy,decreasing=T),][1:20,]
# Plots
low.food.energy <- ggplot(data=data.food.energy.low,
                          aes(x=reorder(ISO3,-Food_energy),
                              y=Food_energy,label=sprintf("%0.1f", round(Food_energy, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=Food_energy < 1),width=.7) + 
  geom_text(size=3,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.food.energy$Food_energy,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.food.energy$Food_energy,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements in food supply",
       title="Energy: lowest 20") +
  theme_bw() + plot_theme

high.food.energy <- ggplot(data=data.food.energy.high,
                           aes(x=reorder(ISO3,-Food_energy),
                               y=Food_energy,label=sprintf("%0.1f", round(Food_energy, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=Food_energy < 1),width=.7) + 
  geom_text(size=3,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.food.energy$Food_energy,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.food.energy$Food_energy,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements in food supply",
       title="Energy: highest 20") +
  theme_bw() + plot_theme


## Zinc
data.food.zinc <- data.wide[,c(1,22)]
# Lowest 20
data.food.zinc.low <- data.food.zinc[order(data.food.zinc$Food_zinc),][1:20,]
# Highest 20
data.food.zinc.high <- data.food.zinc[order(data.food.zinc$Food_zinc,decreasing=T),][1:20,]
# Plots
low.food.zinc <- ggplot(data=data.food.zinc.low,
                        aes(x=reorder(ISO3,-Food_zinc),
                            y=Food_zinc,label=sprintf("%0.1f", round(Food_zinc, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=Food_zinc < 1),width=.7) + 
  geom_text(size=3,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.food.zinc$Food_zinc,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.food.zinc$Food_zinc,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements in food supply",
       title="Zinc: lowest 20") +
  theme_bw() + plot_theme

high.food.zinc <- ggplot(data=data.food.zinc.high,
                         aes(x=reorder(ISO3,-Food_zinc),
                             y=Food_zinc,label=sprintf("%0.1f", round(Food_zinc, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=Food_zinc < 1),width=.7) + 
  geom_text(size=3,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.food.zinc$Food_zinc,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.food.zinc$Food_zinc,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements in food supply",
       title="Zinc: highest 20") +
  theme_bw() + plot_theme


## Calcium
data.food.calcium <- data.wide[,c(1,23)]
# Lowest 20
data.food.calcium.low <- data.food.calcium[order(data.food.calcium$Food_calcium),][1:20,]
# Highest 20
data.food.calcium.high <- data.food.calcium[order(data.food.calcium$Food_calcium,decreasing=T),][1:20,]
# Plots
low.food.calcium <- ggplot(data=data.food.calcium.low,
                           aes(x=reorder(ISO3,-Food_calcium),
                               y=Food_calcium,label=sprintf("%0.1f", round(Food_calcium, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=Food_calcium < 1),width=.7) + 
  geom_text(size=3,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.food.calcium$Food_calcium,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.food.calcium$Food_calcium,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements in food supply",
       title="Calcium: lowest 20") +
  theme_bw() + plot_theme

high.food.calcium <- ggplot(data=data.food.calcium.high,
                            aes(x=reorder(ISO3,-Food_calcium),
                                y=Food_calcium,label=sprintf("%0.1f", round(Food_calcium, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=Food_calcium < 1),width=.7) + 
  geom_text(size=3,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.food.calcium$Food_calcium,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.food.calcium$Food_calcium,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements in food supply",
       title="Calcium: highest 20") +
  theme_bw() + plot_theme


## Iron
data.food.iron <- data.wide[,c(1,24)]
# Lowest 20
data.food.iron.low <- data.food.iron[order(data.food.iron$Food_iron),][1:20,]
# Highest 20
data.food.iron.high <- data.food.iron[order(data.food.iron$Food_iron,decreasing=T),][1:20,]
# Plots
low.food.iron <- ggplot(data=data.food.iron.low,
                        aes(x=reorder(ISO3,-Food_iron),
                            y=Food_iron,label=sprintf("%0.1f", round(Food_iron, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=Food_iron < 1),width=.7) + 
  geom_text(size=3,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.food.iron$Food_iron,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.food.iron$Food_iron,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements in food supply",
       title="Iron: lowest 20") +
  theme_bw() + plot_theme

high.food.iron <- ggplot(data=data.food.iron.high,
                         aes(x=reorder(ISO3,-Food_iron),
                             y=Food_iron,label=sprintf("%0.1f", round(Food_iron, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=Food_iron < 1),width=.7) + 
  geom_text(size=3,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.food.iron$Food_iron,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.food.iron$Food_iron,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements in food supply",
       title="Iron: highest 20") +
  theme_bw() + plot_theme


## Vitamin B12
data.food.vitb12 <- data.wide[,c(1,25)]
# Lowest 20
data.food.vitb12.low <- data.food.vitb12[order(data.food.vitb12$Food_VitB12),][1:20,]
# Highest 20
data.food.vitb12.high <- data.food.vitb12[order(data.food.vitb12$Food_VitB12,decreasing=T),][1:20,]
# Plots
low.food.vitb12 <- ggplot(data=data.food.vitb12.low,
                          aes(x=reorder(ISO3,-Food_VitB12),
                              y=Food_VitB12,label=sprintf("%0.1f", round(Food_VitB12, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=Food_VitB12 < 1),width=.7) + 
  geom_text(size=3,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.food.vitb12$Food_VitB12,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.food.vitb12$Food_VitB12,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements in food supply",
       title="Vitamin B12: lowest 20") +
  theme_bw() + plot_theme

high.food.vitb12 <- ggplot(data=data.food.vitb12.high,
                           aes(x=reorder(ISO3,-Food_VitB12),
                               y=Food_VitB12,label=sprintf("%0.1f", round(Food_VitB12, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=Food_VitB12 < 1),width=.7) + 
  geom_text(size=3,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.food.vitb12$Food_VitB12,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.food.vitb12$Food_VitB12,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements in food supply",
       title="Vitamin B12: highest 20") +
  theme_bw() + plot_theme


## Folate
data.food.folate <- data.wide[,c(1,26)]
# Lowest 20
data.food.folate.low <- data.food.folate[order(data.food.folate$Food_folate),][1:20,]
# Highest 20
data.food.folate.high <- data.food.folate[order(data.food.folate$Food_folate,decreasing=T),][1:20,]
# Plots
low.food.folate <- ggplot(data=data.food.folate.low,
                          aes(x=reorder(ISO3,-Food_folate),
                              y=Food_folate,label=sprintf("%0.1f", round(Food_folate, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=Food_folate < 1),width=.7) + 
  geom_text(size=3,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.food.folate$Food_folate,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.food.folate$Food_folate,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements in food supply",
       title="Folate: lowest 20") +
  theme_bw() + plot_theme

high.food.folate <- ggplot(data=data.food.folate.high,
                           aes(x=reorder(ISO3,-Food_folate),
                               y=Food_folate,label=sprintf("%0.1f", round(Food_folate, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=Food_folate < 1),width=.7) + 
  geom_text(size=3,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.food.folate$Food_folate,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.food.folate$Food_folate,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements in food supply",
       title="Folate: highest 20") +
  theme_bw() + plot_theme


## Vitamin A
data.food.vita <- data.wide[,c(1,27)]

# Lowest 20
data.food.vita.low <- data.food.vita[order(data.food.vita$Food_VitA),][1:20,]
# Highest 20
data.food.vita.high <- data.food.vita[order(data.food.vita$Food_VitA,decreasing=T),][1:20,]
# Plots
low.food.vita <- ggplot(data=data.food.vita.low,
                        aes(x=reorder(ISO3,-Food_VitA),
                            y=Food_VitA,label=sprintf("%0.1f", round(Food_VitA, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=Food_VitA < 1),width=.7) + 
  geom_text(size=3,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.food.vita$Food_VitA,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.food.vita$Food_VitA,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements in food supply",
       title="Vitamin A: lowest 20") +
  theme_bw() + plot_theme

high.food.vita <- ggplot(data=data.food.vita.high,
                         aes(x=reorder(ISO3,-Food_VitA),
                             y=Food_VitA,label=sprintf("%0.1f", round(Food_VitA, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=Food_VitA < 1),width=.7) + 
  geom_text(size=3,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.food.vita$Food_VitA,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.food.vita$Food_VitA,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements in food supply",
       title="Vitamin A: highest 20") +
  theme_bw() + plot_theme


## Write figures to .pdf files to be merged into single figure in Illustrator
plotlist = list()
plotlist[[1]] <- high.food.energy
plotlist[[2]] <- high.food.protein
plotlist[[3]] <- high.food.calcium
plotlist[[4]] <- high.food.folate
plotlist[[5]] <- high.food.iron
plotlist[[6]] <- high.food.zinc
plotlist[[7]] <- high.food.vita
plotlist[[8]] <- high.food.vitb12
plotlist[[9]] <- low.food.energy
plotlist[[10]] <- low.food.protein
plotlist[[11]] <- low.food.calcium
plotlist[[12]] <- low.food.folate
plotlist[[13]] <- low.food.iron
plotlist[[14]] <- low.food.zinc
plotlist[[15]] <- low.food.vita
plotlist[[16]] <- low.food.vitb12

setwd("~/Box Sync/Work/Writing/Manuscripts/Unsubmitted/Nutrient Trade/Figures/Nutrients By Country")
for(i in 1:16){
  ggsave(plot=plotlist[[i]],height=5,width=8,units=c("in"),file=paste("file",i,".pdf",sep=""))
}



## Country rank plotting, with no trade

## Define no trade variables as food supply - trade balance
data.wide$Protein_notrade <- data.wide$Food_protein - (data.wide$`Import Quantity_protein`-data.wide$ExportQuantity_protein)
data.wide$Energy_notrade <- data.wide$Food_energy - (data.wide$`Import Quantity_energy`-data.wide$ExportQuantity_energy)
data.wide$Zinc_notrade <- data.wide$Food_zinc - (data.wide$`Import Quantity_zinc`-data.wide$ExportQuantity_zinc)
data.wide$Iron_notrade <- data.wide$Food_iron - (data.wide$`Import Quantity_iron`-data.wide$ExportQuantity_iron)
data.wide$Calcium_notrade <- data.wide$Food_calcium - (data.wide$`Import Quantity_calcium`-data.wide$ExportQuantity_calcium)
data.wide$Folate_notrade <- data.wide$Food_folate - (data.wide$`Import Quantity_folate`-data.wide$ExportQuantity_folate)
data.wide$VitB12_notrade <- data.wide$Food_VitB12 - (data.wide$`Import Quantity_VitB12`-data.wide$ExportQuantity_VitB12)
data.wide$VitA_notrade <- data.wide$Food_VitA - (data.wide$`Import Quantity_VitA`-data.wide$ExportQuantity_VitA)


## Protein
data.nt.protein <- data.wide[,c(1,92)]
# Lowest 20
data.nt.protein.low <- data.nt.protein[order(data.nt.protein$Protein_notrade),][1:20,]
# Set all values <0 to be 0
data.nt.protein.low$Protein_notrade[data.nt.protein.low$Protein_notrade < 0] <- 0
# Highest 20
data.nt.protein.high <- data.nt.protein[order(data.nt.protein$Protein_notrade,decreasing=T),][1:20,]
# Plots
low.nt.protein <- ggplot(data=data.nt.protein.low,
                         aes(x=reorder(Country,-Protein_notrade),
                             y=Protein_notrade,label=sprintf("%0.2f", round(Protein_notrade, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=Protein_notrade < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.nt.protein$Protein_notrade,na.rm=T)),linetype=2) +
  ylim(c(min(data.nt.protein$Protein_notrade,na.rm=T),max(data.nt.protein$Protein_notrade,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements in food supply\n",
       title="Protein, no trade",
       subtitle="Lowest 20") +
  theme_bw() + plot_theme

high.nt.protein <- ggplot(data=data.nt.protein.high,
                          aes(x=reorder(Country,-Protein_notrade),
                              y=Protein_notrade,label=sprintf("%0.2f", round(Protein_notrade, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=Protein_notrade < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.nt.protein$Protein_notrade,na.rm=T)),linetype=2) +
  ylim(c(min(data.nt.protein$Protein_notrade,na.rm=T),max(data.nt.protein$Protein_notrade,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements in food supply\n",
       title="Protein, no trade",
       subtitle="Highest 20") +
  theme_bw() + plot_theme


## Energy
data.nt.energy <- data.wide[,c(1,93)]
# Lowest 20
data.nt.energy.low <- data.nt.energy[order(data.nt.energy$Energy_notrade),][1:20,]
# Set all values <0 to be 0
data.nt.energy.low$Energy_notrade[data.nt.energy.low$Energy_notrade < 0] <- 0
# Highest 20
data.nt.energy.high <- data.nt.energy[order(data.nt.energy$Energy_notrade,decreasing=T),][1:20,]
# Plots
low.nt.energy <- ggplot(data=data.nt.energy.low,
                        aes(x=reorder(Country,-Energy_notrade),
                            y=Energy_notrade,label=sprintf("%0.2f", round(Energy_notrade, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=Energy_notrade < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.nt.energy$Energy_notrade,na.rm=T)),linetype=2) +
  ylim(c(min(data.nt.energy$Energy_notrade,na.rm=T),max(data.nt.energy$Energy_notrade,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements in food supply\n",
       title="Energy, no trade",
       subtitle="Lowest 20") +
  theme_bw() + plot_theme

high.nt.energy <- ggplot(data=data.nt.energy.high,
                         aes(x=reorder(Country,-Energy_notrade),
                             y=Energy_notrade,label=sprintf("%0.2f", round(Energy_notrade, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=Energy_notrade < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.nt.energy$Energy_notrade,na.rm=T)),linetype=2) +
  ylim(c(min(data.nt.energy$Energy_notrade,na.rm=T),max(data.nt.energy$Energy_notrade,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements in food supply\n",
       title="Energy, no trade",
       subtitle="Highest 20") +
  theme_bw() + plot_theme


## Zinc
data.nt.zinc <- data.wide[,c(1,94)]
# Lowest 20
data.nt.zinc.low <- data.nt.zinc[order(data.nt.zinc$Zinc_notrade),][1:20,]
# Set all values <0 to be 0
data.nt.zinc.low$Zinc_notrade[data.nt.zinc.low$Zinc_notrade < 0] <- 0
# Highest 20
data.nt.zinc.high <- data.nt.zinc[order(data.nt.zinc$Zinc_notrade,decreasing=T),][1:20,]
# Plots
low.nt.zinc <- ggplot(data=data.nt.zinc.low,
                      aes(x=reorder(Country,-Zinc_notrade),
                          y=Zinc_notrade,label=sprintf("%0.2f", round(Zinc_notrade, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=Zinc_notrade < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.nt.zinc$Zinc_notrade,na.rm=T)),linetype=2) +
  ylim(c(min(data.nt.zinc$Zinc_notrade,na.rm=T),max(data.nt.zinc$Zinc_notrade,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements in food supply\n",
       title="Zinc, no trade",
       subtitle="Lowest 20") +
  theme_bw() + plot_theme

high.nt.zinc <- ggplot(data=data.nt.zinc.high,
                       aes(x=reorder(Country,-Zinc_notrade),
                           y=Zinc_notrade,label=sprintf("%0.2f", round(Zinc_notrade, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=Zinc_notrade < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.nt.zinc$Zinc_notrade,na.rm=T)),linetype=2) +
  ylim(c(min(data.nt.zinc$Zinc_notrade,na.rm=T),max(data.nt.zinc$Zinc_notrade,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements in food supply\n",
       title="Zinc, no trade",
       subtitle="Highest 20") +
  theme_bw() + plot_theme


## Calcium
data.nt.calcium <- data.wide[,c(1,96)]
# Lowest 20
data.nt.calcium.low <- data.nt.calcium[order(data.nt.calcium$Calcium_notrade),][1:20,]
# Set all values <0 to be 0
data.nt.calcium.low$Calcium_notrade[data.nt.calcium.low$Calcium_notrade < 0] <- 0
# Highest 20
data.nt.calcium.high <- data.nt.calcium[order(data.nt.calcium$Calcium_notrade,decreasing=T),][1:20,]
# Plots
low.nt.calcium <- ggplot(data=data.nt.calcium.low,
                         aes(x=reorder(Country,-Calcium_notrade),
                             y=Calcium_notrade,label=sprintf("%0.2f", round(Calcium_notrade, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=Calcium_notrade < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.nt.calcium$Calcium_notrade,na.rm=T)),linetype=2) +
  ylim(c(min(data.nt.calcium$Calcium_notrade,na.rm=T),max(data.nt.calcium$Calcium_notrade,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements in food supply\n",
       title="Calcium, no trade",
       subtitle="Lowest 20") +
  theme_bw() + plot_theme

high.nt.calcium <- ggplot(data=data.nt.calcium.high,
                          aes(x=reorder(Country,-Calcium_notrade),
                              y=Calcium_notrade,label=sprintf("%0.2f", round(Calcium_notrade, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=Calcium_notrade < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.nt.calcium$Calcium_notrade,na.rm=T)),linetype=2) +
  ylim(c(min(data.nt.calcium$Calcium_notrade,na.rm=T),max(data.nt.calcium$Calcium_notrade,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements in food supply\n",
       title="Calcium, no trade",
       subtitle="Highest 20") +
  theme_bw() + plot_theme


## Iron
data.nt.iron <- data.wide[,c(1,95)]
# Lowest 20
data.nt.iron.low <- data.nt.iron[order(data.nt.iron$Iron_notrade),][1:20,]
# Set all values <0 to be 0
data.nt.iron.low$Iron_notrade[data.nt.iron.low$Iron_notrade < 0] <- 0
# Highest 20
data.nt.iron.high <- data.nt.iron[order(data.nt.iron$Iron_notrade,decreasing=T),][1:20,]
# Plots
low.nt.iron <- ggplot(data=data.nt.iron.low,
                      aes(x=reorder(Country,-Iron_notrade),
                          y=Iron_notrade,label=sprintf("%0.2f", round(Iron_notrade, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=Iron_notrade < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.nt.iron$Iron_notrade,na.rm=T)),linetype=2) +
  ylim(c(min(data.nt.iron$Iron_notrade,na.rm=T),max(data.nt.iron$Iron_notrade,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements in food supply\n",
       title="Iron, no trade",
       subtitle="Lowest 20") +
  theme_bw() + plot_theme

high.nt.iron <- ggplot(data=data.nt.iron.high,
                       aes(x=reorder(Country,-Iron_notrade),
                           y=Iron_notrade,label=sprintf("%0.2f", round(Iron_notrade, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=Iron_notrade < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.nt.iron$Iron_notrade,na.rm=T)),linetype=2) +
  ylim(c(min(data.nt.iron$Iron_notrade,na.rm=T),max(data.nt.iron$Iron_notrade,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements in food supply\n",
       title="Iron, no trade",
       subtitle="Highest 20") +
  theme_bw() + plot_theme


## Vitamin B12
data.nt.vitb12 <- data.wide[,c(1,98)]
# Lowest 20
data.nt.vitb12.low <- data.nt.vitb12[order(data.nt.vitb12$VitB12_notrade),][1:20,]
# Set all values <0 to be 0
data.nt.vitb12.low$VitB12_notrade[data.nt.vitb12.low$VitB12_notrade < 0] <- 0
# Highest 20
data.nt.vitb12.high <- data.nt.vitb12[order(data.nt.vitb12$VitB12_notrade,decreasing=T),][1:20,]
# Plots
low.nt.vitb12 <- ggplot(data=data.nt.vitb12.low,
                        aes(x=reorder(Country,-VitB12_notrade),
                            y=VitB12_notrade,label=sprintf("%0.2f", round(VitB12_notrade, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=VitB12_notrade < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.nt.vitb12$VitB12_notrade,na.rm=T)),linetype=2) +
  ylim(c(min(data.nt.vitb12$VitB12_notrade,na.rm=T),max(data.nt.vitb12$VitB12_notrade,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements in food supply\n",
       title="Vitamin B12, no trade",
       subtitle="Lowest 20") +
  theme_bw() + plot_theme

high.nt.vitb12 <- ggplot(data=data.nt.vitb12.high,
                         aes(x=reorder(Country,-VitB12_notrade),
                             y=VitB12_notrade,label=sprintf("%0.2f", round(VitB12_notrade, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=VitB12_notrade < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.nt.vitb12$VitB12_notrade,na.rm=T)),linetype=2) +
  ylim(c(min(data.nt.vitb12$VitB12_notrade,na.rm=T),max(data.nt.vitb12$VitB12_notrade,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements in food supply\n",
       title="Vitamin B12, no trade",
       subtitle="Highest 20") +
  theme_bw() + plot_theme


## Folate
data.nt.folate <- data.wide[,c(1,97)]
# Lowest 20
data.nt.folate.low <- data.nt.folate[order(data.nt.folate$Folate_notrade),][1:20,]
# Set all values <0 to be 0
data.nt.folate.low$Folate_notrade[data.nt.folate.low$Folate_notrade < 0] <- 0
# Highest 20
data.nt.folate.high <- data.nt.folate[order(data.nt.folate$Folate_notrade,decreasing=T),][1:20,]
# Plots
low.nt.folate <- ggplot(data=data.nt.folate.low,
                        aes(x=reorder(Country,-Folate_notrade),
                            y=Folate_notrade,label=sprintf("%0.2f", round(Folate_notrade, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=Folate_notrade < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.nt.folate$Folate_notrade,na.rm=T)),linetype=2) +
  ylim(c(min(data.nt.folate$Folate_notrade,na.rm=T),max(data.nt.folate$Folate_notrade,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements in food supply\n",
       title="Folate, no trade",
       subtitle="Lowest 20") +
  theme_bw() + plot_theme

high.nt.folate <- ggplot(data=data.nt.folate.high,
                         aes(x=reorder(Country,-Folate_notrade),
                             y=Folate_notrade,label=sprintf("%0.2f", round(Folate_notrade, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=Folate_notrade < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.nt.folate$Folate_notrade,na.rm=T)),linetype=2) +
  ylim(c(min(data.nt.folate$Folate_notrade,na.rm=T),max(data.nt.folate$Folate_notrade,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements in food supply\n",
       title="Folate, no trade",
       subtitle="Highest 20") +
  theme_bw() + plot_theme


## Vitamin A
data.nt.vita <- data.wide[,c(1,99)]
# Lowest 20
data.nt.vita.low <- data.nt.vita[order(data.nt.vita$VitA_notrade),][1:20,]
# Set all values <0 to be 0
data.nt.vita.low$VitA_notrade[data.nt.vita.low$VitA_notrade < 0] <- 0
# Highest 20
data.nt.vita.high <- data.nt.vita[order(data.nt.vita$VitA_notrade,decreasing=T),][1:20,]
# Plots
low.nt.vita <- ggplot(data=data.nt.vita.low,
                      aes(x=reorder(Country,-VitA_notrade),
                          y=VitA_notrade,label=sprintf("%0.2f", round(VitA_notrade, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=VitA_notrade < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.nt.vita$VitA_notrade,na.rm=T)),linetype=2) +
  ylim(c(min(data.nt.vita$VitA_notrade,na.rm=T),max(data.nt.vita$VitA_notrade,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements in food supply\n",
       title="Vitamin A, no trade",
       subtitle="Lowest 20") +
  theme_bw() + plot_theme

high.nt.vita <- ggplot(data=data.nt.vita.high,
                       aes(x=reorder(Country,-VitA_notrade),
                           y=VitA_notrade,label=sprintf("%0.2f", round(VitA_notrade, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=VitA_notrade < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.nt.vita$VitA_notrade,na.rm=T)),linetype=2) +
  ylim(c(min(data.nt.vita$VitA_notrade,na.rm=T),max(data.nt.vita$VitA_notrade,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements in food supply\n",
       title="Vitamin A, no trade",
       subtitle="Highest 20") +
  theme_bw() + plot_theme


## Write figures to .pdf files to be merged into single figure in Illustrator
plotlist = list()
plotlist[[1]] <- high.nt.energy
plotlist[[2]] <- high.nt.protein
plotlist[[3]] <- high.nt.calcium
plotlist[[4]] <- high.nt.folate
plotlist[[5]] <- high.nt.iron
plotlist[[6]] <- high.nt.zinc
plotlist[[7]] <- high.nt.vita
plotlist[[8]] <- high.nt.vitb12
plotlist[[9]] <- low.nt.energy
plotlist[[10]] <- low.nt.protein
plotlist[[11]] <- low.nt.calcium
plotlist[[12]] <- low.nt.folate
plotlist[[13]] <- low.nt.iron
plotlist[[14]] <- low.nt.zinc
plotlist[[15]] <- low.nt.vita
plotlist[[16]] <- low.nt.vitb12

setwd("~/Documents/Work/Projects/Manuscripts/Unsubmitted/Nutrient Trade/Figures/Nutrients By Country")
for(i in 1:16){
  ggsave(plot=plotlist[[i]],file=paste("no.trade.file",i,".pdf",sep=""))
}


## Country rank plotting, import

## Protein
data.import.protein <- data.wide[,c(1,29)]
names(data.import.protein)[2] <- 'ImportQuantity_protein'
# Lowest 20
data.import.protein.low <- data.import.protein[order(data.import.protein$ImportQuantity_protein),][1:20,]
# Highest 20
data.import.protein.high <- data.import.protein[order(data.import.protein$ImportQuantity_protein,decreasing=T),][1:20,]
# Plots
low.import.protein <- ggplot(data=data.import.protein.low,
                             aes(x=reorder(Country,-ImportQuantity_protein),
                                 y=ImportQuantity_protein,label=sprintf("%0.2f", round(ImportQuantity_protein, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=ImportQuantity_protein < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.import.protein$ImportQuantity_protein,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.import.protein$ImportQuantity_protein,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements imported\n",
       title="Protein",
       subtitle="Lowest 20") +
  theme_bw() + plot_theme

high.import.protein <- ggplot(data=data.import.protein.high,
                              aes(x=reorder(Country,-ImportQuantity_protein),
                                  y=ImportQuantity_protein,label=sprintf("%0.2f", round(ImportQuantity_protein, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=ImportQuantity_protein < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.import.protein$ImportQuantity_protein,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.import.protein$ImportQuantity_protein,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements imported\n",
       title="Protein",
       subtitle="Highest 20") +
  theme_bw() + plot_theme

## Energy
data.import.energy <- data.wide[,c(1,30)]
names(data.import.energy)[2] <- 'ImportQuantity_energy'
# Lowest 20
data.import.energy.low <- data.import.energy[order(data.import.energy$ImportQuantity_energy),][1:20,]
# Highest 20
data.import.energy.high <- data.import.energy[order(data.import.energy$ImportQuantity_energy,decreasing=T),][1:20,]
# Plots
low.import.energy <- ggplot(data=data.import.energy.low,
                            aes(x=reorder(Country,-ImportQuantity_energy),
                                y=ImportQuantity_energy,label=sprintf("%0.2f", round(ImportQuantity_energy, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=ImportQuantity_energy < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.import.energy$ImportQuantity_energy,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.import.energy$ImportQuantity_energy,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements imported\n",
       title="Energy",
       subtitle="Lowest 20") +
  theme_bw() + plot_theme

high.import.energy <- ggplot(data=data.import.energy.high,
                             aes(x=reorder(Country,-ImportQuantity_energy),
                                 y=ImportQuantity_energy,label=sprintf("%0.2f", round(ImportQuantity_energy, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=ImportQuantity_energy < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.import.energy$ImportQuantity_energy,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.import.energy$ImportQuantity_energy,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements imported\n",
       title="Energy",
       subtitle="Highest 20") +
  theme_bw() + plot_theme


## Zinc
data.import.zinc <- data.wide[,c(1,31)]
names(data.import.zinc)[2] <- 'ImportQuantity_zinc'
# Lowest 20
data.import.zinc.low <- data.import.zinc[order(data.import.zinc$ImportQuantity_zinc),][1:20,]
# Highest 20
data.import.zinc.high <- data.import.zinc[order(data.import.zinc$ImportQuantity_zinc,decreasing=T),][1:20,]
# Plots
low.import.zinc <- ggplot(data=data.import.zinc.low,
                          aes(x=reorder(Country,-ImportQuantity_zinc),
                              y=ImportQuantity_zinc,label=sprintf("%0.2f", round(ImportQuantity_zinc, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=ImportQuantity_zinc < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.import.zinc$ImportQuantity_zinc,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.import.zinc$ImportQuantity_zinc,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements imported\n",
       title="Zinc",
       subtitle="Lowest 20") +
  theme_bw() + plot_theme

high.import.zinc <- ggplot(data=data.import.zinc.high,
                           aes(x=reorder(Country,-ImportQuantity_zinc),
                               y=ImportQuantity_zinc,label=sprintf("%0.2f", round(ImportQuantity_zinc, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=ImportQuantity_zinc < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.import.zinc$ImportQuantity_zinc,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.import.zinc$ImportQuantity_zinc,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements imported\n",
       title="Zinc",
       subtitle="Highest 20") +
  theme_bw() + plot_theme


## Calcium
data.import.calcium <- data.wide[,c(1,32)]
names(data.import.calcium)[2] <- 'ImportQuantity_calcium'
# Lowest 20
data.import.calcium.low <- data.import.calcium[order(data.import.calcium$ImportQuantity_calcium),][1:20,]
# Highest 20
data.import.calcium.high <- data.import.calcium[order(data.import.calcium$ImportQuantity_calcium,decreasing=T),][1:20,]
# Plots
low.import.calcium <- ggplot(data=data.import.calcium.low,
                             aes(x=reorder(Country,-ImportQuantity_calcium),
                                 y=ImportQuantity_calcium,label=sprintf("%0.2f", round(ImportQuantity_calcium, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=ImportQuantity_calcium < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.import.calcium$ImportQuantity_calcium,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.import.calcium$ImportQuantity_calcium,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements imported\n",
       title="Calcium",
       subtitle="Lowest 20") +
  theme_bw() + plot_theme

high.import.calcium <- ggplot(data=data.import.calcium.high,
                              aes(x=reorder(Country,-ImportQuantity_calcium),
                                  y=ImportQuantity_calcium,label=sprintf("%0.2f", round(ImportQuantity_calcium, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=ImportQuantity_calcium < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.import.calcium$ImportQuantity_calcium,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.import.calcium$ImportQuantity_calcium,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements imported\n",
       title="Calcium",
       subtitle="Highest 20") +
  theme_bw() + plot_theme


## Iron
data.import.iron <- data.wide[,c(1,33)]
names(data.import.iron)[2] <- 'ImportQuantity_iron'
# Lowest 20
data.import.iron.low <- data.import.iron[order(data.import.iron$ImportQuantity_iron),][1:20,]
# Highest 20
data.import.iron.high <- data.import.iron[order(data.import.iron$ImportQuantity_iron,decreasing=T),][1:20,]
# Plots
low.import.iron <- ggplot(data=data.import.iron.low,
                          aes(x=reorder(Country,-ImportQuantity_iron),
                              y=ImportQuantity_iron,label=sprintf("%0.2f", round(ImportQuantity_iron, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=ImportQuantity_iron < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.import.iron$ImportQuantity_iron,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.import.iron$ImportQuantity_iron,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements imported\n",
       title="Iron",
       subtitle="Lowest 20") +
  theme_bw() + plot_theme

high.import.iron <- ggplot(data=data.import.iron.high,
                           aes(x=reorder(Country,-ImportQuantity_iron),
                               y=ImportQuantity_iron,label=sprintf("%0.2f", round(ImportQuantity_iron, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=ImportQuantity_iron < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.import.iron$ImportQuantity_iron,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.import.iron$ImportQuantity_iron,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements imported\n",
       title="Iron",
       subtitle="Highest 20") +
  theme_bw() + plot_theme


## Vitamin B12
data.import.vitb12 <- data.wide[,c(1,34)]
names(data.import.vitb12)[2] <- 'ImportQuantity_VitB12'
# Lowest 20
data.import.vitb12.low <- data.import.vitb12[order(data.import.vitb12$ImportQuantity_VitB12),][1:20,]
# Highest 20
data.import.vitb12.high <- data.import.vitb12[order(data.import.vitb12$ImportQuantity_VitB12,decreasing=T),][1:20,]
# Plots
low.import.vitb12 <- ggplot(data=data.import.vitb12.low,
                            aes(x=reorder(Country,-ImportQuantity_VitB12),
                                y=ImportQuantity_VitB12,label=sprintf("%0.2f", round(ImportQuantity_VitB12, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=ImportQuantity_VitB12 < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.import.vitb12$ImportQuantity_VitB12,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.import.vitb12$ImportQuantity_VitB12,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements imported\n",
       title="Vitamin B12",
       subtitle="Lowest 20") +
  theme_bw() + plot_theme

high.import.vitb12 <- ggplot(data=data.import.vitb12.high,
                             aes(x=reorder(Country,-ImportQuantity_VitB12),
                                 y=ImportQuantity_VitB12,label=sprintf("%0.2f", round(ImportQuantity_VitB12, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=ImportQuantity_VitB12 < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.import.vitb12$ImportQuantity_VitB12,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.import.vitb12$ImportQuantity_VitB12,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements imported\n",
       title="Vitamin B12",
       subtitle="Highest 20") +
  theme_bw() + plot_theme


## Folate
data.import.folate <- data.wide[,c(1,35)]
names(data.import.folate)[2] <- 'ImportQuantity_folate'
# Lowest 20
data.import.folate.low <- data.import.folate[order(data.import.folate$ImportQuantity_folate),][1:20,]
# Highest 20
data.import.folate.high <- data.import.folate[order(data.import.folate$ImportQuantity_folate,decreasing=T),][1:20,]
# Plots
low.import.folate <- ggplot(data=data.import.folate.low,
                            aes(x=reorder(Country,-ImportQuantity_folate),
                                y=ImportQuantity_folate,label=sprintf("%0.2f", round(ImportQuantity_folate, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=ImportQuantity_folate < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.import.folate$ImportQuantity_folate,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.import.folate$ImportQuantity_folate,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements imported\n",
       title="Folate",
       subtitle="Lowest 20") +
  theme_bw() + plot_theme

high.import.folate <- ggplot(data=data.import.folate.high,
                             aes(x=reorder(Country,-ImportQuantity_folate),
                                 y=ImportQuantity_folate,label=sprintf("%0.2f", round(ImportQuantity_folate, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=ImportQuantity_folate < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.import.folate$ImportQuantity_folate,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.import.folate$ImportQuantity_folate,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements imported\n",
       title="Folate",
       subtitle="Highest 20") +
  theme_bw() + plot_theme


## Vitamin A
data.import.vita <- data.wide[,c(1,36)]
names(data.import.vita)[2] <- 'ImportQuantity_VitA'
# Lowest 20
data.import.vita.low <- data.import.vita[order(data.import.vita$ImportQuantity_VitA),][1:20,]
# Highest 20
data.import.vita.high <- data.import.vita[order(data.import.vita$ImportQuantity_VitA,decreasing=T),][1:20,]
# Plots
low.import.vita <- ggplot(data=data.import.vita.low,
                          aes(x=reorder(Country,-ImportQuantity_VitA),
                              y=ImportQuantity_VitA,label=sprintf("%0.2f", round(ImportQuantity_VitA, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=ImportQuantity_VitA < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.import.vita$ImportQuantity_VitA,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.import.vita$ImportQuantity_VitA,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements imported\n",
       title="Vitamin A",
       subtitle="Lowest 20") +
  theme_bw() + plot_theme

high.import.vita <- ggplot(data=data.import.vita.high,
                           aes(x=reorder(Country,-ImportQuantity_VitA),
                               y=ImportQuantity_VitA,label=sprintf("%0.2f", round(ImportQuantity_VitA, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=ImportQuantity_VitA < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.import.vita$ImportQuantity_VitA,na.rm=T)),linetype=2) +
  ggtitle("Vitamin A\nLowest 20") +
  ylim(c(0,max(data.import.vita$ImportQuantity_VitA,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements imported\n",
       title="Vitamin A",
       subtitle="Highest 20") +
  theme_bw() + plot_theme


## Write figures to .pdf files to be merged into single figure in Illustrator
plotlist = list()
plotlist[[1]] <- high.import.energy
plotlist[[2]] <- high.import.protein
plotlist[[3]] <- high.import.calcium
plotlist[[4]] <- high.import.folate
plotlist[[5]] <- high.import.iron
plotlist[[6]] <- high.import.zinc
plotlist[[7]] <- high.import.vita
plotlist[[8]] <- high.import.vitb12
plotlist[[9]] <- low.import.energy
plotlist[[10]] <- low.import.protein
plotlist[[11]] <- low.import.calcium
plotlist[[12]] <- low.import.folate
plotlist[[13]] <- low.import.iron
plotlist[[14]] <- low.import.zinc
plotlist[[15]] <- low.import.vita
plotlist[[16]] <- low.import.vitb12

setwd("~/Documents/Work/Projects/Manuscripts/Unsubmitted/Nutrient Trade/Figures/Nutrients By Country")
for(i in 1:16){
  ggsave(plot=plotlist[[i]],file=paste("import.file",i,".pdf",sep=""))
}


## Country rank plotting, export

## Protein
data.export.protein <- data.wide[,c(1,2)]
# Lowest 20
data.export.protein.low <- data.export.protein[order(data.export.protein$ExportQuantity_protein),][1:20,]
# Highest 20
data.export.protein.high <- data.export.protein[order(data.export.protein$ExportQuantity_protein,decreasing=T),][1:20,]
# Plots
low.export.protein <- ggplot(data=data.export.protein.low,
                             aes(x=reorder(Country,-ExportQuantity_protein),
                                 y=ExportQuantity_protein,label=sprintf("%0.2f", round(ExportQuantity_protein, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=ExportQuantity_protein < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.export.protein$ExportQuantity_protein,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.export.protein$ExportQuantity_protein,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements exported\n",
       title="Protein",
       subtitle="Lowest 20") +
  theme_bw() + plot_theme

high.export.protein <- ggplot(data=data.export.protein.high,
                              aes(x=reorder(Country,-ExportQuantity_protein),
                                  y=ExportQuantity_protein,label=sprintf("%0.2f", round(ExportQuantity_protein, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=ExportQuantity_protein < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.export.protein$ExportQuantity_protein,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.export.protein$ExportQuantity_protein,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements exported\n",
       title="Protein",
       subtitle="Highest 20") +
  theme_bw() + plot_theme

## Energy
data.export.energy <- data.wide[,c(1,30)]
names(data.export.energy)[2] <- 'ExportQuantity_energy'
# Lowest 20
data.export.energy.low <- data.export.energy[order(data.export.energy$ExportQuantity_energy),][1:20,]
# Highest 20
data.export.energy.high <- data.export.energy[order(data.export.energy$ExportQuantity_energy,decreasing=T),][1:20,]
# Plots
low.export.energy <- ggplot(data=data.export.energy.low,
                            aes(x=reorder(Country,-ExportQuantity_energy),
                                y=ExportQuantity_energy,label=sprintf("%0.2f", round(ExportQuantity_energy, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=ExportQuantity_energy < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.export.energy$ExportQuantity_energy,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.export.energy$ExportQuantity_energy,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements exported\n",
       title="Energy",
       subtitle="Lowest 20") +
  theme_bw() + plot_theme

high.export.energy <- ggplot(data=data.export.energy.high,
                             aes(x=reorder(Country,-ExportQuantity_energy),
                                 y=ExportQuantity_energy,label=sprintf("%0.2f", round(ExportQuantity_energy, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=ExportQuantity_energy < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.export.energy$ExportQuantity_energy,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.export.energy$ExportQuantity_energy,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements exported\n",
       title="Energy",
       subtitle="Highest 20") +
  theme_bw() + plot_theme


## Zinc
data.export.zinc <- data.wide[,c(1,31)]
names(data.export.zinc)[2] <- 'ExportQuantity_zinc'
# Lowest 20
data.export.zinc.low <- data.export.zinc[order(data.export.zinc$ExportQuantity_zinc),][1:20,]
# Highest 20
data.export.zinc.high <- data.export.zinc[order(data.export.zinc$ExportQuantity_zinc,decreasing=T),][1:20,]
# Plots
low.export.zinc <- ggplot(data=data.export.zinc.low,
                          aes(x=reorder(Country,-ExportQuantity_zinc),
                              y=ExportQuantity_zinc,label=sprintf("%0.2f", round(ExportQuantity_zinc, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=ExportQuantity_zinc < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.export.zinc$ExportQuantity_zinc,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.export.zinc$ExportQuantity_zinc,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements exported\n",
       title="Zinc",
       subtitle="Lowest 20") +
  theme_bw() + plot_theme

high.export.zinc <- ggplot(data=data.export.zinc.high,
                           aes(x=reorder(Country,-ExportQuantity_zinc),
                               y=ExportQuantity_zinc,label=sprintf("%0.2f", round(ExportQuantity_zinc, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=ExportQuantity_zinc < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.export.zinc$ExportQuantity_zinc,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.export.zinc$ExportQuantity_zinc,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements exported\n",
       title="Zinc",
       subtitle="Highest 20") +
  theme_bw() + plot_theme


## Calcium
data.export.calcium <- data.wide[,c(1,32)]
names(data.export.calcium)[2] <- 'ExportQuantity_calcium'
# Lowest 20
data.export.calcium.low <- data.export.calcium[order(data.export.calcium$ExportQuantity_calcium),][1:20,]
# Highest 20
data.export.calcium.high <- data.export.calcium[order(data.export.calcium$ExportQuantity_calcium,decreasing=T),][1:20,]
# Plots
low.export.calcium <- ggplot(data=data.export.calcium.low,
                             aes(x=reorder(Country,-ExportQuantity_calcium),
                                 y=ExportQuantity_calcium,label=sprintf("%0.2f", round(ExportQuantity_calcium, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=ExportQuantity_calcium < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.export.calcium$ExportQuantity_calcium,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.export.calcium$ExportQuantity_calcium,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements exported\n",
       title="Calcium",
       subtitle="Lowest 20") +
  theme_bw() + plot_theme

high.export.calcium <- ggplot(data=data.export.calcium.high,
                              aes(x=reorder(Country,-ExportQuantity_calcium),
                                  y=ExportQuantity_calcium,label=sprintf("%0.2f", round(ExportQuantity_calcium, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=ExportQuantity_calcium < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.export.calcium$ExportQuantity_calcium,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.export.calcium$ExportQuantity_calcium,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements exported\n",
       title="Calcium",
       subtitle="Highest 20") +
  theme_bw() + plot_theme


## Iron
data.export.iron <- data.wide[,c(1,33)]
names(data.export.iron)[2] <- 'ExportQuantity_iron'
# Lowest 20
data.export.iron.low <- data.export.iron[order(data.export.iron$ExportQuantity_iron),][1:20,]
# Highest 20
data.export.iron.high <- data.export.iron[order(data.export.iron$ExportQuantity_iron,decreasing=T),][1:20,]
# Plots
low.export.iron <- ggplot(data=data.export.iron.low,
                          aes(x=reorder(Country,-ExportQuantity_iron),
                              y=ExportQuantity_iron,label=sprintf("%0.2f", round(ExportQuantity_iron, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=ExportQuantity_iron < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.export.iron$ExportQuantity_iron,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.export.iron$ExportQuantity_iron,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements exported\n",
       title="Iron",
       subtitle="Lowest 20") +
  theme_bw() + plot_theme

high.export.iron <- ggplot(data=data.export.iron.high,
                           aes(x=reorder(Country,-ExportQuantity_iron),
                               y=ExportQuantity_iron,label=sprintf("%0.2f", round(ExportQuantity_iron, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=ExportQuantity_iron < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.export.iron$ExportQuantity_iron,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.export.iron$ExportQuantity_iron,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements exported\n",
       title="Iron",
       subtitle="Highest 20") +
  theme_bw() + plot_theme


## Vitamin B12
data.export.vitb12 <- data.wide[,c(1,34)]
names(data.export.vitb12)[2] <- 'ExportQuantity_VitB12'
# Lowest 20
data.export.vitb12.low <- data.export.vitb12[order(data.export.vitb12$ExportQuantity_VitB12),][1:20,]
# Highest 20
data.export.vitb12.high <- data.export.vitb12[order(data.export.vitb12$ExportQuantity_VitB12,decreasing=T),][1:20,]
# Plots
low.export.vitb12 <- ggplot(data=data.export.vitb12.low,
                            aes(x=reorder(Country,-ExportQuantity_VitB12),
                                y=ExportQuantity_VitB12,label=sprintf("%0.2f", round(ExportQuantity_VitB12, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=ExportQuantity_VitB12 < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.export.vitb12$ExportQuantity_VitB12,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.export.vitb12$ExportQuantity_VitB12,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements exported\n",
       title="Vitamin B12",
       subtitle="Lowest 20") +
  theme_bw() + plot_theme

high.export.vitb12 <- ggplot(data=data.export.vitb12.high,
                             aes(x=reorder(Country,-ExportQuantity_VitB12),
                                 y=ExportQuantity_VitB12,label=sprintf("%0.2f", round(ExportQuantity_VitB12, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=ExportQuantity_VitB12 < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.export.vitb12$ExportQuantity_VitB12,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.export.vitb12$ExportQuantity_VitB12,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements exported\n",
       title="Vitamin B12",
       subtitle="Highest 20") +
  theme_bw() + plot_theme


## Folate
data.export.folate <- data.wide[,c(1,35)]
names(data.export.folate)[2] <- 'ExportQuantity_folate'
# Lowest 20
data.export.folate.low <- data.export.folate[order(data.export.folate$ExportQuantity_folate),][1:20,]
# Highest 20
data.export.folate.high <- data.export.folate[order(data.export.folate$ExportQuantity_folate,decreasing=T),][1:20,]
# Plots
low.export.folate <- ggplot(data=data.export.folate.low,
                            aes(x=reorder(Country,-ExportQuantity_folate),
                                y=ExportQuantity_folate,label=sprintf("%0.2f", round(ExportQuantity_folate, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=ExportQuantity_folate < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.export.folate$ExportQuantity_folate,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.export.folate$ExportQuantity_folate,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements exported\n",
       title="Folate",
       subtitle="Lowest 20") +
  theme_bw() + plot_theme

high.export.folate <- ggplot(data=data.export.folate.high,
                             aes(x=reorder(Country,-ExportQuantity_folate),
                                 y=ExportQuantity_folate,label=sprintf("%0.2f", round(ExportQuantity_folate, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=ExportQuantity_folate < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.export.folate$ExportQuantity_folate,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.export.folate$ExportQuantity_folate,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements exported\n",
       title="Folate",
       subtitle="Highest 20") +
  theme_bw() + plot_theme


## Vitamin A
data.export.vita <- data.wide[,c(1,36)]
names(data.export.vita)[2] <- 'ExportQuantity_VitA'
# Lowest 20
data.export.vita.low <- data.export.vita[order(data.export.vita$ExportQuantity_VitA),][1:20,]
# Highest 20
data.export.vita.high <- data.export.vita[order(data.export.vita$ExportQuantity_VitA,decreasing=T),][1:20,]
# Plots
low.export.vita <- ggplot(data=data.export.vita.low,
                          aes(x=reorder(Country,-ExportQuantity_VitA),
                              y=ExportQuantity_VitA,label=sprintf("%0.2f", round(ExportQuantity_VitA, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=ExportQuantity_VitA < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.export.vita$ExportQuantity_VitA,na.rm=T)),linetype=2) +
  ylim(c(0,max(data.export.vita$ExportQuantity_VitA,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements exported\n",
       title="Vitamin A",
       subtitle="Lowest 20") +
  theme_bw() + plot_theme

high.export.vita <- ggplot(data=data.export.vita.high,
                           aes(x=reorder(Country,-ExportQuantity_VitA),
                               y=ExportQuantity_VitA,label=sprintf("%0.2f", round(ExportQuantity_VitA, digits = 2)))) + 
  geom_bar(stat="identity",position="dodge",aes(fill=ExportQuantity_VitA < 1),width=.7) + 
  geom_text(size=2,vjust=-0.75) +
  scale_fill_manual(values = colors) +
  geom_hline(aes(yintercept=mean(data.export.vita$ExportQuantity_VitA,na.rm=T)),linetype=2) +
  ggtitle("Vitamin A\nLowest 20") +
  ylim(c(0,max(data.export.vita$ExportQuantity_VitA,na.rm=T))) + 
  labs(y="Fraction of nutrient requirements exported\n",
       title="Vitamin A",
       subtitle="Highest 20") +
  theme_bw() + plot_theme


## Write figures to .pdf files to be merged into single figure in Illustrator
plotlist = list()
plotlist[[1]] <- high.export.energy
plotlist[[2]] <- high.export.protein
plotlist[[3]] <- high.export.calcium
plotlist[[4]] <- high.export.folate
plotlist[[5]] <- high.export.iron
plotlist[[6]] <- high.export.zinc
plotlist[[7]] <- high.export.vita
plotlist[[8]] <- high.export.vitb12
plotlist[[9]] <- low.export.energy
plotlist[[10]] <- low.export.protein
plotlist[[11]] <- low.export.calcium
plotlist[[12]] <- low.export.folate
plotlist[[13]] <- low.export.iron
plotlist[[14]] <- low.export.zinc
plotlist[[15]] <- low.export.vita
plotlist[[16]] <- low.export.vitb12

setwd("~/Documents/Work/Projects/Manuscripts/Unsubmitted/Nutrient Trade/Figures/Nutrients By Country")
for(i in 1:16){
  ggsave(plot=plotlist[[i]],file=paste("export.file",i,".pdf",sep=""))
}
