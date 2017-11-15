# ----------------------------------------
# Make Figure 3
# How many people currently fed by trade?
# ----------------------------------------

library(rworldmap)      # Needed for mapping
library(reshape2)       # Needed for reformatting data
library(ggplot2)        # Needed for plotting
library(maptools)       # Needed for plotting
library(dplyr)


# read in data
rni.cap <- read.csv("/Volumes/My Passport for Mac/Data/Nutrition/Food Balance Sheets/FoodBalanceSheets_FORMAT_7_ALT_20002011AVG_ALL_ELEMENTS_RNIperCapita.csv",header=T)
rni.cap <- rni.cap[,-1]

# set country names to be mapping appropriate
raw <- read.csv("/Volumes/My Passport for Mac/Data/Nutrition/Food Balance Sheets/FoodBalanceSheets_E_All_Data_RAW.csv",header=T)
raw <- raw[,c(1,2)]
raw <- unique(raw)
named <- merge(rni.cap,raw,by="Country")
colnames(named)[12] <- "FAOSTAT"
clu <- read.csv("~/Box Sync/Work/Writing/Manuscripts/Unsubmitted/Nutrient Trade/Data/countryLookUp.csv",header=T)
mapping <- merge(named,clu,by="FAOSTAT")
mapping <- mapping[,c(2:12,16)]

# melt data
mapping <- melt(mapping,id.vars=c("ISO3","Element","Country"))

# convert values to person equivalents
popn <- read.csv("/Volumes/My Passport for Mac/Data/Nutrition/Population.csv")
mapping <- merge(mapping,popn,by="Country")
mapping$value <- mapping$value * mapping$AVG.POPN.2007.2011

# convert to millions of poeple and cast data wide
mapping <- mapping[,-c(1,6)]
mapping$value <- mapping$value/1000000
mapping <- dcast(mapping,ISO3~Element+variable)

# map trade contribution to nourishment by country
area <- readShapePoly("/Volumes/My Passport for Mac/Data/GIS Data/Natural Earth/National Borders/ne_50m_admin_0_countries.shp")

area.points <- fortify(area,region="iso_a3")
area.points <- area.points[area.points$id!='-99',]
names(area.points)[6] <- "ISO3"
library(dplyr)
to.plot <- inner_join(area.points,mapping,by="ISO3")  
names(to.plot)[6] <- "id"

ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  panel.background = element_rect(fill="white"),
  plot.background = element_rect(fill="white"),
  axis.title = element_blank(),
  legend.background = element_rect(fill="white"),
  legend.position="bottom",
  legend.box="horizontal"
)

# create trade balance variables (World without trade - world with trade)
to.plot$Protein <- (to.plot$Food_protein - (to.plot$'Import Quantity_protein' - to.plot$ExportQuantity_protein)) - to.plot$Food_protein
to.plot$Energy <- (to.plot$Food_energy - (to.plot$'Import Quantity_energy' - to.plot$ExportQuantity_energy)) - to.plot$Food_energy
to.plot$Zinc <- (to.plot$Food_zinc - (to.plot$'Import Quantity_zinc' - to.plot$ExportQuantity_zinc)) - to.plot$Food_zinc
to.plot$Calcium <- (to.plot$Food_calcium - (to.plot$'Import Quantity_calcium' - to.plot$ExportQuantity_calcium)) - to.plot$Food_calcium
to.plot$Iron <-  (to.plot$Food_iron - (to.plot$'Import Quantity_iron' - to.plot$ExportQuantity_iron)) - to.plot$Food_iron
to.plot$VitB12 <- (to.plot$Food_VitB12 - (to.plot$'Import Quantity_VitB12' - to.plot$ExportQuantity_VitB12)) - to.plot$Food_VitB12
to.plot$Folate <- (to.plot$Food_folate - (to.plot$'Import Quantity_folate' - to.plot$ExportQuantity_folate)) - to.plot$Food_folate
to.plot$VitA <- (to.plot$Food_VitA - (to.plot$'Import Quantity_VitA' - to.plot$ExportQuantity_VitA)) - to.plot$Food_VitA


to.plot$protein.cut <- cut(to.plot$Protein, c(-934,-12,0,2,154,1123))
cols.protein <- c("#d73027","#fdae61","#e0f3f8","#abd9e9","#74add1")
protein <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = to.plot, aes(fill = protein.cut), color="white", size=0.25) + 
  scale_fill_manual(name="Protein",values=cols.protein) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm"))
protein

to.plot$energy.cut <- cut(to.plot$Energy, c(-228,-6,0,0.3,68,480))
cols.energy <- c("#d73027","#fdae61","#e0f3f8","#abd9e9","#74add1")
energy <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = to.plot, aes(fill = energy.cut), color="white", size=0.25) + 
  scale_fill_manual(name="Energy",values=cols.energy) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm"))
energy 

to.plot$iron.cut <- cut(to.plot$Iron, c(-847,-24,-1,0,101,1700))
cols.iron <- c("#d73027","#fc8d59","#fee090","#e0f3f8","#91bfdb")
iron <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = to.plot, aes(fill = iron.cut), color="white", size=0.25) + 
  scale_fill_manual(name="Iron",values=cols.iron) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm"))
iron 

to.plot$zinc.cut <- cut(to.plot$Zinc, c(-631,-8,-1,0,94,1148))
cols.zinc <- c("#d73027","#fc8d59","#fee090","#e0f3f8","#91bfdb")
zinc <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = to.plot, aes(fill = zinc.cut), color="white", size=0.25) + 
  scale_fill_manual(name="Zinc",values=cols.zinc) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm"))
zinc 

to.plot$calc.cut <- cut(to.plot$Calcium, c(-266,-17,-.3,0,10,236))
cols.calc <- c("#d73027","#fc8d59","#fee090","#e0f3f8","#91bfdb")
calcium <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = to.plot, aes(fill = calc.cut), color="white", size=0.25) + 
  scale_fill_manual(name="Calcium",values=cols.calc) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm"))
calcium 

to.plot$folate.cut <- cut(to.plot$Folate, c(-864,-11,-1,0,51,1019))
cols.folate <- c("#d73027","#fc8d59","#fee090","#e0f3f8","#91bfdb")
folate <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = to.plot, aes(fill = folate.cut), color="white", size=0.25) + 
  scale_fill_manual(name="Folate",values=cols.folate) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm")) 
folate 

to.plot$vita.cut <- cut(to.plot$VitA, c(-146,-14,-0.1,0,17,300))
cols.vita <- c("#d73027","#fc8d59","#fee090","#e0f3f8","#91bfdb")
VitA <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = to.plot, aes(fill = vita.cut), color="white", size=0.25) + 
  scale_fill_manual(name="Vitamin A",values=cols.vita) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm")) 
VitA

to.plot$vitb12.cut <- cut(to.plot$VitB12, c(-377,-27,-1,0,34,371))
cols.vitb12 <- c("#d73027","#fc8d59","#fee090","#e0f3f8","#91bfdb")
VitB12 <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = to.plot, aes(fill = vitb12.cut), color="white", size=0.25) + 
  scale_fill_manual(name="Vitamin B12",values=cols.vitb12) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm"))
VitB12

## Write figures to .pdf files to be merged into single figure in Illustrator
plotlist = list()
plotlist[[1]] <- energy
plotlist[[2]] <- protein
plotlist[[3]] <- calcium
plotlist[[4]] <- folate
plotlist[[5]] <- iron
plotlist[[6]] <- zinc
plotlist[[7]] <- VitA
plotlist[[8]] <- VitB12

setwd("~/Box Sync/Work/Writing/Manuscripts/Unsubmitted/Nutrient Trade/Figures/Nourished Without Trade")
for(i in 1:8){
  ggsave(plot=plotlist[[i]],file=paste("file",i,".pdf",sep=""))
}

