# -------------------------------------------
# Make Figure 2
# How many people could be fed with surplus?
# -------------------------------------------

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

# calculate surplus/deficit of 100% population needs
mapping$value <- mapping$value - mapping$AVG.POPN.2007.2011
mapping <- mapping[,-c(1,6)]

# convert to millions of poeple and cast data wide
mapping$value <- mapping$value/1000000
mapping <- dcast(mapping,ISO3~Element+variable)

# map surplus/deficit of all nutrients by country
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

to.plot$protein.cut <- cut(to.plot$Food_protein, c(0,21,86,244,2196))
cols.protein <- c("#e0f3f8","#abd9e9","#74add1","#4575b4")
protein_food <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = to.plot, aes(fill = protein.cut), color="white", size=0.25) + 
  scale_fill_manual(name="Protein",values=cols.protein) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm"))
protein_food 

to.plot$energy.cut <- cut(to.plot$Food_energy, c(-25,0,2,19,50,251))
cols.energy <- c("#f46d43","#e0f3f8","#abd9e9","#74add1","#4575b4")
energy_food <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = to.plot, aes(fill = energy.cut), color="white", size=0.25) + 
  scale_fill_manual(name="Energy",values=cols.energy) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm"))
energy_food 

to.plot$iron.cut <- cut(to.plot$Food_iron, c(-709,-1,0,11,31,665))
cols.iron <- c("#d73027","#fdae61","#e0f3f8","#abd9e9","#74add1")
iron_food <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = to.plot, aes(fill = iron.cut), color="white", size=0.25) + 
  scale_fill_manual(name="Iron",values=cols.iron) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm"))
iron_food 

to.plot$zinc.cut <- cut(to.plot$Food_zinc, c(-76,0,4,33,100,946))
cols.zinc <- c("#f46d43","#e0f3f8","#abd9e9","#74add1","#4575b4")
zinc_food <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = to.plot, aes(fill = zinc.cut), color="white", size=0.25) + 
  scale_fill_manual(name="Zinc",values=cols.zinc) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm"))
zinc_food 

to.plot$calc.cut <- cut(to.plot$Food_calcium, c(-370,-1,0,26,59,704))
cols.calc <- c("#f46d43","#fdae61","#e0f3f8","#abd9e9","#74add1")
calcium_food <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = to.plot, aes(fill = calc.cut), color="white", size=0.25) + 
  scale_fill_manual(name="Calcium",values=cols.calc) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm"))
calcium_food 

to.plot$folate.cut <- cut(to.plot$Food_folate, c(-444,-1,0,4,15,445))
cols.folate <- c("#f46d43","#fdae61","#e0f3f8","#abd9e9","#74add1")
folate_food <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = to.plot, aes(fill = folate.cut), color="white", size=0.25) + 
  scale_fill_manual(name="Folate",values=cols.folate) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm")) 
folate_food 

to.plot$vita.cut <- cut(to.plot$Food_VitA, c(-84,0,11,70,204,3744))
cols.vita <- c("#f46d43","#e0f3f8","#abd9e9","#74add1","#4575b4")
VitA_food <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = to.plot, aes(fill = vita.cut), color="white", size=0.25) + 
  scale_fill_manual(name="Vitamin A",values=cols.vita) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm")) 
VitA_food 

to.plot$vitb12.cut <- cut(to.plot$Food_VitB12, c(-18,0,26,167,415,2737))
cols.vitb12 <- c("#f46d43","#e0f3f8","#abd9e9","#74add1","#4575b4")
VitB12_food <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = to.plot, aes(fill = vitb12.cut), color="white", size=0.25) + 
  scale_fill_manual(name="Vitamin B12",values=cols.vitb12) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm"))
VitB12_food 

## Write figures to .pdf files to be merged into single figure in Illustrator
plotlist = list()
plotlist[[1]] <- energy_food
plotlist[[2]] <- protein_food
plotlist[[3]] <- calcium_food
plotlist[[4]] <- folate_food
plotlist[[5]] <- iron_food
plotlist[[6]] <- zinc_food
plotlist[[7]] <- VitA_food
plotlist[[8]] <- VitB12_food

setwd("~/Box Sync/Work/Writing/Manuscripts/Unsubmitted/Nutrient Trade/Figures/Nutrient Surplus")
for(i in 1:8){
  ggsave(plot=plotlist[[i]],file=paste("file",i,".pdf",sep=""))
}

# aggregate to global level
plot_theme <- theme(
  panel.grid=element_blank(),
  axis.title.x = element_blank(),
  axis.text.x = element_text(color="black",angle=90,vjust=0.5,hjust=1),
  legend.position = "none",
  plot.title = element_text(hjust=0.5,face="bold"),
  plot.subtitle = element_text(hjust=0.5,face="italic",vjust=1),
  panel.background=element_blank()
)

aggregate <- mapping[,20:28]

aggregate <- data.frame((colSums(aggregate,na.rm=T)/1000),rownames(as.data.frame(colSums(aggregate,na.rm=T))))
rownames(aggregate) <- seq(1:nrow(aggregate))
names(aggregate) <- c("People","Nutrient")
levels(aggregate$Nutrient) <- list("Calcium"="Food_calcium","Energy"="Food_energy",
                                   "Folate"="Food_folate","Iron"="Food_iron",
                                   "Protein"="Food_protein","Vitamin A"="Food_VitA",
                                   "Vitamin B12"="Food_VitB12","Vitamin D"="Food_VitD",
                                   "Zinc"="Food_zinc")

bar <- ggplot(data=aggregate[1:8,],aes(y=People,x=Nutrient)) + 
  geom_bar(stat="identity") + geom_text(aes(label=sprintf("%0.2f", round(People, digits = 2))),vjust=-1) +
  ylab("Additional people potentially nourished (billions)") + 
  ylim(c(-5,20)) + xlab("") + theme_bw() + plot_theme

ggsave(plot=bar,file="bar.pdf")
