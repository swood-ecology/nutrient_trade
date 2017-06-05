library(rworldmap)

# Write multi-plot function to include multiple ggplot2 panels into single figure
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


# read nutrient produciton data
rni.cap <- read.csv("/Volumes/My Passport for Mac/Data/Nutrition/Food Balance Sheets/FoodBalanceSheets_FORMAT_7_20002011AVG_Food_Production_Trade_RNIperCapita.csv",header=T)
rni.cap <- rni.cap[,-1]

# set country names to be mapping appropriate
raw <- read.csv("/Volumes/My Passport for Mac/Data/Nutrition/Food Balance Sheets/FoodBalanceSheets_E_All_Data_RAW.csv",header=T)
raw <- raw[,c(1,2)]
raw <- unique(raw)
named <- merge(rni.cap,raw,by="Country")
colnames(named)[12] <- "FAOSTAT"
clu <- read.csv("~/Documents/Work/Writing/Manuscripts/Unsubmitted/Nutrient Trade/Data/countryLookUp.csv",header=T)
mapping <- merge(named,clu,by="FAOSTAT")
mapping <- mapping[,c(3:12,16)]

# cast data wide
library(reshape2)
mapping <- melt(mapping,id.vars=c("ISO3","Element"))
mapping <- dcast(mapping,ISO3~Element+variable)

write.csv(mapping,"/Volumes/My Passport for Mac/Data/Nutrition/Food Balance Sheets/FoodBalanceSheets_FORMAT_8_20002011AVG_Food_Production_Trade_RNIperCapita_Wide.csv")


# # DO FOR OTHER DATA
#
# # read nutrient produciton data
# rni.cap <- read.csv("/Volumes/My Passport for Mac/Data/Nutrition/Food Balance Sheets/FoodBalanceSheets_FORMAT_7_ALT_20002011AVG_ALL_ELEMENTS_RNIperCapita.csv",header=T)
# rni.cap <- rni.cap[,-1]
#
# # set country names to be mapping appropriate
# raw <- read.csv("/Volumes/My Passport for Mac/Data/Nutrition/Food Balance Sheets/FoodBalanceSheets_E_All_Data_RAW.csv",header=T)
# raw <- raw[,c(1,2)]
# raw <- unique(raw)
# named <- merge(rni.cap,raw,by="Country")
# colnames(named)[12] <- "FAOSTAT"
# clu <- read.csv("~/Documents/Work/Projects/Manuscripts/Unsubmitted/Nutrient Trade/Data/countryLookUp.csv",header=T)
# mapping <- merge(named,clu,by="FAOSTAT")
# mapping <- mapping[,c(3:12,16)]
#
# # cast data wide
# library(reshape2)
# mapping <- melt(mapping,id.vars=c("ISO3","Element"))
# mapping <- dcast(mapping,ISO3~Element+variable)
#
# write.csv(mapping,"/Volumes/My Passport for Mac/Data/Nutrition/Food Balance Sheets/FoodBalanceSheets_FORMAT_8_ALT_20002011AVG_ALL_ELEMENTS_RNIperCapita_Wide.csv")




# Plotting in ggplot2
library(ggplot2)
library(maptools)

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

# FIGURE 1
# Calculate PNA for food and map
PNA <- function(x){
  require(vegan)
  require(plotrix)
  ## calculate J, Pielou's evenness
  H <- diversity(x,"shannon")
  S <- specnumber(x)
  J <- H/log(S)
  J <- rescale(J,c(0.1,1))
  
  ## multiply shannon by average across all nutrients
  means <- rowMeans(x,na.rm=TRUE)
  means <- rescale(means,c(0.1,1))
  nut.fd <- J*means
  nut.fd
}

div.plot <- to.plot[,c(1:14)]
div.plot$nut.fd <- PNA(div.plot[,c(8:14)])
div.plot$abun <- rowMeans(div.plot[,c(8:14)],na.rm=TRUE)
library(vegan)
div.plot$even <- diversity(div.plot[,c(8:14)],"shannon")/log(specnumber(div.plot[,c(8:14)]))

# plot relationship between evenness and abundance
abun.plotting <- unique(div.plot[,-c(1:5,7)])
income <- read.csv("~/Documents/Work/Projects/Manuscripts/Unsubmitted/Nutrient Trade/Data/GNI Per Capita.csv",header=T)
colnames(income)[2] <- "id"
abun.final <- merge(abun.plotting,income,by="id")
abun.final <- abun.final[,c(1,10:12,20,21)]
abun.final <- abun.final[-c(113,127,130),]
abun.final$INCOME.GROUP <- factor(abun.final$INCOME.GROUP)

library(ggrepel)
set.seed(42)

abun.even <- ggplot(data=abun.final,aes(y=even,x=abun,label=id,color=INCOME.GROUP,size=AVERAGE)) + 
  geom_point(stroke=.8,alpha=0.6) + 
  scale_color_manual(values=c("#4daf4a","#e41a1c","#984ea3","#377eb8")) +
  geom_label_repel(color="grey40",size=3) + 
  xlab("\nMean % of population potentially nourished") + 
  ylab("Nutritional evenness\n") + theme_bw() + theme(
    axis.title.y = element_text(size=13),
    axis.title.x = element_text(size=13),
    legend.position="none",
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    panel.border=element_rect(color="black",size=0.75))
abun.even

# map nutrional adequacy score
nut.fd <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = div.plot, aes(fill = nut.fd), color="white",size=0.15) + 
  scale_fill_gradient2(name="",low="#ece2f0",mid="#3690c0",high="#014636",midpoint=0.45) + 
  theme_bw() + ditch_the_axes
nut.fd 

# Plot fraction of population potentially nourished
# Protein scale: -2,0,1,2.3,3,5,7.5,32.06
to.plot$protein.cut <- cut(to.plot$Food_protein, c(1,2.3,3,5))
cols.protein <- c("#e0f3f8","#abd9e9","#74add1")
protein_food <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = to.plot, aes(fill = protein.cut), color="white", size=0.25) + 
  scale_fill_manual(name="Protein",values=cols.protein) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm"))
protein_food 

# Energy scale: -1.34, 0, 0.5, 1, 1.25, 1.5, 2, 3.5, 8
to.plot$energy.cut <- cut(to.plot$Food_energy, c(0.5,1,1.25,1.5,2.01))
cols.energy <- c("#f46d43","#e0f3f8","#abd9e9","#74add1")
energy_food <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = to.plot, aes(fill = energy.cut), color="white", size=0.25) + 
  scale_fill_manual(name="Energy",values=cols.energy) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm"))
energy_food 

# Iron scale: -5, 0, 0.25, 0.5, 1, 1.25, 1.75, 2.3, 5.5, 14
to.plot$iron.cut <- cut(to.plot$Food_iron, c(0,0.25,0.5,1,1.25,1.75,2.3,5.5))
cols.iron <- c("#d73027","#f46d43","#fdae61","#e0f3f8","#abd9e9","#74add1","#4575b4")
iron_food <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = to.plot, aes(fill = iron.cut), color="white", size=0.25) + 
  scale_fill_manual(name="Iron",values=cols.iron) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm"))
iron_food 

# Zinc scale: -2.3, 0, .5, 1, 1.75, 2.5, 3.5, 5.5, 22
to.plot$zinc.cut <- cut(to.plot$Food_zinc, c(0.5,1,1.75,2.5,3.5,5.5))
cols.zinc <- c("#f46d43","#e0f3f8","#abd9e9","#74add1","#4575b4")
zinc_food <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = to.plot, aes(fill = zinc.cut), color="white", size=0.25) + 
  scale_fill_manual(name="Zinc",values=cols.zinc) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm"))
zinc_food 

# Calcium scale: -0.47, 0, 0.2, 0.75, 1, 2, 2.3, 2.75, 5, 33.6
to.plot$calc.cut <- cut(to.plot$Food_calcium, c(0.2,0.75,1,2,2.3,2.75,5))
cols.calc <- c("#f46d43","#fdae61","#e0f3f8","#abd9e9","#74add1","#4575b4","#313695")
calcium_food <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = to.plot, aes(fill = calc.cut), color="white", size=0.25) + 
  scale_fill_manual(name="Calcium",values=cols.calc) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm"))
calcium_food 

# Folate scale: -3.7, 0, 0.3, 0.5, 1, 1.25, 1.5, 2, 3.5, 15 
to.plot$folate.cut <- cut(to.plot$Food_folate, c(0.3,0.5,1,1.25,1.5,2))
cols.folate <- c("#f46d43","#fdae61","#e0f3f8","#abd9e9","#74add1")
folate_food <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = to.plot, aes(fill = folate.cut), color="white", size=0.25) + 
  scale_fill_manual(name="Folate",values=cols.folate) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm")) 
folate_food 

# Vitamin A scale: -0.6, 0, 0.43, 1, 2, 3.75, 5, 9, 26.1
to.plot$vita.cut <- cut(to.plot$Food_VitA, c(0.43,1,2,3.75,5,9))
cols.vita <- c("#f46d43","#e0f3f8","#abd9e9","#74add1","#4575b4")
VitA_food <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = to.plot, aes(fill = vita.cut), color="white", size=0.25) + 
  scale_fill_manual(name="Vitamin A",values=cols.vita) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm")) 
VitA_food 

# Vitamin B12 scale: -0.9, 0, 0.5, 1, 3, 5.5, 8, 12, 91.5
to.plot$vitb12.cut <- cut(to.plot$Food_VitB12, c(0.5,1,3,5.5,8,12))
cols.vitb12 <- c("#f46d43","#e0f3f8","#abd9e9","#74add1","#4575b4")
  VitB12_food <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
    geom_polygon(data = to.plot, aes(fill = vitb12.cut), color="white", size=0.25) + 
    scale_fill_manual(name="Vitamin B12",values=cols.vitb12) + 
    theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm"))
  VitB12_food 

# multiplot(energy_food,protein_food,calcium_food,folate_food,iron_food,zinc_food,VitA_food,
#           VitB12_food,cols=2)



# Plot food nutrients, without trade
mapping.notrade <- read.csv("/Volumes/My Passport for Mac/Data/Nutrition/Food Balance Sheets/FoodBalanceSheets_FORMAT_8_ALT_20002011AVG_ALL_ELEMENTS_RNIperCapita_Wide.csv",header=T)
notrade.plot <- inner_join(area.points,mapping.notrade,by="ISO3")  
names(notrade.plot)[6] <- "id"

# Define no trade variables
notrade.plot$Protein_notrade <- notrade.plot$Food_protein - 
  (notrade.plot$Import.Quantity_protein - notrade.plot$ExportQuantity_protein)
notrade.plot$Energy_notrade <- notrade.plot$Food_energy - 
  (notrade.plot$Import.Quantity_energy - notrade.plot$ExportQuantity_energy)
notrade.plot$Zinc_notrade <- notrade.plot$Food_zinc - 
  (notrade.plot$Import.Quantity_zinc - notrade.plot$ExportQuantity_zinc)
notrade.plot$Calcium_notrade <- notrade.plot$Food_calcium - 
  (notrade.plot$Import.Quantity_calcium - notrade.plot$ExportQuantity_calcium)
notrade.plot$Iron_notrade <- notrade.plot$Food_iron - 
  (notrade.plot$Import.Quantity_iron - notrade.plot$ExportQuantity_iron)
notrade.plot$VitB12_notrade <- notrade.plot$Food_VitB12 - 
  (notrade.plot$Import.Quantity_VitB12 - notrade.plot$ExportQuantity_VitB12)
notrade.plot$Folate_notrade <- notrade.plot$Food_folate - 
  (notrade.plot$Import.Quantity_folate - notrade.plot$ExportQuantity_folate)
notrade.plot$VitA_notrade <- notrade.plot$Food_VitA - 
  (notrade.plot$Import.Quantity_VitA - notrade.plot$ExportQuantity_VitA)

# Make no trade plots
# Protein scale: -2,0,1,2.3,3,5,7.5,32.06
notrade.plot$protein.cut <- cut(notrade.plot$Protein_notrade, c(-2,0,1,2,3.3,5,7.5,32.06))
cols.nt.protein <- c("#a50026","#f46d43","#e0f3f8","#abd9e9","#74add1","#4575b4","#313695")
protein_notrade_plot <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = notrade.plot, aes(fill = protein.cut), color="white", size=0.25) + 
  scale_fill_manual(name="Protein, no trade",values=cols.nt.protein) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm")) 
protein_notrade_plot 

# Energy scale: -1.34, 0, 0.5, 1, 1.25, 1.5, 2, 3.5, 8
notrade.plot$energy.cut <- cut(notrade.plot$Energy_notrade, c(-1.34,0,0.5,1,1.25,1.5,2,3.35,8))
cols.nt.energy <- c("#a50026","#d73027","#f46d43","#e0f3f8","#abd9e9","#74add1","#4575b4","#313695")
energy_notrade_plot <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = notrade.plot, aes(fill = energy.cut), color="white", size=0.25) + 
  scale_fill_manual(name="Energy, no trade",values=cols.nt.energy) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm"))
energy_notrade_plot 

# Iron scale: -5, 0, 0.25, 0.5, 1, 1.25, 1.75, 2.3, 5.5, 14
notrade.plot$iron.cut <- cut(notrade.plot$Iron_notrade, c(-5,0,0.25,0.5,1,1.25,1.75,2.3,5.5,14))
cols.nt.iron <- c("#a50026","#d73027","#f46d43","#fdae61","#e0f3f8","#abd9e9","#74add1","#4575b4","#313695")
iron_notrade_plot <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = notrade.plot, aes(fill = iron.cut), color="white", size=0.25) + 
  scale_fill_manual(name="Iron, no trade",values=cols.nt.iron) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm")) 
iron_notrade_plot

# Zinc scale: -2.3, 0, .5, 1, 1.75, 2.5, 3.5, 5.5, 22
notrade.plot$zinc.cut <- cut(notrade.plot$Zinc_notrade, c(-2.3,0,0.5,1,1.75,2.5,3.5,5.5,22))
cols.nt.zinc <- c("#a50026","#d73027","#f46d43","#e0f3f8","#abd9e9","#74add1","#4575b4","#313695")
zinc_notrade_plot <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = notrade.plot, aes(fill = zinc.cut), color="white", size=0.25) + 
  scale_fill_manual(name="Zinc, no trade",values=cols.nt.zinc) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm")) 
zinc_notrade_plot

# Calcium scale: -0.47, 0, 0.2, 0.75, 1, 2, 2.3, 2.75, 5, 33.6
notrade.plot$calc.cut <- cut(notrade.plot$Calcium_notrade, c(-.47,0,0.2,0.75,1,2,2.3,2.75,5,33.6))
cols.nt.calc <- c("#a50026","#d73027","#f46d43","#fdae61","#e0f3f8","#abd9e9","#74add1","#4575b4","#313695")
calcium_notrade_plot <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = notrade.plot, aes(fill = calc.cut), color="white", size=0.25) + 
  scale_fill_manual(name="Calcium, no trade",values=cols.nt.calc) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm"))
calcium_notrade_plot

# Folate scale: -3.7, 0, 0.3, 0.5, 1, 1.25, 1.5, 2, 3.5, 15 
notrade.plot$folate.cut <- cut(notrade.plot$Folate_notrade, c(-3.7,0,0.3,0.5,1,1.25,1.5,2,3.5,15))
cols.nt.folate <- c("#a50026","#d73027","#f46d43","#fdae61","#e0f3f8","#abd9e9","#74add1","#4575b4","#313695")
folate_notrade_plot <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = notrade.plot, aes(fill = folate.cut), color="white", size=0.25) + 
  scale_fill_manual(name="Folate, no trade",values=cols.nt.folate) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm"))
folate_notrade_plot

# Vitamin A scale: -0.6, 0, 0.43, 1, 2, 3.75, 5, 9, 26.1
notrade.plot$vita.cut <- cut(notrade.plot$VitA_notrade, c(-.6,0,0.43,1,2,3.75,5,9,26.1))
cols.nt.vita <- c("#a50026","#d73027","#f46d43","#e0f3f8","#abd9e9","#74add1","#4575b4","#313695")
VitA_notrade_plot <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = notrade.plot, aes(fill = vita.cut), color="white", size=0.25) + 
  scale_fill_manual(name="Vitamin A, no trade",values=cols.nt.vita) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm")) 
VitA_notrade_plot

# Vitamin B12 scale: -0.9, 0, 0.5, 1, 3, 5.5, 8, 12, 91.5
notrade.plot$vitb12.cut <- cut(notrade.plot$VitB12_notrade, c(-.91,0,0.5,1,3,5.5,8,12,91.5))
cols.nt.vitb12 <- c("#a50026","#d73027","#f46d43","#e0f3f8","#abd9e9","#74add1","#4575b4","#313695")
VitB12_notrade_plot <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = notrade.plot, aes(fill = vitb12.cut), color="white", size=0.25) + 
  scale_fill_manual(name="Vitamin B12, no trade",values=cols.nt.vitb12) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm"))
VitB12_notrade_plot

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
plotlist[[9]] <- energy_notrade_plot
plotlist[[10]] <- protein_notrade_plot
plotlist[[11]] <- calcium_notrade_plot
plotlist[[12]] <- folate_notrade_plot
plotlist[[13]] <- iron_notrade_plot
plotlist[[14]] <- zinc_notrade_plot
plotlist[[15]] <- VitA_notrade_plot
plotlist[[16]] <- VitB12_notrade_plot

setwd("~/Documents/Work/Writing/Manuscripts/Unsubmitted/Nutrient Trade/Figures/Nutrients Trade No Trade")
for(i in 1:8){
  ggsave(plot=plotlist[[i]],file=paste("file",i,".pdf",sep=""))
}
for(i in 9:16){
  ggsave(plot=plotlist[[i]],file=paste("notrade.file",i,".pdf",sep=""))
}



# Ex- FIGURE 2
PNA.Food <- PNA(mapping[,c(2:9)])
PNA.Production <- PNA(mapping[,c(20:27)])
fig2.data <- data.frame(mapping$ISO3,PNA.Food,PNA.Production)
fig2.data$PNA.diff <- fig2.data$PNA.Food - fig2.data$PNA.Production
fig2.data$even.diff <- (diversity(mapping[,c(2:9)],"shannon")/log(specnumber(mapping[,c(2:9)])))-(diversity(mapping[,c(20:27)],"shannon")/log(specnumber(mapping[,c(20:27)])))
fig2.data$abun.diff <- (rowMeans(mapping[,c(2:9)],na.rm=TRUE))-(rowMeans(mapping[,c(20:27)],na.rm=TRUE))
fig2.data$Protein <- mapping$Food_protein - mapping$Production_protein
fig2.data$Energy <- mapping$Food_energy - mapping$Production_energy
fig2.data$Zinc <- mapping$Food_zinc - mapping$Production_zinc
fig2.data$Calcium <- mapping$Food_calcium - mapping$Production_calcium
fig2.data$Iron <- mapping$Food_iron - mapping$Production_iron
fig2.data$VitB12 <- mapping$Food_VitB12 - mapping$Production_VitB12
fig2.data$Folate <- mapping$Food_folate - mapping$Production_folate
fig2.data$VitA <- mapping$Food_VitA - mapping$Production_VitA
names(fig2.data)[1] <- "ISO3"
to.plot <- inner_join(area.points,fig2.data,by="ISO3")

# PNA <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
#   geom_polygon(data = to.plot, aes(fill = PNA.diff), size=0.25) + 
#   scale_fill_gradient2(name="",low="#7f0000",mid="#fff5f0",high="#006837",midpoint=0) + 
#   scale_color_manual(values=c("black","white"),guide='none') + 
#   theme_bw() + ditch_the_axes
# PNA
# 
# even <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
#   geom_polygon(data = to.plot, aes(fill = even.diff), size=0.25) + 
#   scale_fill_gradient2(name="",low="#7f0000",mid="#fff5f0",high="#006837",midpoint=0) + 
#   scale_color_manual(values=c("black","white"),guide='none') + 
#   theme_bw() + ditch_the_axes
# even
# 
# abun <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
#   geom_polygon(data = to.plot, aes(fill = abun.diff), size=0.25) + 
#   scale_fill_gradient2(name="",low="#7f0000",mid="#fff5f0",high="#006837",midpoint=0) + 
#   scale_color_manual(values=c("black","white"),guide='none') + 
#   theme_bw() + ditch_the_axes
# abun
# 


Protein <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = to.plot, aes(fill = Protein), size=0.25) + 
  scale_fill_gradient2(name="",low="#7f0000",mid="#fff5f0",high="#006837",midpoint=0) + 
  scale_color_manual(values=c("black","white"),guide='none') + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm"))
Protein

Energy <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = to.plot, aes(fill = Energy), size=0.25) + 
  scale_fill_gradient2(name="",low="#7f0000",mid="#fff5f0",high="#006837",midpoint=0) + 
  scale_color_manual(values=c("black","white"),guide='none') + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm"))
Energy

Zinc <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = to.plot, aes(fill = Zinc), size=0.25) + 
  scale_fill_gradient2(name="",low="#7f0000",mid="#fff5f0",high="#006837",midpoint=0) + 
  scale_color_manual(values=c("black","white"),guide='none') + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm"))
Zinc

Calcium <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = to.plot, aes(fill = Calcium), size=0.25) + 
  scale_fill_gradient2(name="",low="#7f0000",mid="#fff5f0",high="#006837",midpoint=0) + 
  scale_color_manual(values=c("black","white"),guide='none') + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm"))
Calcium

Iron <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = to.plot, aes(fill = Iron), size=0.25) + 
  scale_fill_gradient2(name="",low="#7f0000",mid="#fff5f0",high="#006837",midpoint=0) + 
  scale_color_manual(values=c("black","white"),guide='none') + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm"))
Iron

VitB12 <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = to.plot, aes(fill = VitB12), size=0.25) + 
  scale_fill_gradient2(name="",low="#7f0000",mid="#fff5f0",high="#006837",midpoint=0) + 
  scale_color_manual(values=c("black","white"),guide='none') + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm"))
VitB12

Folate <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = to.plot, aes(fill = Folate), size=0.25) + 
  scale_fill_gradient2(name="",low="#7f0000",mid="#fff5f0",high="#006837",midpoint=0) + 
  scale_color_manual(values=c("black","white"),guide='none') + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm"))
Folate

VitA <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = to.plot, aes(fill = VitA), size=0.25) + 
  scale_fill_gradient2(name="",low="#7f0000",mid="#fff5f0",high="#006837",midpoint=0) + 
  scale_color_manual(values=c("black","white"),guide='none') + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm"))
VitA

multiplot(Energy, Protein, Calcium, Folate, Iron, Zinc, VitA, VitB12, cols=3)
