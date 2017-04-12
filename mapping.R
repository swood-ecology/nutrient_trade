library(rworldmap)

# read nutrient produciton data
rni.cap <- read.csv("/Volumes/My Passport for Mac/Data/Nutrition/Food Balance Sheets/FoodBalanceSheets_FORMAT_7_20002011AVG_Food_Production_Trade_RNIperCapita.csv",header=T)
rni.cap <- rni.cap[,-1]

# set country names to be mapping appropriate
raw <- read.csv("/Volumes/My Passport for Mac/Data/Nutrition/Food Balance Sheets/FoodBalanceSheets_E_All_Data_RAW.csv",header=T)
raw <- raw[,c(1,2)]
raw <- unique(raw)
named <- merge(rni.cap,raw,by="Country")
colnames(named)[12] <- "FAOSTAT"
clu <- read.csv("~/Documents/Work/Projects/Manuscripts/Unsubmitted/Nutrient Trade/Data/countryLookUp.csv",header=T)
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

area <- readShapePoly("/Volumes/My Passport for Mac/Data/GIS Data/National Borders Natural Earth/ne_50m_admin_0_countries.shp")

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
  panel.background = element_rect(fill="grey40"),
  plot.background = element_rect(fill="grey40"),
  axis.title = element_blank(),
  legend.background = element_rect(fill="grey40"),
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
protein_food <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = to.plot, aes(fill = Food_protein), color="white", size=0.25) + 
  scale_fill_gradient2(name="Protein",low="#7f0000",mid="#fff5f0",high="#006837",midpoint=1) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm")) + 
                                        guides(fill = guide_colourbar(title.position="top"))
protein_food 

energy_food <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = to.plot, aes(fill = Food_energy), color="white", size=0.25) + 
  scale_fill_gradient2(name="Energy",low="#7f0000",mid="#fff5f0",high="#006837",midpoint=1) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm")) + 
  guides(fill = guide_colourbar(title.position="top"))
energy_food 

iron_food <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = to.plot, aes(fill = Food_iron), color="white", size=0.25) + 
  scale_fill_gradient2(name="Iron",low="#7f0000",mid="#fff5f0",high="#006837",midpoint=1) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm")) + 
  guides(fill = guide_colourbar(title.position="top"))
iron_food 

zinc_food <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = to.plot, aes(fill = Food_zinc), color="white", size=0.25) + 
  scale_fill_gradient2(name="Zinc",low="#7f0000",mid="#fff5f0",high="#006837",midpoint=1) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm")) + 
  guides(fill = guide_colourbar(title.position="top"))
zinc_food 

calcium_food <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = to.plot, aes(fill = Food_calcium), color="white", size=0.25) + 
  scale_fill_gradient2(name="Calcium",low="#7f0000",mid="#fff5f0",high="#006837",midpoint=1) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm")) + 
  guides(fill = guide_colourbar(title.position="top"))
calcium_food 

folate_food <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = to.plot, aes(fill = Food_folate), color="white", size=0.25) + 
  scale_fill_gradient2(name="Folate",low="#7f0000",mid="#fff5f0",high="#006837",midpoint=1) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm")) + 
  guides(fill = guide_colourbar(title.position="top"))
folate_food 

VitA_food <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = to.plot, aes(fill = Food_VitA), color="white", size=0.25) + 
  scale_fill_gradient2(name="Vitamin A",low="#7f0000",mid="#fff5f0",high="#006837",midpoint=1) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm")) + 
  guides(fill = guide_colourbar(title.position="top"))
VitA_food 

VitB12_food <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = to.plot, aes(fill = Food_VitB12), color="white", size=0.25) + 
  scale_fill_gradient2(name="Vitamin B12",low="#7f0000",mid="#fff5f0",high="#006837",midpoint=1) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm")) + 
  guides(fill = guide_colourbar(title.position="top"))
VitB12_food 

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
multiplot(energy_food,protein_food,calcium_food,folate_food,iron_food,zinc_food,VitA_food,
          VitB12_food,cols=2)



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
protein_notrade_plot <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = notrade.plot, aes(fill = Protein_notrade), color="white", size=0.25) + 
  scale_fill_gradient2(name="Protein, no trade",low="#7f0000",mid="#fff5f0",high="#006837",midpoint=1) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm")) + 
  guides(fill = guide_colourbar(title.position="top"))
protein_notrade_plot 

energy_notrade_plot <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = notrade.plot, aes(fill = Energy_notrade), color="white", size=0.25) + 
  scale_fill_gradient2(name="Energy, no trade",low="#7f0000",mid="#fff5f0",high="#006837",midpoint=1) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm")) + 
  guides(fill = guide_colourbar(title.position="top"))
energy_notrade_plot 

iron_notrade_plot <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = notrade.plot, aes(fill = Iron_notrade), color="white", size=0.25) + 
  scale_fill_gradient2(name="Iron, no trade",low="#7f0000",mid="#fff5f0",high="#006837",midpoint=1) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm")) + 
  guides(fill = guide_colourbar(title.position="top"))
iron_notrade_plot

zinc_notrade_plot <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = notrade.plot, aes(fill = Zinc_notrade), color="white", size=0.25) + 
  scale_fill_gradient2(name="Zinc, no trade",low="#7f0000",mid="#fff5f0",high="#006837",midpoint=1) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm")) + 
  guides(fill = guide_colourbar(title.position="top"))
zinc_notrade_plot

calcium_notrade_plot <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = notrade.plot, aes(fill = Calcium_notrade), color="white", size=0.25) + 
  scale_fill_gradient2(name="Calcium, no trade",low="#7f0000",mid="#fff5f0",high="#006837",midpoint=1) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm")) + 
  guides(fill = guide_colourbar(title.position="top"))
calcium_notrade_plot

folate_notrade_plot <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = notrade.plot, aes(fill = Folate_notrade), color="white", size=0.25) + 
  scale_fill_gradient2(name="Folate, no trade",low="#7f0000",mid="#fff5f0",high="#006837",midpoint=1) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm")) + 
  guides(fill = guide_colourbar(title.position="top"))
folate_notrade_plot

VitA_notrade_plot <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = notrade.plot, aes(fill = VitA_notrade), color="white", size=0.25) + 
  scale_fill_gradient2(name="Vitamin A, no trade",low="#7f0000",mid="#fff5f0",high="#006837",midpoint=1) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm")) + 
  guides(fill = guide_colourbar(title.position="top"))
VitA_notrade_plot

VitB12_notrade_plot <- ggplot(data = area.points, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = notrade.plot, aes(fill = VitB12_notrade), color="white", size=0.25) + 
  scale_fill_gradient2(name="Vitamin B12, no trade",low="#7f0000",mid="#fff5f0",high="#006837",midpoint=1) + 
  theme_bw() + ditch_the_axes + theme(legend.key.size=unit(.5,"cm")) + 
  guides(fill = guide_colourbar(title.position="top"))
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

setwd("~/Documents/Work/Projects/Manuscripts/Unsubmitted/Nutrient Trade/Figures/Nutrients Trade No Trade")
for(i in 1:8){
  ggsave(plot=plotlist[[i]],file=paste("file",i,".pdf",sep=""))
}
for(i in 9:16){
  ggsave(plot=plotlist[[i]],file=paste("notrade.file",i,".pdf",sep=""))
}



# FIGURE 2
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
multiplot(Energy, Protein, Calcium, Folate, Iron, Zinc, VitA, VitB12, cols=3)


