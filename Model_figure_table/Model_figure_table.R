#########################################################################
library(cowplot)
library(dplyr)
library(visreg)
library(ggplot2)
library(viridis)
library(ggpointdensity)
library(raster)

#############################################
##############################################Table 1, 2
threedata<-read.csv("fliterdata_three_datasets.csv")

Woody1<-subset(threedata,threedata$Types=="Woody") ##15242
herbe1<-subset(threedata,threedata$Types=="Herbaceous") ##1272
Woody1_clean <- Woody1[complete.cases(Woody1[, c("rmflog", "tmean", "lnAI", "lnrzwc", "lnGPP", 
                                                 "pH", "Sand", "tmean", "lnheightT", "lnage", 
                                                 "lnRRD", "lnleafN", "lnSRL", "lnSLA", 
                                                 "lnthickness", "lnLDMC")]), ]
Woody1_clean1 <- Woody1_clean %>% filter(heightT >= 0.5)  # Adjust threshold if needed

model_wood_height_age1_trait<-lm(rmflog~tmean+lnAI+lnrzwc+lnGPP+Sand+tmean*lnAI+lnheightT+lnRRD+lnSLA+lnthickness+lnLDMC,weights = allweightsnew, data=Woody1_clean1)
summary(model_wood_height_age1_trait) ##this is the model I use

###################################
herbe1_clean <- herbe1[complete.cases(herbe1[, c("rmflog", "tmean", "lnAI", "lnrzwc", "lnGPP", 
                                                 "pH", "Sand", "tmean", "lnheightT", 
                                                 "lnRRD", "lnleafN", "lnSRL", "lnSLA", 
                                                 "lnthickness", "lnLDMC")]), ]
herbe1_clean1 <- herbe1_clean %>% filter(heightT <= 2)  # Adjust based on study definition


model_herbe_height_age1_trait<-lm(rmflog~tmean+lnAI+lnGPP+pH+Sand+lnheightT+lnRRD +lnSRL+lnthickness+lnLDMC,weights = allweightsnew, data=herbe1_clean1)
summary(model_herbe_height_age1_trait) ##this is the model I use
formula(model_herbe_height_age1_trait)


################################################################### figure 2
library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library(dplyr)

fulldata<-read.csv("fulldata_three_datasets.csv")

fulldata <- fulldata %>%
  mutate(vegetationType = ifelse(vegetationType == "grass", "herbaceous", vegetationType))

forest<-subset(fulldata,fulldata$vegetationType=="forest")
herb<-subset(fulldata,fulldata$vegetationType=="herbaceous")
shrub<-subset(fulldata,fulldata$vegetationType=="shrub")

##unique_coordinatesforest <- forest %>% distinct(longitude, latitude, .keep_all = TRUE)


# Load world countries data
world <- ne_countries(returnclass = "sf")

# Filter out Antarctica
world <- world[world$name != "Antarctica", ]

# Load world coastline data
worldline <- ne_coastline(returnclass = "sf")

# Filter out Antarctica
worldline <- world[world$name != "Antarctica", ]

# Plot the map
ggplot() +
  geom_sf(data = world,
          fill = "gray90", color = NA) +
  geom_sf(data = worldline) +
  ylab("") +
  xlab("") +
  theme_bw() +
  coord_sf(expand = FALSE) +
  scale_x_continuous(breaks = seq(from = -180, to = 180, by = 60))


p1<-ggplot(data = world) + geom_sf(data = world,
                                   fill = "antiquewhite", color = NA) +
  geom_sf(data = worldline) +
  ylab("") +
  xlab("") +
  theme_bw() +
  coord_sf(expand = FALSE) +
  scale_x_continuous(breaks = seq(from = -180, to = 180, by = 60))+
  geom_point(aes(longitude,latitude), forest, col ="#56B4E9" ,pch=16,size=1)+
  coord_sf(xlim = c(-180, 180), expand = FALSE) + xlab("Longitude") + ylab("Latitude") + 
  theme(panel.grid.major = element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(), panel.background = element_rect(fill = "white"))
p1
p2<-ggplot(data = world) +  geom_sf(data = world,
                                    fill = "antiquewhite", color = NA) +
  geom_sf(data = worldline) +
  ylab("") +
  xlab("") +
  theme_bw() +
  coord_sf(expand = FALSE) +
  scale_x_continuous(breaks = seq(from = -180, to = 180, by = 60))+ 
  geom_point(aes(longitude,latitude), herb,col ="orange",pch=16,size=1)+
  coord_sf(xlim = c(-180, 180), expand = FALSE) + xlab("Longitude") + ylab("Latitude") + 
  theme(panel.grid.major = element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(), panel.background = element_rect(fill = "white"))
p2

p3<-ggplot(data = world) +  geom_sf(data = world,
                                    fill = "antiquewhite", color = NA) +
  geom_sf(data = worldline) +
  ylab("") +
  xlab("") +
  theme_bw() +
  coord_sf(expand = FALSE) +
  scale_x_continuous(breaks = seq(from = -180, to = 180, by = 60))+ 
  geom_point(aes(longitude,latitude), shrub,col ="#009E73",pch=16,size=1)+
  coord_sf(xlim = c(-180, 180), expand = FALSE) + xlab("Longitude") + ylab("Latitude") + 
  theme(panel.grid.major = element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(), panel.background = element_rect(fill = "white"))
p3

library("gridExtra")
combined_plot <- grid.arrange(p1, p2,p3, ncol = 1)

# plot1 with legend
plot1_legend <- ggplot(fulldata, aes(x = longitude, y = latitude, col = vegetationType)) +
  geom_point(size = 4) +
  scale_color_manual(values = c("#56B4E9","orange","#009E73")) +
  theme(legend.position = "bottom",legend.text=element_text(size=15))+ labs(color=NULL)

get_only_legend <- function(plot) {
  plot_table <- ggplot_gtable(ggplot_build(plot))
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box")
  legend <- plot_table$grobs[[legend_plot]]
  return(legend)
}

# extract legend from plot1 using above function
legend <- get_only_legend(plot1_legend)   

# final combined plot with shared legend
grid.arrange(combined_plot, legend, nrow = 2, heights = c(10, 1))
##############################################################PCA data
###############################################################Fig 3
############################################################
library(FactoMineR)
library(factoextra)
pcasource<-data.frame(threedata[, c("tmean", "lnAI", "lnrzwc", "lnGPP", 
                                    "pH", "Sand", "lnheightT", 
                                    "lnRRD", "lnSRL", "lnSLA", 
                                    "lnthickness", "lnLDMC","Types")])
colnames(pcasource)[1] <- 'Tg'
colnames(pcasource)[2] <- 'lnAI'
colnames(pcasource)[3] <- 'lnRZwc'
colnames(pcasource)[4] <- 'lnGPP'
colnames(pcasource)[5] <- 'Soil pH'
colnames(pcasource)[6] <- 'Sand'
colnames(pcasource)[7] <- 'lnH'
colnames(pcasource)[8] <- 'lnRRD'
colnames(pcasource)[9] <- 'lnSRL'
colnames(pcasource)[10] <- 'lnSLA'
colnames(pcasource)[11] <- 'lnLT'
colnames(pcasource)[12] <- 'lnLDMC'
colnames(pcasource)[13] <- 'Types'
##change character in to numberic
i <- c(1:12)     
pcasource[ , i] <- apply(pcasource[ , i], 2,            # Specify own function within apply
                         function(x) as.numeric(as.character(x)))
sapply(pcasource, class)  

rmf.pca <- PCA(pcasource[,1:12], graph = FALSE)
##The value of the argument axes is incorrect. The number of axes in the data is: 5. Please try again with axes between 1 - 5
eig.val <- get_eigenvalue(rmf.pca)
eig.val

library("corrplot")
library(ggrepel)
var <- get_pca_var(rmf.pca)
var$contrib

var$cos2
rownames(var$cos2)[1] <- 'Tg'
rownames(var$cos2)[2] <- 'lnAI'
rownames(var$cos2)[3] <- 'lnRZwc'
rownames(var$cos2)[4] <- 'lnGPP'
rownames(var$cos2)[5] <- 'Soil pH'
rownames(var$cos2)[6] <- 'Sand'
rownames(var$cos2)[7] <- 'lnH'
rownames(var$cos2)[8] <- 'lnRRD'
rownames(var$cos2)[9] <- 'lnSRL'
rownames(var$cos2)[10] <- 'lnSLA'
rownames(var$cos2)[11] <- 'lnLT'
rownames(var$cos2)[12] <- 'lnLDMC'

#######################KMO
library(psych)
cortest.bartlett(pcasource[,1:12])
KMO(pcasource[,1:12])

png("/Users/echo/PhD_first_year/China_global/add_global_forest/aridity index/change Tg/replotfigure1.29_GCB/paperplot/pca_cos2_plot.png", width = 1200, height = 800)  # Adjust width and height as needed

par(mar = c(2, 2, 2, 2))  # Adjust margins as needed


colnames(var$cos2) <- c("PC1", "PC2", "PC3", "PC4", "PC5")

# Create the correlation plot
corrplot(var$cos2, 
         method = 'color',  # Use color to represent values
         is.corr = FALSE,   # Not a correlation matrix
         tl.col = "black",  # Text label color
         tl.cex = 4,     # Text label size
         tl.srt = 45,       # Text label rotation (45 degrees)
         tl.offset = 0.8,   # Text label offset
         cl.cex = 3,      # Color legend text size
         cl.ratio = 0.4,    # Adjust color legend width
         cl.offset = 5,   # Offset of the legend text
         cl.length = 10)     # Number of breakpoints in the color legend
dev.off()

contrib_matrix <- var$contrib  # Contributions of variables to PCs
cos2_matrix <- var$cos2        # Squared cosine values (representative of variable quality)

# Create a matrix to represent positive/negative contributions
# For example, use the loadings (var$coord) to determine the sign
var$coord

loadings_matrix <- var$coord   # Loadings of variables on PCs
colnames(loadings_matrix) <- c("PC1", "PC2", "PC3", "PC4", "PC5")

par(mar = c(2, 2, 2, 2))  # Adjust margins as needed

# Plot with corrplot, using a diverging color scheme
corrplot(loadings_matrix, 
         method = 'color',      # Use color to represent values
         is.corr = FALSE,       # Not a correlation matrix
         tl.col = "black",      # Text label color
         tl.cex = 4,            # Text label size
         tl.srt = 45,           # Text label rotation (45 degrees)
         tl.offset = 0.8,       # Text label offset
         cl.cex = 3,          # Color legend text size
         cl.ratio = 0.4,        # Adjust color legend width
         cl.offset = 5,         # Offset of the legend text
         cl.length = 10,        # Number of breakpoints in the color legend
         col = colorRampPalette(c("cornflowerblue", "white", "darkorange"))(100))  # Diverging color scheme
dev.off()
##########################################################
library(ggrepel)
pca1<-fviz_pca_biplot(rmf.pca,axes = c(1, 2),
                      # Individuals
                      geom.ind = "point",
                      alpha.ind=0.1,
                      fill.ind = pcasource$Types, col.ind = "grey",
                      pointshape = 21, pointsize = 3,
                      palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                      addEllipses = TRUE,
                      # Variables
                      labelsize =10,
                      arrowsize = 1.5,
                      col.var = "contrib",
                      gradient.cols = c("darkorange2", "darkorchid", "blue4"),
                      scale = "none",
                      repel = TRUE,
                      label = "var"
) +
  scale_color_gradientn(colours = c("darkorange2", "darkorchid", "blue4"),
                        limits = c(0,50), # set the limits
                        breaks = seq(0, 50, by = 10)) + # set the breaks
  ylab(expression('PC2 (17.7%)')) + xlab(expression('PC1 (39.2%)')) +
  theme(legend.position = "none", title  =element_blank(),
        axis.title = element_text(size = 35),
        axis.text = element_text(size = 35))
pca1
# Extract variable coordinates and names

pca2<-fviz_pca_biplot(rmf.pca,axes = c(2, 3),
                      # Individuals
                      geom.ind = "point",
                      alpha.ind=0.1,
                      fill.ind = pcasource$Types, col.ind = "grey",
                      pointshape = 21, pointsize = 3,
                      palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                      addEllipses = TRUE,
                      # Variables
                      labelsize = 10,
                      arrowsize = 1.5,
                      col.var = "contrib",
                      gradient.cols = c("darkorange2", "darkorchid", "blue4"),
                      repel = TRUE,
                      legend.title = list(fill = "Vegetation type", color = "Contrib"),
                      label = "var"
)+ scale_color_gradientn(colours = c("darkorange2", "darkorchid", "blue4"),
                         limits = c(0,50), # set the limits
                         breaks = seq(0, 50, by = 10)) + # set the breaks
  ylab(expression('PC3 (10.4%)'))+xlab(expression('PC2 (17.7%)'))+
  theme(legend.position = "none",title  =element_blank(),
        axis.title = element_text(size = 35),
        axis.text = element_text(size = 35)) 
pca2

pca3<-fviz_pca_biplot(rmf.pca,axes = c(3, 4),
                      # Individuals
                      geom.ind = "point",
                      alpha.ind=0.1,
                      fill.ind = pcasource$Types, col.ind = "grey",
                      pointshape = 21, pointsize = 3,
                      palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                      addEllipses = TRUE,
                      # Variables
                      labelsize = 10,
                      arrowsize = 1.5,
                      col.var = "contrib",
                      gradient.cols = c("darkorange2", "darkorchid", "blue4"),
                      repel = TRUE,
                      
                      legend.title = list(fill = "Vegetation type", color = "Contrib"),
                      label = "var"
)+ scale_color_gradientn(colours = c("darkorange2", "darkorchid", "blue4"),
                         limits = c(0,50), # set the limits
                         breaks = seq(0, 50, by = 10)) + # set the breaks
  ylab(expression('PC4 (8.8%)'))+xlab(expression('PC3 (10.4%)'))+
  theme(legend.position = "none",title  =element_blank(),
        axis.title = element_text(size = 35),
        axis.text = element_text(size = 35))

pca3

pca4<-fviz_pca_biplot(rmf.pca,axes = c(4, 5),
                      # Individuals
                      geom.ind = "point",
                      alpha.ind=0.1,
                      fill.ind = pcasource$Types, col.ind = "grey",
                      pointshape = 21, pointsize = 3,
                      palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                      addEllipses = TRUE,
                      # Variables
                      labelsize = 10,
                      arrowsize = 1.5,
                      col.var = "contrib",
                      gradient.cols = c("darkorange2", "darkorchid", "blue4"),
                      repel = TRUE,
                      legend.title = list(fill = "Vegetation type", color = "Contrib"),
                      label = "var"
) + scale_color_gradientn(colours = c("darkorange2", "darkorchid", "blue4"),
                          limits = c(0,50), # set the limits
                          breaks = seq(0, 50, by = 10)) + # set the breaks
  ylab(expression('PC5 (6.2%)'))+xlab(expression('PC4 (8.8%)'))+
  theme(legend.position = "none",title  =element_blank(),
        axis.title = element_text(size = 35),
        axis.text = element_text(size = 35)) 
pca4

legend<-fviz_pca_biplot(rmf.pca,axes = c(4, 5),
                        # Individuals
                        geom.ind = "point",
                        alpha.ind=0.2,
                        fill.ind = pcasource$Types, col.ind = "grey",
                        pointshape = 21, pointsize = 2,
                        palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                        addEllipses = TRUE,
                        # Variables
                        labelsize = 10,
                        arrowsize = 1.5,
                        col.var = "contrib",
                        gradient.cols = c("darkorange2", "darkorchid", "blue4"),
                        repel = TRUE,
                        legend.title = list(fill = "Vegetation type", color = "Contribution (%)"),
                        label = "var"
) + scale_color_gradientn(colours = c("darkorange2", "darkorchid", "blue4"),
                          limits = c(0,50), # set the limits
                          breaks = seq(0, 50, by = 10)) + # set the breaks
  theme(legend.position = "none",legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        title  =element_blank(),
        legend.key.size = unit(1.5, "cm"),
        legend.key.width = unit(1,"cm"),
        axis.title = element_text(size = 35),
        axis.text = element_text(size = 35)) 
legend
library(cowplot)
plot_grid(pca1,
          pca2,pca3, pca4,
          ncol = 2, nrow = 2
)

###########################################################Density plot
############################################################Fig S1
fliterdata<-data.frame(rbind(Woody1_clean1,herbe1_clean1))
plotdata1<-fliterdata[, c("Vegetation.type" ,"tmean", "AI", "rzwc", "GPP", 
                          "pH", "Sand", "heightT", 
                          "RRD", "SRL", "SLA", 
                          "thickness", "LDMC")]
plotdata1<-na.omit(plotdata1)


#plotdata1$Vegetation.type <- as.factor(plotdata1$Vegetation.type)
plotdata1$Vegetation.type <- factor(plotdata1$Vegetation.type, levels = c("forest", "shrub", "grass"))

# Replace "grass" with "herbaceous"
plotdata1 <- plotdata1 %>%
  mutate(Vegetation.type = ifelse(Vegetation.type == "grass", "herbaceous", as.character(Vegetation.type)))

plotTg2<-ggplot(plotdata1, aes(x = tmean, fill = Vegetation.type)) +
  geom_histogram(binwidth = 2, position = "identity") +
  scale_fill_viridis(discrete=TRUE, name="")+
  #scale_fill_manual(values = c("Forest" = "#336633", "Herb" = "#cc6600", "Shrub" = "#56B4E9"))+
  ylab(expression('Frequences of Tg '))+ xlab(expression('Tg'*" ("*paste(degree,C)*")"))+
  theme_bw() +theme(legend.position = "none",axis.title=element_text(size=20),axis.title.y=element_blank(),legend.text=element_text(size=25),
                    axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=27, y=2500, label="(a)",size=10)
plotTg2

plot_rzwc <-ggplot(data = plotdata1,aes(x = rzwc, fill = Vegetation.type)) + 
  geom_histogram(binwidth = 50, position = "identity")+
  scale_fill_viridis(discrete=TRUE, name="")+
  ylab(expression("Frequences of RZwc")) +xlab(expression('RZ'[wc] *~'(mm)'))+
  theme_bw() +theme(legend.position = "none",axis.title=element_text(size=20),axis.title.y=element_blank(),
                    axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=1400, y=2700, label="(b)",size=10)
plot_rzwc

plot_GPP <-ggplot(data = plotdata1,aes(x = GPP, fill = Vegetation.type)) + 
  geom_histogram(binwidth = 0.2, position = "identity") +
  scale_fill_viridis(discrete=TRUE, name="")+
  ylab(expression("Frequences of GPP")) +xlab(expression('lnGPP (gC'*~'m'^-2*~'d'^-1*')'))+
  theme_bw() +theme(legend.position = "none",axis.title=element_text(size=20),axis.title.y=element_blank(),
                    axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=4.8, y=1600, label="(c)",size=10)
plot_GPP

plot_pH <-ggplot(data = plotdata1,aes(x = pH, fill = Vegetation.type)) + 
  geom_histogram(binwidth = 0.2, position = "identity") +
  scale_fill_viridis(discrete=TRUE, name="")+
  ylab(expression("Frequences of Soil pH")) + xlab(expression("Soil pH"))+
  theme_bw() +theme(legend.position = "none",axis.title=element_text(size=20),axis.title.y=element_blank(),
                    axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=8.5, y=2000, label="(d)",size=10)
plot_pH

plot_Sand <-ggplot(data = plotdata1,aes(x = Sand, fill = Vegetation.type)) + 
  geom_histogram(binwidth = 2, position = "identity") +
  scale_fill_viridis(discrete=TRUE, name="")+
  ylab(expression("Frequences of Sand")) + xlab(expression("Sand (% of weight)"))+
  theme_bw() +theme(legend.position = "none",axis.title=element_text(size=20),axis.title.y=element_blank(),
                    axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=92, y=1600, label="(e)",size=10)
plot_Sand

plot_AI <-ggplot(data = plotdata1,aes(x = AI, fill = Vegetation.type)) + 
  geom_histogram(binwidth = 0.2, position = "identity") +
  scale_fill_viridis(discrete=TRUE, name="")+
  ylab(expression("Frequences of AI")) +
  theme_bw() +theme(legend.position = "none",axis.title=element_text(size=20),axis.title.y=element_blank(),
                    axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=18, y=2200, label="(f)",size=10)
plot_AI

plotH<-ggplot(plotdata1, aes(x = heightT, fill = Vegetation.type)) +
  geom_histogram(binwidth = 0.5, position = "identity") +
  scale_fill_viridis(discrete=TRUE, name="")+
  #scale_fill_manual(values = c("Forest" = "#336633", "Herb" = "#cc6600", "Shrub" = "#56B4E9"))+
  ylab(expression('Frequences of H '))+ xlab(expression('H'*" (m)"))+
  theme_bw() +theme(legend.position = "none",axis.title=element_text(size=20),axis.title.y=element_blank(),legend.text=element_text(size=25),
                    axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=15, y=1900, label="(g)",size=10)
plotH

plotRRD<-ggplot(plotdata1, aes(x = RRD, fill = Vegetation.type)) +
  geom_histogram(binwidth = 0.05, position = "identity") +
  scale_fill_viridis(discrete=TRUE, name="")+
  #scale_fill_manual(values = c("Forest" = "#336633", "Herb" = "#cc6600", "Shrub" = "#56B4E9"))+
  ylab(expression('Frequences of RRD '))+ xlab(expression('RRD'*" (m)"))+
  theme_bw() +theme(legend.position = "none",axis.title=element_text(size=20),axis.title.y=element_blank(),legend.text=element_text(size=25),
                    axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=1.58, y=1500, label="(h)",size=10)
plotRRD

plotSRL<-ggplot(plotdata1, aes(x = SRL, fill = Vegetation.type)) +
  geom_histogram(binwidth = 200, position = "identity") +
  scale_fill_viridis(discrete=TRUE, name="")+
  #scale_fill_manual(values = c("Forest" = "#336633", "Herb" = "#cc6600", "Shrub" = "#56B4E9"))+
  ylab(expression('Frequences of SRL '))+ xlab(expression('SRL'*" (cm g)"))+
  theme_bw() +theme(legend.position = "none",axis.title=element_text(size=20),axis.title.y=element_blank(),legend.text=element_text(size=25),
                    axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=9500, y=1850, label="(i)",size=10)
plotSRL

plotSLA<-ggplot(plotdata1, aes(x = SLA, fill = Vegetation.type)) +
  geom_histogram(binwidth = 0.5, position = "identity") +
  scale_fill_viridis(discrete=TRUE, name="")+
  #scale_fill_manual(values = c("Forest" = "#336633", "Herb" = "#cc6600", "Shrub" = "#56B4E9"))+
  ylab(expression('Frequences of SLA '))+xlab(expression('SLA (m'^2~"kg"^-1*")"))+
  theme_bw() +theme(legend.position = "none",axis.title=element_text(size=20),axis.title.y=element_blank(),legend.text=element_text(size=25),
                    axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=25, y=1800, label="(j)",size=10)
plotSLA

plotLT<-ggplot(plotdata1, aes(x = thickness, fill = Vegetation.type)) +
  geom_histogram(binwidth = 0.02, position = "identity") +
  scale_fill_viridis(discrete=TRUE, name="")+
  #scale_fill_manual(values = c("Forest" = "#336633", "Herb" = "#cc6600", "Shrub" = "#56B4E9"))+
  ylab(expression('Frequences of LT '))+ xlab(expression('LT'*" (mm)"))+
  theme_bw() +theme(legend.position = "none",axis.title=element_text(size=20),axis.title.y=element_blank(),legend.text=element_text(size=25),
                    axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=0.43, y=3600, label="(k)",size=10)
plotLT

plotLDMC<-ggplot(plotdata1, aes(x = LDMC, fill = Vegetation.type)) +
  geom_histogram(binwidth = 0.001, position = "identity") +
  scale_fill_viridis(discrete=TRUE, name="")+
  #scale_fill_manual(values = c("Forest" = "#336633", "Herb" = "#cc6600", "Shrub" = "#56B4E9"))+
  ylab(expression('Frequences of LDMC '))+xlab(expression('LDMC (g'~"g"^-1*")")) +
  theme_bw() +theme(legend.position = "none",axis.title=element_text(size=20),axis.title.y=element_blank(),legend.text=element_text(size=25),
                    axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=0.115, y=2050, label="(l)",size=10)
plotLDMC
checkdesity<-plot_grid(plotTg2,plot_rzwc,plot_GPP,plot_pH,plot_Sand,plot_AI,plotH,plotRRD,plotSRL,plotSLA,plotLT,plotLDMC,label_x = 0.2,nrow=3)
checkdesity

#############################################model for shrub and forest seperately
forest1<-subset(fliterdata,fliterdata$Vegetation.type=="forest") ##11372
shrub1<-subset(fliterdata,fliterdata$Vegetation.type=="shrub") ##1028

forst1_clean <- forest1[complete.cases(forest1[, c("rmflog", "tmean", "lnAI", "lnrzwc", "lnGPP", 
                                                   "pH", "Sand", "tmean", "lnheightT", 
                                                   "lnRRD", "lnSRL", "lnSLA", 
                                                   "lnthickness", "lnLDMC")]), ]

model_forest2<-lm(rmflog~tmean+lnAI+lnrzwc+lnGPP+pH+Sand+tmean*lnAI+lnSRL+lnSLA+lnLDMC,weights = allweightsnew, data=forst1_clean)
summary(model_forest2)

#############################################predictor importance 
###########################################Fig 6
library(relaimpo)
rel_imp_all_wood <- calc.relimp(model_wood_height_age1_trait, type = "lmg",rela = TRUE)
importance_df_wood <- data.frame(
  Variable = names(rel_imp_all_wood$lmg),
  Importance = rel_imp_all_wood$lmg
)
importance_df_wood$Variable<-c("Tg","lnAI","lnRZwc","lnGPP","Sand","lnH","lnRRD","lnSLA","lnLT","lnLDMC","Tg:lnAI")
# Create the plot with thin bars and circled values
pall_wood <- ggplot(importance_df_wood, aes(x = reorder(Variable, Importance), y = Importance)) +
  # Thin bars
  geom_bar(stat = "identity", fill = "#56B4E9", width = 0.5) +  # Adjust width for bar thickness
  scale_y_continuous(limits=c(0,0.4))+
  # Add values at the end of the bars
  geom_text(aes(label = sprintf("%.2f", Importance)), 
            hjust = -0.2, size = 4, color = "black") +  # Adjust hjust for positioning
  # Flip coordinates for horizontal bars
  coord_flip() +
  # Customize theme
  theme_minimal() +
  labs(x = "Predictor Variables",
       y = "Relative Importance (LMG)") +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = rel(1.5)),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"))

# Display the plot
print(pall_wood)

rel_imp_all_herb <- calc.relimp(model_herbe_height_age1_trait, type = "lmg",rela = TRUE)
importance_df_herb <- data.frame(
  Variable = names(rel_imp_all_herb$lmg),
  Importance = rel_imp_all_herb$lmg
)
importance_df_herb$Variable<-c("Tg","lnAI","lnGPP","pH","Sand","lnH","lnRRD","lnSRL","lnLT","lnLDMC")
# Create the plot with thin bars and circled values
pall_herb <- ggplot(importance_df_herb, aes(x = reorder(Variable, Importance), y = Importance)) +
  # Thin bars
  geom_bar(stat = "identity", fill = "orange", width = 0.5) +  # Adjust width for bar thickness
  scale_y_continuous(limits=c(0,0.4))+
  # Add values at the end of the bars
  geom_text(aes(label = sprintf("%.2f", Importance)), 
            hjust = -0.2, size = 4, color = "black") +  # Adjust hjust for positioning
  # Flip coordinates for horizontal bars
  coord_flip() +
  # Customize theme
  theme_minimal() +
  labs(x = "Predictor Variables",
       y = "Relative Importance (LMG)") +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = rel(1.5)),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"))

# Display the plot
print(pall_herb)

plot_grid(
  pall_wood, pall_herb,
  ncol = 2, nrow = 1
)

################################################importance predictors for three species
################################################ Fig S4
rel_imp_all_tree <- calc.relimp(model_forest2, type = "lmg",rela = TRUE)
importance_df_tree <- data.frame(
  Variable = names(rel_imp_all_tree$lmg),
  Importance = rel_imp_all_tree$lmg
)
importance_df_tree$Variable<-c("Tg","lnAI","lnRZwc","lnGPP","pH","Sand","lnSRL","lnSLA","lnLDMC","Tg:lnAI")
# Create the plot with thin bars and circled values
pall_tree <- ggplot(importance_df_tree, aes(x = reorder(Variable, Importance), y = Importance)) +
  # Thin bars
  geom_bar(stat = "identity", fill = "#56B4E9", width = 0.5) +  # Adjust width for bar thickness
  scale_y_continuous(limits=c(0,0.4))+
  # Add values at the end of the bars
  geom_text(aes(label = sprintf("%.2f", Importance)), 
            hjust = -0.2, size = 4, color = "black") +  # Adjust hjust for positioning
  # Flip coordinates for horizontal bars
  coord_flip() +
  # Customize theme
  theme_minimal() +
  labs(x = "Predictor Variables",
       y = "Relative Importance (LMG)") +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = rel(1.5)),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"))

# Display the plot
print(pall_tree)

rel_imp_all_shrub <- calc.relimp(model_shrub2, type = "lmg",rela = TRUE)
importance_df_shrub <- data.frame(
  Variable = names(rel_imp_all_shrub$lmg),
  Importance = rel_imp_all_shrub$lmg
)
importance_df_shrub$Variable<-c("lnGPP","Sand","lnH","lnRRD","lnSRL","lnSLA","lnLT","lnLDMC")
# Create the plot with thin bars and circled values
pall_shrub <- ggplot(importance_df_shrub, aes(x = reorder(Variable, Importance), y = Importance)) +
  # Thin bars
  geom_bar(stat = "identity", fill = "#009E73", width = 0.5) +  # Adjust width for bar thickness
  scale_y_continuous(limits=c(0,0.4))+
  # Add values at the end of the bars
  geom_text(aes(label = sprintf("%.2f", Importance)), 
            hjust = -0.2, size = 4, color = "black") +  # Adjust hjust for positioning
  # Flip coordinates for horizontal bars
  coord_flip() +
  # Customize theme
  theme_minimal() +
  labs(x = "Predictor Variables",
       y = "Relative Importance (LMG)") +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = rel(1.5)),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"))

# Display the plot
print(pall_shrub)

plot_grid(
  pall_tree,pall_shrub, pall_herb,
  ncol = 3, nrow = 1
)
###################################################VIF
###########################################Fig S5
library(car)
vif_testwood<-vif(model_wood_height_age1_trait)
test<-data.frame(vif_testwood)
test$row_names <- c("Tg","lnAI",'lnRZwc','lnGPP','Sand',"lnH","lnRRD","lnSLA","lnLT","lnLDMC","Tg:lnAI")
#create horizontal bar chart to display each VIF value
#barplot(vif_testwood, main = "Woody vegetation", width = 5,las=3,cex.axis=1.5,cex.names = 1,font.axis = 1,col = "steelblue",ylim=c(0,2))

vifwood<-ggplot(test, aes(x = row_names, y = vif_testwood)) +
  geom_bar(stat = "identity") +
  ylab(expression('VIF for Woody '))+
  # Rotate the X axis labels by 45 degrees
  theme_bw() +theme(axis.title=element_text(size=25),axis.text=element_text(size=rel(2)),axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x=element_blank(),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))
vifwood

vif_testherb<-vif(model_herbe_height_age1_trait)
#create horizontal bar chart to display each VIF value
#barplot(vif_testherb, main = "Herbaceous vegetation ",width = 5,las=1,cex.axis=1.5,cex.names = 1,font.axis = 1,col = "steelblue",ylim=c(0,2))
test1<-data.frame(vif_testherb)
test1$row_names <- c("Tg","lnAI","lnGPP","pH","Sand","lnH","lnRRD","lnSRL","lnLT","lnLDMC")
vifherb<-ggplot(test1, aes(x = row_names, y = vif_testherb)) +
  geom_bar(stat = "identity") +
  ylab(expression('VIF for herbaceous '))+
  # Rotate the X axis labels by 45 degrees
  theme_bw() +theme(axis.title=element_text(size=25),axis.text=element_text(size=rel(2)),axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x=element_blank(),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))
vifherb

vif_testshrub<-vif(model_shrub2)
#create horizontal bar chart to display each VIF value
#barplot(vif_testshrub, main = "shrubaceous vegetation ",width = 5,las=1,cex.axis=1.5,cex.names = 1,font.axis = 1,col = "steelblue",ylim=c(0,2))
test1<-data.frame(vif_testshrub)
test1$row_names <- c("lnGPP","Sand","lnH","lnRRD","lnSRL","lnSLA","lnLT","lnLDMC")
vifshrub<-ggplot(test1, aes(x = row_names, y = vif_testshrub)) +
  geom_bar(stat = "identity") +
  ylab(expression('VIF for shrub '))+
  # Rotate the X axis labels by 45 degrees
  theme_bw() +theme(axis.title=element_text(size=25),axis.text=element_text(size=rel(2)),axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x=element_blank(),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))
vifshrub

vif_testforest<-vif(model_forest2)
forest<-data.frame(vif_testforest)
forest$row_names <- c("Tg","lnAI",'lnRZwc','lnGPP',"Soil pH",'Sand',"lnSRL","lnSLA","lnLDMC","Tg:lnAI")
#create horizontal bar chart to display each VIF value
#barplot(vif_testwood, main = "Woody vegetation", width = 5,las=3,cex.axis=1.5,cex.names = 1,font.axis = 1,col = "steelblue",ylim=c(0,2))

vifforest<-ggplot(forest, aes(x = row_names, y = vif_testforest)) +
  geom_bar(stat = "identity") +
  ylab(expression('VIF for forest '))+
  # Rotate the X axis labels by 45 degrees
  theme_bw() +theme(axis.title=element_text(size=25),axis.text=element_text(size=rel(2)),axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x=element_blank(),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))
vifforest

multvif<-plot_grid(vifwood,vifherb,vifforest,vifshrub,label_x = 0.2,nrow = 1)
multvif

########################################################Pearson correlation coefficients
#####################################################Fig S6
plotdata_all<-fliterdata[,c( "tmean", "lnAI", "lnrzwc", "lnGPP", 
                             "pH", "Sand", "lnheightT", 
                             "lnRRD", "lnSRL", "lnSLA", 
                             "lnthickness", "lnLDMC")]
plotdata_all<- as.data.frame(sapply(plotdata_all, as.numeric))
plotdata_all<-na.omit(plotdata_all)
sapply(plotdata_all, class)
cormat_all <- round(cor(plotdata_all),2)
head(cormat_all)
rownames(cormat_all)<-c("Tg","lnAI","lnRZwc","lnGPP", "pH", "Sand", "lnH","lnRRD","lnSRL","lnSLA","lnLT","lnLDMC")
colnames(cormat_all)<-c("Tg","lnAI","lnRZwc","lnGPP", "pH", "Sand", "lnH","lnRRD","lnSRL","lnSLA","lnLT","lnLDMC")
#  get_lower_tri<-function(cormat_all){
# cormat_all[upper.tri(cormat_all)] <- NA
# return(cormat_all)
# }
library(reshape2)
get_upper_tri <- function(cormat_all){
  cormat_all[lower.tri(cormat_all)]<- NA
  return(cormat_all)
}
upper_tri <- get_upper_tri(cormat_all)
melted_cormat_all <- melt(upper_tri,na.rm = TRUE)
head(melted_cormat_all)

# Heatmap
library(ggplot2)
ggheatmap_all <- ggplot(melted_cormat_all, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "Orange", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  #ggtitle("Forest")+
  theme_minimal()+ # minimal theme
  theme(legend.position = "none",axis.text.x = element_text(angle = 45, vjust = 1, 
                                                            size = 12, hjust = 1),
        axis.text.y = element_text(size = 12))+
  coord_fixed()

coplot_all<-ggheatmap_all + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(legend.position = "right",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank())
coplot_all
#####################################################Observed VS predicted
#####################################################Fig.S7 and Fig.S8

library(qpcR)
##the length of prediction  
pred  <- predict(model_wood_height_age1_trait)
coef  <- coefficients(model_wood_height_age1_trait)
summary_model<-summary(model_wood_height_age1_trait)
R2 <- summary_model$r.squared
# R2 <- Rsq.ad(lmtwotypesnoin)
RMSE <- RMSE(model_wood_height_age1_trait)

Df.pred<-Woody1_clean1[,c( "tmean", "lnAI", "lnrzwc", "lnGPP", 
                           "pH", "Sand", "lnheightT", 
                           "lnRRD", "lnSRL", "lnSLA", 
                           "lnthickness", "lnLDMC","rmflog","allweightsnew")]

Df.pred$Predlnrs <- predict(model_wood_height_age1_trait, newdata = Df.pred) ## Asking the model to predict values corresponding to Df.pred based on mod

test <- lm(rmflog~Predlnrs,weights = allweightsnew, data = Df.pred)
summary(test)
R2 <- Rsq.ad(test)
RMSE <- RMSE(test)

p22<- ggplot(Df.pred, aes(x=Predlnrs, y=rmflog))+ 
  geom_pointdensity(size = 1) +
  scale_color_viridis(option = 'viridis', direction = -1)+
  geom_smooth(col="blue",method='lm',se=TRUE,size=1)+
  geom_abline(intercept=0, slope=1,aes(colour='black'))+
  ylab(expression('lnObserved R:S')) + xlab(expression('lnPredicted R:S'))+
  theme_bw() +theme( legend.position = "right",axis.title.x=element_text(size=25),axis.title.y=element_text(size=25),
                     axis.text=element_text(size=rel(2)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate('text', x = -1, y = -4.4, label = expression(paste("R"^2, " = 0.128")), size = 6) +
  annotate('text', x = -1, y = -5, label = paste("RMSE = ",round(RMSE,2),sep = ""), size = 6)
p22

pred_herb  <- predict(model_herbe_height_age1_trait)
coef_herb  <- coefficients(model_herbe_height_age1_trait)
summary_model_herb<-summary(model_herbe_height_age1_trait)
R2_herb <- summary_model_herb$r.squared
# R2 <- Rsq.ad(lmtwotypesnoin)
RMSE_herb <- RMSE(model_herbe_height_age1_trait)

Df.pred_herb<-herbe1_clean1[,c( "tmean", "lnAI", "lnrzwc", "lnGPP", 
                                "pH", "Sand", "lnheightT", 
                                "lnRRD", "lnSRL", "lnSLA", 
                                "lnthickness", "lnLDMC","rmflog","allweightsnew")]

Df.pred_herb$pred_herblnrs <- predict(model_herbe_height_age1_trait, newdata = Df.pred_herb) ## Asking the model to pred_herbict values corresponding to Df.pred_herb based on mod

test_herb <- lm(rmflog~pred_herblnrs,weights = allweightsnew, data = Df.pred_herb)
summary(test_herb)
R2 <- Rsq.ad(test_herb)
RMSE <- RMSE(test_herb)

p22_herb<- ggplot(Df.pred_herb, aes(x=pred_herblnrs, y=rmflog))+ 
  geom_pointdensity(size = 1) +
  scale_color_viridis(option = 'viridis', direction = -1)+
  geom_smooth(col="blue",method='lm',se=TRUE,size=1)+
  geom_abline(intercept=0, slope=1,aes(colour='black'))+
  ylab(expression('lnObserved R:S')) + xlab(expression('lnpredicted R:S'))+
  theme_bw() +theme( legend.position = "right",axis.title.x=element_text(size=25),axis.title.y=element_text(size=25),
                     axis.text=element_text(size=rel(2)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate('text', x = 2.2, y = -4.4, label = expression(paste("R"^2, " = 0.309")), size = 6) +
  annotate('text', x = 2.2, y = -5, label = paste("RMSE = ",round(RMSE,2),sep = ""), size = 6)
p22_herb
######################################################
#####################################################
min(fliterdata$ratio,na.rm = TRUE)
##0.008633158
mix_row_numbers <- which.min(fliterdata$ratio)
max(fliterdata$ratio,na.rm = TRUE)
##14.4692
max_row_numbers <- which.max(fliterdata$ratio)
min_row_numbers <- which.min(fliterdata$ratio)
median(fliterdata$ratio,na.rm = TRUE)
## 0.26
min(Woody1_clean1$ratio,na.rm = TRUE)
##0.008633158
max(Woody1_clean1$ratio,na.rm = TRUE)
## 2.125654
median(Woody1_clean1$ratio,na.rm = TRUE)
##0.25
##############
min(herbe1_clean1$ratio,na.rm = TRUE)
## 0.03208326
max(herbe1_clean1$ratio,na.rm = TRUE)
## 14.4692
median(herbe1_clean1$ratio,na.rm = TRUE)
## 4.7
##########################
##############################################high density
density_Tg <- density(fliterdata$Tg)
mode_values <- density_Tg$x[density_Tg$y == max(density_Tg$y)]
##27.807
density_rzwc <- density(fliterdata$rzwc)
mode_values <- density_rzwc$x[density_rzwc$y == max(density_rzwc$y)]
##159.341
density_GPP <- density(fliterdata$GPP)
mode_values <- density_GPP$x[density_GPP$y == max(density_GPP$y)]
##3.464
density_pH <- density(fliterdata$pH)
mode_values <- density_pH$x[density_pH$y == max(density_pH$y)]
##4.106
density_Sand <- density(fliterdata$Sand)
mode_values <- density_Sand$x[density_Sand$y == max(density_Sand$y)]
##78.375
density_AI <- density(fliterdata$AI)
mode_values <- density_AI$x[density_AI$y == max(density_AI$y)]
##1.063
density_H <- density(fliterdata$heightT)
mode_values <- density_H$x[density_H$y == max(density_H$y)]
##0.865
density_SLA <- density(fliterdata$SLA)
mode_values <- density_SLA$x[density_SLA$y == max(density_SLA$y)]
##14.125
density_SRL <- density(fliterdata$SRL)
mode_values <- density_SRL$x[density_SRL$y == max(density_SRL$y)]
##1380.163
density_RRD <- density(fliterdata$RRD)
mode_values <- density_RRD$x[density_RRD$y == max(density_RRD$y)]
##0.445
density_LT <- density(fliterdata$thickness)
mode_values <- density_LT$x[density_LT$y == max(density_LT$y)]
##0.199
density_LDMC <- density(fliterdata$LDMC)
mode_values <- density_LDMC$x[density_LDMC$y == max(density_LDMC$y)]
##0.109


##################################################plot the partial residual plot
######################################################################### Fig 4,5,S2,S3
library(cowplot)
library(dplyr)
library(visreg)
library(ggplot2)
library(viridis)
library(ggpointdensity)
library(raster)
f2t_types1<-visreg(model_wood_height_age1_trait,"lnrzwc",ylab="ln(Root:Shoot)",line=c(col="deeppink"))
lower_bound <- quantile(f2t_types1$res$visregRes, 0.0025, na.rm = TRUE)
upper_bound <- quantile(f2t_types1$res$visregRes, 0.9975, na.rm = TRUE)


f2t_types1$res <- f2t_types1$res[f2t_types1$res$visregRes >= lower_bound & f2t_types1$res$visregRes <= upper_bound, ]

f2t_typesplot1<- ggplot(data=f2t_types1$res, aes(x=lnrzwc, y=visregRes))+
  geom_pointdensity(size = 1.5) +
  scale_color_viridis(option = 'inferno', direction = -1, breaks = c(100, 500, 1000,4000,7000,10000,13000), begin = 0.25,limits=c(floor(0), ceiling(13000)))+
  geom_ribbon(data=f2t_types1$fit,aes(x=lnrzwc, y=visregFit,ymin = visregLwr, ymax = visregUpr), alpha = 0.5)+
  geom_smooth(data=f2t_types1$fit, aes(x=lnrzwc, y=visregFit),col="dodgerblue3",method='lm',se=TRUE,size=2)+
  xlab(expression('lnRZ'[wc] *~'(mm)')) +
  theme_bw() +theme( legend.position = "none",axis.title.x=element_text(size=20),axis.title.y=element_blank(),
                     axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=4.5, y=0.9, label="(b)",size=10)
f2t_typesplot1

f3t_types1<-visreg(model_wood_height_age1_trait,"lnGPP",ylab="ln(Root:Shoot)",line=c(col="deeppink"))
lower_bound <- quantile(f3t_types1$res$visregRes, 0.0025, na.rm = TRUE)
upper_bound <- quantile(f3t_types1$res$visregRes, 0.9975, na.rm = TRUE)

f3t_types1$res <- f3t_types1$res[f3t_types1$res$visregRes >= lower_bound & f3t_types1$res$visregRes <= upper_bound, ]

f3t_typesplot1<- ggplot(data=f3t_types1$res, aes(x=lnGPP, y=visregRes))+ 
  geom_pointdensity(size = 1.5) +
  scale_color_viridis(option = 'inferno', direction = -1, breaks = c(100, 500, 1000,4000,7000,10000,13000), begin = 0.25,limits=c(floor(0), ceiling(13000)))+
  geom_ribbon(data=f3t_types1$fit,aes(x=lnGPP, y=visregFit,ymin = visregLwr, ymax = visregUpr), alpha = 0.5)+
  geom_smooth(data=f3t_types1$fit, aes(x=lnGPP, y=visregFit),col="dodgerblue3",method='lm',se=TRUE,size=2)+
  xlab(expression('lnGPP (gC'*~'m'^-2*~'d'^-1*')')) +
  theme_bw() +theme( legend.position = "none",axis.title.x=element_text(size=20),axis.title.y=element_blank(),
                     axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=0.3, y=0.9, label="(c)",size=10)
f3t_typesplot1

f5t_types1<-visreg(model_wood_height_age1_trait,"Sand",ylab="ln(Root:Shoot)",line=c(col="deeppink"))
lower_bound <- quantile(f5t_types1$res$visregRes, 0.0025, na.rm = TRUE)
upper_bound <- quantile(f5t_types1$res$visregRes, 0.9975, na.rm = TRUE)

f5t_types1$res <- f5t_types1$res[f5t_types1$res$visregRes >= lower_bound & f5t_types1$res$visregRes <= upper_bound, ]
f5t_typesplot1<- ggplot(data=f5t_types1$res, aes(x=Sand, y=visregRes))+ 
  geom_pointdensity(size = 1.5) +
  scale_color_viridis(option = 'inferno', direction = -1, breaks = c(100, 500, 1000,4000,7000,10000,13000), begin = 0.25,limits=c(floor(0), ceiling(13000)))+
  geom_ribbon(data=f5t_types1$fit,aes(x=Sand, y=visregFit,ymin = visregLwr, ymax = visregUpr), alpha = 0.5)+
  geom_smooth(data=f5t_types1$fit, aes(x=Sand, y=visregFit),col="dodgerblue3",method='lm',se=TRUE,size=2)+
  scale_x_continuous(limits=c(0,105))+
  xlab(expression('Sand (% of weight)')) +
  theme_bw() +theme( legend.position = "none",axis.title.x=element_text(size=20),axis.title.y=element_blank(),
                     axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=50, y=0.9, label="(d)",size=10)
f5t_typesplot1

f7t_types1<-visreg(model_wood_height_age1_trait,"lnAI",ylab="ln(Root:Shoot)",line=c(col="darkred"))
lower_bound <- quantile(f7t_types1$res$visregRes, 0.0025, na.rm = TRUE)
upper_bound <- quantile(f7t_types1$res$visregRes, 0.9975, na.rm = TRUE)

f7t_types1$res <- f7t_types1$res[f7t_types1$res$visregRes >= lower_bound & f7t_types1$res$visregRes <= upper_bound, ]

f7t_typesplot1<-ggplot(data=f7t_types1$res, aes(x=lnAI, y=visregRes))+
  geom_pointdensity(size = 1.5) +
  scale_color_viridis(option = 'inferno', direction = -1, breaks = c(100, 500, 1000,4000,7000,10000,13000), begin = 0.25,limits=c(floor(0), ceiling(13000)))+
  geom_ribbon(data=f7t_types1$fit,aes(x=lnAI, y=visregFit,ymin = visregLwr, ymax = visregUpr), alpha = 0.5)+
  geom_smooth(data=f7t_types1$fit, aes(x=lnAI, y=visregFit),col="dodgerblue3",method='lm',se=TRUE,size=2)+
  xlab(expression('lnAI (unitless)')) +
  theme_bw() +theme( legend.position = "none",axis.title.x=element_text(size=20),axis.title.y=element_blank(),
                     axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=1, y=0.9, label="(e)",size=10)
f7t_typesplot1


test1<-visreg(model_wood_height_age1_trait,"tmean",ylab="ln(Root:Shoot)",by="lnAI",layout=c(3,1))
lower_bound <- quantile(test1$res$visregRes, 0.0025, na.rm = TRUE)
upper_bound <- quantile(test1$res$visregRes, 0.9975, na.rm = TRUE)

test1$res <- test1$res[test1$res$visregRes >= lower_bound & test1$res$visregRes <= upper_bound, ]



test1plot<-ggplot(data=test1$res, aes(x=tmean, y=visregRes))+
  geom_pointdensity(size = 1.5) +
  facet_wrap(~lnAI,labeller = labeller(lnAI = 
                                         c("-0.0546680809027556" = "ln AI: -0.05",
                                           "0.456913802472033" = "ln AI: 0.46",
                                           "1.26699553988706" = "ln AI: 1.27")))+
  scale_color_viridis(option = 'inferno', direction = -1, breaks = c(100, 500, 1000,4000,7000,10000,13000), begin = 0.25,limits=c(floor(0), ceiling(13000)))+
  geom_ribbon(data=test1$fit,aes(x=tmean, y=visregFit,ymin = visregLwr, ymax = visregUpr), alpha = 0.5)+
  geom_smooth(data=test1$fit, aes(x=tmean, y=visregFit),col="dodgerblue3",method='lm',se=TRUE,size=2)+
  xlab(expression('Tg'*" ("*paste(degree,C)*")")) +
  theme_bw() +theme( legend.position = "none",axis.title.x=element_text(size=20),axis.title.y=element_blank(),
                     axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"),
                     strip.text = element_text(size = 12)) #+
#annotate("text", x=30, y=3.8, label="(a)",size=5)
test1plot

trait1_woody<-visreg(model_wood_height_age1_trait,"lnheightT",ylab="ln(Root:Shoot)",line=c(col="darkred"))
lower_bound <- quantile(trait1_woody$res$visregRes, 0.0025, na.rm = TRUE)
upper_bound <- quantile(trait1_woody$res$visregRes, 0.9975, na.rm = TRUE)

trait1_woody$res <- trait1_woody$res[trait1_woody$res$visregRes >= lower_bound & trait1_woody$res$visregRes <= upper_bound, ]
trait1_woodyp1<-ggplot(data=trait1_woody$res, aes(x=lnheightT, y=visregRes))+
  geom_pointdensity(size = 1.5) +
  scale_color_viridis(option = 'inferno', direction = -1, breaks = c(100, 500, 1000,4000,7000,10000,13000), begin = 0.25,limits=c(floor(0), ceiling(13000)))+
  geom_ribbon(data=trait1_woody$fit,aes(x=lnheightT, y=visregFit,ymin = visregLwr, ymax = visregUpr), alpha = 0.5)+
  geom_smooth(data=trait1_woody$fit, aes(x=lnheightT, y=visregFit),col="dodgerblue3",method='lm',se=TRUE,size=2)+
  xlab(expression('lnH (m)')) +
  theme_bw() +theme( legend.position = "none",axis.title.x=element_text(size=20),axis.title.y=element_blank(),
                     axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=1, y=0.9, label="(f)",size=10)
trait1_woodyp1

trait2_woody<-visreg(model_wood_height_age1_trait,"lnRRD",ylab="ln(Root:Shoot)",line=c(col="darkred"))
lower_bound <- quantile(trait2_woody$res$visregRes, 0.0025, na.rm = TRUE)
upper_bound <- quantile(trait2_woody$res$visregRes, 0.9975, na.rm = TRUE)

trait2_woody$res <- trait2_woody$res[trait2_woody$res$visregRes >= lower_bound & trait2_woody$res$visregRes <= upper_bound, ]

trait2_woodyp1<-ggplot(data=trait2_woody$res, aes(x=lnRRD, y=visregRes))+
  geom_pointdensity(size = 1.5) +
  scale_color_viridis(option = 'inferno', direction = -1, breaks = c(100, 500, 1000,4000,7000,10000,13000), begin = 0.25,limits=c(floor(0), ceiling(13000)))+
  geom_ribbon(data=trait2_woody$fit,aes(x=lnRRD, y=visregFit,ymin = visregLwr, ymax = visregUpr), alpha = 0.5)+
  geom_smooth(data=trait2_woody$fit, aes(x=lnRRD, y=visregFit),col="dodgerblue3",method='lm',se=TRUE,size=2)+
  xlab(expression('lnRRD (m)')) +
  theme_bw() +theme( legend.position = "none",axis.title.x=element_text(size=20),axis.title.y=element_blank(),
                     axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=-0.48, y=0.9, label="(g)",size=10)
trait2_woodyp1

trait3_woody<-visreg(model_wood_height_age1_trait,"lnSLA",ylab="ln(Root:Shoot)",line=c(col="darkred"))
lower_bound <- quantile(trait3_woody$res$visregRes, 0.0025, na.rm = TRUE)
upper_bound <- quantile(trait3_woody$res$visregRes, 0.9975, na.rm = TRUE)

trait3_woody$res <- trait3_woody$res[trait3_woody$res$visregRes >= lower_bound & trait3_woody$res$visregRes <= upper_bound, ]
trait3_woodyp1<-ggplot(data=trait3_woody$res, aes(x=lnSLA, y=visregRes))+
  geom_pointdensity(size = 1.5) +
  scale_color_viridis(option = 'inferno', direction = -1, breaks = c(100, 500, 1000,4000,7000,10000,13000), begin = 0.25,limits=c(floor(0), ceiling(13000)))+
  geom_ribbon(data=trait3_woody$fit,aes(x=lnSLA, y=visregFit,ymin = visregLwr, ymax = visregUpr), alpha = 0.5)+
  geom_smooth(data=trait3_woody$fit, aes(x=lnSLA, y=visregFit),col="dodgerblue3",method='lm',se=TRUE,size=2)+
  xlab(expression('lnSLA (m'^2~"kg"^-1*")")) +
  theme_bw() +theme( legend.position = "none",axis.title.x=element_text(size=20),axis.title.y=element_blank(),
                     axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=2.5, y=0.9, label="(h)",size=10)
trait3_woodyp1

trait4_woody<-visreg(model_wood_height_age1_trait,"lnthickness",ylab="ln(Root:Shoot)",line=c(col="darkred"))
lower_bound <- quantile(trait4_woody$res$visregRes, 0.0025, na.rm = TRUE)
upper_bound <- quantile(trait4_woody$res$visregRes, 0.9975, na.rm = TRUE)

trait4_woody$res <- trait4_woody$res[trait4_woody$res$visregRes >= lower_bound & trait4_woody$res$visregRes <= upper_bound, ]

trait4_woodyp1<-ggplot(data=trait4_woody$res, aes(x=lnthickness, y=visregRes))+
  geom_pointdensity(size = 1.5) +
  scale_color_viridis(option = 'inferno', direction = -1, breaks = c(100, 500, 1000,4000,7000,10000,13000), begin = 0.25,limits=c(floor(0), ceiling(13000)))+
  geom_ribbon(data=trait4_woody$fit,aes(x=lnthickness, y=visregFit,ymin = visregLwr, ymax = visregUpr), alpha = 0.5)+
  geom_smooth(data=trait4_woody$fit, aes(x=lnthickness, y=visregFit),col="dodgerblue3",method='lm',se=TRUE,size=2)+
  scale_x_continuous(limits=c(-2.2,-0.8))+
  xlab(expression('lnLT (mm)')) +
  theme_bw() +theme( legend.position = "none",axis.title.x=element_text(size=20),axis.title.y=element_blank(),
                     axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=-1.5, y=0.9, label="(i)",size=10)
trait4_woodyp1
trait5_woody<-visreg(model_wood_height_age1_trait,"lnLDMC",ylab="ln(Root:Shoot)",line=c(col="darkred"))
lower_bound <- quantile(trait5_woody$res$visregRes, 0.0025, na.rm = TRUE)
upper_bound <- quantile(trait5_woody$res$visregRes, 0.9975, na.rm = TRUE)

trait5_woody$res <- trait5_woody$res[trait5_woody$res$visregRes >= lower_bound & trait5_woody$res$visregRes <= upper_bound, ]

trait5_woodyp1<-ggplot(data=trait5_woody$res, aes(x=lnLDMC, y=visregRes))+
  geom_pointdensity(size = 1.5) +
  scale_color_viridis(option = 'inferno', direction = -1, breaks = c(100, 500, 1000,4000,7000,10000,13000), begin = 0.25,limits=c(floor(0), ceiling(13000)))+
  geom_ribbon(data=trait5_woody$fit,aes(x=lnLDMC, y=visregFit,ymin = visregLwr, ymax = visregUpr), alpha = 0.5)+
  geom_smooth(data=trait5_woody$fit, aes(x=lnLDMC, y=visregFit),col="dodgerblue3",method='lm',se=TRUE,size=2)+
  xlab(expression('lnLDMC (g'~"g"^-1*")")) +
  theme_bw() +theme( legend.position = "none",axis.title.x=element_text(size=20),axis.title.y=element_blank(),legend.text = element_text(angle = 30, size = 12),    legend.key.size = unit(1.5, "cm"),        # Increase legend key size
                     legend.key.height = unit(0.8, "cm"),  # Make the legend longer
                     legend.key.width = unit(3, "cm"),
                     legend.title=element_blank(),
                     axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=-2.25, y=0.9, label="(j)",size=10)
trait5_woodyp1
plot_grid(
  test1plot, f2t_typesplot1, f3t_typesplot1, f5t_typesplot1, f7t_typesplot1,
  trait1_woodyp1,trait2_woodyp1,trait3_woodyp1,trait4_woodyp1,trait5_woodyp1,
  ncol = 5, nrow = 2
)
##########################################herb
f1t_herba1<-visreg(model_herbe_height_age1_trait,"tmean",ylab="ln(Root:Shoot)",line=c(col="darkred"))
lower_bound <- quantile(f1t_herba1$res$visregRes, 0.0025, na.rm = TRUE)
upper_bound <- quantile(f1t_herba1$res$visregRes, 0.9975, na.rm = TRUE)

f1t_herba1$res <- f1t_herba1$res[f1t_herba1$res$visregRes >= lower_bound & f1t_herba1$res$visregRes <= upper_bound, ]

f1t_herbaplot1<-ggplot(data=f1t_herba1$res, aes(x=tmean, y=visregRes))+
  geom_pointdensity(size = 1.5) +
  scale_color_viridis(option = 'inferno', direction = -1, breaks = c(100, 200,400,600,800,1000,1200), begin = 0.25,limits=c(floor(0), ceiling(1200)))+
  geom_ribbon(data=f1t_herba1$fit,aes(x=tmean, y=visregFit,ymin = visregLwr, ymax = visregUpr), alpha = 0.5)+
  geom_smooth(data=f1t_herba1$fit, aes(x=tmean, y=visregFit),col="dodgerblue3",method='lm',se=TRUE,size=2)+
  xlab(expression('Tg'*" ("*paste(degree,C)*")")) +
  theme_bw() +theme( legend.position = "none",axis.title.x=element_text(size=20),axis.title.y=element_blank(),
                     axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=15, y=3, label="(a)",size=10)
f1t_herbaplot1


f4t_herba1<-visreg(model_herbe_height_age1_trait,"pH",ylab="ln(Root:Shoot)",line=c(col="deeppink"))
lower_bound <- quantile(f4t_herba1$res$visregRes, 0.0025, na.rm = TRUE)
upper_bound <- quantile(f4t_herba1$res$visregRes, 0.9975, na.rm = TRUE)

f4t_herba1$res <- f4t_herba1$res[f4t_herba1$res$visregRes >= lower_bound & f4t_herba1$res$visregRes <= upper_bound, ]
f4t_herbaplot1<- ggplot(data=f4t_herba1$res, aes(x=pH, y=visregRes))+ 
  geom_pointdensity(size = 1.5) +
  scale_color_viridis(option = 'inferno', direction = -1, breaks = c(100, 200,400,600,800,1000,1200), begin = 0.25,limits=c(floor(0), ceiling(1200)))+
  geom_ribbon(data=f4t_herba1$fit,aes(x=pH, y=visregFit,ymin = visregLwr, ymax = visregUpr), alpha = 0.5)+
  geom_smooth(data=f4t_herba1$fit, aes(x=pH, y=visregFit),col="dodgerblue3",method='lm',se=TRUE,size=2)+
  xlab(expression('Soil pH')) +
  theme_bw() +theme( legend.position = "none",axis.title.x=element_text(size=20),axis.title.y=element_blank(),
                     axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=6.2, y=3, label="(b)",size=10)
f4t_herbaplot1
f3t_herba1<-visreg(model_herbe_height_age1_trait,"lnGPP",ylab="ln(Root:Shoot)",line=c(col="deeppink"))
lower_bound <- quantile(f3t_herba1$res$visregRes, 0.0025, na.rm = TRUE)
upper_bound <- quantile(f3t_herba1$res$visregRes, 0.9975, na.rm = TRUE)

f3t_herba1$res <- f3t_herba1$res[f3t_herba1$res$visregRes >= lower_bound & f3t_herba1$res$visregRes <= upper_bound, ]
f3t_herbaplot1<- ggplot(data=f3t_herba1$res, aes(x=lnGPP, y=visregRes))+ 
  geom_pointdensity(size = 1.5) +
  scale_x_continuous(limits = c(-1.5, 1.5), breaks = seq(-1.5, 1.5, by = 0.6))+
  scale_color_viridis(option = 'inferno', direction = -1, breaks = c(100, 200,400,600,800,1000,1200), begin = 0.25,limits=c(floor(0), ceiling(1200)))+
  geom_ribbon(data=f3t_herba1$fit,aes(x=lnGPP, y=visregFit,ymin = visregLwr, ymax = visregUpr), alpha = 0.5)+
  geom_smooth(data=f3t_herba1$fit, aes(x=lnGPP, y=visregFit),col="dodgerblue3",method='lm',se=TRUE,size=2)+
  xlab(expression('lnGPP (gC'*~'m'^-2*~'d'^-1*')')) +
  theme_bw() +theme( legend.position = "none",axis.title.x=element_text(size=20),axis.title.y=element_blank(),
                     axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=0.2, y=3, label="(c)",size=10)
f3t_herbaplot1
f5t_herba1<-visreg(model_herbe_height_age1_trait,"Sand",ylab="ln(Root:Shoot)",line=c(col="deeppink"))
lower_bound <- quantile(f5t_herba1$res$visregRes, 0.0025, na.rm = TRUE)
upper_bound <- quantile(f5t_herba1$res$visregRes, 0.9975, na.rm = TRUE)

f5t_herba1$res <- f5t_herba1$res[f5t_herba1$res$visregRes >= lower_bound & f5t_herba1$res$visregRes <= upper_bound, ]
f5t_herbaplot1<- ggplot(data=f5t_herba1$res, aes(x=Sand, y=visregRes))+ 
  geom_pointdensity(size = 1.5) +
  scale_color_viridis(option = 'inferno', direction = -1, breaks = c(100, 200,400,600,800,1000,1200), begin = 0.25,limits=c(floor(0), ceiling(1200)))+
  geom_ribbon(data=f5t_herba1$fit,aes(x=Sand, y=visregFit,ymin = visregLwr, ymax = visregUpr), alpha = 0.5)+
  geom_smooth(data=f5t_herba1$fit, aes(x=Sand, y=visregFit),col="dodgerblue3",method='lm',se=TRUE,size=2)+
  xlab(expression('Sand (% of weight)')) +
  theme_bw() +theme( legend.position = "none",axis.title.x=element_text(size=20),axis.title.y=element_blank(),
                     axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=50, y=3, label="(d)",size=10)
f5t_herbaplot1

f7t_herba1<-visreg(model_herbe_height_age1_trait,"lnAI",ylab="ln(Root:Shoot)",line=c(col="darkred"))
lower_bound <- quantile(f7t_herba1$res$visregRes, 0.0025, na.rm = TRUE)
upper_bound <- quantile(f7t_herba1$res$visregRes, 0.9975, na.rm = TRUE)

f7t_herba1$res <- f7t_herba1$res[f7t_herba1$res$visregRes >= lower_bound & f7t_herba1$res$visregRes <= upper_bound, ]

f7t_herbaplot1<-ggplot(data=f7t_herba1$res, aes(x=lnAI, y=visregRes))+
  geom_pointdensity(size = 1.5) +
  scale_color_viridis(option = 'inferno', direction = -1, breaks = c(100, 200,400,600,800,1000,1200), begin = 0.25,limits=c(floor(0), ceiling(1200)))+
  geom_ribbon(data=f7t_herba1$fit,aes(x=lnAI, y=visregFit,ymin = visregLwr, ymax = visregUpr), alpha = 0.5)+
  geom_smooth(data=f7t_herba1$fit, aes(x=lnAI, y=visregFit),col="dodgerblue3",method='lm',se=TRUE,size=2)+
  xlab(expression('lnAI (unitless)')) +
  theme_bw() +theme( legend.position = "none",axis.title.x=element_text(size=20),axis.title.y=element_blank(),
                     axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=1, y=3, label="(e)",size=10)
f7t_herbaplot1



trait1_herba<-visreg(model_herbe_height_age1_trait,"lnheightT",ylab="ln(Root:Shoot)",line=c(col="darkred"))
lower_bound <- quantile(trait1_herba$res$visregRes, 0.0025, na.rm = TRUE)
upper_bound <- quantile(trait1_herba$res$visregRes, 0.9975, na.rm = TRUE)

trait1_herba$res <- trait1_herba$res[trait1_herba$res$visregRes >= lower_bound & trait1_herba$res$visregRes <= upper_bound, ]
trait1_herbap1<-ggplot(data=trait1_herba$res, aes(x=lnheightT, y=visregRes))+
  geom_pointdensity(size = 1.5) +
  scale_color_viridis(option = 'inferno', direction = -1, breaks = c(100, 200,400,600,800,1000,1200), begin = 0.25,limits=c(floor(0), ceiling(1200)))+
  geom_ribbon(data=trait1_herba$fit,aes(x=lnheightT, y=visregFit,ymin = visregLwr, ymax = visregUpr), alpha = 0.5)+
  geom_smooth(data=trait1_herba$fit, aes(x=lnheightT, y=visregFit),col="dodgerblue3",method='lm',se=TRUE,size=2)+
  xlab(expression('lnH (m)')) +
  theme_bw() +theme( legend.position = "none",axis.title.x=element_text(size=20),axis.title.y=element_blank(),
                     axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=-1, y=3, label="(f)",size=10)
trait1_herbap1

trait2_herba<-visreg(model_herbe_height_age1_trait,"lnRRD",ylab="ln(Root:Shoot)",line=c(col="darkred"))
lower_bound <- quantile(trait2_herba$res$visregRes, 0.0025, na.rm = TRUE)
upper_bound <- quantile(trait2_herba$res$visregRes, 0.9975, na.rm = TRUE)

trait2_herba$res <- trait2_herba$res[trait2_herba$res$visregRes >= lower_bound & trait2_herba$res$visregRes <= upper_bound, ]
trait2_herbap1<-ggplot(data=trait2_herba$res, aes(x=lnRRD, y=visregRes))+
  geom_pointdensity(size = 1.5) +
  scale_color_viridis(option = 'inferno', direction = -1, breaks = c(100, 500, 1000,4000,7000,10000,13000), begin = 0.25,limits=c(floor(0), ceiling(13000)))+
  geom_ribbon(data=trait2_herba$fit,aes(x=lnRRD, y=visregFit,ymin = visregLwr, ymax = visregUpr), alpha = 0.5)+
  geom_smooth(data=trait2_herba$fit, aes(x=lnRRD, y=visregFit),col="dodgerblue3",method='lm',se=TRUE,size=2)+
  xlab(expression('lnRRD (m)')) +
  theme_bw() +theme( legend.position = "none",axis.title.x=element_text(size=20),axis.title.y=element_blank(),
                     axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=-0.8, y=3, label="(g)",size=10)
trait2_herbap1

trait3_herba<-visreg(model_herbe_height_age1_trait,"lnSRL",ylab="ln(Root:Shoot)",line=c(col="darkred"))
lower_bound <- quantile(trait3_herba$res$visregRes, 0.0025, na.rm = TRUE)
upper_bound <- quantile(trait3_herba$res$visregRes, 0.9975, na.rm = TRUE)

trait3_herba$res <- trait3_herba$res[trait3_herba$res$visregRes >= lower_bound & trait3_herba$res$visregRes <= upper_bound, ]
trait3_herbap1<-ggplot(data=trait3_herba$res, aes(x=lnSRL, y=visregRes))+
  geom_pointdensity(size = 1.5) +
  scale_color_viridis(option = 'inferno', direction = -1, breaks = c(100, 200,400,600,800,1000,1200), begin = 0.25,limits=c(floor(0), ceiling(1200)))+
  geom_ribbon(data=trait3_herba$fit,aes(x=lnSRL, y=visregFit,ymin = visregLwr, ymax = visregUpr), alpha = 0.5)+
  geom_smooth(data=trait3_herba$fit, aes(x=lnSRL, y=visregFit),col="dodgerblue3",method='lm',se=TRUE,size=2)+
  xlab(expression('lnSRL (cm g)')) +
  theme_bw() +theme( legend.position = "none",axis.title.x=element_text(size=20),axis.title.y=element_blank(),
                     axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=7.5, y=3, label="(h)",size=10)
trait3_herbap1

trait4_herba<-visreg(model_herbe_height_age1_trait,"lnthickness",ylab="ln(Root:Shoot)",line=c(col="darkred"))
lower_bound <- quantile(trait4_herba$res$visregRes, 0.0025, na.rm = TRUE)
upper_bound <- quantile(trait4_herba$res$visregRes, 0.9975, na.rm = TRUE)

trait4_herba$res <- trait4_herba$res[trait4_herba$res$visregRes >= lower_bound & trait4_herba$res$visregRes <= upper_bound, ]
trait4_herbap1<-ggplot(data=trait4_herba$res, aes(x=lnthickness, y=visregRes))+
  geom_pointdensity(size = 1.5) +
  scale_color_viridis(option = 'inferno', direction = -1, breaks = c(100, 500, 1000,4000,7000,10000,13000), begin = 0.25,limits=c(floor(0), ceiling(13000)))+
  geom_ribbon(data=trait4_herba$fit,aes(x=lnthickness, y=visregFit,ymin = visregLwr, ymax = visregUpr), alpha = 0.5)+
  geom_smooth(data=trait4_herba$fit, aes(x=lnthickness, y=visregFit),col="dodgerblue3",method='lm',se=TRUE,size=2)+
  xlab(expression('lnLT (mm)')) +
  theme_bw() +theme( legend.position = "none",axis.title.x=element_text(size=20),axis.title.y=element_blank(),
                     axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=-1.4, y=3, label="(i)",size=10)
trait4_herbap1
trait5_herba<-visreg(model_herbe_height_age1_trait,"lnLDMC",ylab="ln(Root:Shoot)",line=c(col="darkred"))
lower_bound <- quantile(trait5_herba$res$visregRes, 0.0025, na.rm = TRUE)
upper_bound <- quantile(trait5_herba$res$visregRes, 0.9975, na.rm = TRUE)

trait5_herba$res <- trait5_herba$res[trait5_herba$res$visregRes >= lower_bound & trait5_herba$res$visregRes <= upper_bound, ]
trait5_herbap1<-ggplot(data=trait5_herba$res, aes(x=lnLDMC, y=visregRes))+
  geom_pointdensity(size = 1.5) +
  scale_color_viridis(option = 'inferno', direction = -1, breaks = c(100, 200,400,600,800,1000,1200), begin = 0.25,limits=c(floor(0), ceiling(1200)))+
  geom_ribbon(data=trait5_herba$fit,aes(x=lnLDMC, y=visregFit,ymin = visregLwr, ymax = visregUpr), alpha = 0.5)+
  geom_smooth(data=trait5_herba$fit, aes(x=lnLDMC, y=visregFit),col="dodgerblue3",method='lm',se=TRUE,size=2)+
  scale_x_continuous(
    limits = c(-2.30, -2.18), 
    breaks = seq(-2.30, -2.18, by = 0.05),
    labels = function(x) sprintf("%.2f", x)  # Format labels to 2 decimal places
  )+
  xlab(expression('lnLDMC (g'~"g"^-1*")")) +
  theme_bw() +theme( legend.position = "none",axis.title.x=element_text(size=20),axis.title.y=element_blank(),legend.text = element_text(angle = 30, size = 12),    legend.key.size = unit(1.5, "cm"),        # Increase legend key size
                     legend.key.height = unit(0.8, "cm"),  # Make the legend longer
                     legend.key.width = unit(3, "cm"),
                     legend.title=element_blank(),
                     axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=-2.235, y=3, label="(j)",size=10)
trait5_herbap1
plot_grid(
  f1t_herbaplot1, f4t_herbaplot1,f3t_herbaplot1, f5t_herbaplot1, f7t_herbaplot1,
  trait1_herbap1,trait2_herbap1,trait3_herbap1,trait4_herbap1,trait5_herbap1,
  ncol = 5, nrow = 2
)
#############################################################################
f2t_tree1<-visreg(model_forest2,"lnrzwc",ylab="ln(Root:Shoot)",line=c(col="deeppink"))
lower_bound <- quantile(f2t_tree1$res$visregRes, 0.0025, na.rm = TRUE)
upper_bound <- quantile(f2t_tree1$res$visregRes, 0.9975, na.rm = TRUE)

f2t_tree1$res <- f2t_tree1$res[f2t_tree1$res$visregRes >= lower_bound & f2t_tree1$res$visregRes <= upper_bound, ]
f2t_treeplot1<- ggplot(data=f2t_tree1$res, aes(x=lnrzwc, y=visregRes))+
  geom_pointdensity(size = 1.5) +
  scale_color_viridis(option = 'inferno', direction = -1, breaks = c(100, 500, 1000,4000,7000,10000,13000), begin = 0.25,limits=c(floor(0), ceiling(13000)))+
  geom_ribbon(data=f2t_tree1$fit,aes(x=lnrzwc, y=visregFit,ymin = visregLwr, ymax = visregUpr), alpha = 0.5)+
  geom_smooth(data=f2t_tree1$fit, aes(x=lnrzwc, y=visregFit),col="dodgerblue3",method='lm',se=TRUE,size=2)+
  xlab(expression('lnRZ'[wc] *~'(mm)')) +
  theme_bw() +theme( legend.position = "none",axis.title.x=element_text(size=20),axis.title.y=element_blank(),
                     axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=1.5, y=-2.7, label="(b)",size=10)
f2t_treeplot1

f3t_tree1<-visreg(model_forest2,"lnGPP",ylab="ln(Root:Shoot)",line=c(col="deeppink"))
lower_bound <- quantile(f3t_tree1$res$visregRes, 0.0025, na.rm = TRUE)
upper_bound <- quantile(f3t_tree1$res$visregRes, 0.9975, na.rm = TRUE)

f3t_tree1$res <- f3t_tree1$res[f3t_tree1$res$visregRes >= lower_bound & f3t_tree1$res$visregRes <= upper_bound, ]
f3t_treeplot1<- ggplot(data=f3t_tree1$res, aes(x=lnGPP, y=visregRes))+ 
  geom_pointdensity(size = 1.5) +
  scale_color_viridis(option = 'inferno', direction = -1, breaks = c(100, 500, 1000,4000,7000,10000,13000), begin = 0.25,limits=c(floor(0), ceiling(13000)))+
  geom_ribbon(data=f3t_tree1$fit,aes(x=lnGPP, y=visregFit,ymin = visregLwr, ymax = visregUpr), alpha = 0.5)+
  geom_smooth(data=f3t_tree1$fit, aes(x=lnGPP, y=visregFit),col="dodgerblue3",method='lm',se=TRUE,size=2)+
  xlab(expression('lnGPP (gC'*~'m'^-2*~'d'^-1*')')) +
  theme_bw() +theme( legend.position = "none",axis.title.x=element_text(size=20),axis.title.y=element_blank(),
                     axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=-0.8, y=-2.7, label="(c)",size=10)
f3t_treeplot1

f4t_tree1<-visreg(model_forest2,"pH",ylab="ln(Root:Shoot)",line=c(col="deeppink"))
lower_bound <- quantile(f4t_tree1$res$visregRes, 0.0025, na.rm = TRUE)
upper_bound <- quantile(f4t_tree1$res$visregRes, 0.9975, na.rm = TRUE)

f4t_tree1$res <- f4t_tree1$res[f4t_tree1$res$visregRes >= lower_bound & f4t_tree1$res$visregRes <= upper_bound, ]
f4t_treeplot1<- ggplot(data=f4t_tree1$res, aes(x=pH, y=visregRes))+ 
  geom_pointdensity(size = 1.5) +
  scale_color_viridis(option = 'inferno', direction = -1, breaks = c(100, 500, 1000,4000,7000,10000,13000), begin = 0.25,limits=c(floor(0), ceiling(13000)))+
  geom_ribbon(data=f4t_tree1$fit,aes(x=pH, y=visregFit,ymin = visregLwr, ymax = visregUpr), alpha = 0.5)+
  geom_smooth(data=f4t_tree1$fit, aes(x=pH, y=visregFit),col="dodgerblue3",method='lm',se=TRUE,size=2)+
  xlab(expression('Soil pH')) +
  theme_bw() +theme( legend.position = "none",axis.title.x=element_text(size=20),axis.title.y=element_blank(),
                     axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=3.5, y=-2.7, label="(d)",size=10)
f4t_treeplot1

f5t_tree1<-visreg(model_forest2,"Sand",ylab="ln(Root:Shoot)",line=c(col="deeppink"))
lower_bound <- quantile(f5t_tree1$res$visregRes, 0.0025, na.rm = TRUE)
upper_bound <- quantile(f5t_tree1$res$visregRes, 0.9975, na.rm = TRUE)

f5t_tree1$res <- f5t_tree1$res[f5t_tree1$res$visregRes >= lower_bound & f5t_tree1$res$visregRes <= upper_bound, ]

f5t_treeplot1<- ggplot(data=f5t_tree1$res, aes(x=Sand, y=visregRes))+ 
  geom_pointdensity(size = 1.5) +
  scale_color_viridis(option = 'inferno', direction = -1, breaks = c(100, 500, 1000,4000,7000,10000,13000), begin = 0.25,limits=c(floor(0), ceiling(13000)))+
  geom_ribbon(data=f5t_tree1$fit,aes(x=Sand, y=visregFit,ymin = visregLwr, ymax = visregUpr), alpha = 0.5)+
  geom_smooth(data=f5t_tree1$fit, aes(x=Sand, y=visregFit),col="dodgerblue3",method='lm',se=TRUE,size=2)+
  scale_x_continuous(limits=c(0,110))+
  xlab(expression('Sand (% of weight)')) +
  theme_bw() +theme( legend.position = "none",axis.title.x=element_text(size=20),axis.title.y=element_blank(),
                     axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=10, y=-2.7, label="(e)",size=10)
f5t_treeplot1

f7t_tree1<-visreg(model_forest2,"lnAI",ylab="ln(Root:Shoot)",line=c(col="darkred"))
lower_bound <- quantile(f7t_tree1$res$visregRes, 0.0025, na.rm = TRUE)
upper_bound <- quantile(f7t_tree1$res$visregRes, 0.9975, na.rm = TRUE)

f7t_tree1$res <- f7t_tree1$res[f7t_tree1$res$visregRes >= lower_bound & f7t_tree1$res$visregRes <= upper_bound, ]

f7t_treeplot1<-ggplot(data=f7t_tree1$res, aes(x=lnAI, y=visregRes))+
  geom_pointdensity(size = 1.5) +
  scale_color_viridis(option = 'inferno', direction = -1, breaks = c(100, 500, 1000,4000,7000,10000,13000), begin = 0.25,limits=c(floor(0), ceiling(13000)))+
  geom_ribbon(data=f7t_tree1$fit,aes(x=lnAI, y=visregFit,ymin = visregLwr, ymax = visregUpr), alpha = 0.5)+
  geom_smooth(data=f7t_tree1$fit, aes(x=lnAI, y=visregFit),col="dodgerblue3",method='lm',se=TRUE,size=2)+
  xlab(expression('lnAI (unitless)')) +
  theme_bw() +theme( legend.position = "none",axis.title.x=element_text(size=20),axis.title.y=element_blank(),
                     axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=-1.2, y=-2.7, label="(f)",size=10)
f7t_treeplot1


tree1<-visreg(model_forest2,"tmean",ylab="ln(Root:Shoot)",by="lnAI",layout=c(3,1))
lower_bound <- quantile(tree1$res$visregRes, 0.0025, na.rm = TRUE)
upper_bound <- quantile(tree1$res$visregRes, 0.9975, na.rm = TRUE)

tree1$res <- tree1$res[tree1$res$visregRes >= lower_bound & tree1$res$visregRes <= upper_bound, ]
tree1plot<-ggplot(data=tree1$res, aes(x=tmean, y=visregRes))+
  geom_pointdensity(size = 1.5) +
  facet_wrap(~lnAI,labeller = labeller(lnAI = 
                                         c("-0.083160239889446" = "ln AI: -0.08",
                                           "0.394887156701838" = "ln AI: 0.39",
                                           "1.26729053264175" = "ln AI: 1.27")))+
  scale_color_viridis(option = 'inferno', direction = -1, breaks = c(100, 500, 1000,4000,7000,10000,13000), begin = 0.25,limits=c(floor(0), ceiling(13000)))+
  geom_ribbon(data=tree1$fit,aes(x=tmean, y=visregFit,ymin = visregLwr, ymax = visregUpr), alpha = 0.5)+
  geom_smooth(data=tree1$fit, aes(x=tmean, y=visregFit),col="dodgerblue3",method='lm',se=TRUE,size=2)+
  xlab(expression('Tg'*" ("*paste(degree,C)*")")) +
  theme_bw() +theme( legend.position = "none",axis.title.x=element_text(size=20),axis.title.y=element_blank(),
                     axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"),
                     strip.text = element_text(size = 12)) #+
#annotate("text", x=30, y=3.8, label="(a)",size=5)
tree1plot


trait3_tree<-visreg(model_forest2,"lnSRL",ylab="ln(Root:Shoot)",line=c(col="darkred"))
lower_bound <- quantile(trait3_tree$res$visregRes, 0.0025, na.rm = TRUE)
upper_bound <- quantile(trait3_tree$res$visregRes, 0.9975, na.rm = TRUE)

trait3_tree$res <- trait3_tree$res[trait3_tree$res$visregRes >= lower_bound & trait3_tree$res$visregRes <= upper_bound, ]

trait3_treep1<-ggplot(data=trait3_tree$res, aes(x=lnSRL, y=visregRes))+
  geom_pointdensity(size = 1.5) +
  scale_color_viridis(option = 'inferno', direction = -1, breaks = c(100, 500, 1000,4000,7000,10000,13000), begin = 0.25,limits=c(floor(0), ceiling(13000)))+
  geom_ribbon(data=trait3_tree$fit,aes(x=lnSRL, y=visregFit,ymin = visregLwr, ymax = visregUpr), alpha = 0.5)+
  geom_smooth(data=trait3_tree$fit, aes(x=lnSRL, y=visregFit),col="dodgerblue3",method='lm',se=TRUE,size=2)+
  xlab(expression('lnSRL (cm g)')) +
  theme_bw() +theme( legend.position = "none",axis.title.x=element_text(size=20),axis.title.y=element_blank(),
                     axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=6.5, y=-2.7, label="(g)",size=10)
trait3_treep1

trait3_tree1<-visreg(model_forest2,"lnSLA",ylab="ln(Root:Shoot)",line=c(col="darkred"))
lower_bound <- quantile(trait3_tree1$res$visregRes, 0.0025, na.rm = TRUE)
upper_bound <- quantile(trait3_tree1$res$visregRes, 0.9975, na.rm = TRUE)

trait3_tree1$res <- trait3_tree1$res[trait3_tree1$res$visregRes >= lower_bound & trait3_tree1$res$visregRes <= upper_bound, ]

trait3_treep11<-ggplot(data=trait3_tree1$res, aes(x=lnSLA, y=visregRes))+
  geom_pointdensity(size = 1.5) +
  scale_color_viridis(option = 'inferno', direction = -1, breaks = c(100, 500, 1000,4000,7000,10000,13000), begin = 0.25,limits=c(floor(0), ceiling(13000)))+
  geom_ribbon(data=trait3_tree1$fit,aes(x=lnSLA, y=visregFit,ymin = visregLwr, ymax = visregUpr), alpha = 0.5)+
  geom_smooth(data=trait3_tree1$fit, aes(x=lnSLA, y=visregFit),col="dodgerblue3",method='lm',se=TRUE,size=2)+
  xlab(expression('lnSLA (m'^2~"kg"^-1*")")) +
  theme_bw() +theme( legend.position = "none",axis.title.x=element_text(size=20),axis.title.y=element_blank(),
                     axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=2, y=-2.7, label="(h)",size=10)
trait3_treep11

trait5_tree<-visreg(model_forest2,"lnLDMC",ylab="ln(Root:Shoot)",line=c(col="darkred"))
lower_bound <- quantile(trait5_tree$res$visregRes, 0.0025, na.rm = TRUE)
upper_bound <- quantile(trait5_tree$res$visregRes, 0.9975, na.rm = TRUE)

trait5_tree$res <- trait5_tree$res[trait5_tree$res$visregRes >= lower_bound & trait5_tree$res$visregRes <= upper_bound, ]

trait5_treep1<-ggplot(data=trait5_tree$res, aes(x=lnLDMC, y=visregRes))+
  geom_pointdensity(size = 1.5) +
  scale_color_viridis(option = 'inferno', direction = -1, breaks = c(100, 500, 1000,4000,7000,10000,13000), begin = 0.25,limits=c(floor(0), ceiling(13000)))+
  geom_ribbon(data=trait5_tree$fit,aes(x=lnLDMC, y=visregFit,ymin = visregLwr, ymax = visregUpr), alpha = 0.5)+
  geom_smooth(data=trait5_tree$fit, aes(x=lnLDMC, y=visregFit),col="dodgerblue3",method='lm',se=TRUE,size=2)+
  scale_x_continuous(
    limits = c(-2.30, -2.18), 
    breaks = seq(-2.30, -2.18, by = 0.05),
    labels = function(x) sprintf("%.2f", x)  # Format labels to 2 decimal places
  )+
  xlab(expression('lnLDMC (g'~"g"^-1*")")) +
  theme_bw() +theme( legend.position = "none",axis.title.x=element_text(size=20),axis.title.y=element_blank(),legend.text = element_text(angle = 30, size = 12),    legend.key.size = unit(1.5, "cm"),        # Increase legend key size
                     legend.key.height = unit(0.8, "cm"),  # Make the legend longer
                     legend.key.width = unit(3, "cm"),
                     legend.title=element_blank(),
                     axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=-2.29, y=-2.7, label="(i)",size=10)
trait5_treep1
plot_grid(
  tree1plot, f2t_treeplot1, f3t_treeplot1,f4t_treeplot1, f5t_treeplot1, f7t_treeplot1,
  trait3_treep1,trait3_treep11,trait5_treep1,
  ncol = 3, nrow = 3
) 

################################################
f3t_shrub1<-visreg(model_shrub2,"lnGPP",ylab="ln(Root:Shoot)",line=c(col="deeppink"))
lower_bound <- quantile(f3t_shrub1$res$visregRes, 0.0025, na.rm = TRUE)
upper_bound <- quantile(f3t_shrub1$res$visregRes, 0.9975, na.rm = TRUE)

f3t_shrub1$res <- f3t_shrub1$res[f3t_shrub1$res$visregRes >= lower_bound & f3t_shrub1$res$visregRes <= upper_bound, ]

f3t_shrubplot1<- ggplot(data=f3t_shrub1$res, aes(x=lnGPP, y=visregRes))+ 
  geom_pointdensity(size = 1.5) +
  scale_color_viridis(option = 'inferno', direction = -1, breaks = c(100, 200,400,600,800,1000,1200), begin = 0.25,limits=c(floor(0), ceiling(1200)))+
  geom_ribbon(data=f3t_shrub1$fit,aes(x=lnGPP, y=visregFit,ymin = visregLwr, ymax = visregUpr), alpha = 0.5)+
  geom_smooth(data=f3t_shrub1$fit, aes(x=lnGPP, y=visregFit),col="dodgerblue3",method='lm',se=TRUE,size=2)+
  xlab(expression('lnGPP (gC'*~'m'^-2*~'d'^-1*')')) +
  theme_bw() +theme( legend.position = "none",axis.title.x=element_text(size=20),axis.title.y=element_blank(),
                     axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=0.3, y=1.2, label="(a)",size=10)
f3t_shrubplot1

f5t_shrub1<-visreg(model_shrub2,"Sand",ylab="ln(Root:Shoot)",line=c(col="deeppink"))
lower_bound <- quantile(f5t_shrub1$res$visregRes, 0.0025, na.rm = TRUE)
upper_bound <- quantile(f5t_shrub1$res$visregRes, 0.9975, na.rm = TRUE)

f5t_shrub1$res <- f5t_shrub1$res[f5t_shrub1$res$visregRes >= lower_bound & f5t_shrub1$res$visregRes <= upper_bound, ]
f5t_shrubplot1<- ggplot(data=f5t_shrub1$res, aes(x=Sand, y=visregRes))+ 
  geom_pointdensity(size = 1.5) +
  scale_color_viridis(option = 'inferno', direction = -1, breaks = c(100, 200,400,600,800,1000,1200), begin = 0.25,limits=c(floor(0), ceiling(1200)))+
  geom_ribbon(data=f5t_shrub1$fit,aes(x=Sand, y=visregFit,ymin = visregLwr, ymax = visregUpr), alpha = 0.5)+
  geom_smooth(data=f5t_shrub1$fit, aes(x=Sand, y=visregFit),col="dodgerblue3",method='lm',se=TRUE,size=2)+
  scale_x_continuous(limits=c(0,110))+
  xlab(expression('Sand (% of weight)')) +
  theme_bw() +theme( legend.position = "none",axis.title.x=element_text(size=20),axis.title.y=element_blank(),
                     axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=50, y=1.2, label="(b)",size=10)
f5t_shrubplot1


trait1_shrub<-visreg(model_shrub2,"lnheightT",ylab="ln(Root:Shoot)",line=c(col="darkred"))
lower_bound <- quantile(trait1_shrub$res$visregRes, 0.0025, na.rm = TRUE)
upper_bound <- quantile(trait1_shrub$res$visregRes, 0.9975, na.rm = TRUE)

trait1_shrub$res <- trait1_shrub$res[trait1_shrub$res$visregRes >= lower_bound & trait1_shrub$res$visregRes <= upper_bound, ]
trait1_shrubp1<-ggplot(data=trait1_shrub$res, aes(x=lnheightT, y=visregRes))+
  geom_pointdensity(size = 1.5) +
  scale_color_viridis(option = 'inferno', direction = -1, breaks = c(100, 200,400,600,800,1000,1200), begin = 0.25,limits=c(floor(0), ceiling(1200)))+
  geom_ribbon(data=trait1_shrub$fit,aes(x=lnheightT, y=visregFit,ymin = visregLwr, ymax = visregUpr), alpha = 0.5)+
  geom_smooth(data=trait1_shrub$fit, aes(x=lnheightT, y=visregFit),col="dodgerblue3",method='lm',se=TRUE,size=2)+
  xlab(expression('lnH (m)')) +
  theme_bw() +theme( legend.position = "none",axis.title.x=element_text(size=20),axis.title.y=element_blank(),
                     axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=0.6, y=1.2, label="(c)",size=10)
trait1_shrubp1

trait2_shrub<-visreg(model_shrub2,"lnRRD",ylab="ln(Root:Shoot)",line=c(col="darkred"))
lower_bound <- quantile(trait2_shrub$res$visregRes, 0.0025, na.rm = TRUE)
upper_bound <- quantile(trait2_shrub$res$visregRes, 0.9975, na.rm = TRUE)

trait2_shrub$res <- trait2_shrub$res[trait2_shrub$res$visregRes >= lower_bound & trait2_shrub$res$visregRes <= upper_bound, ]
trait2_shrubp1<-ggplot(data=trait2_shrub$res, aes(x=lnRRD, y=visregRes))+
  geom_pointdensity(size = 1.5) +
  scale_color_viridis(option = 'inferno', direction = -1, breaks = c(100, 200,400,600,800,1000,1200), begin = 0.25,limits=c(floor(0), ceiling(1200)))+
  geom_ribbon(data=trait2_shrub$fit,aes(x=lnRRD, y=visregFit,ymin = visregLwr, ymax = visregUpr), alpha = 0.5)+
  geom_smooth(data=trait2_shrub$fit, aes(x=lnRRD, y=visregFit),col="dodgerblue3",method='lm',se=TRUE,size=2)+
  xlab(expression('lnRRD (m)')) +
  theme_bw() +theme( legend.position = "none",axis.title.x=element_text(size=20),axis.title.y=element_blank(),
                     axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=-0.45, y=1.2, label="(d)",size=10)
trait2_shrubp1

trait3_shrub<-visreg(model_shrub2,"lnSRL",ylab="ln(Root:Shoot)",line=c(col="darkred"))
lower_bound <- quantile(trait3_shrub$res$visregRes, 0.0025, na.rm = TRUE)
upper_bound <- quantile(trait3_shrub$res$visregRes, 0.9975, na.rm = TRUE)

trait3_shrub$res <- trait3_shrub$res[trait3_shrub$res$visregRes >= lower_bound & trait3_shrub$res$visregRes <= upper_bound, ]
trait3_shrubp1<-ggplot(data=trait3_shrub$res, aes(x=lnSRL, y=visregRes))+
  geom_pointdensity(size = 1.5) +
  scale_color_viridis(option = 'inferno', direction = -1, breaks = c(100, 200,400,600,800,1000,1200), begin = 0.25,limits=c(floor(0), ceiling(1200)))+
  geom_ribbon(data=trait3_shrub$fit,aes(x=lnSRL, y=visregFit,ymin = visregLwr, ymax = visregUpr), alpha = 0.5)+
  geom_smooth(data=trait3_shrub$fit, aes(x=lnSRL, y=visregFit),col="dodgerblue3",method='lm',se=TRUE,size=2)+
  xlab(expression('lnSRL (cm g)')) +
  theme_bw() +theme( legend.position = "none",axis.title.x=element_text(size=20),axis.title.y=element_blank(),
                     axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=7.5, y=1.2, label="(e)",size=10)
trait3_shrubp1

trait3_shrub1<-visreg(model_shrub2,"lnSLA",ylab="ln(Root:Shoot)",line=c(col="darkred"))
lower_bound <- quantile(trait3_shrub1$res$visregRes, 0.0025, na.rm = TRUE)
upper_bound <- quantile(trait3_shrub1$res$visregRes, 0.9975, na.rm = TRUE)

trait3_shrub1$res <- trait3_shrub1$res[trait3_shrub1$res$visregRes >= lower_bound & trait3_shrub1$res$visregRes <= upper_bound, ]
trait3_shrubp11<-ggplot(data=trait3_shrub1$res, aes(x=lnSLA, y=visregRes))+
  geom_pointdensity(size = 1.5) +
  scale_color_viridis(option = 'inferno', direction = -1, breaks = c(100, 200,400,600,800,1000,1200), begin = 0.25,limits=c(floor(0), ceiling(1200)))+
  geom_ribbon(data=trait3_shrub1$fit,aes(x=lnSLA, y=visregFit,ymin = visregLwr, ymax = visregUpr), alpha = 0.5)+
  geom_smooth(data=trait3_shrub1$fit, aes(x=lnSLA, y=visregFit),col="dodgerblue3",method='lm',se=TRUE,size=2)+
  scale_x_continuous(
    limits = c(2.2, 3.2), 
    breaks = seq(2.2, 3.2, by = 0.4)  # Format labels to 2 decimal places
  )+
  xlab(expression('lnSLA (m'^2~"kg"^-1*")")) +
  theme_bw() +theme( legend.position = "none",axis.title.x=element_text(size=20),axis.title.y=element_blank(),
                     axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=2.75, y=1.2, label="(f)",size=10)
trait3_shrubp11

trait4_shrub<-visreg(model_shrub2,"lnthickness",ylab="ln(Root:Shoot)",line=c(col="darkred"))
lower_bound <- quantile(trait4_shrub$res$visregRes, 0.0025, na.rm = TRUE)
upper_bound <- quantile(trait4_shrub$res$visregRes, 0.9975, na.rm = TRUE)

trait4_shrub$res <- trait4_shrub$res[trait4_shrub$res$visregRes >= lower_bound & trait4_shrub$res$visregRes <= upper_bound, ]

trait4_shrubp1<-ggplot(data=trait4_shrub$res, aes(x=lnthickness, y=visregRes))+
  geom_pointdensity(size = 1.5) +
  scale_color_viridis(option = 'inferno', direction = -1, breaks = c(100, 200,400,600,800,1000,1200), begin = 0.25,limits=c(floor(0), ceiling(1200)))+
  geom_ribbon(data=trait4_shrub$fit,aes(x=lnthickness, y=visregFit,ymin = visregLwr, ymax = visregUpr), alpha = 0.5)+
  geom_smooth(data=trait4_shrub$fit, aes(x=lnthickness, y=visregFit),col="dodgerblue3",method='lm',se=TRUE,size=2)+
  scale_x_continuous(
    limits = c(-1.9, -1.1), 
    breaks = seq(-1.9, -1.1, by = 0.3)  # Format labels to 2 decimal places
  )+
  xlab(expression('lnLT (mm)')) +
  theme_bw() +theme( legend.position = "none",axis.title.x=element_text(size=20),axis.title.y=element_blank(),
                     axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=-1.50, y=1.2, label="(g)",size=10)
trait4_shrubp1

trait5_shrub<-visreg(model_shrub2,"lnLDMC",ylab="ln(Root:Shoot)",line=c(col="darkred"))
lower_bound <- quantile(trait5_shrub$res$visregRes, 0.0025, na.rm = TRUE)
upper_bound <- quantile(trait5_shrub$res$visregRes, 0.9975, na.rm = TRUE)

trait5_shrub$res <- trait5_shrub$res[trait5_shrub$res$visregRes >= lower_bound & trait5_shrub$res$visregRes <= upper_bound, ]
trait5_shrubp1<-ggplot(data=trait5_shrub$res, aes(x=lnLDMC, y=visregRes))+
  geom_pointdensity(size = 1.5) +
  scale_x_continuous(
    limits = c(-2.30, -2.18), 
    breaks = seq(-2.30, -2.18, by = 0.05),
    labels = function(x) sprintf("%.2f", x)  # Format labels to 2 decimal places
  )+
  scale_color_viridis(option = 'inferno', direction = -1, breaks = c(100, 200,400,600,800,1000,1200), begin = 0.25,limits=c(floor(0), ceiling(1200)))+
  geom_ribbon(data=trait5_shrub$fit,aes(x=lnLDMC, y=visregFit,ymin = visregLwr, ymax = visregUpr), alpha = 0.5)+
  geom_smooth(data=trait5_shrub$fit, aes(x=lnLDMC, y=visregFit),col="dodgerblue3",method='lm',se=TRUE,size=2)+
  xlab(expression('lnLDMC (g'~"g"^-1*")")) +
  theme_bw() +theme( legend.position = "none",axis.title.x=element_text(size=20),axis.title.y=element_blank(),legend.text = element_text(angle = 30, size = 12),    legend.key.size = unit(1.5, "cm"),        # Increase legend key size
                     legend.key.height = unit(0.8, "cm"),  # Make the legend longer
                     legend.key.width = unit(3, "cm"),
                     legend.title=element_blank(),
                     axis.text=element_text(size=rel(1.7)),panel.grid.major=element_line(color="white"),panel.grid.minor=element_line(color="white"))+
  annotate("text", x=-2.25, y=1.2, label="(h)",size=10)
trait5_shrubp1
plot_grid(
  f3t_shrubplot1, f5t_shrubplot1,trait1_shrubp1,trait2_shrubp1,
  trait3_shrubp1,trait3_shrubp11,trait4_shrubp1,trait5_shrubp1,
  ncol = 4, nrow = 2
)



