install.packages('psych')
library(reshape2)
library(psych)
library(ggplot2)
library("FactoMineR")
library(factoextra)
PCAdata<-read.csv(file.choose())
data.pca<-PCA(PCAdata[,-1],graph=FALSE)
fviz_pca_ind(data.pca,
             geom.ind = "point", # show points only (nbut not "text")
             fill.ind = PCAdata$sample, 
             col.ind = "black",# color by groups
             pointshape = 21, 
             pointsize = 2.5,
             palette = "Set3",
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Temperature",
)

fviz_pca_biplot(data.pca, 
                col.ind = PCAdata$sample, palette = "jco", 
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Species") 

fviz_pca_biplot(data.pca, 
               # Fill individuals by groups
               geom.ind = "point",
               pointshape = 21,
               pointsize = 2.5,
               fill.ind = PCAdata$sample,
               col.ind = "black",
               palette = "Set3",
               # Color variable by groups
               col.var = "contrib",
               legend.title = list(fill = "Temperature", color = "Contrib"),
               repel = TRUE        # Avoid label overplotting
)
