# packages needed
install.packages("Hmisc")
install.packages('pheatmap')
install.packages("wesanderson")
library(Hmisc)
library(corrplot)
library(pheatmap)
library(RColorBrewer)
library(wesanderson)
# data file preparation
cordata<-read.csv(file.choose())
cordata1<-read.csv(file.choose())
res1<-cor(cordata, method = "pearson")
res2<-cor(cordata1)
mydatap<-cor.mtest(cordata, conf.level = .95)
p.mat = mydatap$p
corrplot(res1,type = "upper",tl.pos = "tp",outline="white",method = "color",
         tl.col = "black", addCoef.col = "black",
        col = brewer.pal (n = 7, name = "RdBu"
         ))
corrplot(res1, add=TRUE, type = "lower",method = "color", tl.pos = "n",outline="white",
         tl.col = "black", tl.srt = 40,cl.pos = "n", cl.ratio = 0.2,
         p.mat=mydatap$p, sig.level = c(.001, .01, .05),
         insig = "label_sig",pch.cex = 1.2, pch.col = "white",col = brewer.pal (n = 7, name = "RdBu"
         ))


