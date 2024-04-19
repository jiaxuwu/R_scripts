install.packages("chromoMap")
library(chromoMap)
setwd("/Users/jiaxuwu/OneDrive - Université Laval/research/experiments/NLRome")
data1 = read.table("chromosome.txt",sep = "\t")
anno_file = read.table("annotation_westar.txt", sep = "\t")


chromoMap(c("chromosome_A.txt","chromosome_C.txt"), c("anno_chrom_A.txt","anno_chrom_C.txt"),  ploidy = 2,chr_color = c("#D7E4C0","#FFE4C9"),
          label_font = 4, label_angle = -65, labels=T, export.options = T, anno_col = c("#FF8080","#211C6A"))
