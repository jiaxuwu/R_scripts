setwd()

df <- read.table("gene_counts", header = T,
                 sep = "\t",
                 stringsAsFactors = F)
expr_df <- df[,c(1,8,9,10,11,14,15,16,17)]
colnames(expr_df) <- c("geneid",
                       "CSV_1","CSV_2","CSV_3","CSV_4",
                       "CTV_1","CTV_2","CTV_3","CTV_4")
BiocManager::install("DESeq2")
library(DESeq2)
coldata <- data.frame(condition=factor(c("CSV","CSV","CSV","CSV",
                                         "CTV","CTV","CTV","CTV")))
dds <- DESeqDataSetFromMatrix (expr_df,
                              colData = coldata,
                              design = ~ condition,
                              tidy = TRUE)
dds <- DESeq(dds)
rld <- rlog(dds)
plotPCA(rld)
res <- results(dds)
plot_df <- data.frame(res)
plot_df <- cbind(geneid=row.names(plot_df),
                 plot_df)
plot_df <- plot_df[!is.na(plot_df$padj), ]
library(ggplot2)
ggplot(data=plot_df, aes(x=log2FoldChange, 
                         y =-log10(padj))) +
  geom_point(alpha=0.8, size=1.2)+
  labs(title="Volcanoplot", x="log2 (fold change)",y="-log10 (q-value)")+
  theme(plot.title = element_text(hjust = 0.4))+
  geom_hline(yintercept = -log10(0.05),lty=4,color="red")+
  geom_hline(yintercept = -log10(0.01),lty=4,color="blue")+
  geom_vline(xintercept = c(1,-1),lty=4,alpha=0.8,color="blue")+
  geom_vline(xintercept = c(2,-2),lty=4,alpha=0.8,color="red")+
  theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))
res_sub <- subset(res, abs(log2FoldChange) > 2 & padj <0.01)
gene_up <- row.names(res_sub[res_sub$log2FoldChange > 0, ])
length(gene_up) # 1024
gene_down <- row.names(res_sub[res_sub$log2FoldChange < 0, ])
length(gene_down) # 702
install.packages("https://github.com/xuzhougeng/org.Osativa.eg.db/releases/download/v0.01/org.Osativa.eg.db.tar.gz", 
                 repos = NULL, 
                 type="source")
BiocManager::install("clusterProfiler")
library(clusterProfiler)
library(org.Osativa.eg.db)
org <- org.Osativa.eg.db
ego_up <- enrichGO(gene_up,
                   OrgDb = org,
                   keyType = "GID",
                   pAdjustMethod = "none",
                   ont="BP")
p1 <- dotplot(ego_up)
ego_down <- enrichGO(gene_down,
                     OrgDb = org,
                     keyType = "GID",
                     pAdjustMethod = "none",
                     ont="BP")
p2 <- dotplot(ego_down)
library(cowplot)
plot_grid(p1,p2)
fc <- plot_df$log2FoldChange
names(fc) <-  plot_df$geneid
heatplot(ego_down, foldChange = fc)
rap_id <- mapIds(x = org, 
                 keys = gene_up, 
                 column = "RAP",
                 "GID")
rap_id <- paste0(rap_id[!is.na(rap_id)], "-01")
rap_id <- gsub("g","t",rap_id)
ekegg <- enrichKEGG(rap_id, organism = "dosa", pAdjustMethod = "none")
rice_kegg <- clusterProfiler::download_KEGG("dosa")
kegg_df <- rice_kegg$KEGGPATHID2EXTID
kegg_df <- kegg_df[kegg_df$to %in% rap_id,]

kegg_df <- merge(kegg_df, rice_kegg$KEGGPATHID2NAME,
                 by.x="from",by.y="from")

kegg_class <- as.data.frame(sort(table(kegg_df$to.y), decreasing = T)[1:10])

colnames(kegg_class) <- c("pathway","times")
ggplot(kegg_class,aes(x=pathway, y = times)) +
  geom_bar(fill="#ca0020",stat="identity") + coord_flip() +
  theme_bw() + geom_text(aes(y = times+1, label = times))

rice_kegg <- clusterProfiler::download_KEGG("dosa")

# get up-regualted KEGG PATHWAY
up_rap_id <- mapIds(x = org, 
                    keys = gene_up, 
                    column = "RAP",
                    "GID")
up_rap_id <- paste0(up_rap_id[!is.na(up_rap_id)], "-01")

up_rap_id <- gsub("g","t",up_rap_id)

up_df <- rice_kegg$KEGGPATHID2EXTID
up_df <- kegg_df[kegg_df$to %in% up_rap_id,]
up_df <- merge(kegg_df, rice_kegg$KEGGPATHID2NAME,
               by.x="from",by.y="from")

# get down-regulated KEGG PATHWAY
down_rap_id <- mapIds(x = org, 
                      keys = gene_down, 
                      column = "RAP",
                      "GID")
down_rap_id <- paste0(down_rap_id[!is.na(down_rap_id)], "-01")
down_rap_id <- gsub("g","t",down_rap_id)
down_df <- rice_kegg$KEGGPATHID2EXTID
down_df <- down_df[down_df$to %in% down_rap_id,]
down_df <- merge(down_df, rice_kegg$KEGGPATHID2NAME,
                 by.x="from",by.y="from")

# statistics
kegg_class_up <- as.data.frame(sort(table(up_df$to.y), 
                                    decreasing = T)[1:10])
kegg_class_down <- as.data.frame(sort(table(down_df$to.y), 
                                      decreasing = T)[1:10])
# combine
kegg_class <- rbind(kegg_class_up, kegg_class_down)
colnames(kegg_class) <- c("pathway","times")
kegg_class$source <- rep(c("up","down"),
                         times=c(nrow(kegg_class_up),nrow(kegg_class_down)))
# graph
ggplot(kegg_class,aes(x=pathway, y = times)) +
  geom_bar(aes(fill=source),stat="identity",position = "dodge") + 
  scale_fill_manual(values = c(up="#ca0020",down="#2b83ba")) +
  coord_flip() +
  theme_bw()
