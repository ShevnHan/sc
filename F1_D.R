library(Seurat)
library(DEsingle)
library(SingleCellExperiment)
sc_T = subset(scR_T,celltype=="CD8+T Cell")
pbmc_CD <- CreateSeuratObject(counts = sc_T@assays$RNA$counts, 
                              meta.data = scR_T@meta.data,
                              project = "CD8T Cell", min.cells = 3, min.features = 200)
sce <- SingleCellExperiment(assays = list(counts = as.matrix(pbmc_CD@assays$RNA$counts)))
group <- factor(c(rep("HC",1059), rep("EPL",1133)))
results <- DEsingle(counts = sce, group = group)
results$gene=rownames(results)
result=results[,c("foldChange","pvalue","gene")]
result$foldChange=log2(result$foldChange)
colnames(result)=c("log2FC","pvalue","gene")
result=result[-c(1:4),]
write.xlsx(result,file="D:/Rproject/CD8T.xlsx")
colnames(result)
result$regulated <- ifelse(result$log2FC>1&result$pvalue<0.05,                               
                           "up",ifelse(result$log2FC< -1&result$pvalue<0.05,"down","normal"))
gene=result[which(result$gene=="ITGAX"),]
DEG=subset(result,regulated!="normal")

###绘制火山图
p2 <-ggplot(result,aes(x=log2FC,y =-log10(pvalue))) +
  geom_point(aes(color=regulated),size=2) +
  scale_color_manual(values=c("#4DBBD5FF","grey","#E64B35FF"))+
  geom_text_repel(data=gene,aes(label=top20$gene),size=5,
                  box.padding=unit(0.35,"lines"),
                  arrow=arrow(angle = 45,length = unit(0.05,"inches"),ends="first",type="open"),
                  nudge_x=5,
                  nudge_y = 1.5,
                  point.padding=unit(0.3,"lines"),family="Times New Roman") +
  geom_hline(yintercept=-log10(0.05),linetype=2,show.legend=TRUE)+ 
  geom_vline(xintercept=1,linetype=3)+ 
  geom_vline(xintercept=-1,linetype=3)+ 
  labs(x="log2FoldChange",y="-log10 (pvalue)")+
  xlim(-7.5,7.5)+
  theme(legend.title = element_text(size=15,
                                    face=c("plain"),
                                    family="Times New Roman"))+
  theme(legend.text =element_text(size=10, 
                                  face=c("plain"),
                                  family="Times New Roman"
  ) );p2