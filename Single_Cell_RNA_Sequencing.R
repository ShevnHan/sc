library(Seurat)#(V4)
library(harmony)
library(ggplot2)
library(scHCLlite)
library(tidyverse)
library(dplyr)
library(scCustomize)
library(clustree)
library(openxlsx)
m_hsa=read.xlsx("D:/Wechat/WeChat Files/wxid_aupnntjm8etk22/FileStorage/File/2024-02/Cell_marker_Human.xlsx")
dirs <- dir("D:/Rproject/NCPF/GSE164449_RAW")
pbmc.data_NC <- Read10X(data.dir="D:/Rproject/NCPF/GSE164449_RAW")
pbmc.data_RC <- Read10X(data.dir="D:/Rproject/RCPF/GSE164449_RAW")
scRNA_NC <- CreateSeuratObject(pbmc.data_NC,min.cell=3,min.feature=200)
scRNA_NC@meta.data$orig.ident="HC"
scRNA_RC@meta.data$orig.ident="EPL"
scRNA_RC <- CreateSeuratObject(pbmc.data_RC,min.cell=3,min.feature=200)
#scRNA_RC@project.name="RC"
scRNA=merge(scRNA_NC,scRNA_RC)
#scRNA_RC@active.assay="RC"

#####Run normalition, dimensionality reduction, FindClusters and so on
#####filter data
scRNA[["percent.mt"]] <- PercentageFeatureSet(scRNA,pattern="^MT-")
HB.genes <- c("HBA1","HBA2","HBB","HBD","HBE1","HBG1","HBG2","HBM","HBQ1","HBZ")
HB_m <- match(HB.genes, rownames(scRNA@assays$RNA)) 
HB.genes <- rownames(scRNA@assays$RNA)[HB_m] 
HB.genes <- HB.genes[!is.na(HB.genes)]
scRNA[["percent.HB"]]<-PercentageFeatureSet(scRNA, features=HB.genes)
col.num <- length(levels(scRNA@active.ident))
violin <- VlnPlot(scRNA,
                  features = c("nFeature_RNA", "nCount_RNA", "percent.mt","percent.HB"), 
                  cols =rainbow(col.num), 
                  pt.size = 0, 
                  ncol = 4) + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) 
violin
scRNA <- subset(scRNA, subset = nFeature_RNA > 300&percent.mt < 20)
#######Normalize
scRNA <- NormalizeData(scRNA,normalization.method = "LogNormalize",scale.factor = 10000)
scRNA <- FindVariableFeatures(scRNA, selection.method = "vst", nfeatures = 3000)
scale.genes <-  rownames(scRNA)
scRNA <- ScaleData(scRNA, features = scale.genes)
scRNA <- RunPCA(scRNA, features = VariableFeatures(scRNA))
plot1 <- DimPlot(scRNA, reduction = "pca", group.by="orig.ident");plot1
system.time({scRNA_harmony <- RunHarmony(scRNA, group.by.vars = "orig.ident")})
ElbowPlot(scRNA_harmony)
pc.num=20
scRNA_harmony <- FindNeighbors(scRNA_harmony, reduction = "harmony", dims = 1:21)
seq <- seq(0.1,1,by=0.1)
for(res in seq){scRNA_harmony <- FindClusters(scRNA_harmony,resolution = res)}
p1 <- clustree(scRNA_harmony,prefix = 'RNA_snn_res.')
p1+coord_flip()
scRNA_harmony <- FindClusters(scRNA_harmony, resolution = 0.5)
scRNA_harmony <- RunTSNE(scRNA_harmony, reduction = "harmony", dims = 1:20)
DimPlot(scRNA_harmony1,reduction = "tsne")
save(scRNA_harmony,file="D:/Rproject/sc_1.rdata")
Org <- 'hsa'
selectAnnotator <- function(Org) {
  Org <- tolower(Org)
  if (Org == 'hsa'){
    return(scHCLlite)
  }else{
    return(scMCA)
  }
  
}
annotator <- selectAnnotator(Org)
anno_res <- annotator(scdata = GetAssayData(scRNA_harmony, slot='data')) # time-consuming step 
scRNA_harmony$celltype <- plyr::mapvalues(x=rownames(scRNA_harmony@meta.data),                                                        #某个向量映射为新的向量
                                          from=names(anno_res$scHCL),
                                          to=anno_res$scHCL)
markers_sc <- FindAllMarkers(scRNA_harmony1, 
                             min.pct = 0.25, 
                             logfc.threshold = 0.25)
top10 <-  markers_sc %>% group_by(cluster) %>% top_n(n = 10, wt = avg_log2FC)