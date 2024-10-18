##T Cell
T=subset(scRNA_harmony2,celltypes=="T Cell")
sc_T=T

scR_T <- CreateSeuratObject(counts = sc_T@assays$RNA$counts, 
                           meta.data = sc_T@meta.data,
                           project = "T Cell", min.cells = 3, min.features = 200)
scR_T <- NormalizeData(scR_T,normalization.method = "LogNormalize",scale.factor = 10000)
scR_T <- FindVariableFeatures(scR_T, selection.method = "vst", nfeatures = 3000)
scale.genes <-  rownames(scR_T)
scR_T <- ScaleData(scR_T, features = scale.genes)
scR_T <- RunPCA(scR_T, features = VariableFeatures(scR_T))
plot1 <- DimPlot(scR_T, reduction = "pca", group.by="orig.ident");plot1
system.time({scR_T <- RunHarmony(scR_T, group.by.vars = "orig.ident")})
ElbowPlot(scR_T)
pc.num=17
scR_T <- FindNeighbors(scR_T, reduction = "harmony", dims = 1:17)
seq <- seq(0.1,1,by=0.1)
for(res in seq){scR_T <- FindClusters(scR_T,resolution = res)}
p1 <- clustree(scR_T,prefix = 'RNA_snn_res.')
p1+coord_flip()
scR_T <- FindClusters(scR_T, resolution = 0.1)
scR_T <- RunTSNE(scR_T, reduction = "harmony", dims = 1:17)
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
anno_res <- annotator(scdata = GetAssayData(scR_T, slot='data')) # time-consuming step 
scR_T$celltype <- plyr::mapvalues(x=rownames(scR_T@meta.data),                                                        #某个向量映射为新的向量
                                          from=names(anno_res$scHCL),
                                          to=anno_res$scHCL)
markers_sc <- FindAllMarkers(scR_T, 
                             min.pct = 0.25, 
                             logfc.threshold = 0.25)
top10 <-  markers_sc %>% group_by(cluster) %>% top_n(n = 10, wt = avg_log2FC)