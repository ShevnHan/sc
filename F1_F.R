NK=subset(scRNA_harmony2,celltypes=="dNK")
sc_NK <- CreateSeuratObject(counts = NK@assays$RNA$counts, 
                            meta.data = NK@meta.data,
                            project = "NK Cell", min.cells = 3, min.features = 200)
sc_NK <- NormalizeData(sc_NK,normalization.method = "LogNormalize",scale.factor = 10000)
sc_NK <- FindVariableFeatures(sc_NK, selection.method = "vst", nfeatures = 3000)
scale.genes <-  rownames(sc_NK)
sc_NK <- ScaleData(sc_NK, features = scale.genes)
sc_NK <- RunPCA(sc_NK, features = VariableFeatures(sc_NK))
plot1 <- DimPlot(sc_NK, reduction = "pca", group.by="orig.ident");plot1
system.time({sc_NK <- RunHarmony(sc_NK, group.by.vars = "orig.ident")})
ElbowPlot(sc_NK)
pc.num=17
sc_NK <- FindNeighbors(sc_NK, reduction = "harmony", dims = 1:17)
seq <- seq(0.1,1,by=0.1)
for(res in seq){sc_NK <- FindClusters(sc_NK,resolution = res)}
p1 <- clustree(sc_NK,prefix = 'RNA_snn_res.')
p1+coord_flip()
sc_NK <- FindClusters(sc_NK, resolution = 0.1)
sc_NK <- RunTSNE(sc_NK, reduction = "harmony", dims = 1:17)

save(NK,file="D:/Rproject/NK_vln.rdata")

#保留数据：GZMB，KLRC1，IL2RB，PRF1，IRF8，NCAM1，CXCL8
#CCL3,CCL4L2,IL32
genes_to_check1=c("GZMB","IL2RB")
genes_to_check1=c("GZMB","PRF1","IL2RB","IL32","CCL3","CCL4L2")
genes_to_check2=c("GZMB","IL32")
genes_to_check3 = c("GZMB","PRF1")
genes_to_check4=c("GZMB","CCL3")
genes_to_check5=c("GZMB","CCL4L2")
genes_to_check_df<-as.data.frame(genes_to_check1)
marker_gene<-genes_to_check_df
vln.data=as.data.frame(NK@assays$RNA$data)[marker_gene$genes_to_check1,]
vln.data<-na.omit(vln.data)
library(data.table)
vln.data$gene=rownames(vln.data)
vln.data=melt(vln.data,id="gene")
colnames(vln.data)[c(2,3)]=c("cell","exp")


data_marker=NK@meta.data[,c("cell","celltypes")]
vln.data=merge(vln.data,data_marker,by="cell")
data_group=NK@meta.data[,c("cell","orig.ident")]
vln.data=merge(vln.data,data_group,by="cell")

vln.data=subset(vln.data,celltypes=="dNK")
colnames(vln.data)
blue1 = "#3B4992FF"
red1 = "#D43F3AFF"
colnames(vln.data)=c("cell","gene","exp","celltype","Sample")

GZMB = subset(vln.data,gene == "GZMB")
t.test(exp~Sample,GZMB)

PRF1 =subset(vln.data,gene =="PRF1")
t.test(exp~Sample,PRF1)

IL2RB = subset(vln.data,gene =="IL2RB")
t.test(exp~Sample,IL2RB)

IL32 = subset(vln.data,gene == "IL32")
t.test(exp~Sample,IL32)

CCL3 = subset(vln.data,gene == "CCL3")
t.test(exp~Sample,CCL3)

CCL4L2 = subset(vln.data,gene =="CCL4L2")
t.test(exp~Sample,CCL4L2)

vln.data$gene=factor(vln.data$gene,levels = c("GZMB","PRF1","IL2RB","IL32","CCL3","CCL4L2"))
p<-ggplot(vln.data,aes(x=gene,y=exp))+geom_violin(aes(fill=Sample),scale = "width")+
  scale_fill_manual(values = c(red1,blue1))+
  scale_x_discrete("")+scale_y_continuous("")+
  theme(legend.position.inside = c(0.8,0.8))+
  theme_bw()+
  theme(axis.line = element_line(colour = "black",linewidth = 0.5,linetype = "solid"))+
  #theme(axis.ticks = element_line(color = "black"))+
  theme(panel.border = element_blank(),panel.grid = element_blank())+
  theme(
    axis.text.x.bottom = element_text(angle = 0,hjust = 0.5,vjust = 1,face=c("bold"),
                                      family="Times New Roman"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #legend.position = "none",
    axis.text.y = element_text(color="black",size=16,face="bold",family = "Times New Roman"),
    
    axis.text.x = element_text(color = 'black',size = 12,face = "bold",family = "Times New Roman"),
    #axis.text.y = element_blank(),
    axis.title.x = element_text(color = 'black', size = 16),
    axis.ticks.x = element_line(color = 'black'),
    #axis.ticks.y = element_blank(),
  )+
  theme(strip.text = element_text(size=10, 
                                  face=c("bold"),
                                  family="Times New Roman"
  ))+
  theme(legend.title = element_text(size=15,
                                    face=c("bold"),
                                    family="Times New Roman"))+
  theme(legend.text =element_text(size=10, 
                                  face=c("plain"),
                                  family="Times New Roman"
  ) );p

#####non-character
p<-ggplot(vln.data,aes(x=gene,y=exp))+geom_violin(aes(fill=Sample),scale = "width")+
  scale_fill_manual(values = c(red1,blue1))+
  scale_x_discrete("")+scale_y_continuous("")+
  theme(legend.position.inside = c(0.8,0.8))+
  theme_bw()+
  theme(axis.line = element_line(colour = "black",linewidth = 0.5,linetype = "solid"))+
  #theme(axis.ticks = element_line(color = "black"))+
  theme(panel.border = element_blank(),panel.grid = element_blank())+
  theme(
    axis.text.x.bottom = element_text(angle = 0,hjust = 0.5,vjust = 1,face=c("bold"),
                                      family="Times New Roman"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #legend.position = "none",
    axis.text.y = element_text(color="white",size=16,face="bold",family = "Times New Roman"),
    
    axis.text.x = element_text(color = 'white',size = 12,face = "bold",family = "Times New Roman"),
    #axis.text.y = element_blank(),
    axis.title.x = element_text(color = 'white', size = 16),
    axis.ticks.x = element_line(color = 'white'),
    #axis.ticks.y = element_blank(),
  )+
  theme(strip.text = element_text(size=10, 
                                  face=c("bold"),
                                  family="Times New Roman"
  ))+
  theme(legend.title = element_text(size=15,
                                    colour = "white",
                                    face=c("bold"),
                                    family="Times New Roman"))+
  theme(legend.text =element_text(size=10, 
                                  colour = "white",
                                  face=c("plain"),
                                  family="Times New Roman"
  ) );p