#all sc seurat_clusters
##
celltype2 <- DimPlot(scRNA_harmony2, reduction = 'tsne', group.by = 'seurat',label = TRUE)+
  theme_bw();celltype2
scRNA_harmony2@meta.data$Barcode <-rownames(scRNA_harmony2@meta.data)
x<-as.data.frame(scRNA_harmony2@reductions$tsne@cell.embeddings) 
x$Barcode <-rownames(x)
y <-data.frame(scRNA_harmony2@meta.data[,c('Barcode','seurat_clusters','orig.ident',"celltypes","seurat")])
lab <-merge(x,y,barcode='Barcode')
lab$seurat_clusters <-factor(lab$seurat_clusters,levels=0:12)   ##
lab$seurat <-factor(lab$seurat,levels=0:12)   ##
label1$celltypes
label1=lab[c(6004,1337,4841,976,2319,801,11721,3749),]

col = c("#FDAF91FF","#AD002AFF","#008B45FF","#4DBBD5FF","#95C11FFF","#8C564BFF","#925E9FFF","#DC0000FF")

##plot all
celltype2 <- DimPlot(scRNA_harmony2, pt.size=0.4,
                     cols = c("#FDAF91FF","#AD002AFF","#008B45FF","#4DBBD5FF","#95C11FFF","#8C564BFF","#925E9FFF","#DC0000FF"),##注释一下
                     reduction = 'tsne', group.by = 'celltypes',label = FALSE)+
  theme_bw()+theme(panel.grid = element_blank());celltype2#ggrepel::geom_text_repel(label1,mapping=aes(x=tSNE_1,y=tSNE_2,
#size=16,
#label=seurat,
#fontface = "bold",fontfamily="Times New Roman"));celltype2
celltype1=celltype2+theme(legend.position =c(0.85,0.20))+
  theme(axis.text.y = element_text(size=12, colour = "black",face=c("bold"),
                                   family="Times New Roman"),
        axis.title.y = element_text(size=12, colour = "black",face=c("bold"),
                                    family="Times New Roman"))+
  theme(axis.text.x = element_text(size=12, colour = "black",face=c("bold"),
                                   family="Times New Roman"),
        axis.title.x = element_text(size=12, colour = "black",face=c("bold"),
                                    family="Times New Roman"))+
  #theme(legend.position = "none")+
  theme(legend.text = element_text(family = "Times New Roman",size=20,face = "bold"))+
  theme(title = element_blank());celltype1
celltype1


celltype2_orig <- DimPlot(scRNA_harmony2, pt.size=0.4,
                          cols = c(red_1_sample,blue_1_sample),##
                          reduction = 'tsne', group.by = 'orig.ident',label = FALSE)+
  theme_bw();celltype2_orig
celltype1_orig=celltype2_orig+theme(legend.position =c(0.90,0.80))+
  theme(axis.text.y = element_text(size=12, colour = "black",face=c("bold"),
                                   family="Times New Roman"),
        axis.title.y = element_text(size=12, colour = "black",face=c("bold"),
                                    family="Times New Roman"))+
  theme(axis.text.x = element_text(size=12, colour = "black",face=c("bold"),
                                   family="Times New Roman"),
        axis.title.x = element_text(size=12, colour = "black",face=c("bold"),
                                    family="Times New Roman"))+
  theme(legend.text = element_text(family = "Times New Roman",size=18,face="bold"))+
  theme(title = element_blank(),panel.grid =element_blank());celltype1_orig


#**************************************************#
#non-character
celltype2_none <- DimPlot(scRNA_harmony2, pt.size=0.4,
                          cols = c("#FDAF91FF","#AD002AFF","#008B45FF","#4DBBD5FF","#95C11FFF","#8C564BFF","#925E9FFF","#DC0000FF"),##注释一下
                          reduction = 'tsne', group.by = 'celltypes',label = FALSE)+
  theme_bw()+theme(panel.grid = element_blank());celltype2_none#ggrepel::geom_text_repel(label1,mapping=aes(x=tSNE_1,y=tSNE_2,
#size=16,
#label=seurat,
#fontface = "bold",fontfamily="Times New Roman"));celltype2
celltype1_none=celltype2_none+theme(legend.position =c(0.85,0.20))+
  theme(axis.text.y = element_text(size=12, colour = "white",face=c("bold"),
                                   family="Times New Roman"),
        axis.title.y = element_text(size=12, colour = "white",face=c("bold"),
                                    family="Times New Roman"))+
  theme(axis.text.x = element_text(size=12, colour = "white",face=c("bold"),
                                   family="Times New Roman"),
        axis.title.x = element_text(size=12, colour = "white",face=c("bold"),
                                    family="Times New Roman"))+
  #theme(legend.position = "none")+
  theme(legend.text = element_text(family = "Times New Roman",size=20,face = "bold"))+
  theme(title = element_blank());celltype1_none
celltype1_none



celltype2_orig_none <- DimPlot(scRNA_harmony2, pt.size=0.4,
                               cols = c(red_1_sample,blue_1_sample),##注释一下
                               reduction = 'tsne', group.by = 'orig.ident',label = FALSE)+
  theme_bw();celltype2_orig_none
celltype1_orig_none=celltype2_orig_none+theme(legend.position =c(0.90,0.80))+
  theme(axis.text.y = element_text(size=12, colour = "white",face=c("bold"),
                                   family="Times New Roman"),
        axis.title.y = element_text(size=12, colour = "white",face=c("bold"),
                                    family="Times New Roman"))+
  theme(axis.text.x = element_text(size=12, colour = "white",face=c("bold"),
                                   family="Times New Roman"),
        axis.title.x = element_text(size=12, colour = "white",face=c("bold"),
                                    family="Times New Roman"))+
  theme(legend.text = element_text(family = "Times New Roman",size=18,face="bold"))+
  theme(title = element_blank(),panel.grid =element_blank());celltype1_orig_none
