setwd("E:/F_figure")
##F1 
pbmc_T=scR_T
##all clusters
celltype2 <- DimPlot(pbmc_T, pt.size=0.4,
                     cols = c("#42B540FF","#FF9900FF","#8C564BFF"),
                     reduction = 'tsne', group.by = 'celltype',label = FALSE)+
  theme_bw()+theme(panel.grid = element_blank());celltype2
celltype1=celltype2+#theme(legend.position =c(0.85,0.20))+
  theme(axis.text.y = element_text(size=12, colour = "black",face=c("bold"),
                                   family="Times New Roman"),
        axis.title.y = element_text(size=12, colour = "black",face=c("bold"),
                                    family="Times New Roman"))+
  theme(axis.text.x = element_text(size=12, colour = "black",face=c("bold"),
                                   family="Times New Roman"),
        axis.title.x = element_text(size=12, colour = "black",face=c("bold"),
                                    family="Times New Roman"))+
  theme(legend.position = "none")+
  #theme(legend.text = element_text(family = "Times New Roman",size=20,face="bold"))+
  theme(title = element_blank());celltype1
#ggsave("F1_A_chara.svg",plot = celltype1,dpi = 300, width = 6 ,height = 5,limitsize = FALSE)
#for sample
celltype2_sample <- DimPlot(pbmc_T, pt.size=0.4,
                            cols = c(red_1_sample,blue_1_sample),
                            reduction = 'tsne', group.by = 'orig.ident',label = FALSE)+
  theme_bw();celltype2_sample
celltype1_sample=celltype2_sample+theme(legend.position =c(0.90,0.80))+
  theme(axis.text.y = element_text(size=12, colour = "black",face=c("bold"),
                                   family="Times New Roman"),
        axis.title.y = element_text(size=12, colour = "black",face=c("bold"),
                                    family="Times New Roman"))+
  theme(axis.text.x = element_text(size=12, colour = "black",face=c("bold"),
                                   family="Times New Roman"),
        axis.title.x = element_text(size=12, colour = "black",face=c("bold"),
                                    family="Times New Roman"))+
  #theme(legend.position = "none")+
  theme(legend.text = element_text(family = "Times New Roman",size=18,face = "bold"))+
  theme(title = element_blank(),panel.grid =element_blank());celltype1_sample
#ggsave("F1_A_chara_sample.svg",plot = celltype1_sample,dpi = 300, width = 6 ,height = 5,limitsize = FALSE)

celltype2 <- DimPlot(pbmc_T, pt.size=0.4,
                     cols = c("#42B540FF","#FF9900FF","#8C564BFF"),
                     reduction = 'tsne', group.by = 'celltype',label = FALSE)+
  theme_bw()+theme(panel.grid = element_blank());celltype2
celltype1_legend=celltype2+#theme(legend.position =c(0.85,0.20))+
  theme(axis.text.y = element_text(size=12, colour = "black",face=c("bold"),
                                   family="Times New Roman"),
        axis.title.y = element_text(size=12, colour = "black",face=c("bold"),
                                    family="Times New Roman"))+
  theme(axis.text.x = element_text(size=12, colour = "black",face=c("bold"),
                                   family="Times New Roman"),
        axis.title.x = element_text(size=12, colour = "black",face=c("bold"),
                                    family="Times New Roman"))+
  #theme(legend.position = "none")+
  theme(legend.text = element_text(family = "Times New Roman",size=12,face="bold"))+
  theme(title = element_blank());celltype1_legend
#ggsave("F1_A_chara_legend.svg",plot = celltype1_legend,dpi = 300, width = 6 ,height = 5,limitsize = FALSE)

###non-character
celltype2 <- DimPlot(pbmc_T, pt.size=0.4,
                     cols = c("#42B540FF","#FF9900FF","#8C564BFF"),
                     reduction = 'tsne', group.by = 'celltype',label = FALSE)+
  theme_bw()+theme(panel.grid = element_blank());celltype2
celltype1_none=celltype2+#theme(legend.position =c(0.85,0.20))+
  theme(axis.text.y = element_text(size=12, colour = "white",face=c("bold"),
                                   family="Times New Roman"),
        axis.title.y = element_text(size=12, colour = "white",face=c("bold"),
                                    family="Times New Roman"))+
  theme(axis.text.x = element_text(size=12, colour = "white",face=c("bold"),
                                   family="Times New Roman"),
        axis.title.x = element_text(size=12, colour = "white",face=c("bold"),
                                    family="Times New Roman"))+
  theme(legend.position = "none")+
  #theme(legend.text = element_text(family = "Times New Roman",size=20,face="bold"))+
  theme(title = element_blank());celltype1_none
#ggsave("F1_A_none_chara.svg",plot = celltype1_none,dpi = 300, width = 6 ,height = 5,limitsize = FALSE)
#for sample
celltype2_sample <- DimPlot(pbmc_T, pt.size=0.4,
                            cols = c(red_1_sample,blue_1_sample),
                            reduction = 'tsne', group.by = 'orig.ident',label = FALSE)+
  theme_bw();celltype2_sample
celltype1_sample_none=celltype2_sample+theme(legend.position =c(0.90,0.80))+
  theme(axis.text.y = element_text(size=12, colour = "white",face=c("bold"),
                                   family="Times New Roman"),
        axis.title.y = element_text(size=12, colour = "white",face=c("bold"),
                                    family="Times New Roman"))+
  theme(axis.text.x = element_text(size=12, colour = "white",face=c("bold"),
                                   family="Times New Roman"),
        axis.title.x = element_text(size=12, colour = "white",face=c("bold"),
                                    family="Times New Roman"))+
  #theme(legend.position = "none")+
  theme(legend.text = element_text(family = "Times New Roman",color="white",size=12,face = "bold"))+
  theme(title = element_blank(),panel.grid =element_blank());celltype1_sample_none
#ggsave("F1_A_none_chara_sample.svg",plot = celltype1_sample_none,dpi = 300, width = 6 ,height = 5,limitsize = FALSE)

celltype2 <- DimPlot(pbmc_T, pt.size=0.4,
                     cols = c("#42B540FF","#FF9900FF","#8C564BFF"),
                     reduction = 'tsne', group.by = 'celltype',label = FALSE)+
  theme_bw()+theme(panel.grid = element_blank());celltype2
celltype1_legend=celltype2+#theme(legend.position =c(0.85,0.20))+
  theme(axis.text.y = element_text(size=12, colour = "black",face=c("bold"),
                                   family="Times New Roman"),
        axis.title.y = element_text(size=12, colour = "black",face=c("bold"),
                                    family="Times New Roman"))+
  theme(axis.text.x = element_text(size=12, colour = "black",face=c("bold"),
                                   family="Times New Roman"),
        axis.title.x = element_text(size=12, colour = "black",face=c("bold"),
                                    family="Times New Roman"))+
  #theme(legend.position = "none")+
  theme(legend.text = element_text(family = "Times New Roman",color = "white",size=12,face="bold"))+
  theme(title = element_blank());celltype1_legend
#ggsave("F1_A_none_chara_legend.svg",plot = celltype1_legend,dpi = 300, width = 6 ,height = 5,limitsize = FALSE)


