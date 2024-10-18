#filter PCA

##PCA
plot1 <- DimPlot(scRNA_harmony2, reduction = "pca",cols=c(red1,blue1),
                 group.by = "orig.ident",split.by = "orig.ident")+
  theme_bw()+
  theme(title = element_text(size=12, colour = "black",face=c("bold"),
                             family="Times New Roman",hjust = 0.5))+
  theme(legend.position = "none")+
  theme(axis.text.y = element_text(size=12, colour = "black",face=c("bold"),
                                   family="Times New Roman"))+
  theme(axis.text.x = element_text(size=12, colour = "black",face=c("bold"),
                                   family="Times New Roman"))+
  theme(
    strip.background = element_rect(
      color = "white", fill = "white"),
    panel.grid = element_blank(),strip.text =element_text(size=12, colour = "black",face=c("bold"),
                                                          family="Times New Roman") );plot1
;plot1


#******************************************************************************#
plot1_none <- DimPlot(scRNA_harmony2, reduction = "pca",cols=c(red1,blue1),
                      group.by = "orig.ident",split.by = "orig.ident")+
  theme_bw()+
  theme(title = element_text(size=12, colour = "white",face=c("bold"),
                             family="Times New Roman",hjust = 0.5))+
  theme(legend.position = "none")+
  theme(axis.text.y = element_text(size=12, colour = "white",face=c("bold"),
                                   family="Times New Roman"))+
  theme(axis.text.x = element_text(size=12, colour = "white",face=c("bold"),
                                   family="Times New Roman"))+
  theme(
    strip.background = element_rect(
      color = "white", fill = "white"),
    panel.grid = element_blank(),strip.text =element_text(size=12, colour = "white",face=c("bold"),
                                                          family="Times New Roman") );plot1_none
