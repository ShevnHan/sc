#feature plot
###FeaturePlot
###macro CD4
M1 = FeaturePlot(scRNA_harmony2,features = "CD14",
                 min.cutoff = 0,
                 max.cutoff = 2,
                 reduction = "tsne",pt.size = 0.5,cols = if (FALSE) {
                   c("lightgrey", "#ff0000", "#00ff00")
                 } else {c("lightgrey", "#BB0021FF")
                 })+
  scale_x_continuous("")+scale_y_continuous("")+
  theme_bw()+ 
  labs(title = "CD14")+
  theme(title = element_text(family = "Times New Roman",size=12,face="bold"))+
  theme(  
    panel.border = element_blank(),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(),axis.text = element_blank(),  
    legend.text = element_text(family = "Times New Roman",size=12,face="bold"),
    legend.position = "none",  
    plot.title = element_text(hjust = 0.5)  
  );M1
M2 = FeaturePlot(scRNA_harmony2,features = "FOLR2",
                 min.cutoff = 0,
                 max.cutoff = 2,
                 reduction = "tsne",pt.size = 0.5,cols = if (FALSE) {
                   c("lightgrey", "#ff0000", "#00ff00")
                 } else {c("lightgrey", "#BB0021FF")
                 })+
  scale_x_continuous("")+scale_y_continuous("")+
  theme_bw()+ 
  labs(title = "FOLR2")+
  theme(title = element_text(family = "Times New Roman",size=12,face="bold"))+
  theme(  
    panel.border = element_blank(),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(),axis.text = element_blank(),  
    legend.text = element_text(family = "Times New Roman",size=12,face="bold"),
    legend.position = "none",  
    plot.title = element_text(hjust = 0.5)  
  );M2

Macrophage = M1/M2+plot_annotation(title = "Macrophage",
                                   theme= theme(
                                     plot.title=element_text(
                                       family = "Times New Roman",size=18,
                                       face="bold",hjust = 0.5)
                                   ));Macrophage
#dNK KLRC1 KLRD1
dNK1 = FeaturePlot(scRNA_harmony2,features = "KLRC1",
                   min.cutoff = 0,
                   max.cutoff = 2,
                   reduction = "tsne",pt.size = 0.5,cols = if (FALSE) {
                     c("lightgrey", "#ff0000", "#00ff00")
                   } else {c("lightgrey", "#BB0021FF")
                   })+
  scale_x_continuous("")+scale_y_continuous("")+
  theme_bw()+ 
  labs(title = "KLRC1")+
  theme(title = element_text(family = "Times New Roman",size=12,face="bold"))+
  theme(  
    panel.border = element_blank(),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(),axis.text = element_blank(),  
    legend.text = element_text(family = "Times New Roman",size=12,face="bold"),
    legend.position = "none",  
    plot.title = element_text(hjust = 0.5)  
  );dNK1
dNK2 = FeaturePlot(scRNA_harmony2,features = "KLRD1",
                   min.cutoff = 0,
                   max.cutoff = 2,
                   reduction = "tsne",pt.size = 0.5,cols = if (FALSE) {
                     c("lightgrey", "#ff0000", "#00ff00")
                   } else {c("lightgrey", "#BB0021FF")
                   })+
  scale_x_continuous("")+scale_y_continuous("")+
  theme_bw()+ 
  labs(title = "KLRD1")+
  theme(title = element_text(family = "Times New Roman",size=12,face="bold"))+
  theme(  
    panel.border = element_blank(),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(),axis.text = element_blank(),  
    legend.text = element_text(family = "Times New Roman",size=12,face="bold"),
    legend.position = "none",  
    plot.title = element_text(hjust = 0.5)  
  );dNK2

dNK = dNK1/dNK2+plot_annotation(title = "dNK",
                                theme= theme(
                                  plot.title=element_text(
                                    family = "Times New Roman",size=18,
                                    face="bold",hjust = 0.5)
                                ));dNK

#T Cell CD3D CD8A
T1 = FeaturePlot(scRNA_harmony2,features = "CD3D",
                 min.cutoff = 0,
                 max.cutoff = 2,
                 reduction = "tsne",pt.size = 0.5,cols = if (FALSE) {
                   c("lightgrey", "#ff0000", "#00ff00")
                 } else {c("lightgrey", "#BB0021FF")
                 })+
  scale_x_continuous("")+scale_y_continuous("")+
  theme_bw()+ 
  labs(title = "CD3D")+
  theme(title = element_text(family = "Times New Roman",size=12,face="bold"))+
  theme(  
    panel.border = element_blank(),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(),axis.text = element_blank(),  
    legend.text = element_text(family = "Times New Roman",size=12,face="bold"),
    legend.position = "none",  
    plot.title = element_text(hjust = 0.5)  
  );T1
T2 = FeaturePlot(scRNA_harmony2,features = "CD8A",
                 min.cutoff = 0,
                 max.cutoff = 2,
                 reduction = "tsne",pt.size = 0.5,cols = if (FALSE) {
                   c("lightgrey", "#ff0000", "#00ff00")
                 } else {c("lightgrey", "#BB0021FF")
                 })+
  scale_x_continuous("")+scale_y_continuous("")+
  theme_bw()+ 
  labs(title = "CD8A")+
  theme(title = element_text(family = "Times New Roman",size=12,face="bold"))+
  theme(  
    panel.border = element_blank(),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(),axis.text = element_blank(),  
    legend.text = element_text(family = "Times New Roman",size=12,face="bold"),
    legend.position = "none",  
    plot.title = element_text(hjust = 0.5)  
  );T2

T = T1/T2+plot_annotation(title = "T Cell",
                          theme= theme(
                            plot.title=element_text(
                              family = "Times New Roman",size=18,
                              face="bold",hjust = 0.5)
                          ));T

#Monocyte FCN1 VCAN
mono1 = FeaturePlot(scRNA_harmony2,features = "FCN1",
                    min.cutoff = 0,
                    max.cutoff = 2,
                    reduction = "tsne",pt.size = 0.5,cols = if (FALSE) {
                      c("lightgrey", "#ff0000", "#00ff00")
                    } else {c("lightgrey", "#BB0021FF")
                    })+
  scale_x_continuous("")+scale_y_continuous("")+
  theme_bw()+ 
  labs(title = "FCN1")+
  theme(title = element_text(family = "Times New Roman",size=12,face="bold"))+
  theme(  
    panel.border = element_blank(),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(),axis.text = element_blank(),  
    legend.text = element_text(family = "Times New Roman",size=12,face="bold"),
    legend.position = "none",  
    plot.title = element_text(hjust = 0.5)  
  );mono1
mono2 = FeaturePlot(scRNA_harmony2,features = "VCAN",
                    min.cutoff = 0,
                    max.cutoff = 2,
                    reduction = "tsne",pt.size = 0.5,cols = if (FALSE) {
                      c("lightgrey", "#ff0000", "#00ff00")
                    } else {c("lightgrey", "#BB0021FF")
                    })+
  scale_x_continuous("")+scale_y_continuous("")+
  theme_bw()+ 
  labs(title = "VCAN")+
  theme(title = element_text(family = "Times New Roman",size=12,face="bold"))+
  theme(  
    panel.border = element_blank(),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(),axis.text = element_blank(),  
    legend.text = element_text(family = "Times New Roman",size=12,face="bold"),
    legend.position = "none",  
    plot.title = element_text(hjust = 0.5)  
  );mono2

Monocyte = mono1/mono2+plot_annotation(title = "Monocyte",
                                       theme= theme(
                                         plot.title=element_text(
                                           family = "Times New Roman",size=18,
                                           face="bold",hjust = 0.5)
                                       ));Monocyte
#DC NCR1 CLEC9A
DC1 = FeaturePlot(scRNA_harmony2,features = "XCR1",
                  min.cutoff = 0,
                  max.cutoff = 2,
                  reduction = "tsne",pt.size = 0.5,cols = if (FALSE) {
                    c("lightgrey", "#ff0000", "#00ff00")
                  } else {c("lightgrey", "#BB0021FF")
                  })+
  scale_x_continuous("")+scale_y_continuous("")+
  theme_bw()+ 
  labs(title = "XCR1")+
  theme(title = element_text(family = "Times New Roman",size=12,face="bold"))+
  theme(  
    panel.border = element_blank(),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(),axis.text = element_blank(),  
    legend.text = element_text(family = "Times New Roman",size=12,face="bold"),
    legend.position = "none",  
    plot.title = element_text(hjust = 0.5)  
  );DC1
DC2 = FeaturePlot(scRNA_harmony2,features = "CLEC9A",
                  min.cutoff = 0,
                  max.cutoff = 2,
                  reduction = "tsne",pt.size = 0.5,cols = if (FALSE) {
                    c("lightgrey", "#ff0000", "#00ff00")
                  } else {c("lightgrey", "#BB0021FF")
                  })+
  scale_x_continuous("")+scale_y_continuous("")+
  theme_bw()+ 
  labs(title = "CLEC9A")+
  theme(title = element_text(family = "Times New Roman",size=12,face="bold"))+
  theme(  
    panel.border = element_blank(),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(),axis.text = element_blank(),  
    legend.text = element_text(family = "Times New Roman",size=12,face="bold"),
    legend.position = "none",  
    plot.title = element_text(hjust = 0.5)  
  );DC2

DC = DC1/DC2+plot_annotation(title = "DC",
                             theme= theme(
                               plot.title=element_text(
                                 family = "Times New Roman",size=18,
                                 face="bold",hjust = 0.5)
                             ));DC
#B MS4A1 CD79A
B1 = FeaturePlot(scRNA_harmony2,features = "MS4A1",
                 min.cutoff = 0,
                 max.cutoff = 2,
                 reduction = "tsne",pt.size = 0.5,cols = if (FALSE) {
                   c("lightgrey", "#ff0000", "#00ff00")
                 } else {c("lightgrey", "#BB0021FF")
                 })+
  scale_x_continuous("")+scale_y_continuous("")+
  theme_bw()+ 
  labs(title = "MS4A1")+
  theme(title = element_text(family = "Times New Roman",size=12,face="bold"))+
  theme(  
    panel.border = element_blank(),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(),axis.text = element_blank(),  
    legend.text = element_text(family = "Times New Roman",size=12,face="bold"),
    legend.position = "none",  
    plot.title = element_text(hjust = 0.5)  
  );B1
B2 = FeaturePlot(scRNA_harmony2,features = "CD79A",
                 min.cutoff = 0,
                 max.cutoff = 2,
                 reduction = "tsne",pt.size = 0.5,cols = if (FALSE) {
                   c("lightgrey", "#ff0000", "#00ff00")
                 } else {c("lightgrey", "#BB0021FF")
                 })+
  scale_x_continuous("")+scale_y_continuous("")+
  theme_bw()+ 
  labs(title = "CD79A")+
  theme(title = element_text(family = "Times New Roman",size=12,face="bold"))+
  theme(  
    panel.border = element_blank(),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(),axis.text = element_blank(),  
    legend.text = element_text(family = "Times New Roman",size=12,face="bold"),
    legend.position = "none",  
    plot.title = element_text(hjust = 0.5)  
  );B2

B = B1/B2+plot_annotation(title = "B Cell",
                          theme= theme(
                            plot.title=element_text(
                              family = "Times New Roman",size=18,
                              face="bold",hjust = 0.5)
                          ));B

#Mast  TPSAB1 MS4A2
Mast1 = FeaturePlot(scRNA_harmony2,features = "TPSAB1",
                    min.cutoff = 0,
                    max.cutoff = 2,
                    reduction = "tsne",pt.size = 0.5,cols = if (FALSE) {
                      c("lightgrey", "#ff0000", "#00ff00")
                    } else {c("lightgrey", "#BB0021FF")
                    })+
  scale_x_continuous("")+scale_y_continuous("")+
  theme_bw()+ 
  labs(title = "TPSAB1")+
  theme(title = element_text(family = "Times New Roman",size=12,face="bold"))+
  theme(  
    panel.border = element_blank(),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(),axis.text = element_blank(),  
    legend.text = element_text(family = "Times New Roman",size=12,face="bold"),
    legend.position = "none",  
    plot.title = element_text(hjust = 0.5)  
  );Mast1
Mast2 = FeaturePlot(scRNA_harmony2,features = "MS4A2",
                    min.cutoff = 0,
                    max.cutoff = 2,
                    reduction = "tsne",pt.size = 0.5,cols = if (FALSE) {
                      c("lightgrey", "#ff0000", "#00ff00")
                    } else {c("lightgrey", "#BB0021FF")
                    })+
  scale_x_continuous("")+scale_y_continuous("")+
  theme_bw()+ 
  labs(title = "MS4A2")+
  theme(title = element_text(family = "Times New Roman",size=12,face="bold"))+
  theme(  
    panel.border = element_blank(),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(),axis.text = element_blank(),  
    legend.text = element_text(family = "Times New Roman",size=12,face="bold"),
    legend.position = "none",  
    plot.title = element_text(hjust = 0.5)  
  );Mast2

Mast = Mast1/Mast2+plot_annotation(title = "Mast",
                                   theme= theme(
                                     plot.title=element_text(
                                       family = "Times New Roman",size=18,
                                       face="bold",hjust = 0.5)
                                   ));Mast

##Endothelial TIMP3 IGFBP4
Endothelial1 = FeaturePlot(scRNA_harmony2,features = "TIMP3",
                           min.cutoff = 0,
                           max.cutoff = 2,
                           reduction = "tsne",pt.size = 0.5,cols = if (FALSE) {
                             c("lightgrey", "#ff0000", "#00ff00")
                           } else {c("lightgrey", "#BB0021FF")
                           })+
  scale_x_continuous("")+scale_y_continuous("")+
  theme_bw()+ 
  labs(title = "TIMP3")+
  theme(title = element_text(family = "Times New Roman",size=12,face="bold"))+
  theme(  
    panel.border = element_blank(),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(),axis.text = element_blank(),  
    legend.text = element_text(family = "Times New Roman",size=12,face="bold"),
    legend.position = "none",  
    plot.title = element_text(hjust = 0.5)  
  );Endothelial1
Endothelial2 = FeaturePlot(scRNA_harmony2,features = "IGFBP4",
                           min.cutoff = 0,
                           max.cutoff = 2,
                           reduction = "tsne",pt.size = 0.5,cols = if (FALSE) {
                             c("lightgrey", "#ff0000", "#00ff00")
                           } else {c("lightgrey", "#BB0021FF")
                           })+
  scale_x_continuous("")+scale_y_continuous("")+
  theme_bw()+ 
  labs(title = "IGFBP4")+
  theme(title = element_text(family = "Times New Roman",size=12,face="bold"))+
  theme(  
    panel.border = element_blank(),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(),axis.text = element_blank(),  
    legend.text = element_text(family = "Times New Roman",size=12,face="bold"),
    legend.position = "none",  
    plot.title = element_text(hjust = 0.5)  
  );Endothelial2

#
Endothelial3 = FeaturePlot(scRNA_harmony2,features = "TIMP3",
                           min.cutoff = 0,
                           max.cutoff = 2,
                           reduction = "tsne",pt.size = 0.5,cols = if (FALSE) {
                             c("white", "white", "white")
                           } else {c("lightgrey", "#BB0021FF")
                           })+
  scale_x_continuous("")+scale_y_continuous("")+
  theme_bw()+ 
  labs(title = "TIMP3")+
  theme(title = element_text(family = "Times New Roman",color = "white",size=12,face="bold"))+
  theme(  
    panel.border = element_blank(),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(),axis.text = element_blank(),  
    legend.text = element_text(family = "Times New Roman",size=12,face="bold"),
    #legend.position = "none",  
    plot.title = element_text(hjust = 0.5)  
  );Endothelial3

Endothelial = Endothelial1/Endothelial2+plot_annotation(title = "Endothelial",
                                                        theme= theme(
                                                          plot.title=element_text(
                                                            family = "Times New Roman",size=18,
                                                            face="bold",hjust = 0.5)))

F_all = Macrophage|dNK|T|Monocyte|DC|B|Mast|Endothelial
F_all=plot_grid(Macrophage,dNK,T,Monocyte,DC,B,Mast,Endothelial,ncol = 8)





###non
###FeaturePlot
###macro CD4
M1_none = FeaturePlot(scRNA_harmony2,features = "CD14",
                      min.cutoff = 0,
                      max.cutoff = 2,
                      reduction = "tsne",pt.size = 0.5,cols = if (FALSE) {
                        c("lightgrey", "#ff0000", "#00ff00")
                      } else {c("lightgrey", "#BB0021FF")
                      })+
  scale_x_continuous("")+scale_y_continuous("")+
  theme_bw()+ 
  labs(title = "CD14")+
  theme(title = element_text(family = "Times New Roman",color="white",size=12,face="bold"))+
  theme(  
    panel.border = element_blank(),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(),axis.text = element_blank(),  
    legend.text = element_text(family = "Times New Roman",size=12,face="bold"),
    legend.position = "none",  
    plot.title = element_text(hjust = 0.5)  
  );M1_none
M2_none = FeaturePlot(scRNA_harmony2,features = "FOLR2",
                      min.cutoff = 0,
                      max.cutoff = 2,
                      reduction = "tsne",pt.size = 0.5,cols = if (FALSE) {
                        c("lightgrey", "#ff0000", "#00ff00")
                      } else {c("lightgrey", "#BB0021FF")
                      })+
  scale_x_continuous("")+scale_y_continuous("")+
  theme_bw()+ 
  labs(title = "FOLR2")+
  theme(title = element_text(family = "Times New Roman",color = "white",size=12,face="bold"))+
  theme(  
    panel.border = element_blank(),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(),axis.text = element_blank(),  
    legend.text = element_text(family = "Times New Roman",size=12,face="bold"),
    legend.position = "none",  
    plot.title = element_text(hjust = 0.5)  
  );M2_none

Macrophage_none = M1_none/M2_none+plot_annotation(title = "Macrophage",
                                                  theme= theme(
                                                    plot.title=element_text(
                                                      colour = "white",
                                                      family = "Times New Roman",size=18,
                                                      face="bold",hjust = 0.5)
                                                  ));Macrophage_none
#dNK KLRC1 KLRD1
dNK1_none = FeaturePlot(scRNA_harmony2,features = "KLRC1",
                        min.cutoff = 0,
                        max.cutoff = 2,
                        reduction = "tsne",pt.size = 0.5,cols = if (FALSE) {
                          c("lightgrey", "#ff0000", "#00ff00")
                        } else {c("lightgrey", "#BB0021FF")
                        })+
  scale_x_continuous("")+scale_y_continuous("")+
  theme_bw()+ 
  labs(title = "KLRC1")+
  theme(title = element_text(family = "Times New Roman",colour = "white",
                             size=12,face="bold"))+
  theme(  
    panel.border = element_blank(),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(),axis.text = element_blank(),  
    legend.text = element_text(family = "Times New Roman",size=12,face="bold"),
    legend.position = "none",  
    plot.title = element_text(hjust = 0.5)  
  );dNK1_none
dNK2_none = FeaturePlot(scRNA_harmony2,features = "KLRD1",
                        min.cutoff = 0,
                        max.cutoff = 2,
                        reduction = "tsne",pt.size = 0.5,cols = if (FALSE) {
                          c("lightgrey", "#ff0000", "#00ff00")
                        } else {c("lightgrey", "#BB0021FF")
                        })+
  scale_x_continuous("")+scale_y_continuous("")+
  theme_bw()+ 
  labs(title = "KLRD1")+
  theme(title = element_text(family = "Times New Roman",
                             color = "white",
                             size=12,face="bold"))+
  theme(  
    panel.border = element_blank(),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(),axis.text = element_blank(),  
    legend.text = element_text(family = "Times New Roman",size=12,face="bold"),
    legend.position = "none",  
    plot.title = element_text(hjust = 0.5)  
  );dNK2_none

dNK_none = dNK1_none/dNK2_none+plot_annotation(title = "dNK",
                                               theme= theme(
                                                 plot.title=element_text(
                                                   colour = "white",
                                                   family = "Times New Roman",size=18,
                                                   face="bold",hjust = 0.5)
                                               ));dNK_none

#T Cell CD3D CD8A
T1_none = FeaturePlot(scRNA_harmony2,features = "CD3D",
                      min.cutoff = 0,
                      max.cutoff = 2,
                      reduction = "tsne",pt.size = 0.5,cols = if (FALSE) {
                        c("lightgrey", "#ff0000", "#00ff00")
                      } else {c("lightgrey", "#BB0021FF")
                      })+
  scale_x_continuous("")+scale_y_continuous("")+
  theme_bw()+ 
  labs(title = "CD3D")+
  theme(title = element_text(family = "Times New Roman",colour = "white",
                             size=12,face="bold"))+
  theme(  
    panel.border = element_blank(),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(),axis.text = element_blank(),  
    legend.text = element_text(family = "Times New Roman",size=12,face="bold"),
    legend.position = "none",  
    plot.title = element_text(hjust = 0.5)  
  );T1_none
T2_none = FeaturePlot(scRNA_harmony2,features = "CD8A",
                      min.cutoff = 0,
                      max.cutoff = 2,
                      reduction = "tsne",pt.size = 0.5,cols = if (FALSE) {
                        c("lightgrey", "#ff0000", "#00ff00")
                      } else {c("lightgrey", "#BB0021FF")
                      })+
  scale_x_continuous("")+scale_y_continuous("")+
  theme_bw()+ 
  labs(title = "CD8A")+
  theme(title = element_text(family = "Times New Roman",colour = "white",
                             size=12,face="bold"))+
  theme(  
    panel.border = element_blank(),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(),axis.text = element_blank(),  
    legend.text = element_text(family = "Times New Roman",size=12,face="bold"),
    legend.position = "none",  
    plot.title = element_text(hjust = 0.5)  
  );T2_none

T_none = T1_none/T2_none+plot_annotation(title = "T Cell",
                                         theme= theme(
                                           plot.title=element_text(
                                             colour = "white",
                                             family = "Times New Roman",size=18,
                                             face="bold",hjust = 0.5)
                                         ));T_none

#Monocyte FCN1 VCAN
mono1_none = FeaturePlot(scRNA_harmony2,features = "FCN1",
                         min.cutoff = 0,
                         max.cutoff = 2,
                         reduction = "tsne",pt.size = 0.5,cols = if (FALSE) {
                           c("lightgrey", "#ff0000", "#00ff00")
                         } else {c("lightgrey", "#BB0021FF")
                         })+
  scale_x_continuous("")+scale_y_continuous("")+
  theme_bw()+ 
  labs(title = "FCN1")+
  theme(title = element_text(family = "Times New Roman",colour = "white",
                             size=12,face="bold"))+
  theme(  
    panel.border = element_blank(),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(),axis.text = element_blank(),  
    legend.text = element_text(family = "Times New Roman",size=12,face="bold"),
    legend.position = "none",  
    plot.title = element_text(hjust = 0.5)  
  );mono1_none
mono2_none = FeaturePlot(scRNA_harmony2,features = "VCAN",
                         min.cutoff = 0,
                         max.cutoff = 2,
                         reduction = "tsne",pt.size = 0.5,cols = if (FALSE) {
                           c("lightgrey", "#ff0000", "#00ff00")
                         } else {c("lightgrey", "#BB0021FF")
                         })+
  scale_x_continuous("")+scale_y_continuous("")+
  theme_bw()+ 
  labs(title = "VCAN")+
  theme(title = element_text(family = "Times New Roman",colour = "white",
                             size=12,face="bold"))+
  theme(  
    panel.border = element_blank(),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(),axis.text = element_blank(),  
    legend.text = element_text(family = "Times New Roman",size=12,face="bold"),
    legend.position = "none",  
    plot.title = element_text(hjust = 0.5)  
  );mono2_none

Monocyte_none = mono1_none/mono2_none+plot_annotation(title = "Monocyte",
                                                      theme= theme(
                                                        plot.title=element_text(
                                                          colour = "white",
                                                          family = "Times New Roman",size=18,
                                                          face="bold",hjust = 0.5)
                                                      ));Monocyte_none
#DC NCR1 CLEC9A
DC1_none = FeaturePlot(scRNA_harmony2,features = "XCR1",
                       min.cutoff = 0,
                       max.cutoff = 2,
                       reduction = "tsne",pt.size = 0.5,cols = if (FALSE) {
                         c("lightgrey", "#ff0000", "#00ff00")
                       } else {c("lightgrey", "#BB0021FF")
                       })+
  scale_x_continuous("")+scale_y_continuous("")+
  theme_bw()+ 
  labs(title = "XCR1")+
  theme(title = element_text(family = "Times New Roman",colour = "white",
                             size=12,face="bold"))+
  theme(  
    panel.border = element_blank(),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(),axis.text = element_blank(),  
    legend.text = element_text(family = "Times New Roman",size=12,face="bold"),
    legend.position = "none",  
    plot.title = element_text(hjust = 0.5)  
  );DC1_none
DC2_none = FeaturePlot(scRNA_harmony2,features = "CLEC9A",
                       min.cutoff = 0,
                       max.cutoff = 2,
                       reduction = "tsne",pt.size = 0.5,cols = if (FALSE) {
                         c("lightgrey", "#ff0000", "#00ff00")
                       } else {c("lightgrey", "#BB0021FF")
                       })+
  scale_x_continuous("")+scale_y_continuous("")+
  theme_bw()+ 
  labs(title = "CLEC9A")+
  theme(title = element_text(family = "Times New Roman",colour = "white",
                             size=12,face="bold"))+
  theme(  
    panel.border = element_blank(),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(),axis.text = element_blank(),  
    legend.text = element_text(family = "Times New Roman",size=12,face="bold"),
    legend.position = "none",  
    plot.title = element_text(hjust = 0.5)  
  );DC2_none

DC_none = DC1_none/DC2_none+plot_annotation(title = "DC",
                                            theme= theme(
                                              plot.title=element_text(colour = "white",
                                                                      family = "Times New Roman",size=18,
                                                                      face="bold",hjust = 0.5)
                                            ));DC_none
#B MS4A1 CD79A
B1_none = FeaturePlot(scRNA_harmony2,features = "MS4A1",
                      min.cutoff = 0,
                      max.cutoff = 2,
                      reduction = "tsne",pt.size = 0.5,cols = if (FALSE) {
                        c("lightgrey", "#ff0000", "#00ff00")
                      } else {c("lightgrey", "#BB0021FF")
                      })+
  scale_x_continuous("")+scale_y_continuous("")+
  theme_bw()+ 
  labs(title = "MS4A1")+
  theme(title = element_text(family = "Times New Roman",colour = "white",
                             size=12,face="bold"))+
  theme(  
    panel.border = element_blank(),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(),axis.text = element_blank(),  
    legend.text = element_text(family = "Times New Roman",size=12,face="bold"),
    legend.position = "none",  
    plot.title = element_text(hjust = 0.5)  
  );B1_none
B2_none = FeaturePlot(scRNA_harmony2,features = "CD79A",
                      min.cutoff = 0,
                      max.cutoff = 2,
                      reduction = "tsne",pt.size = 0.5,cols = if (FALSE) {
                        c("lightgrey", "#ff0000", "#00ff00")
                      } else {c("lightgrey", "#BB0021FF")
                      })+
  scale_x_continuous("")+scale_y_continuous("")+
  theme_bw()+ 
  labs(title = "CD79A")+
  theme(title = element_text(family = "Times New Roman",color = "white",
                             size=12,face="bold"))+
  theme(  
    panel.border = element_blank(),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(),axis.text = element_blank(),  
    legend.text = element_text(family = "Times New Roman",size=12,face="bold"),
    legend.position = "none",  
    plot.title = element_text(hjust = 0.5)  
  );B2_none

B_none = B1_none/B2_none+plot_annotation(title = "B Cell",
                                         theme= theme(
                                           plot.title=element_text(colour = "white",
                                                                   family = "Times New Roman",size=18,
                                                                   face="bold",hjust = 0.5)
                                         ));B_none

#Mast   TPSAB1 MS4A2
Mast1_none = FeaturePlot(scRNA_harmony2,features = "TPSAB1",
                         min.cutoff = 0,
                         max.cutoff = 2,
                         reduction = "tsne",pt.size = 0.5,cols = if (FALSE) {
                           c("lightgrey", "#ff0000", "#00ff00")
                         } else {c("lightgrey", "#BB0021FF")
                         })+
  scale_x_continuous("")+scale_y_continuous("")+
  theme_bw()+ 
  labs(title = "TPSAB1")+
  theme(title = element_text(family = "Times New Roman",colour = "white",
                             size=12,face="bold"))+
  theme(  
    panel.border = element_blank(),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(),axis.text = element_blank(),  
    legend.text = element_text(family = "Times New Roman",size=12,face="bold"),
    legend.position = "none",  
    plot.title = element_text(hjust = 0.5)  
  );Mast1_none
Mast2_none = FeaturePlot(scRNA_harmony2,features = "MS4A2",
                         min.cutoff = 0,
                         max.cutoff = 2,
                         reduction = "tsne",pt.size = 0.5,cols = if (FALSE) {
                           c("lightgrey", "#ff0000", "#00ff00")
                         } else {c("lightgrey", "#BB0021FF")
                         })+
  scale_x_continuous("")+scale_y_continuous("")+
  theme_bw()+ 
  labs(title = "MS4A2")+
  theme(title = element_text(family = "Times New Roman",color="white",size=12,face="bold"))+
  theme(  
    panel.border = element_blank(),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(),axis.text = element_blank(),  
    legend.text = element_text(family = "Times New Roman",size=12,face="bold"),
    legend.position = "none",  
    plot.title = element_text(hjust = 0.5)  
  );Mast2_none

Mast_none = Mast1_none/Mast2_none+plot_annotation(title = "Mast",
                                                  theme= theme(
                                                    plot.title=element_text(
                                                      colour = "white",
                                                      family = "Times New Roman",size=18,
                                                      face="bold",hjust = 0.5)
                                                  ));Mast_none

##Endothelial TIMP3 IGFBP4
Endothelial1_none = FeaturePlot(scRNA_harmony2,features = "TIMP3",
                                min.cutoff = 0,
                                max.cutoff = 2,
                                reduction = "tsne",pt.size = 0.5,cols = if (FALSE) {
                                  c("lightgrey", "#ff0000", "#00ff00")
                                } else {c("lightgrey", "#BB0021FF")
                                })+
  scale_x_continuous("")+scale_y_continuous("")+
  theme_bw()+ 
  labs(title = "TIMP3")+
  theme(title = element_text(family = "Times New Roman",colour = "white",
                             size=12,face="bold"))+
  theme(  
    panel.border = element_blank(),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(),axis.text = element_blank(),  
    legend.text = element_text(family = "Times New Roman",size=12,face="bold"),
    legend.position = "none",  
    plot.title = element_text(hjust = 0.5)  
  );Endothelial1_none
Endothelial2_none = FeaturePlot(scRNA_harmony2,features = "IGFBP4",
                                min.cutoff = 0,
                                max.cutoff = 2,
                                reduction = "tsne",pt.size = 0.5,cols = if (FALSE) {
                                  c("lightgrey", "#ff0000", "#00ff00")
                                } else {c("lightgrey", "#BB0021FF")
                                })+
  scale_x_continuous("")+scale_y_continuous("")+
  theme_bw()+ 
  labs(title = "IGFBP4")+
  theme(title = element_text(family = "Times New Roman",colour = "white",
                             size=12,face="bold"))+
  theme(  
    panel.border = element_blank(),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(),axis.text = element_blank(),  
    legend.text = element_text(family = "Times New Roman",size=12,face="bold"),
    legend.position = "none",  
    plot.title = element_text(hjust = 0.5)  
  );Endothelial2_none

#
Endothelial3 = FeaturePlot(scRNA_harmony2,features = "TIMP3",
                           min.cutoff = 0,
                           max.cutoff = 2,
                           reduction = "tsne",pt.size = 0.5,cols = if (FALSE) {
                             c("white", "white", "white")
                           } else {c("lightgrey", "#BB0021FF")
                           })+
  scale_x_continuous("")+scale_y_continuous("")+
  theme_bw()+ 
  labs(title = "TIMP3")+
  theme(title = element_text(family = "Times New Roman",color = "white",size=12,face="bold"))+
  theme(  
    panel.border = element_blank(),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(),axis.text = element_blank(),  
    legend.text = element_text(family = "Times New Roman",size=12,face="bold"),
    #legend.position = "none",  
    plot.title = element_text(hjust = 0.5)  
  );Endothelial3

Endothelial_none = Endothelial1_none/Endothelial2_none+plot_annotation(title = "Endothelial",
                                                                       theme= theme(
                                                                         plot.title=element_text(colour = "white",
                                                                                                 family = "Times New Roman",size=18,
                                                                                                 face="bold",hjust = 0.5)))

F_all_none=plot_grid(Macrophage_none,dNK_none,T_none,Monocyte_none,DC_none,B_none,Mast_none,Endothelial_none,ncol = 8)