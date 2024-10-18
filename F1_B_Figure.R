###Feature Plot
###CD4+T CD4
CD41 = FeaturePlot(pbmc_T,features = "CD4",
                   min.cutoff = 0,
                   max.cutoff = 2,
                   reduction = "tsne",pt.size = 0.5,cols = if (FALSE) {
                     c("lightgrey", "#ff0000", "#00ff00")
                   } else {c("lightgrey", "#BB0021FF")
                   })+
  scale_x_continuous("")+scale_y_continuous("")+
  theme_bw()+ 
  labs(title = "CD4")+
  theme(title = element_text(family = "Times New Roman",size=12,face="bold"))+
  theme(  
    panel.border = element_blank(),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(),axis.text = element_blank(),  
    legend.position = "none", 
    legend.text = element_text(family = "Times New Roman",size=12,face="bold"),
    #plot.title = element_blank()
    plot.title = element_text(hjust = 0.5,size=14)  
  );CD41


#IL7R
CD42=FeaturePlot(pbmc_T,features = "IL7R",
                 min.cutoff = 0,
                 max.cutoff = 2,
                 reduction = "tsne",pt.size = 0.5,cols = if (FALSE) {
                   c("lightgrey", "#ff0000", "#00ff00")
                 } else {c("lightgrey", "#BB0021FF")
                 })+
  scale_x_continuous("")+scale_y_continuous("")+
  theme_bw()+ 
  labs(title = "IL7R")+
  theme(title = element_text(family = "Times New Roman",size=12,face="bold"))+
  theme(  
    panel.border = element_blank(),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(),axis.text = element_blank(),  
    legend.position = "none",  
    #plot.title = element_blank()
    plot.title = element_text(hjust = 0.5,size=14)  
  );CD42

CD4 = CD41/CD42+plot_annotation(title = "CD4+T Cell",
                                theme= theme(
                                  plot.title=element_text(
                                    family = "Times New Roman",size=18,
                                    face="bold",hjust = 0.5)
                                ));CD4

#Treg IL2RA
Treg1=FeaturePlot(pbmc_T,features = "IL2RA",
                  min.cutoff = 0,
                  max.cutoff = 2,
                  reduction = "tsne",pt.size = 0.5,cols = if (FALSE) {
                    c("lightgrey", "#ff0000", "#00ff00")
                  } else {c("lightgrey", "#BB0021FF")
                  })+
  scale_x_continuous("")+scale_y_continuous("")+
  theme_bw()+ 
  labs(title = "IL2RA")+
  theme(title = element_text(family = "Times New Roman",size=12,face="bold"))+
  theme(  
    panel.border = element_blank(),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(),axis.text = element_blank(),  
    legend.position = "none", 
    #plot.title = element_blank()
    plot.title = element_text(hjust = 0.5,size=14)  
  );Treg1
#FOXP3
Treg2 = FeaturePlot(pbmc_T,features = "FOXP3",
                    min.cutoff = 0,
                    max.cutoff = 2,
                    reduction = "tsne",pt.size = 0.5,cols = if (FALSE) {
                      c("lightgrey", "#ff0000", "#00ff00")
                    } else {c("lightgrey", "#BB0021FF")
                    })+
  scale_x_continuous("")+scale_y_continuous("")+
  theme_bw()+
  labs(title = "FOXP3")+
  theme(title = element_text(family = "Times New Roman",size=12,face="bold"))+
  theme(  
    panel.border = element_blank(),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(),axis.text = element_blank(),  
    legend.position = "none",  
    #plot.title = element_blank()
    plot.title = element_text(hjust = 0.5,size=14)  
  );Treg2


Treg = Treg1/Treg2+plot_annotation(title = "Treg",
                                   theme= theme(
                                     plot.title=element_text(
                                       family = "Times New Roman",size=18,
                                       face="bold",hjust = 0.5)
                                   ));Treg

##CD8+T CD3D
CD81 = FeaturePlot(pbmc_T,features = "CD3D",
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
    legend.position = "none",  
    #plot.title = element_blank()
    plot.title = element_text(hjust = 0.5,size=14)  
  );CD81

#CD8A
CD82 = FeaturePlot(pbmc_T,features = "CD8A",
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
    legend.position = "none",  
    #plot.title = element_blank()
    plot.title = element_text(hjust = 0.5,size=14)  
  );CD82
CD8 = CD81/CD82+plot_annotation(title = "CD8+T Cell",
                                theme= theme(
                                  plot.title=element_text(
                                    family = "Times New Roman",size=18,
                                    face="bold",hjust = 0.5)
                                ));CD8

F_all = plot_grid(CD4,Treg,CD8,ncol = 3)


###CD4+T CD4
CD41_none = FeaturePlot(pbmc_T,features = "CD4",
                        min.cutoff = 0,
                        max.cutoff = 2,
                        reduction = "tsne",pt.size = 0.5,cols = if (FALSE) {
                          c("lightgrey", "#ff0000", "#00ff00")
                        } else {c("lightgrey", "#BB0021FF")
                        })+
  scale_x_continuous("")+scale_y_continuous("")+
  theme_bw()+ 
  labs(title = "CD4")+
  theme(title = element_text(family = "Times New Roman",colour = "white",
                             size=12,face="bold"))+
  theme(  
    panel.border = element_blank(),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(),axis.text = element_blank(),  
    legend.position = "none", 
    legend.text = element_text(family = "Times New Roman",size=12,face="bold"),
    #plot.title = element_blank()
    plot.title = element_text(hjust = 0.5,size=14)  
  );CD41_none


#IL7R
CD42_none=FeaturePlot(pbmc_T,features = "IL7R",
                      min.cutoff = 0,
                      max.cutoff = 2,
                      reduction = "tsne",pt.size = 0.5,cols = if (FALSE) {
                        c("lightgrey", "#ff0000", "#00ff00")
                      } else {c("lightgrey", "#BB0021FF")
                      })+
  scale_x_continuous("")+scale_y_continuous("")+
  theme_bw()+ 
  labs(title = "IL7R")+
  theme(title = element_text(family = "Times New Roman",color = "white",
                             size=12,face="bold"))+
  theme(  
    panel.border = element_blank(),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(),axis.text = element_blank(),  
    legend.position = "none",  
    #plot.title = element_blank()
    plot.title = element_text(hjust = 0.5,size=14)  
  );CD42_none

CD4_none = CD41_none/CD42_none+plot_annotation(title = "CD4+T Cell",
                                               theme= theme(
                                                 plot.title=element_text(colour = "white",
                                                                         family = "Times New Roman",size=18,
                                                                         face="bold",hjust = 0.5)
                                               ));CD4_none

#Treg IL2RA
Treg1_none=FeaturePlot(pbmc_T,features = "IL2RA",
                       min.cutoff = 0,
                       max.cutoff = 2,
                       reduction = "tsne",pt.size = 0.5,cols = if (FALSE) {
                         c("lightgrey", "#ff0000", "#00ff00")
                       } else {c("lightgrey", "#BB0021FF")
                       })+
  scale_x_continuous("")+scale_y_continuous("")+
  theme_bw()+ 
  labs(title = "IL2RA")+
  theme(title = element_text(family = "Times New Roman",color = "white",
                             size=12,face="bold"))+
  theme(  
    panel.border = element_blank(),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(),axis.text = element_blank(),  
    legend.position = "none", 
    #plot.title = element_blank()
    plot.title = element_text(hjust = 0.5,size=14)  
  );Treg1_none
#FOXP3
Treg2_none = FeaturePlot(pbmc_T,features = "FOXP3",
                         min.cutoff = 0,
                         max.cutoff = 2,
                         reduction = "tsne",pt.size = 0.5,cols = if (FALSE) {
                           c("lightgrey", "#ff0000", "#00ff00")
                         } else {c("lightgrey", "#BB0021FF")
                         })+
  scale_x_continuous("")+scale_y_continuous("")+
  theme_bw()+
  labs(title = "FOXP3")+
  theme(title = element_text(family = "Times New Roman",colour = "white",
                             size=12,face="bold"))+
  theme(  
    panel.border = element_blank(),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(),axis.text = element_blank(),  
    legend.position = "none",  
    #plot.title = element_blank()
    plot.title = element_text(hjust = 0.5,size=14)  
  );Treg2_none


Treg_none = Treg1_none/Treg2_none+plot_annotation(title = "Treg",
                                                  theme= theme(
                                                    plot.title=element_text(
                                                      color = "white",
                                                      family = "Times New Roman",size=18,
                                                      face="bold",hjust = 0.5)
                                                  ));Treg_none

##CD8+T CD3D
CD81_none = FeaturePlot(pbmc_T,features = "CD3D",
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
    legend.position = "none",  
    #plot.title = element_blank()
    plot.title = element_text(hjust = 0.5,size=14)  
  );CD81_none

#CD8A
CD82_none = FeaturePlot(pbmc_T,features = "CD8A",
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
    legend.position = "none",  
    #plot.title = element_blank()
    plot.title = element_text(hjust = 0.5,size=14)  
  );CD82_none
CD8_none = CD81_none/CD82_none+plot_annotation(title = "CD8+T Cell",
                                               theme= theme(
                                                 plot.title=element_text(
                                                   colour = "white",
                                                   family = "Times New Roman",size=18,
                                                   face="bold",hjust = 0.5)
                                               ));CD8_none


CD83 = FeaturePlot(pbmc_T,features = "CD8A",
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
    legend.text = element_text(family = "Times New Roman",colour = "white",
                               size=12,face="bold"),
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),  
    axis.ticks = element_blank(),axis.text = element_blank(),  
    #legend.position = "none",  
    #plot.title = element_blank()
    plot.title = element_text(hjust = 0.5,size=14)  
  );CD83
F_all_none=plot_grid(CD4_none,Treg_none,CD8_none,ncol = 3)