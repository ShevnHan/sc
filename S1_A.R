library(jpeg)
library(cowplot)
library(ggSCvis)
library(ggsci)
library(scales)
library(patchwork)
library(tidyverse)
library(grid)
library(ggpubr)
library(ggrepel)
red_1_sample="#ED0000FF"
blue_1_sample="#4DBBD5FF"
pal_aaas=pal_aaas()(10)
blue1=pal_aaas[1]
pal_locuszoom=pal_locuszoom()(7)
show_col(pal_locuszoom)
red1=pal_locuszoom[1]
show_col(c(red1,blue1))
colnames(scRNA_harmony2@meta.data)

#nFeature
violin_nFeature <- VlnPlot(scRNA_harmony2,
                           group.by = "orig.ident",
                           features = "nFeature_RNA", 
                           cols =c(red1,blue1), 
                           pt.size = 0.01 #
)+
  theme_bw()+
  theme(axis.title.x = element_blank())+
  theme(title =  element_text(size=12, colour = "black",face=c("bold"),
                              family="Times New Roman",hjust = 0.5))+
  theme(legend.position = "none")+
  theme(axis.text.y = element_text(size=12, colour = "black",face=c("bold"),
                                   family="Times New Roman"))+
  theme(axis.text.x = element_text(size=12, colour = "black",face=c("bold"),
                                   family="Times New Roman"));violin_nFeature

#nCount_RNA
violin_nCount <- VlnPlot(scRNA_harmony2,
                         group.by = "orig.ident",
                         features = "nCount_RNA", 
                         cols =c(red1,blue1), 
                         pt.size = 0.01 #
)+
  theme_bw()+
  theme(axis.title.x = element_blank())+
  theme(title = element_text(size=12, colour = "black",face=c("bold"),
                             family="Times New Roman",hjust = 0.5))+
  theme(legend.position = "none")+
  theme(axis.text.y = element_text(size=12, colour = "black",face=c("bold"),
                                   family="Times New Roman"))+
  theme(axis.text.x = element_text(size=12, colour = "black",face=c("bold"),
                                   family="Times New Roman"));violin_nCount

##percent.mt
violin_mt <- VlnPlot(scRNA_harmony2,
                     group.by = "orig.ident",
                     features = "percent.mt", 
                     cols =c(red1,blue1), 
                     pt.size = 0.01 #
)+
  theme_bw()+
  theme(axis.title.x = element_blank())+
  theme(title = element_text(size=12, colour = "black",face=c("bold"),
                             family="Times New Roman",hjust = 0.5))+
  theme(legend.position = "none")+
  theme(axis.text.y = element_text(size=12, colour = "black",face=c("bold"),
                                   family="Times New Roman"))+
  theme(axis.text.x = element_text(size=12, colour = "black",face=c("bold"),
                                   family="Times New Roman"));violin_mt


p1_violin = violin_nFeature | violin_nCount | violin_mt


#non-character
#nFeature
violin_nFeature_none <- VlnPlot(scRNA_harmony2,
                                group.by = "orig.ident",
                                features = "nFeature_RNA", 
                                cols =c(red1,blue1), 
                                pt.size = 0.01 #
)+
  theme_bw()+
  theme(title =  element_text(size=12, colour = "white",face=c("bold"),
                              family="Times New Roman",hjust = 0.5))+
  theme(legend.position = "none")+
  theme(axis.text.y = element_text(size=12, colour = "white",face=c("bold"),
                                   family="Times New Roman"))+
  theme(axis.text.x = element_text(size=12, colour = "white",face=c("bold"),
                                   family="Times New Roman"));violin_nFeature_none

#nCount_RNA
violin_nCount_none <- VlnPlot(scRNA_harmony2,
                              group.by = "orig.ident",
                              features = "nCount_RNA", 
                              cols =c(red1,blue1), 
                              pt.size = 0.01 #
)+
  theme_bw()+
  theme(title = element_text(size=12, colour = "white",face=c("bold"),
                             family="Times New Roman",hjust = 0.5))+
  theme(legend.position = "none")+
  theme(axis.text.y = element_text(size=12, colour = "white",face=c("bold"),
                                   family="Times New Roman"))+
  theme(axis.text.x = element_text(size=12, colour = "white",face=c("bold"),
                                   family="Times New Roman"));violin_nCount_none

##percent.mt
violin_mt_none <- VlnPlot(scRNA_harmony2,
                          group.by = "orig.ident",
                          features = "percent.mt", 
                          cols =c(red1,blue1), 
                          pt.size = 0.01 #
)+
  theme_bw()+
  theme(title = element_text(size=12, colour = "white",face=c("bold"),
                             family="Times New Roman",hjust = 0.5))+
  theme(legend.position = "none")+
  theme(axis.text.y = element_text(size=12, colour = "white",face=c("bold"),
                                   family="Times New Roman"))+
  theme(axis.text.x = element_text(size=12, colour = "white",face=c("bold"),
                                   family="Times New Roman"));violin_mt_none

p1_violin_none = violin_nFeature_none | violin_nCount_none | violin_mt_none

p1_violin_none

