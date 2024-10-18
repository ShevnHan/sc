genes_to_check = c("ITGAX")
genes_to_check2<-as.data.frame(genes_to_check)
marker_gene<-genes_to_check2

vln.data=as.data.frame(pbmc_T@assays$RNA$data)[marker_gene$genes_to_check,]
vln.data<-na.omit(vln.data)

library(data.table)
vln.data$gene=rownames(vln.data)
vln.data=melt(vln.data,id="gene")
colnames(vln.data)[c(2,3)]=c("cell","exp")


data_marker=pbmc_T@meta.data[,c("cell","celltype")]
vln.data=merge(vln.data,data_marker,by="cell")
rownames(vln.data) = vln.data$Row.names
data_group=pbmc_T@meta.data[,c("cell","orig.ident")]
vln.data=merge(vln.data,data_group,by="cell")
color<-color

vln.data=subset(vln.data,celltype=="CD8+T Cell")
colnames(vln.data)
colnames(vln.data)=c("cell","gene","exp","celltype","Sample")
p<-ggplot(vln.data,aes(gene,exp))+geom_violin(aes(fill=Sample),scale = "width")+
  scale_fill_manual(values = c(red1,blue1))+
  scale_x_discrete("")+scale_y_continuous("")+
  theme_bw()+
  ylim(c(0,4))+
  theme(panel.grid = element_blank())+
  theme(
    axis.text.x.bottom = element_text(angle = 0,hjust = 0.5,vjust = 1,face=c("plain"),
                                      family="Times New Roman"),
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    #legend.position = "none",
    axis.text.y = element_text(color="black",size=12,
                               face = c("bold"),family = "Times New Roman"),
    
    axis.text.x = element_text(color = 'black',size = 12,
                               face = c("bold"),family = "Times New Roman"),
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
                                    colour = "black",
                                    face=c("bold"),
                                    family="Times New Roman"))+
  theme(legend.text =element_text(size=10, 
                                  colour = "black",
                                  face=c("bold"),
                                  family="Times New Roman"
  ) );p
