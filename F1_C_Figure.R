###F1_C
library(reshape2)
library(RColorBrewer)
library(ggvis)
df <- table(pbmc_T@meta.data$celltype,pbmc_T@meta.data$orig.ident) %>% melt()
colnames(df) <- c("Cluster","Sample","Number")
df$Cluster <- factor(df$Cluster)
df$Type=factor(df$Cluster,levels = c("Treg","CD4+T Cell","CD8+T Cell"))
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
sample_color <- col_vector[]
p1=ggplot(data = df, aes(x =Number, y = Type, fill =  Sample)) +
  geom_bar(stat = "identity", width=0.3,position="fill")+
  scale_fill_manual(values=c(red1,blue1)) +
  theme_bw()+
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(colour = "black",linewidth = 0.5))+
  theme(panel.grid =element_blank()) +
  labs(x="Fraction of cells",y="")+
  theme(text = element_text(family = "Times New Roman",size=12,face = c("bold")))+
  ####用来将y轴移动位置
  theme(axis.text.y = element_text(size=12, colour = "black",face=c("bold"),
                                   family="Times New Roman"))+
  theme(axis.text.x = element_text(size=12, colour = "black",face=c("bold"),
                                   family="Times New Roman"))+
  theme(
    axis.text.x.bottom = element_text(hjust = 0.1, vjust = 0.2, angle = 0)
  )+
  theme(legend.title = element_text(size=15,
                                    face=c("bold"),
                                    family="Times New Roman"))+
  theme(legend.text =element_text(size=10, 
                                  face=c("bold"),
                                  family="Times New Roman"
  ) );p1
p1=p1+theme(legend.position = "none")+
  scale_x_continuous(position = "top")+
  geom_vline(xintercept =0.50, color="black", linetype="dashed");p1
p2=ggplot(data = df, aes(x =Number, y = Type, fill =  Sample)) +
  geom_bar(stat = "identity", width=0.3)+
  scale_fill_manual(values=c(red1,blue1)) +
  theme_bw()+
  theme(panel.border = element_blank())+
  theme(axis.line =element_line(colour = "black",linewidth = 0.5) )+
  theme(panel.grid =element_blank()) +
  labs(x="Number of cells",y="")+
  theme(text = element_text(family = "Times New Roman",size=12,face = c("bold")))+
  ###move y
  theme(axis.text.y = element_text(size=12, colour = "black",face=c("bold"),
                                   family="Times New Roman"))+
  theme(axis.text.x = element_text(size=12, colour = "black",face=c("bold"),
                                   family="Times New Roman"))+
  theme(
    axis.text.x.bottom = element_text(hjust = 0.1, vjust = 0.2, angle = 0)
  )+
  theme(legend.title = element_text(size=15,
                                    face=c("bold"),
                                    family="Times New Roman"))+
  theme(legend.text =element_text(size=10, 
                                  face=c("bold"),
                                  family="Times New Roman"
  ) );p2
p2=p2+theme(axis.text.y=element_blank())+
  scale_x_continuous(position = "top");p2
p_rate = p1 | p2
p_rate


###non_character
p1_none=ggplot(data = df, aes(x =Number, y = Type, fill =  Sample)) +
  geom_bar(stat = "identity", width=0.3,position="fill")+
  scale_fill_manual(values=c(red1,blue1)) +
  theme_bw()+
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(colour = "black",linewidth = 0.5))+
  theme(panel.grid =element_blank()) +
  labs(x="Fraction of cells",y="")+
  theme(text = element_text(family = "Times New Roman",size=12,color = "white",face = c("bold")))+
  ####move y
  theme(axis.text.y = element_text(size=12, colour = "white",face=c("bold"),
                                   family="Times New Roman"))+
  theme(axis.text.x = element_text(size=12, colour = "white",face=c("bold"),
                                   family="Times New Roman"))+
  theme(
    axis.text.x.bottom = element_text(hjust = 0.1, vjust = 0.2, angle = 0)
  )+
  theme(legend.title = element_text(size=15,
                                    face=c("bold"),
                                    colour = "white",
                                    family="Times New Roman"))+
  theme(legend.text =element_text(size=10, 
                                  face=c("bold"),
                                  colour = "white",
                                  family="Times New Roman"
  ) );p1_none
p1_none=p1_none+theme(legend.position = "none")+
  scale_x_continuous(position = "top")+
  geom_vline(xintercept =0.50, color="black", linetype="dashed");p1_none
p2_none=ggplot(data = df, aes(x =Number, y = Type, fill =  Sample)) +
  geom_bar(stat = "identity", width=0.3)+
  scale_fill_manual(values=c(red1,blue1)) +
  theme_bw()+
  theme(panel.border = element_blank())+
  theme(axis.line =element_line(colour = "black",linewidth = 0.5) )+
  theme(panel.grid =element_blank()) +
  labs(x="Number of cells",y="")+
  theme(text = element_text(family = "Times New Roman",colour = "white",
                            size=12,face = c("bold")))+
  theme(axis.text.y = element_text(size=12, colour = "white",face=c("bold"),
                                   family="Times New Roman"))+
  theme(axis.text.x = element_text(size=12, colour = "white",face=c("bold"),
                                   family="Times New Roman"))+
  theme(
    axis.text.x.bottom = element_text(hjust = 0.1, vjust = 0.2, angle = 0)
  )+
  theme(legend.title = element_text(size=15,
                                    face=c("bold"),
                                    colour = "white",
                                    family="Times New Roman"))+
  theme(legend.text =element_text(size=10, 
                                  face=c("bold"),
                                  colour = "white",
                                  family="Times New Roman"
  ) );p2_none
p2_none=p2_none+theme(axis.text.y=element_blank())+
  scale_x_continuous(position = "top");p2_none
p_rate_none = p1_none | p2_none
p_rate_none
