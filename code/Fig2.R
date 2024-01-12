rm(list=ls())
#调用包
# .libPaths("/home/renmy/R/x86_64-pc-linux-gnu-library/4.3")
#load packages
pacman::p_load(
        "gmodels",
        "rstatix",
        "foreign",
        "haven",
        "openxlsx",
        "tidyverse",
        "naniar",
        "Hmisc",
        "mice",
        "tableone",
        "caret",
        "magrittr",
        "R6",
        "lubridate",
        "tictoc",
        "vroom",
        "rstatix",
        "ggplot2",
        "ggfortify",
        "GGally",
        "zip",
        "ddpcr",
        "grid",
        "flextable",
        "ggrepel",
        "paletteer",
        "furrr",
        "rlist",
        "readxl",
        "writexl",
        "bkmr")
# 0-Prepare step ----------------------------------
#set work directory
inpath = stringr::str_c(getwd(), "/@-IVF-Hair-Serum-FF/input")
outpath = stringr::str_c(getwd(), "/@-IVF-Hair-Serum-FF/output")

if(!file.exists(inpath)){dir.create(inpath)}
if(!file.exists(outpath)){dir.create(outpath)}

# 1-Load data---------------------------
readxl::read_xlsx(str_c(outpath, "/2.MetalDescript/3-Correlation.results.xlsx")) -> plotdata
plotdata %>% 
  dplyr::filter(p < 0.05)

unique(c(plotdata$var1,plotdata$var2))

#1. Nodes
unique(c(plotdata$var1,plotdata$var2)) %>%
  as_tibble() %>%
  dplyr::mutate(id = value, label = value) %>%
  dplyr::mutate(group = case_when(
    str_detect(id, "_1$") ~ "Serum #1",
    str_detect(id, "_2$") ~ "Serum #2",
    str_detect(id, "_F$") ~ "Follicular Fluid",
    str_detect(id, "_H1$") ~ "Hair #1",
    str_detect(id, "_H2$") ~ "Hair #2",
    str_detect(id, "_H3$") ~ "Hair #3",
    str_detect(id, "_H4$") ~ "Hair #4"),
    color = group) %>%
  dplyr::distinct(id,.keep_all = TRUE) %>%
  dplyr::select(id,label,group,color) -> nodes

#2. nodes size
load(str_c(outpath,"/1-Metal.Stats.Rdata"))
cordata
F1
purrr::map2_dfr("X1生化妊娠_Y0_N1",
                unique(c(plotdata$var1,plotdata$var2)) ,
                F1) -> node.size
node.size %>%
  dplyr::mutate(MinusLogP = -log(p+0.00000001),
                id = var2) %>%
  dplyr::select(MinusLogP) %>%
  bind_cols(nodes) %>%
  dplyr::select(id, label, group, color, MinusLogP) -> nodes
nodes %>% 
  dplyr::mutate(size = case_when(
        MinusLogP <= 0.4 ~ 5,
        MinusLogP > 0.4 & MinusLogP <= 0.8 ~ 20,
        MinusLogP > 0.8 & MinusLogP <= 1.5 ~ 40,
        MinusLogP > 1.5 ~ 60
  )) -> nodes


#3. Edges
plotdata %>% 
    dplyr::mutate(
      group1 = case_when(
        str_detect(var1, "_1$") ~ "Serum #1",
        str_detect(var1, "_2$") ~ "Serum #2",
        str_detect(var1, "_F$") ~ "Follicular Fluid",
        str_detect(var1, "_H1$") ~ "Hair #1",
        str_detect(var1, "_H2$") ~ "Hair #2",
        str_detect(var1, "_H3$") ~ "Hair #3",
        str_detect(var1, "_H4$") ~ "Hair #4"),
      group2 = case_when(
        str_detect(var2, "_1$") ~ "Serum #1",
        str_detect(var2, "_2$") ~ "Serum #2",
        str_detect(var2, "_F$") ~ "Follicular Fluid",
        str_detect(var2, "_H1$") ~ "Hair #1",
        str_detect(var2, "_H2$") ~ "Hair #2",
        str_detect(var2, "_H3$") ~ "Hair #3",
        str_detect(var2, "_H4$") ~ "Hair #4")) -> plotdata

plotdata %>%
  dplyr::filter(p < 0.05) %>%
  dplyr::mutate(interaction = "association") %>%
  dplyr::mutate(source.class = group1) %>%
  dplyr::mutate(target.class = group2) %>%
  dplyr::mutate(edge.color = "black") %>%
  dplyr::select(var1,var2,interaction,source.class,target.class,edge.color) %>%
  setNames(c("source","target","interaction","source.class","target.class","edge.color")) %>%
  dplyr::mutate(dep = stringr::str_c(source,"-",target)) %>%
  dplyr::distinct(dep,.keep_all = TRUE) -> edges

#4. export
writexl::write_xlsx(nodes,
                    str_c(outpath2, "/Figure-3A.Nodes.for.Plotting.xlsx"))
writexl::write_xlsx(edges,
                    str_c(outpath2, "/Figure-3A.Edges.for.Plotting.xlsx"))

# # Open sytoscape software for drawing Fig2A----------------------------------------------------------
# install.packages("RCy3")
# library("RCy3")
# RCy3::cytoscapePing()
# RCy3::closeSession(F)
# RCy3::createNetworkFromDataFrames(nodes,
#                                   edges,
#                                   title = "GO",
#                                   collection = "biolinker")

# #default style--------------------------------------------------------
# nodes$group %>%
#   unique()
# nodes$MinusLogP %>%
#   summary()
# style = "Style1"
# defaults <- list(NODE_SHAPE="ELLIPSE",NODE_SIZE=20,
#                  EDGE_TRANSPARENCY=225,NODE_LABEL_POSITION="S,W,c,0.00,0.00")
# nodeLabels <- RCy3::mapVisualProperty('node label','label','p')
# nodeShape <- RCy3::mapVisualProperty('Node Shape','group',"d",
#                                      nodes$group %>%
#                                        unique(),
#                                      c("ELLIPSE", "ELLIPSE", "ELLIPSE",
#                                        "ELLIPSE", "ELLIPSE", "ELLIPSE"))
# nodeSize <- RCy3::mapVisualProperty('Node Size','MinusLogP',"c",
#                                     c(0.01106, 6.83078),
#                                     c(100, 600))
# nodeLableFontSize <- RCy3::mapVisualProperty('Node Label Font Size','group',"d",
#                                              nodes$group %>%
#                                                unique(),
#                                              c(30, 30, 25, 30, 30, 30))
# nodeWidth <- RCy3::mapVisualProperty('Node Width','group',"d",
#                                      nodes$group %>%
#                                        unique(),
#                                      c(30, 30, 30, 30, 30, 30))
# nodeZLocation <- RCy3::mapVisualProperty('Node Z Location',"group",'d',
#                                          nodes$group %>%
#                                            unique(),
#                                          c(6, 5, 4, 3, 2, 1))
# RCy3::createVisualStyle(style,
#                         defaults,
#                         list(nodeLabels,
#                              nodeShape,
#                              nodeSize,
#                              nodeLableFontSize,
#                              nodeWidth,
#                              nodeZLocation))
# RCy3::setVisualStyle(style)

# #plot---------------------
# RCy3::setNodeColorMapping(mapping.type = 'd',
#                           table.column = 'group',
#                           table.column.values = nodes$group %>%
#                             unique(),
#                           c("#D64358","#4EB3D3","#005A32",
#                             "#D64358","#4EB3D3","#005A32"),
#                           style.name = style)




#---------------------------------------------------------------------------------
# Fig2B---------------------------
plotdata
plotdata %>% 
  dplyr::filter(p < 0.05) -> fig2b
fig2b %>% 
  dplyr::mutate(newgrp1 = case_when(
        group1 == "Hair #4" ~ "9-12 months ago",
        group1 == "Hair #3" ~ "6-9 months ago",
        group1 == "Hair #2" ~ "3-6 months ago",
        group1 == "Hair #1" ~ "0-3 months ago",
        group1 == "Serum #1" ~ "Recruitment",
        group1 == "Serum #2" ~ "Day of Oocyte retrieval",
        group1 == "Follicular Fluid" ~ "Day of Oocyte retrieval"),
        newgrp2 = case_when(
        group2 == "Hair #4" ~ "9-12 months ago",
        group2 == "Hair #3" ~ "6-9 months ago",
        group2 == "Hair #2" ~ "3-6 months ago",
        group2 == "Hair #1" ~ "0-3 months ago",
        group2 == "Serum #1" ~ "Recruitment",
        group2 == "Serum #2" ~ "Day of Oocyte retrieval",
        group2 == "Follicular Fluid" ~ "Day of Oocyte retrieval"))  -> b1


# install.packages("GlobalOptions")
# install.packages("circlize")
library(circlize)
#define color
color = c("9-12 months ago" = "#DEEBF7",
          "6-9 months ago" = "#C6DBEF",
          "3-6 months ago" = "#73A8CF",
          "0-3 months ago" = "#3590C0",
          "Recruitment" = "#FDE69F",
          "Day of Oocyte retrieval" = "#F4B300")
#re-group
table(b1$newgrp1, b1$newgrp2)  %>% 
  as.data.frame() %>% 
  arrange(Var1,Var2) %>% 
  dplyr::filter(Var1 == Var2)  -> plt1
table(b1$newgrp1, b1$newgrp2)  %>% 
  as.data.frame() %>% 
  arrange(Var1,Var2) %>% 
  dplyr::filter(Var1 != Var2) -> plt2
tws = c("9-12 months ago",
          "6-9 months ago" ,
          "3-6 months ago",
          "0-3 months ago" ,
          "Recruitment",
          "Day of Oocyte retrieval")
combn(tws, 2)%>% t() %>%
  as_tibble()  -> com
plt2 %>% 
  dplyr::filter((Var1==com$V1[1] & Var2==com$V2[1]) | (Var1==com$V2[1] & Var2==com$V1[1])) %>% 
  dplyr::select(Freq) %>% 
  sum()
F2 = function(i){
  tmp = com
  plt2 %>% 
    dplyr::filter((Var1==com$V1[i] & Var2==com$V2[i]) | (Var1==com$V2[i] & Var2==com$V1[i])) %>% 
    dplyr::select(Freq) %>% 
    sum() -> tmp[i,"value"]
  tmp %<>% 
    dplyr::filter(!is.na(value))
  return(tmp)
}
purrr::map_dfr(1:15, F2) %>% 
  set_names(c("Var1", "Var2", "Freq")) %>% 
  rbind(plt1 %>% as_tibble()) %>% 
  print(n=30)  -> plt

#plot Fig2B1
png(str_c(outpath2,"/Fig2B1.png"),width = 2400, height = 2400, res = 600)
plt %>% 
  chordDiagram(grid.col = color, 
               link.zindex = rank(plt$Freq),
               annotationTrack = c("name", "grid"),
               annotationTrackHeight = c(0.01, 0.02))
dev.off()
circos.clear()
png(str_c(outpath2,"/Fig2B1.without.labels.png"),width = 2400, height = 2400, res = 600)
plt %>% 
  chordDiagram(grid.col = color, 
               link.zindex = rank(plt$Freq),
               annotationTrack = c("grid"),
               annotationTrackHeight = c(0.01, 0.02))
dev.off()
circos.clear()


#plot Fig2B2
fig2b %>%  
     dplyr::mutate(newgrp1 = case_when(
        group1 == "Hair #4" ~ "Hair",
        group1 == "Hair #3" ~ "Hair",
        group1 == "Hair #2" ~ "Hair",
        group1 == "Hair #1" ~ "Hair",
        group1 == "Serum #1" ~ "Serum",
        group1 == "Serum #2" ~ "Serum",
        group1 == "Follicular Fluid" ~ "Follicular Fluid"),
        newgrp2 = case_when(
        group2 == "Hair #4" ~ "Hair",
        group2 == "Hair #3" ~ "Hair",
        group2 == "Hair #2" ~ "Hair",
        group2 == "Hair #1" ~ "Hair",
        group2 == "Serum #1" ~ "Serum",
        group2 == "Serum #2" ~ "Serum",
        group2 == "Follicular Fluid" ~ "Follicular Fluid")) -> b2
color2 = c("Hair" = "#73A8CF",
           "Serum" = "#FDE69F",
           "Follicular Fluid" = "#FC8D62")
#plot Fig2B2
png(str_c(outpath2,"/Fig2B2.png"),width = 2400, height = 2400, res = 600)
table(b2$newgrp1, b2$newgrp2) %>% 
      chordDiagram(grid.col = color2, 
                  link.zindex = rank(table(b2$newgrp1, b2$newgrp2)),
                  annotationTrack = c("name", "grid"),
                  annotationTrackHeight = c(0.01, 0.02)) 
dev.off()
circos.clear()
png(str_c(outpath2,"/Fig2B2.without.labels.png"),width = 2400, height = 2400, res = 600)
table(b2$newgrp1, b2$newgrp2) %>% 
      chordDiagram(grid.col = color2, 
                  link.zindex = rank(table(b2$newgrp1, b2$newgrp2)),
                  annotationTrack = c("grid"),
                  annotationTrackHeight = c(0.01, 0.02)) 
dev.off()
circos.clear()

#Fig2C------------------
histogram(abs(fig2b$cor))
#Hair #1
fig2b %>% 
  dplyr::filter(group1=="Hair #1"|group2=="Hair #1") %>% 
  dplyr::mutate(cor = abs(cor)) %>% 
  dplyr::mutate(corsize = case_when(
          cor <= 0.2 ~ "0-0.2",
          cor > 0.2 & cor <= 0.6 ~ "0.2-0.6",
          cor > 0.6 ~ ">0.6"
  )) %>% 
  count(by = corsize) %>% 
  dplyr::mutate(group = "Hair #1") %>% 
rbind(
fig2b %>% 
  dplyr::filter(group1=="Hair #2"|group2=="Hair #2") %>% 
  dplyr::mutate(cor = abs(cor)) %>% 
  dplyr::mutate(corsize = case_when(
          cor <= 0.2 ~ "0-0.2",
          cor > 0.2 & cor <= 0.6 ~ "0.2-0.6",
          cor > 0.6 ~ ">0.6"
  )) %>% 
  count(by = corsize) %>% 
  dplyr::mutate(group = "Hair #2") 
) %>% 
rbind(
fig2b %>% 
  dplyr::filter(group1=="Hair #3"|group2=="Hair #3") %>% 
  dplyr::mutate(cor = abs(cor)) %>% 
  dplyr::mutate(corsize = case_when(
          cor <= 0.2 ~ "0-0.2",
          cor > 0.2 & cor <= 0.6 ~ "0.2-0.6",
          cor > 0.6 ~ ">0.6"
  )) %>% 
  count(by = corsize) %>% 
  dplyr::mutate(group = "Hair #3") 
)  %>% 
rbind(
fig2b %>% 
  dplyr::filter(group1=="Hair #4"|group2=="Hair #4") %>% 
  dplyr::mutate(cor = abs(cor)) %>% 
  dplyr::mutate(corsize = case_when(
          cor <= 0.2 ~ "0-0.2",
          cor > 0.2 & cor <= 0.6 ~ "0.2-0.6",
          cor > 0.6 ~ ">0.6"
  )) %>% 
  count(by = corsize) %>% 
  dplyr::mutate(group = "Hair #4") 
) %>% 
rbind(
fig2b %>% 
  dplyr::filter(group1=="Serum #1"|group2=="Serum #1") %>% 
  dplyr::mutate(cor = abs(cor)) %>% 
  dplyr::mutate(corsize = case_when(
          cor <= 0.2 ~ "0-0.2",
          cor > 0.2 & cor <= 0.6 ~ "0.2-0.6",
          cor > 0.6 ~ ">0.6"
  )) %>% 
  count(by = corsize) %>% 
  dplyr::mutate(group = "Serum #1") 
)  %>% 
rbind(
fig2b %>% 
  dplyr::filter(group1=="Serum #2"|group2=="Serum #2") %>% 
  dplyr::mutate(cor = abs(cor)) %>% 
  dplyr::mutate(corsize = case_when(
          cor <= 0.2 ~ "0-0.2",
          cor > 0.2 & cor <= 0.6 ~ "0.2-0.6",
          cor > 0.6 ~ ">0.6"
  )) %>% 
  count(by = corsize) %>% 
  dplyr::mutate(group = "Serum #2") 
)  %>% 
rbind(
fig2b %>% 
  dplyr::filter(group1=="Follicular Fluid"|group2=="Follicular Fluid") %>% 
  dplyr::mutate(cor = abs(cor)) %>% 
  dplyr::mutate(corsize = case_when(
          cor <= 0.2 ~ "0-0.2",
          cor > 0.2 & cor <= 0.6 ~ "0.2-0.6",
          cor > 0.6 ~ ">0.6"
  )) %>% 
  count(by = corsize) %>% 
  dplyr::mutate(group = "Follicular Fluid") 
)  -> fig2c
fig2c$by = factor(fig2c$by, levels = c("0-0.2","0.2-0.6",">0.6"))
fills = c("Hair #1" = "#3690C0",
          "Hair #2" = "#73A8CF",
          "Hair #3" = "#C6DBEF",
          "Hair #4" = "#DEEBF7",
          "Serum #1" = "#FDE69F",
          "Serum #2" = "#F4B300",
          "Follicular Fluid" = "#FC8D62")
fig2c %>% 
  arrange(by, -n) %>% 
  dplyr::mutate(order = row_number()) %>% 
  ggplot(aes(x = order, y = n, fill = group)) +
  geom_bar(stat = "identity") + 
  facet_grid(.~by, scale = "free_x") +
  scale_x_continuous(labels = NULL) +
  scale_y_continuous(limits = c(0, 1000),
                     expand=expansion(add = c(3, 3))) +
  scale_fill_manual(values = fills) + 
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.ticks.x = element_blank())


ggsave(str_c(outpath2, "/Fig2C.png"),
       height = 4,
       width = 12, 
       dpi  =600)



