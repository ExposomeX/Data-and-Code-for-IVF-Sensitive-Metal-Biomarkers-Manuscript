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
        "rlist")

# 0-Prepare step ----------------------------------
#set work directory
inpath = stringr::str_c(getwd(), "/@-IVF-Hair-Serum-FF/input")
outpath = stringr::str_c(getwd(), "/@-IVF-Hair-Serum-FF/output")

if(!file.exists(inpath)){dir.create(inpath)}
if(!file.exists(outpath)){dir.create(outpath)}

#load data
load(str_c(outpath,"/1-Metal.Stats.Rdata"))


#plotdata
df_hair.metal %>% 
    dplyr::select(Ag_H1:Zn_H4) %>% 
    pivot_longer(cols = matches("_H\\d$"),
                 names_to = "metal",
                 values_to = "value") %>% 
    dplyr::mutate(Subgrp = case_when(
                        str_detect(metal, "_H1$") ~ "Hair #1",
                        str_detect(metal, "_H2$") ~ "Hair #2",
                        str_detect(metal, "_H3$") ~ "Hair #3",
                        str_detect(metal, "_H4$") ~ "Hair #4"
                        ),
                  metal = str_extract(metal, "\\w+(?=_)")) -> plotdata
# extract order of metals
outdf1 %>% 
    dplyr::filter(str_detect(Pollu, "_H1$")) %>% 
    dplyr::mutate(Pollu = str_extract(Pollu, "\\w+(?=_)")) %>% 
    arrange(-P90) %>% 
    dplyr::select(Pollu) %>% as.matrix() %>% as.vector() -> metal.order                  
plotdata %>% 
    dplyr::select(metal) %>% 
    unique()  %>% as.matrix() %>% as.vector() -> a1
metal.order %>% 
    as_tibble() %>% 
    dplyr::filter(value  %in%  a1) %>% as.matrix %>% as.vector -> metal.order

plotdata$metal = factor(plotdata$metal, levels = metal.order)

plotdata %>% 
    ggplot(aes(x = metal, y = log10(value), fill = Subgrp)) + 
        geom_boxplot(outlier.shape = NA, 
                     alpha = 0.6,
                     width = 0.7,
                     size = 0.25,
                     position = position_dodge(0.8)) + 
        scale_y_continuous( limits = c(-4, 3), 
                            expand=expansion(add = c(0.1, 0.1)),
                            breaks = c(-4,-3,-2,-1,0,1,2,3))+
    theme_bw() +
    theme(panel.grid = element_blank())
ggsave(str_c(outpath2, "/FigS2B.png"),
       width = 18,
       height = 6,
       dpi = 600)




#plotdata for serum1 serum2 and ff
df_serum1.metal %>% 
    dplyr::select(Ag_1:Zn_1) %>% 
    pivot_longer(cols = matches("_1"),
                 names_to = "metal",
                 values_to = "value") %>% 
    dplyr::mutate(Subgrp = "Serum #1",
                  metal = str_extract(metal, "\\w+(?=_)")) %>% 
rbind(
df_serum2.metal %>% 
    dplyr::select(Ag_2:Zn_2) %>% 
    pivot_longer(cols = matches("_2"),
                 names_to = "metal",
                 values_to = "value") %>% 
    dplyr::mutate(Subgrp = "Serum #2",
                  metal = str_extract(metal, "\\w+(?=_)"))
) %>% 
rbind(
df_ff.metal %>% 
    dplyr::select(Ag_F:Zn_F) %>% 
    pivot_longer(cols = matches("_F"),
                 names_to = "metal",
                 values_to = "value") %>% 
    dplyr::mutate(Subgrp = "Follicular fluid",
                  metal = str_extract(metal, "\\w+(?=_)"))
) -> plotdata
# extract order of metals (serum1)
outdf2 %>% 
    dplyr::mutate(Pollu = str_extract(Pollu, "\\w+(?=_)")) %>% 
    arrange(-P90) %>% 
    dplyr::select(Pollu) %>% as.matrix() %>% as.vector() -> metal.order
plotdata %>% 
    dplyr::select(metal) %>% 
    unique()  %>% as.matrix() %>% as.vector() -> a1
metal.order %>% 
    as_tibble() %>% 
    dplyr::filter(value  %in%  a1) %>% as.matrix %>% as.vector -> metal.order

plotdata$metal = factor(plotdata$metal, levels = metal.order)
plotdata$Subgrp = factor(plotdata$Subgrp, levels = c("Serum #1", "Serum #2", "Follicular fluid"))
plotdata %>% 
    ggplot(aes(x = metal, y = log10(value), fill = Subgrp)) + 
        geom_boxplot(outlier.shape = NA, 
                     alpha = 0.6,
                     width = 0.7,
                     size = 0.25,
                     position = position_dodge(0.8)) + 
        scale_y_continuous( limits = c(-3.5, 3.5), 
                            expand = expansion(add = c(0.1,0.1)),
                            breaks = c(-3,-2,-1,0,1,2,3))+
    theme_bw() +
    theme(panel.grid = element_blank())
ggsave(str_c(outpath2, "/FigS2A.png"),
       width = 18,
       height = 6,
       dpi = 600)
