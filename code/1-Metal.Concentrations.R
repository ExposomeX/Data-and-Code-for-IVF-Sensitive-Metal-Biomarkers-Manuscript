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
load(str_c(outpath,"/0-BaselineInfo.Rdata"))


# 1-.元素描述统计（min Q1 median Q3 max, DOR...)   用0算检出率--------------
source("@-IVF-Hair-Serum-FF/functions/Descript.R")
colnames(df_hair.metal)
df_hair.metal  %>%
    dplyr::select(Ag_H1:Zn_H4)  %>% 
    Descript()  %>% 
    dplyr::mutate(Group = "Hair") -> outdf1
outdf1 %>% 
    dplyr::filter(str_detect(Pollu, "_H1$")) %>% 
    arrange(-Median) %>% 
    print(n=10)

#serum 1
#deal with Y_1: a missing value obs that didn't treat before
df_serum1.metal %>% 
    dplyr::select(Y_1) %>% 
    dplyr::mutate(Y_1 = tidyr::replace_na(Y_1, 0))  -> temp
df_serum1.metal %<>% 
    dplyr::select(-Y_1) %>% 
    cbind(temp) %>% 
    as_tibble() %>% 
    dplyr::select(colnames(df_serum1.metal))
df_serum1.metal  %>%
    dplyr::select(any_of(X1))  %>% 
    Descript() %>% 
    dplyr::mutate(Group = "Serum1")  -> outdf2
outdf2 %>% 
    arrange(-Median)%>% 
    print(n=10)
#serum2
df_serum2.metal  %>%
    dplyr::select(any_of(X2))  %>% 
    Descript() %>% 
    dplyr::mutate(Group = "Serum2")  -> outdf3
outdf3 %>% 
    arrange(-Median)%>% 
    print(n=10)
#ff
df_ff.metal  %>%
    dplyr::select(any_of(X3))  %>% 
    Descript()  %>% 
    dplyr::mutate(Group = "FF") -> outdf4
outdf4 %>% 
    arrange(-Median)%>% 
    print(n=10)

rbind(outdf1, outdf2, outdf3, outdf4) -> outdf

#输出路径
outpath2 = stringr::str_c(outpath, "/2.MetalDescript")
if(!file.exists(outpath2)){dir.create(outpath2)}
write.xlsx(outdf, str_c(outpath2, "/1.Descript.Stats.among.fullPopu.xlsx"))



#2. 删除检出率<66.7%的变量--------------------
outdf %>% 
    dplyr::rename(DoR = 'DoR_%') %>% 
    dplyr::filter(DoR < 66.7) %>% 
    dplyr::select(Pollu) %>% as.matrix()  -> del.list

#3. 计算MU秩和检验结果------------------------------
p25 = function(x){quantile(x, probs = 0.25, na.rm = TRUE)}
p75 = function(x){quantile(x, probs = 0.75, na.rm = TRUE)}


MUtest = function(x){
p = wilcox.test((data[x] %>% as.matrix() %>% as.vector) ~ (data[VarY] %>% as.matrix() %>% as.vector))$p.value[[1]]
aggregate(data[x], data[VarY], median) %>% 
    dplyr::mutate(Stats = "Median") %>% 
rbind(aggregate(data[x], data[VarY], p25) %>% 
    dplyr::mutate(Stats = "P25")) %>% 
rbind(aggregate(data[x], data[VarY], p75) %>% 
    dplyr::mutate(Stats = "P75"))  %>% 
    as_tibble() %>% 
    dplyr::mutate(Outcome = if_else(X1生化妊娠_Y0_N1==0, "CTL", "EPL"),
                  Metal = x) %>% 
    pivot_wider(id_cols = Metal, names_from = c(Stats, Outcome), values_from = x) %>% 
    dplyr::mutate(Pvalue = p) -> out
    return(out)
}
#Hair
outdf1 %>% 
    dplyr::filter(!Pollu %in% del.list)  %>% 
    dplyr::select(Pollu) %>% 
    as.matrix() %>% as.vector() -> hair.lst
#计算
data = df_hair.metal
purrr::map_dfr(hair.lst, MUtest)  %>% 
    write.xlsx(str_c(outpath2, "/2-MUtest.results.hair.xlsx"))

#Serum1
outdf2 %>% 
    dplyr::filter(!Pollu %in% del.list)  %>% 
    dplyr::select(Pollu) %>% 
    as.matrix() %>% as.vector() -> serum1.lst
#计算
data = df_serum1.metal
purrr::map_dfr(serum1.lst, MUtest)  %>% 
    write.xlsx(str_c(outpath2, "/2-MUtest.results.serum1.xlsx"))

#Serum2
outdf3 %>% 
    dplyr::filter(!Pollu %in% del.list)  %>% 
    dplyr::select(Pollu) %>% 
    as.matrix() %>% as.vector() -> serum2.lst
#计算
data = df_serum2.metal
purrr::map_dfr(serum2.lst, MUtest)  %>% 
    write.xlsx(str_c(outpath2, "/2-MUtest.results.serum2.xlsx"))

#FF
outdf4 %>% 
    dplyr::filter(!Pollu %in% del.list)  %>% 
    dplyr::select(Pollu) %>% 
    as.matrix() %>% as.vector() -> ff.lst
#计算
data = df_ff.metal
purrr::map_dfr(ff.lst, MUtest)  %>% 
    write.xlsx(str_c(outpath2, "/2-MUtest.results.ff.xlsx"))


#4. 插补：0(NA) to LOD/sqrt(2)---------------------------
#load LOD
readxl::read_xlsx(str_c(inpath, "/LOD_Total.xlsx"))  -> LODs
LODs


# hair
df_hair.metal %>%
    dplyr::select(Ag_H1:Zn_H4) %>% 
    naniar::miss_var_summary()
# delete var with DoR < 66.7
df_hair.metal %<>% 
    dplyr::select(-any_of(as.vector(del.list)))

# metal list to be imputed
df_hair.metal %>% 
    dplyr::select(Ag_H1:Zn_H4) %>% 
    colnames() %>% as.matrix %>% as.vector -> lst1
LODs %>% 
    dplyr::filter(Varname  %in% lst1) %>% 
    arrange(Varname)  -> lst1

#低于LOD，替换为LOD/sqrt(2)
LODreplace = function(var){
df %>% 
    dplyr::select(any_of(var)) %>% 
    dplyr::rename(tmp = var) %>% 
    dplyr::mutate(lod = as.vector(as.matrix(lst[which(lst$Varname==var),2]/sqrt(2)))) %>% 
    dplyr::mutate(tmp = if_else(tmp < lod, #if
                                lod/sqrt(2), #yes
                                tmp)) %>% 
    dplyr::select(tmp) %>% 
    set_names(var) -> temp
    return(temp)
}
#execute
lst = lst1
df = df_hair.metal
purrr::map_dfc(lst1$Varname, LODreplace) -> temp
# check na
temp %>% 
    naniar::miss_var_summary()
# complete imputation
df_hair.metal %<>%
    dplyr::select(-(Ag_H1:Zn_H4)) %>% 
    cbind(temp) %>%
    as_tibble()


# 血清1
colnames(df_serum1.metal)
df_serum1.metal %>%
    dplyr::select(As_1:Li_1) %>% 
    naniar::miss_var_summary()

# delete var with DoR < 66.7
colnames(df_serum1.metal)
df_serum1.metal %<>% 
    dplyr::select(-any_of(as.vector(del.list)))
# metal list to be imputed
df_serum1.metal %>% 
    dplyr::select(As_1:Li_1) %>% 
    colnames() %>% as.matrix %>% as.vector -> lst2
LODs %>% 
    dplyr::filter(Varname  %in% lst2) %>% 
    arrange(Varname)  -> lst2
#execute
lst = lst2
df = df_serum1.metal
purrr::map_dfc(lst2$Varname, LODreplace) -> temp
# check na
temp %>% 
    naniar::miss_var_summary()
# complete imputation
df_serum1.metal %<>%
    dplyr::select(-(As_1:Li_1)) %>% 
    cbind(temp) %>%
    as_tibble()

# 血清2
df_serum2.metal %>%
    dplyr::select(As_2:Li_2) %>% 
    naniar::miss_var_summary()
# delete var with DoR < 66.7
df_serum2.metal %<>% 
    dplyr::select(-any_of(as.vector(del.list)))
# metal list to be imputed
df_serum2.metal %>% 
    dplyr::select(As_2:Li_2) %>% 
    colnames() %>% as.matrix %>% as.vector -> lst3
LODs %>% 
    dplyr::filter(Varname  %in% lst3) %>% 
    arrange(Varname)  -> lst3
#execute
lst = lst3
df = df_serum2.metal
purrr::map_dfc(lst3$Varname, LODreplace) -> temp
# check na
temp %>% 
    naniar::miss_var_summary()
# complete imputation
df_serum2.metal %<>%
    dplyr::select(-(As_2:Li_2)) %>% 
    cbind(temp) %>%
    as_tibble()

# 卵泡液FF
df_ff.metal %>%
    dplyr::select(As_F:Li_F) %>% 
    naniar::miss_var_summary()
# delete var with DoR < 66.7
df_ff.metal %<>% 
    dplyr::select(-any_of(as.vector(del.list)))
# metal list to be imputed
df_ff.metal %>% 
    dplyr::select(As_F:Li_F) %>% 
    colnames() %>% as.matrix %>% as.vector -> lst4
LODs %>% 
    dplyr::filter(Varname  %in% lst4) %>% 
    arrange(Varname)  -> lst4
#execute
lst = lst4
df = df_ff.metal
purrr::map_dfc(lst4$Varname, LODreplace) -> temp
# check na
temp %>% 
    naniar::miss_var_summary()
# complete imputation
df_ff.metal %<>%
    dplyr::select(-(As_F:Li_F)) %>% 
    cbind(temp) %>%
    as_tibble()
colnames(df_ff.metal)


# 6.相关系数及P值-----------------------------------------------
colnames(df_serum1.metal)
#prepare data
df_hair.metal %>%
    dplyr::select(ID, X1生化妊娠_Y0_N1, Ag_H1:Zn_H4) %>% 
dplyr::full_join(
df_serum1.metal %>% 
    dplyr::select(ID, X1生化妊娠_Y0_N1, Ag_1:Zn_1), by = c("ID","X1生化妊娠_Y0_N1")
) %>% 
dplyr::full_join(
df_serum2.metal %>% 
    dplyr::select(ID, X1生化妊娠_Y0_N1, Ag_2:Zn_2), by = c("ID","X1生化妊娠_Y0_N1")
) %>% 
dplyr::full_join(
df_ff.metal %>% 
    dplyr::select(ID, X1生化妊娠_Y0_N1, Ag_F:Zn_F), by = c("ID","X1生化妊娠_Y0_N1")
) -> cordata

# hair section list
hair.lst %>% 
    as_tibble() %>% 
    dplyr::filter(str_detect(value, "\\w+_H1$")) %>% 
    as.matrix() %>% as.vector() -> hair1
hair.lst %>% 
    as_tibble() %>% 
    dplyr::filter(str_detect(value, "\\w+_H2$")) %>% 
    as.matrix() %>% as.vector() -> hair2
hair.lst %>% 
    as_tibble() %>% 
    dplyr::filter(str_detect(value, "\\w+_H3$")) %>% 
    as.matrix() %>% as.vector() -> hair3
hair.lst %>% 
    as_tibble() %>% 
    dplyr::filter(str_detect(value, "\\w+_H4$")) %>% 
    as.matrix() %>% as.vector() -> hair4


# function for spearman correlation
F1 = function(v1, v2){
  cor_test(cordata, vars = v1, vars2 = v2, method = "spearman", use = "pairwise.complete.obs")
}

colnames(cordata)
# create list
cordata %>% 
    dplyr::select(-ID, -X1生化妊娠_Y0_N1) %>% 
    colnames() %>% as.matrix() %>% as.vector() -> corlist
combn(corlist,2) %>% t() %>%
  as_tibble() -> comb
# execute. That would be time costy
purrr::map2_dfr(comb$V1, comb$V2, F1) -> cor.results


cor.results %>% 
    write.xlsx(str_c(outpath2, "/3-Correlation.results.xlsx"))

save.image(str_c(outpath,"/1-Metal.Stats.Rdata"))

#########################################################

#plot for FigS2

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

