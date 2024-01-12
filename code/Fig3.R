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
        # "mediation",#the below packages are required for mediation analyses
        # "gcdnet",
        # "caret",
        # "bama",
        # "PDM",
        # "HIMA",
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
readxl::read_xlsx(str_c(outpath, "/3.Associations.Logbinomial/1.Continuous.ff.xlsx")) %>% 
  dplyr::select(Element, aRR, Pvalue_a) %>% 
  dplyr::mutate(Group = "Follicular fluid") %>% 
rbind(
readxl::read_xlsx(str_c(outpath, "/3.Associations.Logbinomial/1.Continuous.hair.xlsx")) %>% 
  dplyr::select(Element, aRR, Pvalue_a) %>% 
  dplyr::mutate(Group = "Hair") # %>% 
#  dplyr::filter(str_detect(Element, "_H1$"))
)  %>% 
rbind(
readxl::read_xlsx(str_c(outpath, "/3.Associations.Logbinomial/1.Continuous.serum1.xlsx")) %>% 
  dplyr::select(Element, aRR, Pvalue_a) %>% 
  dplyr::mutate(Group = "Serum #1")
) %>% 
rbind(
readxl::read_xlsx(str_c(outpath, "/3.Associations.Logbinomial/1.Continuous.serum2.xlsx")) %>% 
  dplyr::select(Element, aRR, Pvalue_a) %>% 
  dplyr::mutate(Group = "Serum #2")
)  -> fig2a

fig2a %>% 
  dplyr::mutate(P = -log10(Pvalue_a)) %>% 
  dplyr::mutate(color = if_else(P>1.30103, Group, "Nonsig")) -> fig2a


# install.packages("ggbreak")
library(ggbreak)
fig2a %>% 
arrange(-P) %>% 
print(n=72)

fills <-  c("Follicular fluid" = "#e0860f", 
            "Hair" = "#45daee", 
            "Serum #1" = "#df85df", 
            "Serum #2" = "#00CD66", 
            "Nonsig" = "#696969")

fig2a  %>% 
  ggplot(aes(x = aRR, y = P, color = color)) +
  geom_point(size = 3) +
  scale_y_continuous(limits = c(0, 12.9),
                     breaks = c(0, 0.5, 1, 1.5, 2, 2.5)) +
  scale_y_break(c(2.53,11.45), scales = 0.1,
                ticklabels = c(11.5,12, 12.5),
                expand=expansion(add = c(0.02, 0))) +
  geom_vline(xintercept = 1, linetype = "dashed" , color = "grey") +
  geom_hline(yintercept = 1.30103, linetype = "dashed" , color = "grey") +
  scale_color_manual(values = fills) +
  theme_bw() +
  theme(panel.grid = element_blank())
  
ggsave(str_c(outpath, "/3.Associations.Logbinomial/Fig3A.png"), 
       height = 6, 
       width = 7)


# Fig2B---------------
rm(list=ls())
inpath = stringr::str_c(getwd(), "/@-IVF-Hair-Serum-FF/input")
outpath = stringr::str_c(getwd(), "/@-IVF-Hair-Serum-FF/output")
#read data
hair = readxl::read_xlsx(str_c(outpath, "/3.Associations.Logbinomial/1.Continuous.hair.xlsx"))
serum1 = readxl::read_xlsx(str_c(outpath, "/3.Associations.Logbinomial/1.Continuous.serum1.xlsx"))
serum2 = readxl::read_xlsx(str_c(outpath, "/3.Associations.Logbinomial/1.Continuous.serum2.xlsx"))
ff = readxl::read_xlsx(str_c(outpath, "/3.Associations.Logbinomial/1.Continuous.ff.xlsx"))

#bind
hair %>% 
    dplyr::filter(str_detect(Element, "_H1$")) %>% #only section 1 was chosen for fig3
    dplyr::mutate(Element = str_extract(Element,"\\w+(?=_H\\d$)")) %>% 
    dplyr::mutate(Group = "Hair") %>% 
    dplyr::select(-cRR, -LCI_c, -UCI_c, -Pvalue_c) %>% 
    rbind(
serum1 %>% 
    dplyr::mutate(Element = str_extract(Element,"\\w+(?=_1)")) %>% 
    dplyr::mutate(Group = "Serum1") %>% 
    dplyr::select(-cRR, -LCI_c, -UCI_c, -Pvalue_c)        
    ) %>% 
    rbind(
serum2 %>% 
    dplyr::mutate(Element = str_extract(Element,"\\w+(?=_2)")) %>% 
    dplyr::mutate(Group = "Serum2") %>% 
    dplyr::select(-cRR, -LCI_c, -UCI_c, -Pvalue_c)
    ) %>% 
    rbind(
ff %>% 
    dplyr::mutate(Element = str_extract(Element,"\\w+(?=_F)")) %>% 
    dplyr::mutate(Group = "Follicular Fluid") %>% 
    dplyr::select(-cRR, -LCI_c, -UCI_c, -Pvalue_c)
    ) -> plotdata


plotdata$Group <- factor(plotdata$Group, levels = c("Hair", "Serum1", "Serum2", "Follicular Fluid"))

plotdata %>% 
  dplyr::filter(Pvalue_a < 0.05) %>% 
  dplyr::select(Element) %>% 
  unique()  %>% as.matrix() %>% as.vector() -> list

plotdata %>% 
    dplyr::filter(Element %in% list)  %>% 
    arrange(Pvalue_a)

plotdata %>% 
    dplyr::filter(Element %in% list) %>% 
    ggplot(aes(x = aRR, y = Element, group = Group)) +
    geom_point(size = 1) +
    geom_pointrange(aes(y = Element, x = aRR, xmin = LCI_a, xmax = UCI_a))+
    geom_vline(xintercept = 1, linetype = "dashed" , color = "red")+
    scale_x_continuous(limits = c(0, 4),
                       breaks = c(0, 2, 4))+
    facet_grid(.~Group) + 
    theme_bw() +
    theme(panel.grid = element_blank())

ggsave(str_c(outpath, "/3.Associations.Logbinomial/Fig3B.png"),
       height = 6.5, 
       width = 7,
       dpi=600)

#FigS1---------------------
#bind
hair %>% 
    dplyr::filter(str_detect(Element, "_H1$")) %>% #only section 1 was chosen for fig3
    dplyr::mutate(Element = str_extract(Element,"\\w+(?=_H\\d$)")) %>% 
    dplyr::mutate(Group = "Hair") %>% 
    dplyr::select(-cRR, -LCI_c, -UCI_c, -Pvalue_c) %>% 
    rbind(
serum1 %>% 
    dplyr::mutate(Element = str_extract(Element,"\\w+(?=_1)")) %>% 
    dplyr::mutate(Group = "Serum1") %>% 
    dplyr::select(-cRR, -LCI_c, -UCI_c, -Pvalue_c)        
    ) %>% 
    rbind(
serum2 %>% 
    dplyr::mutate(Element = str_extract(Element,"\\w+(?=_2)")) %>% 
    dplyr::mutate(Group = "Serum2") %>% 
    dplyr::select(-cRR, -LCI_c, -UCI_c, -Pvalue_c)
    ) %>% 
    rbind(
ff %>% 
    dplyr::mutate(Element = str_extract(Element,"\\w+(?=_F)")) %>% 
    dplyr::mutate(Group = "Follicular Fluid") %>% 
    dplyr::select(-cRR, -LCI_c, -UCI_c, -Pvalue_c)
    ) -> plotdata

plotdata$Group <- factor(plotdata$Group, levels = c("Hair", "Serum1", "Serum2", "Follicular Fluid"))

plotdata %>% 
#  dplyr::filter(Pvalue_a < 0.05) %>% 
  dplyr::select(Element) %>% 
  unique()  %>% as.matrix() %>% as.vector() -> list

plotdata %>% 
    dplyr::filter(Element %in% list) %>% 
    ggplot(aes(x = aRR, y = Element, group = Group)) +
    geom_point(size = 1) +
    geom_pointrange(aes(y = Element, x = aRR, xmin = LCI_a, xmax = UCI_a))+
    geom_vline(xintercept = 1, linetype = "dashed" , color = "red")+
    facet_grid(.~Group) + 
    theme_bw() +
    theme(panel.grid = element_blank())
ggsave(str_c(outpath, "/3.Associations.Logbinomial/FigS3.png"),
       height = 12, 
       width = 7,
       dpi=600)

#FigS2---------------------
#bind
hair %>% 
    dplyr::mutate(section = str_extract(Element, "(?<=_H)\\d")) %>% 
    dplyr::mutate(Element = str_extract(Element,"\\w+(?=_H\\d$)")) %>% 
    dplyr::mutate(Group = str_c("Hair #", section)) %>% 
    dplyr::select(-cRR, -LCI_c, -UCI_c, -Pvalue_c, -section) -> plotdata

plotdata$Group <- factor(plotdata$Group, levels =  c("Hair #1", "Hair #2", "Hair #3", "Hair #4"))

plotdata %>% 
#  dplyr::filter(Pvalue_a < 0.05) %>% 
  dplyr::select(Element) %>% 
  unique()  %>% as.matrix() %>% as.vector() -> list

plotdata %>% 
    dplyr::filter(Element %in% list) %>% 
    ggplot(aes(x = aRR, y = Element, group = Group)) +
    geom_point(size = 1) +
    geom_pointrange(aes(y = Element, x = aRR, xmin = LCI_a, xmax = UCI_a))+
    geom_vline(xintercept = 1, linetype = "dashed" , color = "red")+
    facet_grid(.~Group) + 
    theme_bw() +
    theme(panel.grid = element_blank())
ggsave(str_c(outpath, "/3.Associations.Logbinomial/FigS4.png"),
       height = 12, 
       width = 7,
       dpi=600)

