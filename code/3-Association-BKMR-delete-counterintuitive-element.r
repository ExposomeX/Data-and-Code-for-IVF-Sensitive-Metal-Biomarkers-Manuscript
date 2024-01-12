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

#load data for BKMR building
load(str_c(outpath, "/2.Associations.Rdata"))


#2. Serum1--------------------------
# extract sensitive biomarkers in single element analyese results
readxl::read_xlsx(str_c(outpath3, "/1.Continuous.serum1.xlsx")) %>% 
    dplyr::filter(Pvalue_a < 0.05) %>% 
    dplyr::select(Element) %>% as.matrix %>% as.vector %>% unique()  -> serum1.list.bkmr1
readxl::read_xlsx(str_c(outpath3, "/2.Category3.serum1.xlsx")) %>% 
    dplyr::filter(Pvalue_a < 0.05) %>% 
    dplyr::select(Element) %>% as.matrix %>% as.vector %>% unique()  -> serum1.list.bkmr2

union(serum1.list.bkmr1, serum1.list.bkmr2)  %>% 
    as_tibble() %>% as.matrix() %>% as.vector -> serum1.list.bkmr
colnames(df_serum1.metal)
# 处理离群值
df_serum1.metal %>% 
  dplyr::select(As_1:Li_1) %>% 
  colnames() -> elements
con_l <- vector()
con_r <- vector()
df = df_serum1.metal

for (i in 1:length(elements)) {
    con_l[i] <- (2.5 * quantile(as.matrix(df[,elements[i]]),0.25)) - (1.5 * quantile(as.matrix(df[,elements[i]]),0.75))
    con_r[i] <- (2.5 * quantile(as.matrix(df[,elements[i]]),0.75)) - (1.5 * quantile(as.matrix(df[,elements[i]]),0.25))
    for (j in 1:nrow(df)) {
        if (df[j,elements[i]] > con_r[i]) {
            df[j,elements[i]] <- con_r[i]
        }
        if (df[j,elements[i]] < con_l[i]) {
            df[j,elements[i]] <- con_l[i]
        }
    }
}
# define Y X C
Y <- df$X1生化妊娠_Y0_N1
corva <- data.matrix(df[,c("年龄_2Cat", "BMI_3Cat", "地区","IVF卵子数", "M2成熟卵子", "受精卵数2PN", "X1移植胚胎数量", "HCG日_内膜")])
colnames(df)
serum1.list.bkmr %>% 
    as_tibble() %>% 
    dplyr::filter(!value %in% c("Th_1")) %>% 
    as.matrix() %>% as.vector()  -> serum1.list.bkmr
df %>% 
  dplyr::select(any_of(serum1.list.bkmr)) %>% as.matrix() -> expo
# log
for (i in 1:ncol(expo)) {
    expo[,i]<-log(expo[,i])
}
# scale at same level
scale_expo <- scale(expo)

#fit model
set.seed(20231217)
#fit
tictoc::tic()
fitkm.serum1 <- kmbayes(Y, Z=scale_expo, X=corva, iter=50000, varsel=TRUE, family = "binomial", est.h=TRUE)
tictoc::toc()



#4. FF--------------------------
# extract sensitive biomarkers in single element analyese results
readxl::read_xlsx(str_c(outpath3, "/1.Continuous.ff.xlsx")) %>% 
    dplyr::filter(Pvalue_a < 0.05) %>% 
    dplyr::select(Element) %>% as.matrix %>% as.vector %>% unique()  -> ff.list.bkmr1
readxl::read_xlsx(str_c(outpath3, "/2.Category3.ff.xlsx")) %>% 
    dplyr::filter(Pvalue_a < 0.05) %>% 
    dplyr::select(Element) %>% as.matrix %>% as.vector %>% unique()  -> ff.list.bkmr2

union(ff.list.bkmr1, ff.list.bkmr2)  %>% 
    as_tibble() %>% as.matrix() %>% as.vector -> ff.list.bkmr
colnames(df_ff.metal)
# 处理离群值
df_ff.metal %>% 
  dplyr::select(As_F:Li_F) %>% 
  colnames() -> elements
con_l <- vector()
con_r <- vector()
df = df_ff.metal

for (i in 1:length(elements)) {
    con_l[i] <- (2.5 * quantile(as.matrix(df[,elements[i]]),0.25)) - (1.5 * quantile(as.matrix(df[,elements[i]]),0.75))
    con_r[i] <- (2.5 * quantile(as.matrix(df[,elements[i]]),0.75)) - (1.5 * quantile(as.matrix(df[,elements[i]]),0.25))
    for (j in 1:nrow(df)) {
        if (df[j,elements[i]] > con_r[i]) {
            df[j,elements[i]] <- con_r[i]
        }
        if (df[j,elements[i]] < con_l[i]) {
            df[j,elements[i]] <- con_l[i]
        }
    }
}
# define Y X C
Y <- df$X1生化妊娠_Y0_N1
corva <- data.matrix(df[,c("年龄_2Cat", "BMI_3Cat", "地区","IVF卵子数", "M2成熟卵子", "受精卵数2PN", "X1移植胚胎数量", "HCG日_内膜")])
colnames(df)
ff.list.bkmr %>% 
    as_tibble() %>% 
    dplyr::filter(!value %in% c("Cd_F")) %>% 
    as.matrix() %>% as.vector()  -> ff.list.bkmr
df %>% 
  dplyr::select(any_of(ff.list.bkmr)) %>% as.matrix() -> expo
# log
for (i in 1:ncol(expo)) {
    expo[,i]<-log(expo[,i])
}
# scale at same level
scale_expo <- scale(expo)

#fit model
set.seed(20231217)
#fit
tictoc::tic()
fitkm.ff <- kmbayes(Y, Z=scale_expo, X=corva, iter=50000, varsel=TRUE, family = "binomial", est.h=TRUE)
tictoc::toc()


#save
save.image(str_c(outpath, "/3.BKMR_delete_conterintuitive_element_20231229.Rdata"))



# export results for BKMR models
load(str_c(outpath, "/3.BKMR_delete_conterintuitive_element_20231229.Rdata"))
outpath6 = str_c(outpath, "/4.Associations.BKMR/Sensitive.analyses.1.Del.Counterintuitive.element")
if(!file.exists(outpath6)){dir.create(outpath6)}


# serum1
ExtractPIPs(fitkm.serum1)
# #Partial dependence
# pred.resp.univar <- PredictorResponseUnivar(fit = fitkm.serum1, q.fixed = 0.5)
# pred.resp.univar
# ggplot(pred.resp.univar, aes(z, est, ymin = est - 1.96*se,
#                                ymax = est + 1.96*se)) +
#     geom_smooth(stat = "identity") +
#     facet_wrap(~variable, ncol =4) +
#     xlab("scaled concentrations") +
#     ylab("h(exposures)") +
#     theme_bw() +
#     theme(panel.grid = element_blank()) +
#     geom_hline(yintercept = 0, linetype=2,color="red")
# ggsave(str_c(outpath6, "/PD.Serum1.png"), height = 8, width = 12, dpi = 600)
# # mixture effect
# risks.overall <- OverallRiskSummaries(fit = fitkm.serum1, qs = seq(0.1, 0.9, by = 0.05), q.fixed = 0.5)
# risks.overall
# ggplot(risks.overall, aes(quantile, est, ymin = est - 1.96*sd, 
#                           ymax = est + 1.96*sd)) + 
#     geom_hline(yintercept = 0, lty = 2, col = "brown") +
#     geom_pointrange() +
#     theme_bw() +
#     theme(panel.grid = element_blank()) 
# ggsave(str_c(outpath6, "/MixEffect.Serum1.png"), height = 4, width = 6, dpi = 600)
#Single exposure estimate
risks.singvar <- SingVarRiskSummaries(
    fit = fitkm.serum1, qs.diff = c(0.25,0.75), 
    q.fixed = c(0.25,0.5,0.75))
ggplot(risks.singvar, aes(variable, est, ymin = est - 1.96*sd, ymax = est + 1.96*sd, 
                          color = q.fixed, shape=q.fixed)) +
    geom_pointrange(position = position_dodge(width = 0.75)) + 
    geom_hline(yintercept = 0, linetype=2,color="red") +
    scale_y_continuous(limits = c(-0.5, 0.5),
                       breaks = c(-0.5,-0.25,0,0.25,0.5),
                       expand=expansion(add = c(0.01, 0.01)))+ 
    theme_bw() +
    theme(panel.grid = element_blank()) 
ggsave(str_c(outpath6, "/Single.risk.p25vsp75.Serum1.png"), height = 4, width = 6, dpi = 600)


# ff
ExtractPIPs(fitkm.ff)
# #Partial dependence
# pred.resp.univar <- PredictorResponseUnivar(fit = fitkm.ff, q.fixed = 0.5)
# pred.resp.univar
# ggplot(pred.resp.univar, aes(z, est, ymin = est - 1.96*se,
#                                ymax = est + 1.96*se)) +
#     geom_smooth(stat = "identity") +
#     facet_wrap(~variable, ncol =4) +
#     xlab("scaled concentrations") +
#     ylab("h(exposures)") +
#     geom_hline(yintercept = 0, linetype=2,color="red") +
#     theme_bw() +
#     theme(panel.grid = element_blank()) 
# ggsave(str_c(outpath6, "/PD.ff.png"), height = 8, width = 12, dpi = 600)
# #mix effect
# risks.overall <- OverallRiskSummaries(fit = fitkm.ff, qs = seq(0.1, 0.9, by = 0.05), q.fixed = 0.5)
# risks.overall
# ggplot(risks.overall, aes(quantile, est, ymin = est - 1.96*sd, 
#                           ymax = est + 1.96*sd)) + 
#     geom_hline(yintercept = 0, lty = 2, col = "brown") +
#     geom_pointrange() +
#     theme_bw() +
#     theme(panel.grid = element_blank()) 
# ggsave(str_c(outpath6, "/MixEffect.ff.png"), height = 4, width = 6, dpi = 600)
#Single exposure estimate
risks.singvar <- SingVarRiskSummaries(
    fit = fitkm.ff, qs.diff = c(0.25,0.75), 
    q.fixed = c(0.25,0.5,0.75))
ggplot(risks.singvar, aes(variable, est, ymin = est - 1.96*sd, ymax = est + 1.96*sd, 
                          color = q.fixed, shape=q.fixed)) +
    geom_pointrange(position = position_dodge(width = 0.75)) + 
    geom_hline(yintercept = 0, linetype=2,color="red") +
    scale_y_continuous(limits = c(-0.65, 0.65),
                       breaks = c(-0.6,-0.3,0,0.3,0.6),
                       expand=expansion(add = c(0.01, 0.01)))+ 
    theme_bw() +
    theme(panel.grid = element_blank()) 
ggsave(str_c(outpath6, "/Single.risk.p25vsp75.ff.png"), height = 4, width = 6, dpi = 600)


# extract PIPs for various groups
ExtractPIPs(fitkm.serum1) %>% 
    dplyr::mutate(Group = "Serum#1") %>% 
    arrange(-PIP) %>% 
rbind(
ExtractPIPs(fitkm.ff) %>% 
    dplyr::mutate(Group = "FF") %>% 
    arrange(-PIP))  -> PIPs
PIPs %>% 
    ggplot(aes(x = PIP, y = reorder(variable, PIP), fill = Group)) +
    geom_bar(stat = "identity") +
    facet_wrap(~Group, ncol =7,scales = "free")
ggsave(str_c(outpath6, "/PIPs.png"), height = 4, width = 10, dpi = 600)

PIPs %>% 
    writexl::write_xlsx(str_c(outpath6, "/PIPs.for.BKMR.models.xlsx"))
###################################################################################################
