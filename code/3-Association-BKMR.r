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


#1. Hair-------------------------------------------------------------
# extract sensitive biomarkers in single element analyese results

readxl::read_xlsx(str_c(outpath3, "/1.Continuous.hair.xlsx")) %>% 
    dplyr::filter(Pvalue_a < 0.05) %>% 
    dplyr::select(Element) %>% as.matrix %>% as.vector %>% unique()  -> hair.list.bkmr1
readxl::read_xlsx(str_c(outpath3, "/2.Category3.hair.xlsx")) %>% 
    dplyr::filter(Pvalue_a < 0.05) %>% 
    dplyr::select(Element) %>% as.matrix %>% as.vector %>% unique()  -> hair.list.bkmr2

union(hair.list.bkmr1, hair.list.bkmr2)  %>% 
    as_tibble() %>% 
    dplyr::filter(str_detect(value, "_H1$")) %>% as.matrix() %>% as.vector -> hair1.list.bkmr
union(hair.list.bkmr1, hair.list.bkmr2)  %>% 
    as_tibble() %>% 
    dplyr::filter(str_detect(value, "_H2$")) %>% as.matrix() %>% as.vector -> hair2.list.bkmr
union(hair.list.bkmr1, hair.list.bkmr2)  %>% 
    as_tibble() %>% 
    dplyr::filter(str_detect(value, "_H3$")) %>% as.matrix() %>% as.vector -> hair3.list.bkmr
union(hair.list.bkmr1, hair.list.bkmr2)  %>% 
    as_tibble() %>% 
    dplyr::filter(str_detect(value, "_H4$")) %>% as.matrix() %>% as.vector -> hair4.list.bkmr


colnames(df_hair.metal)
# 处理离群值
df_hair.metal %>% 
  dplyr::select(Ag_H1:Zn_H4) %>% 
  colnames() -> elements
con_l <- vector()
con_r <- vector()
df = df_hair.metal
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

# for hair section 1---------
# define Y X C
Y <- df$X1生化妊娠_Y0_N1
corva <- data.matrix(df[,c("年龄_2Cat", "BMI_3Cat", "地区","IVF卵子数", "M2成熟卵子", "受精卵数2PN", "X1移植胚胎数量", "HCG日_内膜")])
colnames(df)
hair1.list.bkmr
df %>% 
  dplyr::select(any_of(hair1.list.bkmr)) %>% as.matrix() -> expo
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
fitkm1 <- kmbayes(Y, Z=scale_expo, X=corva, iter=50000, varsel=TRUE, family = "binomial", est.h=TRUE)
tictoc::toc()

# for hair section 2---------
# define Y X C
Y <- df$X1生化妊娠_Y0_N1
corva <- data.matrix(df[,c("年龄_2Cat", "BMI_3Cat", "地区","IVF卵子数", "M2成熟卵子", "受精卵数2PN", "X1移植胚胎数量", "HCG日_内膜")])
colnames(df)
hair2.list.bkmr
df %>% 
  dplyr::select(any_of(hair2.list.bkmr)) %>% as.matrix() -> expo
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
fitkm2 <- kmbayes(Y, Z=scale_expo, X=corva, iter=50000, varsel=TRUE, family = "binomial", est.h=TRUE)
tictoc::toc()


# for hair section 3---------
# define Y X C
Y <- df$X1生化妊娠_Y0_N1
corva <- data.matrix(df[,c("年龄_2Cat", "BMI_3Cat", "地区","IVF卵子数", "M2成熟卵子", "受精卵数2PN", "X1移植胚胎数量", "HCG日_内膜")])
colnames(df)
hair3.list.bkmr
df %>% 
  dplyr::select(any_of(hair3.list.bkmr)) %>% as.matrix() -> expo
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
fitkm3 <- kmbayes(Y, Z=scale_expo, X=corva, iter=50000, varsel=TRUE, family = "binomial", est.h=TRUE)
tictoc::toc()

# for hair section 4---------
# define Y X C
Y <- df$X1生化妊娠_Y0_N1
corva <- data.matrix(df[,c("年龄_2Cat", "BMI_3Cat", "地区","IVF卵子数", "M2成熟卵子", "受精卵数2PN", "X1移植胚胎数量", "HCG日_内膜")])
colnames(df)
hair4.list.bkmr
df %>% 
  dplyr::select(any_of(hair4.list.bkmr)) %>% as.matrix() -> expo
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
fitkm4 <- kmbayes(Y, Z=scale_expo, X=corva, iter=50000, varsel=TRUE, family = "binomial", est.h=TRUE)
tictoc::toc()




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


#3. Serum2--------------------------
# extract sensitive biomarkers in single element analyese results
readxl::read_xlsx(str_c(outpath3, "/1.Continuous.serum2.xlsx")) %>% 
    dplyr::filter(Pvalue_a < 0.05) %>% 
    dplyr::select(Element) %>% as.matrix %>% as.vector %>% unique()  -> serum2.list.bkmr1
readxl::read_xlsx(str_c(outpath3, "/2.Category3.serum2.xlsx")) %>% 
    dplyr::filter(Pvalue_a < 0.05) %>% 
    dplyr::select(Element) %>% as.matrix %>% as.vector %>% unique()  -> serum2.list.bkmr2

union(serum2.list.bkmr1, serum2.list.bkmr2)  %>% 
    as_tibble() %>% as.matrix() %>% as.vector -> serum2.list.bkmr
colnames(df_serum2.metal)
# 处理离群值
df_serum2.metal %>% 
  dplyr::select(As_2:Li_2) %>% 
  colnames() -> elements
con_l <- vector()
con_r <- vector()
df = df_serum2.metal

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

df %>% 
  dplyr::select(any_of(serum2.list.bkmr)) %>% as.matrix() -> expo
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
fitkm.serum2 <- kmbayes(Y, Z=scale_expo, X=corva, iter=50000, varsel=TRUE, family = "binomial", est.h=TRUE)
tictoc::toc()
# ExtractPIPs(fitkm3)


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
save.image(str_c(outpath, "/3.BKMR_final_20231226.Rdata"))


# extract PIPs for various groups
ExtractPIPs(fitkm1) %>% 
    dplyr::mutate(Group = "Hair#1") %>% 
    arrange(-PIP) %>% 
rbind(
ExtractPIPs(fitkm2) %>% 
    dplyr::mutate(Group = "Hair#2") %>% 
    arrange(-PIP)) %>% 
rbind(
ExtractPIPs(fitkm3) %>% 
    dplyr::mutate(Group = "Hair#3") %>% 
    arrange(-PIP)) %>% 
rbind(
ExtractPIPs(fitkm4) %>% 
    dplyr::mutate(Group = "Hair#4") %>% 
    arrange(-PIP)) %>% 
rbind(
ExtractPIPs(fitkm.serum1) %>% 
    dplyr::mutate(Group = "Serum#1") %>% 
    arrange(-PIP)) %>% 
rbind(
ExtractPIPs(fitkm.serum2) %>% 
    dplyr::mutate(Group = "Serum#2") %>% 
    arrange(-PIP)) %>% 
rbind(
ExtractPIPs(fitkm.ff) %>% 
    dplyr::mutate(Group = "FF") %>% 
    arrange(-PIP))  -> PIPs
PIPs %>% 
    ggplot(aes(x = PIP, y = reorder(variable, PIP), fill = Group)) +
    geom_bar(stat = "identity") +
    facet_wrap(~Group, ncol =7,scales = "free")
ggsave(str_c(outpath4, "/PIPs.png"), height = 4, width = 18, dpi = 600)

PIPs %>% 
    writexl::write_xlsx(str_c(outpath4, "/PIPs.for.BKMR.models.xlsx"))
##########################################################################################