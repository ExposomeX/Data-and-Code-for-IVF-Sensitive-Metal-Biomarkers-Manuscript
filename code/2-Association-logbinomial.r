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
        "writexl")
# 0-Prepare step ----------------------------------
#set work directory
inpath = stringr::str_c(getwd(), "/@-IVF-Hair-Serum-FF/input")
outpath = stringr::str_c(getwd(), "/@-IVF-Hair-Serum-FF/output")

if(!file.exists(inpath)){dir.create(inpath)}
if(!file.exists(outpath)){dir.create(outpath)}

#load data
load(str_c(outpath,"/1-Metal.Stats.Rdata"))

outpath3 = stringr::str_c(outpath, "/3.Associations.Logbinomial")
if(!file.exists(outpath3)){dir.create(outpath3)}

# 1. 头发--------------------
colnames(df_hair.metal)
#处理移植胚胎数量的因子
df_hair.metal %>% 
    dplyr::mutate(X1移植胚胎数量 = if_else(X1移植胚胎数量==3,factor(2),X1移植胚胎数量))  -> df_hair.metal 
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

#Function: obtain  RR of log-binomial models
Ex.RR = function(Y,X,C){
  #Y: dataframe containing outcome
  #X: dataframe containing exposures
  #C: dataframe containing covariates
    df <- dplyr::bind_cols(Y,X,C)
    name <- colnames(X)
    #loop function
    F1 = function(
        expo,#exposure
        name #name of exposure
    ){
        #1. cRR and CI
        fit2 <- glm(as.matrix(df[,1]) ~ log(expo),
                    family = binomial(link = "log"),
                    weights = copy$weight,
                    start = c(log(mean(as.matrix(df[,1]))), rep(0,1)),
                    maxit = 10000,
                    data = df)
        tmp <- summary(fit2)$coefficients
        
        tmp1 <- cbind(cRR = exp(coef(fit2))[[2]],
                      LCI_c = exp(confint.default(fit2)[2,1]),
                      UCI_c = exp(confint.default(fit2)[2,2]), 
                      Pvalue_c = tmp[2,4])
        #2. aRR and CI
        fit4 <- glm(as.matrix(df[,1]) ~ log(expo) + 年龄_2Cat + BMI_3Cat + 地区 + IVF卵子数 + M2成熟卵子 + 受精卵数2PN + X1移植胚胎数量 + HCG日_内膜, 
                    family = binomial(link = "log"), 
                    weights = copy$weight,
                    start = c(log(mean(as.matrix(df[,1]))), rep(0,10)),
                    maxit = 10000,
                    data = df)
        tmp <- summary(fit4)$coefficients
        
        tmp2 <- cbind(aRR = exp(coef(fit4))[[2]],
                      LCI_a = exp(confint.default(fit4)[2,1]),
                      UCI_a = exp(confint.default(fit4)[2,2]), 
                      Pvalue_a = tmp[2,4])
        #3. combine
        cbind(tmp1, tmp2) %>% 
            tibble::as_tibble() %>% 
            dplyr::mutate(Element = name) %>% 
            dplyr::select(Element, everything()) -> tmp3
        print(paste(name, " accomplished"))
        return(tmp3)
    }
    result <- purrr::map2_dfr(X, name, F1)
    #names(result) <- c("Element", "cRR", "c_LCL", "c_UCL", "P_c", "aRR", "a_LCL", "a_UCL", "P_a")
    return(result)
}
copy %>% 
    dplyr::select(VarY) -> Y
copy %>% 
    dplyr::select(Ag_H1:Zn_H4) -> X
copy %>% 
    dplyr::select(any_of(c("年龄_2Cat", "BMI_3Cat", "地区","IVF卵子数", "M2成熟卵子", "受精卵数2PN", "X1移植胚胎数量", "HCG日_内膜"))) -> C

warnings()
A <- Ex.RR(Y,X,C)
A %>% 
    print(n=117)
A %>% 
    writexl::write_xlsx(str_c(outpath3,"/1.Continuous.hair.xlsx"))



# 2. Serum1--------------------
colnames(df_serum1.metal)
#处理移植胚胎数量的因子
df_serum1.metal %>% 
    dplyr::mutate(X1移植胚胎数量 = if_else(X1移植胚胎数量==3,factor(2),X1移植胚胎数量))  -> df_serum1.metal 
# 处理离群值
df_serum1.metal %>% 
    dplyr::select(Ag_1:Zn_1) %>% 
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

#COPY method for log-binomial model
df %>% 
    dplyr::mutate(weight = 0.9999)  -> cp1
df %>% 
    dplyr::mutate(weight = 0.0001) %>% 
    dplyr::mutate(X1生化妊娠_Y0_N1=1-X1生化妊娠_Y0_N1) -> cp2
copy = rbind(cp1, cp2)

copy %>% 
    dplyr::select(VarY) -> Y
copy %>% 
    dplyr::select(Ag_1:Zn_1) -> X
copy %>% 
    dplyr::select(any_of(c("年龄_2Cat", "BMI_3Cat", "地区","IVF卵子数", "M2成熟卵子", "受精卵数2PN", "X1移植胚胎数量", "HCG日_内膜"))) -> C

B <- Ex.RR(Y,X,C)
B %>% 
    print(n=117)
B %>% 
    writexl::write_xlsx(str_c(outpath3,"/1.Continuous.serum1.xlsx"))


# 3. Serum2--------------------
colnames(df_serum2.metal)
#处理移植胚胎数量的因子
df_serum2.metal %>% 
    dplyr::mutate(X1移植胚胎数量 = if_else(X1移植胚胎数量==3,factor(2),X1移植胚胎数量))  -> df_serum2.metal 
# 处理离群值
df_serum2.metal %>% 
    dplyr::select(Ag_2:Zn_2) %>% 
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

#COPY method for log-binomial model
df %>% 
    dplyr::mutate(weight = 0.9999)  -> cp1
df %>% 
    dplyr::mutate(weight = 0.0001) %>% 
    dplyr::mutate(X1生化妊娠_Y0_N1=1-X1生化妊娠_Y0_N1) -> cp2
copy = rbind(cp1, cp2)


copy %>% 
    dplyr::select(VarY) -> Y
copy %>% 
    dplyr::select(Ag_2:Zn_2) -> X
copy %>% 
    dplyr::select(any_of(c("年龄_2Cat", "BMI_3Cat", "地区","IVF卵子数", "M2成熟卵子", "受精卵数2PN", "X1移植胚胎数量", "HCG日_内膜"))) -> C

D <- Ex.RR(Y,X,C)
D %>% 
    print(n=117)
D %>% 
    writexl::write_xlsx(str_c(outpath3,"/1.Continuous.serum2.xlsx"))


# 4. FF--------------------
colnames(df_ff.metal)
#处理移植胚胎数量的因子
df_ff.metal %>% 
    dplyr::mutate(X1移植胚胎数量 = if_else(X1移植胚胎数量==3,factor(2),X1移植胚胎数量))  -> df_ff.metal 
# 处理离群值
df_ff.metal %>% 
    dplyr::select(Ag_F:Zn_F) %>% 
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

#COPY method for log-binomial model
df %>% 
    dplyr::mutate(weight = 0.9999)  -> cp1
df %>% 
    dplyr::mutate(weight = 0.0001) %>% 
    dplyr::mutate(X1生化妊娠_Y0_N1=1-X1生化妊娠_Y0_N1) -> cp2
copy = rbind(cp1, cp2)


copy %>% 
    dplyr::select(VarY) -> Y
copy %>% 
    dplyr::select(Ag_F:Zn_F) -> X
copy %>% 
    dplyr::select(any_of(c("年龄_2Cat", "BMI_3Cat", "地区","IVF卵子数", "M2成熟卵子", "受精卵数2PN", "X1移植胚胎数量", "HCG日_内膜"))) -> C

E <- Ex.RR(Y,X,C)
E %>% 
    print(n=117)
E %>% 
    writexl::write_xlsx(str_c(outpath3,"/1.Continuous.ff.xlsx"))




# 5.1.三分类 头发--------------------
colnames(df_hair.metal)

# 处理为三分类变量
df = df_hair.metal
df %>% 
    dplyr::select(-(Ag_H1:Zn_H4))  -> temp1
df %>% 
    dplyr::select(Ag_H1:Zn_H4) %>% 
    colnames() %>% as.matrix() %>% as.vector() -> elements
#3 category function
Cat3 = function(x){
    p33 = quantile(as.matrix(df[x]), probs = (1/3), na.rm=T)
    p66 = quantile(as.matrix(df[x]), probs = (2/3), na.rm=T)
    df %>% 
        dplyr::select(any_of(x))  %>%
        as.matrix() %>% as.vector() %>%  
        cut(breaks = c(-Inf, p33[[1]], p66[[1]], Inf),
            labels = c("Low", "Middle", "High")) %>% 
        factor(levels = c("Low", "Middle", "High")) %>% 
        as_tibble() %>% 
        set_names(x) -> temp2
    return(temp2)
}
purrr::map_dfc(elements, Cat3)  -> temp2
cbind(temp1, temp2) -> df

#COPY method for log-binomial model
df %>% 
  dplyr::mutate(weight = 0.9999)  -> cp1
df %>% 
    dplyr::mutate(weight = 0.0001) %>% 
    dplyr::mutate(X1生化妊娠_Y0_N1=1-X1生化妊娠_Y0_N1) -> cp2
copy = rbind(cp1, cp2)

# #For ptrend(if needed)
# copy["Ag_H1"] %>% as.matrix() %>% factor(levels = c("Low", "Middle", "High")) %>% as.numeric()
#Function: obtain  RR of log-binomial models(3Category)
Ex.RR = function(Y,X,C){
  #Y: dataframe containing outcome
  #X: dataframe containing exposures
  #C: dataframe containing covariates
    df <- dplyr::bind_cols(Y,X,C)
    name <- colnames(X)
    #loop function
    F1 = function(
        expo,#exposure
        name #name of exposure
    ){
        #1. cRR and CI
        fit2 <- glm(as.matrix(df[,1]) ~ 
                (df[name]  %>% 
                    as.matrix %>% 
                    factor(levels = c("Low", "Middle", "High"))),
                    family = binomial(link = "log"),
                    weights = copy$weight,
                    start = c(log(mean(as.matrix(df[,1]))), rep(0,2)),
                    maxit = 10000,
                    data = df)
        tmp1_1 <- cbind(
                      Level = "Low",
                      cRR = 1,
                      LCI_c = 1,
                      UCI_c = 1, 
                      Pvalue_c = NA
                      )
        tmp1_2 <- cbind(
                      Level = "Middle",
                      cRR = exp(coef(fit2))[[2]],
                      LCI_c = exp(confint.default(fit2)[2,1]),
                      UCI_c = exp(confint.default(fit2)[2,2]), 
                      Pvalue_c = summary(fit2)$coefficients[2,4]
                      )
        tmp1_3 <- cbind(
                      Level = "High",
                      cRR = exp(coef(fit2))[[3]],
                      LCI_c = exp(confint.default(fit2)[3,1]),
                      UCI_c = exp(confint.default(fit2)[3,2]), 
                      Pvalue_c = summary(fit2)$coefficients[3,4]
                      )
        tmp1 = rbind(tmp1_1, tmp1_2, tmp1_3)
        #2. aRR and CI
        fit4 <- glm(as.matrix(df[,1]) ~ 
                    (df[name]  %>% 
                        as.matrix %>% 
                        factor(levels = c("Low", "Middle", "High"))) 
                    + 年龄_2Cat + BMI_3Cat + 地区 
                    + IVF卵子数 + M2成熟卵子 + 受精卵数2PN + X1移植胚胎数量 + HCG日_内膜, 
                    family = binomial(link = "log"), 
                    weights = copy$weight,
                    start = c(log(mean(as.matrix(df[,1]))), rep(0,11)),
                    maxit = 10000,
                    data = df)
        tmp2_1 <- cbind(
                      Level = "Low",
                      aRR = 1,
                      LCI_a = 1,
                      UCI_a = 1, 
                      Pvalue_a = NA
                      )
        tmp2_2 <- cbind(
                      Level = "Middle",
                      aRR = exp(coef(fit4))[[2]],
                      LCI_a = exp(confint.default(fit4)[2,1]),
                      UCI_a = exp(confint.default(fit4)[2,2]), 
                      Pvalue_a = summary(fit4)$coefficients[2,4]
                      )
        tmp2_3 <- cbind(
                      Level = "High",
                      aRR = exp(coef(fit4))[[3]],
                      LCI_a = exp(confint.default(fit4)[3,1]),
                      UCI_a = exp(confint.default(fit4)[3,2]), 
                      Pvalue_a = summary(fit4)$coefficients[3,4]
                      )
        tmp2 = rbind(tmp2_1, tmp2_2, tmp2_3)
        #3. combine
        cbind(tmp1, tmp2) %>% 
            tibble::as_tibble() %>% 
            dplyr::mutate(Element = name) %>% 
            dplyr::select(Element, everything()) -> tmp3
        print(paste(name, " accomplished"))
        return(tmp3)
    }
    result <- purrr::map2_dfr(X, name, F1)
    #names(result) <- c("Element", "cRR", "c_LCL", "c_UCL", "P_c", "aRR", "a_LCL", "a_UCL", "P_a")
    return(result)
}

#Hair 
copy %>% 
    dplyr::select(VarY) -> Y
copy %>% 
    dplyr::select(Ag_H1:Zn_H4) -> X
copy %>% 
    dplyr::select(any_of(c("年龄_2Cat", "BMI_3Cat", "地区","IVF卵子数", "M2成熟卵子", "受精卵数2PN", "X1移植胚胎数量", "HCG日_内膜"))) -> C
A <- Ex.RR(Y,X,C)
A %>% 
    print(n=400)
A %>% 
    dplyr::mutate(across(.cols = 3:11, as.numeric)) %>% 
    dplyr::select(-V6) %>% 
    writexl::write_xlsx(str_c(outpath3,"/2.Category3.hair.xlsx"))


# 5.2.三分类 Serum1--------------------
colnames(df_serum1.metal)
# 处理为三分类变量
df = df_serum1.metal
df %>% 
    dplyr::select(-(Ag_1:Zn_1))  -> temp1
df %>% 
    dplyr::select(Ag_1:Zn_1) %>% 
    colnames() %>% as.matrix() %>% as.vector() -> elements
#3 category function
Cat3 = function(x){
    p33 = quantile(as.matrix(df[x]), probs = (1/3))
    p66 = quantile(as.matrix(df[x]), probs = (2/3))
    df %>% 
        dplyr::select(any_of(x))  %>%
        as.matrix() %>% as.vector() %>%  
        cut(breaks = c(-Inf, p33[[1]], p66[[1]], Inf),
            labels = c("Low", "Middle", "High")) %>% 
        factor(levels = c("Low", "Middle", "High")) %>% 
        as_tibble() %>% 
        set_names(x) -> temp2
    return(temp2)
}
purrr::map_dfc(elements, Cat3)  -> temp2
cbind(temp1, temp2) -> df

#COPY method for log-binomial model
df %>% 
  dplyr::mutate(weight = 0.9999)  -> cp1
df %>% 
    dplyr::mutate(weight = 0.0001) %>% 
    dplyr::mutate(X1生化妊娠_Y0_N1=1-X1生化妊娠_Y0_N1) -> cp2
copy = rbind(cp1, cp2)

copy %>% 
    dplyr::select(VarY) -> Y
copy %>% 
    dplyr::select(Ag_1:Zn_1) -> X
copy %>% 
    dplyr::select(any_of(c("年龄_2Cat", "BMI_3Cat", "地区","IVF卵子数", "M2成熟卵子", "受精卵数2PN", "X1移植胚胎数量", "HCG日_内膜"))) -> C
B <- Ex.RR(Y,X,C)
B %>% 
    print(n=400)
B %>% 
    dplyr::mutate(across(.cols = 3:11, as.numeric)) %>% 
    dplyr::select(-V6) %>% 
    writexl::write_xlsx(str_c(outpath3,"/2.Category3.serum1.xlsx"))


# 5.3.三分类 Serum2--------------------
colnames(df_serum2.metal)
# 处理为三分类变量
df = df_serum2.metal
df %>% 
    dplyr::select(-(Ag_2:Zn_2))  -> temp1
df %>% 
    dplyr::select(Ag_2:Zn_2) %>% 
    colnames() %>% as.matrix() %>% as.vector() -> elements
#3 category function
Cat3 = function(x){
    p33 = quantile(as.matrix(df[x]), probs = (1/3))
    p66 = quantile(as.matrix(df[x]), probs = (2/3))
    df %>% 
        dplyr::select(any_of(x))  %>%
        as.matrix() %>% as.vector() %>%  
        cut(breaks = c(-Inf, p33[[1]], p66[[1]], Inf),
            labels = c("Low", "Middle", "High")) %>% 
        factor(levels = c("Low", "Middle", "High")) %>% 
        as_tibble() %>% 
        set_names(x) -> temp2
    return(temp2)
}
purrr::map_dfc(elements, Cat3)  -> temp2
cbind(temp1, temp2) -> df

#COPY method for log-binomial model
df %>% 
  dplyr::mutate(weight = 0.9999)  -> cp1
df %>% 
    dplyr::mutate(weight = 0.0001) %>% 
    dplyr::mutate(X1生化妊娠_Y0_N1=1-X1生化妊娠_Y0_N1) -> cp2
copy = rbind(cp1, cp2)

copy %>% 
    dplyr::select(VarY) -> Y
copy %>% 
    dplyr::select(Ag_2:Zn_2) -> X
copy %>% 
    dplyr::select(any_of(c("年龄_2Cat", "BMI_3Cat", "地区","IVF卵子数", "M2成熟卵子", "受精卵数2PN", "X1移植胚胎数量", "HCG日_内膜"))) -> C

D <- Ex.RR(Y,X,C)
D %>% 
    print(n=400)
D %>% 
    dplyr::mutate(across(.cols = 3:11, as.numeric)) %>% 
    dplyr::select(-V6) %>% 
    writexl::write_xlsx(str_c(outpath3,"/2.Category3.serum2.xlsx"))


# 5.4.三分类 FF--------------------
colnames(df_ff.metal)
# 处理为三分类变量
df = df_ff.metal
df %>% 
    dplyr::select(-(Ag_F:Zn_F))  -> temp1
df %>% 
    dplyr::select(Ag_F:Zn_F) %>% 
    colnames() %>% as.matrix() %>% as.vector() -> elements
#3 category function
Cat3 = function(x){
    p33 = quantile(as.matrix(df[x]), probs = (1/3))
    p66 = quantile(as.matrix(df[x]), probs = (2/3))
    df %>% 
        dplyr::select(any_of(x))  %>%
        as.matrix() %>% as.vector() %>%  
        cut(breaks = c(-Inf, p33[[1]], p66[[1]], Inf),
            labels = c("Low", "Middle", "High")) %>% 
        factor(levels = c("Low", "Middle", "High")) %>% 
        as_tibble() %>% 
        set_names(x) -> temp2
    return(temp2)
}
purrr::map_dfc(elements, Cat3)  -> temp2
cbind(temp1, temp2) -> df

#COPY method for log-binomial model
df %>% 
  dplyr::mutate(weight = 0.9999)  -> cp1
df %>% 
    dplyr::mutate(weight = 0.0001) %>% 
    dplyr::mutate(X1生化妊娠_Y0_N1=1-X1生化妊娠_Y0_N1) -> cp2
copy = rbind(cp1, cp2)

copy %>% 
    dplyr::select(VarY) -> Y
copy %>% 
    dplyr::select(Ag_F:Zn_F) -> X
copy %>% 
    dplyr::select(any_of(c("年龄_2Cat", "BMI_3Cat", "地区","IVF卵子数", "M2成熟卵子", "受精卵数2PN", "X1移植胚胎数量", "HCG日_内膜"))) -> C

E <- Ex.RR(Y,X,C)
E %>% 
    print(n=400)
E %>% 
    dplyr::mutate(across(.cols = 3:11, as.numeric)) %>% 
    dplyr::select(-V6) %>% 
    writexl::write_xlsx(str_c(outpath3,"/2.Category3.ff.xlsx"))

save.image(str_c(outpath, "/2.Associations.Rdata"))

####################################################################

