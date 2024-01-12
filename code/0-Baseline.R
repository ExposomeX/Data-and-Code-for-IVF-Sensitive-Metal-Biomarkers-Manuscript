rm(list=ls())
#调用包
.libPaths("/home/renmy/R/x86_64-pc-linux-gnu-library/4.3")
# install.packages("pacman")
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

# 1-----Input data---------------------------
#缺失值未填补数据
exp1 <- read.xlsx(stringr::str_c(inpath, "/ExFile-1-IVF305Hair-ngmg-lessLOD-na.xlsx"))

#主数据库
data <- read_sav(stringr::str_c(inpath, "/IVF-total-20200603_P1_305_P2_183.sav"), encoding = "UTF-8")
data %>%
    dplyr::mutate(ID = as.numeric(ID)) -> data#convert ID datatype as numeric

#将缺失未填补数据NA替换为0
exp1 %>%
    as_tibble() %>% 
    dplyr::select(ID:Zn_4) %>%
    purrr::map_dfc( ~ tidyr::replace_na(.x, 0)) -> tmp

#合并缺失值不填补数据（此时已经与主数据库保持一致，未检出替换为0）至主数据库
tmp %>%
    dplyr::rename_with(~sub("_", "_H", .x), .cols = everything()) %>%
    # Hmisc::upData(labels = c(#将头发测定结果的变量名称更改为_H后缀缩写，添加标签说明
    #     Ag_H1 = "Ag_hair(1-3cm),ng/mg",
    #     Ag_H2 = "Ag_hair(4-6cm),ng/mg",
    #     Ag_H3 = "Ag_hair(7-9cm),ng/mg",
    #     Ag_H4 = "Ag_hair(10-12cm),ng/mg"))  %>%
    merge(data, by = "ID", all.y = T) %>% 
    as_tibble() -> data
colnames(data)

data %>% 
    dplyr::mutate(dplyr::across(all_of(
      c("X1移植胚胎数量", "Gn天数","X1卵裂囊胚", "IVF卵子数", "M2成熟卵子", "受精卵数2PN","卵泡数")
      ), as.integer)) -> data

# Define Y X C 
# Define Y: 胚胎着床
VarY = "X1生化妊娠_Y0_N1"

# Define C  基本信息
C1 = c("年龄_2Cat", "BMI_3Cat", "民族_2Cat","文化程度_4Cat","职业_2Cat", #基本信息
          "主动吸烟","被动吸烟",# 生活习惯
          "病因_3Cat")# 病因
# Define C  临床诊疗信息
C2 = c("治疗方案_3Cat", "受精方式","X1移植胚胎数量", "X1新鲜冷冻","Gn量", "Gn天数", # 治疗方案相关
  "X1受精率", "X1优质胚胎率", "X1卵裂囊胚", "IVF卵子数", "M2成熟卵子", "受精卵数2PN",# 胚胎质量相关
  "基础E2", "基础LH", "基础FSH", "基础T睾酮","甲功FT4","甲功FT3","甲功TSH", # 激素相关
  "卵泡数", "HCG日_内膜","HCG日_E2", "HCG日_LH"  # 取卵日相关
  )
# Define X  第一次血清金属
data %>%
  dplyr::select(As_1:Li_1) %>%
  colnames() -> X1
# Define X  第二次血清金属
data %>%
  dplyr::select(As_2:Li_2) %>%
  colnames() -> X2
# Define X  卵泡液金属
data %>%
  dplyr::select(As_F:Li_F) %>%
  colnames() -> X3
# Define X  第一次血清PFAS
data %>%
  dplyr::select(PFBA_1:P_62Cl_PFESA_1) %>%
  colnames() -> X4
# Define X  第二次血清PFAS
data %>%
  dplyr::select(PFBA_2:P_62Cl_PFESA_2) %>%
  colnames() -> X5
# Define X  头发金属
data %>%
  dplyr::select(Ag_H1:Zn_H4) %>%
  colnames() -> X6
# Define M: 
data %>%
  dplyr::select(IL8_1:总脂肪_1, IL8_2:总脂肪_2) %>%
  colnames() -> M1
# Bind columns
data %>%
  dplyr::select(ID,
                any_of(VarY),
                地区,# group variable to distinguish train and test set
                年龄,BMI,
                any_of(C1), # baseline vars
                any_of(C2), # clinical associated vars
                any_of(X1), # first serum metals
                any_of(X2), # second serum metals
                any_of(X3), # FF metals
                any_of(X4), # first serum PFAS
                any_of(X5), # second serum PFAS
                any_of(X6), # hair metals
                any_of(M1) # mediators
                ) -> tmp

colnames(tmp)


# 2-缺失值填补-基于全人群数据库-----------------------------------
# 2.1 社会人口学变量填补
tmp %>% 
  dplyr::select(all_of(C1),
                all_of(C2)) %>% 
  mice::mice(m = 10,
             seed = 20231213,
             method = "cart",
             print = F) %>%
  complete() %>%
  as_tibble() -> tmp2

tmp %>% 
  dplyr::select(ID, any_of(VarY)) %>% 
  cbind(tmp2) %>% 
  cbind(tmp %>% 
          dplyr::select(-ID,
                        -all_of(VarY),
                        -all_of(C1),
                        -all_of(C2))) %>% 
  as_tibble() -> tmp
colnames(tmp)
# 设置变量为factor,并根据Zn检出率为100%的特性，将各样本集划分为不同的数据集，进行暴露的插补以及数据分析
# hair.metal
tmp %>%
  dplyr::filter(is.na(Zn_H1) == FALSE) %>% #Zn_H1检出率为100%
  dplyr::select(any_of(c("ID", VarY,C1, C2, X6, M1, "地区", "年龄", "BMI"))) %>% 
  dplyr::mutate(dplyr::across(all_of(c("年龄_2Cat", "BMI_3Cat", "民族_2Cat", "文化程度_4Cat", "职业_2Cat", 
                                      "主动吸烟", "被动吸烟", 
                                      "受精方式", "病因_3Cat", "治疗方案_3Cat", "X1新鲜冷冻", "X1移植胚胎数量",
                                      "地区")), as.factor)) -> df_hair.metal
# serum1.metal
tmp %>%
  dplyr::filter(is.na(Zn_1) == FALSE) %>% #Zn_1检出率为100%
  dplyr::select(any_of(c("ID",VarY,C1, C2, X1, M1, "地区", "年龄", "BMI"))) %>% 
  dplyr::mutate(dplyr::across(all_of(c("年龄_2Cat", "BMI_3Cat", "民族_2Cat", "文化程度_4Cat", "职业_2Cat", 
                                      "主动吸烟", "被动吸烟", 
                                      "受精方式", "病因_3Cat", "治疗方案_3Cat", "X1新鲜冷冻", "X1移植胚胎数量",
                                      "年龄","BMI",
                                      "地区")), as.factor)) -> df_serum1.metal
# serum2.metal
tmp %>%
  dplyr::filter(is.na(Zn_2) == FALSE) %>% #Zn_1检出率为100%
  dplyr::select(any_of(c("ID",VarY,C1, C2, X2, M1, "地区", "年龄", "BMI"))) %>% 
  dplyr::mutate(dplyr::across(all_of(c("年龄_2Cat", "BMI_3Cat", "民族_2Cat", "文化程度_4Cat", "职业_2Cat", 
                                      "主动吸烟", "被动吸烟", 
                                      "受精方式", "病因_3Cat", "治疗方案_3Cat", "X1新鲜冷冻", "X1移植胚胎数量",
                                      "地区")), as.factor)) -> df_serum2.metal
# FollicularFluid.metal
tmp %>%
  dplyr::filter(is.na(Zn_F) == FALSE) %>% #Zn_1检出率为100%
  dplyr::select(any_of(c("ID",VarY,C1, C2, X3, M1, "地区", "年龄", "BMI"))) %>% 
  dplyr::mutate(dplyr::across(all_of(c("年龄_2Cat", "BMI_3Cat", "民族_2Cat", "文化程度_4Cat", "职业_2Cat", 
                                      "主动吸烟", "被动吸烟", 
                                      "受精方式", "病因_3Cat", "治疗方案_3Cat", "X1新鲜冷冻", "X1移植胚胎数量",
                                      "地区")), as.factor)) -> df_ff.metal


# 3 描述统计（附件内容）：比较全人群与各亚人群（头发人群、血清人群、卵泡液人群）基本特征 --------------------
#设置输出路径
outpath1 = stringr::str_c(outpath, "/1.BasicInfo")
if(!file.exists(outpath1)){dir.create(outpath1)}
#待用变量列表
ttvars <- c("年龄_2Cat", "BMI_3Cat", "民族_2Cat",
             "地区", "文化程度_4Cat", "职业_2Cat", #基本信息
             "主动吸烟", "被动吸烟",# 生活习惯
             "病因_3Cat", #不孕原因相关
             "治疗方案_3Cat", "受精方式",#治疗方案相关
             "X1移植胚胎数量", "X1新鲜冷冻",#移植相关
             "Gn量", "Gn天数", # 治疗方案相关
             "X1受精率", "X1优质胚胎率", "X1卵裂囊胚", "IVF卵子数", "M2成熟卵子", "受精卵数2PN",# 胚胎质量相关
             "基础E2", "基础LH", "基础FSH", "基础T睾酮","甲功FT4","甲功FT3","甲功TSH", # 激素相关
             "卵泡数", "HCG日_内膜","HCG日_E2", "HCG日_LH",# 取卵日相关
             "X1生化妊娠_Y0_N1",#结局相关
             "年龄", "BMI"#连续的年龄BMI
)

catvars <- c("年龄_2Cat", "BMI_3Cat", "民族_2Cat",
             "地区", "文化程度_4Cat", "职业_2Cat", #基本信息
             "主动吸烟", "被动吸烟",# 生活习惯
             "病因_3Cat", #不孕原因相关
             "治疗方案_3Cat", "受精方式",#治疗方案相关
             "X1新鲜冷冻","X1移植胚胎数量",#移植相关
             "X1生化妊娠_Y0_N1"#结局相关
)
nonnormalvars = c("X1移植胚胎数量","Gn量", "Gn天数", # 治疗方案相关
                  "X1受精率", "X1优质胚胎率", "X1卵裂囊胚", "IVF卵子数", "M2成熟卵子", "受精卵数2PN",# 胚胎质量相关
                  "基础E2", "基础LH", "基础FSH", "基础T睾酮","甲功FT4","甲功FT3","甲功TSH", # 激素相关
                  "卵泡数", "HCG日_内膜","HCG日_E2", "HCG日_LH")
# 3.1 头发样本
data %>%
  dplyr::mutate(include = dplyr::if_else(is.na(Zn_H1),0,1)) -> data1

tab1 <-  print(CreateTableOne(vars = ttvars, strata = "include",
                              data = data1, factorVars = catvars,
                              includeNA = FALSE),
               exact = c("民族_2Cat",
                         "X1移植胚胎数量",
                         "治疗方案_3Cat",
                         "文化程度_4Cat",
                         "主动吸烟"),
               nonnormal = nonnormalvars,
               showAllLevels = TRUE)

rownames(tab1)  %>% 
  as_tibble()  %>% 
  cbind(tab1 %>% 
  as_tibble()) %>% 
  as_tibble() %>% 
  set_names(c("Variables", "Levels","FullPopu", "HairPopu", "Pvalue", "Test type")) %>% 
writexl::write_xlsx(stringr::str_c(outpath1,"/TableS1-BasicCharac.FullPopu.vs.HairPopu.xlsx"))
# 3.2 血清(第二次）样本（因为第一次就是305人全人群，不需要比较）
data %>%
  dplyr::mutate(include = dplyr::if_else(is.na(Zn_2),0,1)) -> data1

tab2 <-  print(CreateTableOne(vars = ttvars, strata = "include",
                              data = data1, factorVars = catvars,
                              includeNA = FALSE),
               exact = c("民族_2Cat",
                         "X1移植胚胎数量",
                         "治疗方案_3Cat",
                         "文化程度_4Cat",
                         "主动吸烟"),
               nonnormal = nonnormalvars,
               showAllLevels = TRUE)
rownames(tab2)  %>% 
  as_tibble()  %>% 
  cbind(tab2 %>% 
  as_tibble()) %>% 
  as_tibble() %>% 
  set_names(c("Variables", "Levels","FullPopu", "SerumPopu", "Pvalue", "Test type")) %>% 
writexl::write_xlsx(stringr::str_c(outpath1,"/TableS2-BasicCharac.FullPopu.vs.SerumPopu.xlsx"))
# 3.3 卵泡液样本
data %>%
  dplyr::mutate(include = dplyr::if_else(is.na(Zn_F),0,1)) -> data1

tab3 <-  print(CreateTableOne(vars = ttvars, strata = "include",
                              data = data1, factorVars = catvars,
                              includeNA = FALSE),
               exact = c("民族_2Cat",
                         "X1移植胚胎数量",
                         "治疗方案_3Cat",
                         "文化程度_4Cat",
                         "主动吸烟"),
               nonnormal = nonnormalvars,
               showAllLevels = TRUE)
rownames(tab3)  %>% 
  as_tibble()  %>% 
  cbind(tab3 %>% 
  as_tibble()) %>% 
  as_tibble() %>% 
  set_names(c("Variables", "Levels","FullPopu", "FFPopu", "Pvalue", "Test type")) %>% 
writexl::write_xlsx(stringr::str_c(outpath1,"/TableS3-BasicCharac.FullPopu.vs.FFPopu.xlsx"))
#删除无用变量--释放内存
rm(tab1, tab2, tab3, data1, catvars, ttvars)


# 4. TableOne-比较变量为地区 -------------------------------
#待用变量列表
ttvars <- c("年龄_2Cat", "BMI_3Cat", "民族_2Cat",
             "文化程度_4Cat", "职业_2Cat", #基本信息
             "主动吸烟", "被动吸烟",# 生活习惯
             "病因_3Cat", #不孕原因相关
             "治疗方案_3Cat", "受精方式",#治疗方案相关
             "X1移植胚胎数量", "X1新鲜冷冻",#移植相关
             "Gn量", "Gn天数", # 治疗方案相关
             "X1受精率", "X1优质胚胎率", "X1卵裂囊胚", "IVF卵子数", "M2成熟卵子", "受精卵数2PN",# 胚胎质量相关
             "基础E2", "基础LH", "基础FSH", "基础T睾酮","甲功FT4","甲功FT3","甲功TSH", # 激素相关
             "卵泡数", "HCG日_内膜","HCG日_E2", "HCG日_LH",# 取卵日相关
             "X1生化妊娠_Y0_N1",#结局相关
             "年龄", "BMI"#连续的年龄BMI
)
catvars <- c("年龄_2Cat", "BMI_3Cat", "民族_2Cat",
             "文化程度_4Cat", "职业_2Cat", #基本信息
             "主动吸烟", "被动吸烟",# 生活习惯
             "病因_3Cat", #不孕原因相关
             "治疗方案_3Cat", "受精方式",#治疗方案相关
             "X1新鲜冷冻","X1移植胚胎数量",#移植相关
             "X1生化妊娠_Y0_N1"#结局相关
)
nonnormalvars = c("X1移植胚胎数量","Gn量", "Gn天数", # 治疗方案相关
                  "X1受精率", "X1优质胚胎率", "X1卵裂囊胚", "IVF卵子数", "M2成熟卵子", "受精卵数2PN",# 胚胎质量相关
                  "基础E2", "基础LH", "基础FSH", "基础T睾酮","甲功FT4","甲功FT3","甲功TSH", # 激素相关
                  "卵泡数", "HCG日_内膜","HCG日_E2", "HCG日_LH")

# 全人群
tab4 <-  print(CreateTableOne(vars = ttvars, strata = "地区",
                              data = data, factorVars = catvars,
                              includeNA = FALSE),
               exact = c("民族_2Cat",
                         "X1移植胚胎数量",
                         "治疗方案_3Cat",
                         "文化程度_4Cat",
                         "主动吸烟"),
               nonnormal = nonnormalvars,
               showAllLevels = TRUE)
rownames(tab4)  %>% 
  as_tibble()  %>% 
  cbind(tab4 %>% 
  as_tibble()) %>% 
  as_tibble() %>% 
  set_names(c("Variables", "Levels","FullPopu", "FFPopu", "Pvalue", "Test type")) %>% 
writexl::write_xlsx(stringr::str_c(outpath1,"/TableS4-BasicCharac.Beijing.vs.Shandong.xlsx"))
rm(tab4, catvars, ttvars, nonnormalvars)


# 5 TableOne-比较变量为生化妊娠(X1生化妊娠_Y0_N1) -------------------------------------------------------------
#待用变量列表
ttvars <- c("年龄_2Cat", "BMI_3Cat", "民族_2Cat",
            "地区", "文化程度_4Cat", "职业_2Cat", #基本信息
             "主动吸烟", "被动吸烟",# 生活习惯
             "病因_3Cat", #不孕原因相关
             "治疗方案_3Cat", "受精方式",#治疗方案相关
             "X1移植胚胎数量", "X1新鲜冷冻",#移植相关
             "Gn量", "Gn天数", # 治疗方案相关
             "X1受精率", "X1优质胚胎率", "X1卵裂囊胚", "IVF卵子数", "M2成熟卵子", "受精卵数2PN",# 胚胎质量相关
             "基础E2", "基础LH", "基础FSH", "基础T睾酮","甲功FT4","甲功FT3","甲功TSH", # 激素相关
             "卵泡数", "HCG日_内膜","HCG日_E2", "HCG日_LH",# 取卵日相关
             "年龄", "BMI"#连续的年龄BMI
)
catvars <- c("年龄_2Cat", "BMI_3Cat", "民族_2Cat",
             "地区", "文化程度_4Cat", "职业_2Cat", #基本信息
             "主动吸烟", "被动吸烟",# 生活习惯
             "病因_3Cat", #不孕原因相关
             "治疗方案_3Cat", "受精方式",#治疗方案相关
             "X1新鲜冷冻","X1移植胚胎数量"#移植相关
)
nonnormalvars = c("X1移植胚胎数量","Gn量", "Gn天数", # 治疗方案相关
                  "X1受精率", "X1优质胚胎率", "X1卵裂囊胚", "IVF卵子数", "M2成熟卵子", "受精卵数2PN",# 胚胎质量相关
                  "基础E2", "基础LH", "基础FSH", "基础T睾酮","甲功FT4","甲功FT3","甲功TSH", # 激素相关
                  "卵泡数", "HCG日_内膜","HCG日_E2", "HCG日_LH")

# 5.1 头发样本
tab1 <-  print(CreateTableOne(vars = ttvars, strata = "X1生化妊娠_Y0_N1",
                              data = df_hair.metal, factorVars = catvars,
                              includeNA = FALSE),
               exact = c("民族_2Cat",
                         "X1移植胚胎数量",
                         "治疗方案_3Cat",
                         "文化程度_4Cat",
                         "主动吸烟"),
               nonnormal = nonnormalvars,
               showAllLevels = TRUE)
rownames(tab1)  %>% 
  as_tibble()  %>% 
  cbind(tab1 %>% 
  as_tibble()) %>% 
  as_tibble() %>% 
  set_names(c("Variables", "Levels","Control", "CPF", "Pvalue", "Test type")) %>% 
writexl::write_xlsx(stringr::str_c(outpath1,"/TableS5-BasicCharac.HairPopu.Outcome.xlsx"))

# 5.2 血清样本
tab2 <-  print(CreateTableOne(vars = ttvars, strata = "X1生化妊娠_Y0_N1",
                              data = df_serum2.metal, factorVars = catvars,
                              includeNA = FALSE),
               exact = c("民族_2Cat",
                         "X1移植胚胎数量",
                         "治疗方案_3Cat",
                         "文化程度_4Cat",
                         "主动吸烟"),
               nonnormal = nonnormalvars,
               showAllLevels = TRUE)
rownames(tab2)  %>% 
  as_tibble()  %>% 
  cbind(tab2 %>% 
  as_tibble()) %>% 
  as_tibble() %>% 
  set_names(c("Variables", "Levels","Control", "CPF", "Pvalue", "Test type")) %>% 
writexl::write_xlsx(stringr::str_c(outpath1,"/TableS6-BasicCharac.SerumPopu.Outcome.xlsx"))

# 5.3 卵泡液样本
tab3 <-  print(CreateTableOne(vars = ttvars, strata = "X1生化妊娠_Y0_N1",
                              data = df_ff.metal, factorVars = catvars,
                              includeNA = FALSE),
               exact = c("民族_2Cat",
                         "X1移植胚胎数量",
                         "治疗方案_3Cat",
                         "文化程度_4Cat",
                         "主动吸烟"),
               nonnormal = nonnormalvars,
               showAllLevels = TRUE)
rownames(tab3)  %>% 
  as_tibble()  %>% 
  cbind(tab3 %>% 
  as_tibble()) %>% 
  as_tibble() %>% 
  set_names(c("Variables", "Levels","Control", "CPF", "Pvalue", "Test type")) %>% 
writexl::write_xlsx(stringr::str_c(outpath1,"/TableS7-BasicCharac.FFPopu.Outcome.xlsx"))

# 5.4 全人群
tab4 <-  print(CreateTableOne(vars = ttvars, strata = "X1生化妊娠_Y0_N1",
                              data = data, factorVars = catvars,
                              includeNA = FALSE),
               exact = c("民族_2Cat",
                         "X1移植胚胎数量",
                         "治疗方案_3Cat",
                         "文化程度_4Cat",
                         "主动吸烟"),
               nonnormal = nonnormalvars,
               showAllLevels = TRUE)
rownames(tab4)  %>% 
  as_tibble()  %>% 
  cbind(tab4 %>% 
  as_tibble()) %>% 
  as_tibble() %>% 
  set_names(c("Variables", "Levels","Control", "CPF", "Pvalue", "Test type")) %>% 
writexl::write_xlsx(stringr::str_c(outpath1,"/Table1-BasicCharac.FullPopu.Outcome.xlsx"))
#删除无用变量--释放内存
rm(tab1, tab2, tab3, tab4, catvars, ttvars,nonnormalvars)


# 3-Save Rdata -------------------------------------------------------
save.image(str_c(outpath,"/0-BaselineInfo.Rdata"))

###############################################################
