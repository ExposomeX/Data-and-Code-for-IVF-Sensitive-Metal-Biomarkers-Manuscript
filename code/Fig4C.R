rm(list=ls())
#调用包
# .libPaths("/home/renmy/R/x86_64-pc-linux-gnu-library/4.3")

#install packages
pacman::p_load(
  "bestNormalize",
  "broom",
  "car",
  "caret",
  "cluster",
  "DALEX",
  "DALEXtra",
  "data.table",
  "ddpcr",
  "factoextra",
  "fs",
  "future",
  "furrr",
  "gee",
  "geepack",
  "ggplot2",
  "ggfortify",
  "gridExtra",
  "GGally",
  "gtsummary",
  "ggpattern",
  "Hmisc",
  "lmtest",
  "lubridate",
  "magrittr",
  "mice",
  "mlr3verse",
  "mlr3viz",
  "mlr3extralearners",
  "naniar",
  "patchwork",
  "precrec",
  "rlist",
  "rstatix",
  "R6",
  "rstatix",
  "RCy3",
  "tidyverse",
  "tictoc",
  "vip",
  "vroom",
  "zip",
  "ggsci",
  "RColorBrewer",
  "circlize",
  "C50", #below machine learning
  "cluster",
  "coin",
  "dbarts",
  "e1071",
  "earth",
  "FNN",
  "gbm",
  "glmnet",
  "kernlab",
  "kknn",
  "LiblineaR",
  "lightgbm",
  "MASS",
  "mboost",
  "mgcv",
  "nnet",
  "partykit",
  "ranger",
  "rpart",
  "sandwich",
  "stats",
  "xgboost"
)

# 0-Prepare step ----------------------------------
#set work directory
inpath = stringr::str_c(getwd(), "/@-IVF-Hair-Serum-FF/input")
outpath = stringr::str_c(getwd(), "/@-IVF-Hair-Serum-FF/output")

if(!file.exists(inpath)){dir.create(inpath)}
if(!file.exists(outpath)){dir.create(outpath)}


load(str_c(outpath, "/3.BKMR_final_20231226.Rdata"))


outpath5 = stringr::str_c(outpath, "/5.Explainer")
if(!file.exists(outpath5)){dir.create(outpath5)}


#------------Fig 4C1----------------------
df = df_hair.metal
lst = hair2.list.bkmr
label = "Hair#2"



# 处理离群值
df %>% 
  dplyr::select(all_of(lst)) %>% 
  colnames() -> elements
con_l <- vector()
con_r <- vector()

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
# generate train dataset
df %>% 
  dplyr::select(all_of(VarY),
                all_of(C %>% as_tibble() %>% colnames)) %>% 
  set_names(c("Y", "Age_2Cat", "BMI_3Cat", "Region", "IVF_oocyte", "M2_mature", "PN2", "ET_number", "hCG_endo")) %>% 
  cbind(scale(df %>% dplyr::select(all_of(lst)))) %>% 
  as_tibble()   -> df.train
# set task
df.train %>%
  as_task_classif(target = "Y") -> task
# learner ranger: initial
learner_rf <- lrn(str_c("classif",".ranger"),
                  predict_type = "prob",
                  predict_sets = c("train", "test"),
                  id = "random forest")
# auto-tune range for learner_rf
set.seed(1)
at_rf = AutoTuner$new(
  learner = learner_rf,
  resampling = rsmp("cv",folds = 5),
  measure = msr("classif.acc"),
  search_space = ps(
    max.depth = p_int(lower = 6, upper = 100),
    mtry.ratio = p_dbl(lower = 0.3, upper = 0.7),
    num.trees = p_int(lower = 10, upper = 500)
  ),
  terminator = trm("evals", n_evals = 20),
  tuner = tnr("random_search")
)
# autotune for learner_rf
ddpcr::quiet(
  at_rf$train(task)
)
# set parameters for learner_rf
learner_rf$param_set$values <- at_rf$model$tuning_instance$result_learner_param_vals
learner_rf$param_set$values$importance <- "permutation"
# train rf model
learner_rf$train(task)

# explain using DALEX
df.train$Y = as.numeric(df.train$Y)
set.seed(1)
ranger_exp = explain_mlr3(learner_rf,
                          data = df.train[,-1],
                          y = df.train$Y,
                          label = "Ranger RF",
                          colorize = F)                        
rf_imp = model_parts(ranger_exp)

#plotting----
fills = c("#B03060", "#008B8B","#818181","#2A5522","#BF9895", "#E07E35", "#F2CCA0", "#A9C4E6", "#D1392B")
#order
rf_imp %>% 
  dplyr::filter(variable  %in% lst)  %>% as_tibble() %>% 
  arrange(dropout_loss,variable)  %>% 
  group_by(variable) %>% 
  dplyr::summarize(mean = mean(dropout_loss)) %>% 
  arrange(mean) %>% 
  dplyr::select(variable) %>% 
  unique() %>% as.matrix %>% as.vector  -> order
rf_imp %>% 
  dplyr::filter(variable  %in% lst)  %>% as_tibble() %>% 
  arrange(-dropout_loss,variable)  -> plt

plt$variable <- factor(plt$variable, levels = order)


#partial dependence----
rf_pd = model_profile(ranger_exp,
                      variables = df %>% dplyr::select(all_of(lst)) %>% colnames())$agr_profiles
rf_pd  %>% 
  as_tibble()  %>% 
  set_names(c("vname", "rflabel", "x", "yhat", "ids")) -> plotdata


#extract cp profile----
rf_cp = predict_profile(explainer = ranger_exp,
                        new_observation = df.train)
rf_cp %>% 
  as_tibble() %>%
  dplyr::select(all_of(lst), "_yhat_", "_vname_","_ids_", "_label_")  %>% 
  set_names(c(lst, "yhat", "vname","ids", "label")) %>% 
  dplyr::filter(vname  %in% lst) -> plt2

F2 = function(x){
  plt2 %>% 
  dplyr::filter(vname == x) %>% 
  dplyr::select(all_of(c(x, "yhat", "vname", "ids"))) %>% 
  dplyr::rename(metal = x)
}
plotdata2 = purrr::map_dfr(lst, F2)

plotdata2  %>% 
  dplyr::filter(vname=="Se_H2") %>% 
  ggplot(aes(x = metal, y = yhat, group = ids)) +
  geom_line(color = "grey", alpha = 0.3) +
  geom_smooth(data = (plotdata  %>% dplyr::filter(vname=="Se_H2")) , aes(x = x, y = yhat), se = T) + 
  scale_y_continuous(limits = c(0, 0.9), breaks = c(0,0.3,0.6,0.9),
                     expand=expansion(add = c(0.01, 0.01))) + 
  scale_x_continuous(limits = c(-2.5, 2.5), breaks = c(-2,-1,0, 1, 2),
                     expand=expansion(add = c(0.01, 0.01))) + 
  theme_bw()+
  theme(strip.text.x = element_text(size = 8),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        panel.grid = element_blank())


ggsave(str_c(outpath, "/Fig4C1.", "Se_H2", ".png"),
       height = 6,
       width = 6,
       dpi = 600)


#------------Fig 4C2----------------------
df = df_serum2.metal
lst = serum2.list.bkmr
label = "Serum#2"



# 处理离群值
df %>% 
  dplyr::select(all_of(lst)) %>% 
  colnames() -> elements
con_l <- vector()
con_r <- vector()

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
# generate train dataset
df %>% 
  dplyr::select(all_of(VarY),
                all_of(C %>% as_tibble() %>% colnames)) %>% 
  set_names(c("Y", "Age_2Cat", "BMI_3Cat", "Region", "IVF_oocyte", "M2_mature", "PN2", "ET_number", "hCG_endo")) %>% 
  cbind(scale(df %>% dplyr::select(all_of(lst)))) %>% 
  as_tibble()   -> df.train
# set task
df.train %>%
  as_task_classif(target = "Y") -> task
# learner ranger: initial
learner_rf <- lrn(str_c("classif",".ranger"),
                  predict_type = "prob",
                  predict_sets = c("train", "test"),
                  id = "random forest")
# auto-tune range for learner_rf
set.seed(1)
at_rf = AutoTuner$new(
  learner = learner_rf,
  resampling = rsmp("cv",folds = 5),
  measure = msr("classif.acc"),
  search_space = ps(
    max.depth = p_int(lower = 6, upper = 100),
    mtry.ratio = p_dbl(lower = 0.3, upper = 0.7),
    num.trees = p_int(lower = 10, upper = 500)
  ),
  terminator = trm("evals", n_evals = 20),
  tuner = tnr("random_search")
)
# autotune for learner_rf
ddpcr::quiet(
  at_rf$train(task)
)
# set parameters for learner_rf
learner_rf$param_set$values <- at_rf$model$tuning_instance$result_learner_param_vals
learner_rf$param_set$values$importance <- "permutation"
# train rf model
learner_rf$train(task)

# explain using DALEX
df.train$Y = as.numeric(df.train$Y)
set.seed(1)
ranger_exp = explain_mlr3(learner_rf,
                          data = df.train[,-1],
                          y = df.train$Y,
                          label = "Ranger RF",
                          colorize = F)                        
rf_imp = model_parts(ranger_exp)

#plotting----
fills = c("#B03060", "#008B8B","#818181","#2A5522","#BF9895", "#E07E35", "#F2CCA0", "#A9C4E6", "#D1392B")
#order
rf_imp %>% 
  dplyr::filter(variable  %in% lst)  %>% as_tibble() %>% 
  arrange(dropout_loss,variable)  %>% 
  group_by(variable) %>% 
  dplyr::summarize(mean = mean(dropout_loss)) %>% 
  arrange(mean) %>% 
  dplyr::select(variable) %>% 
  unique() %>% as.matrix %>% as.vector  -> order
rf_imp %>% 
  dplyr::filter(variable  %in% lst)  %>% as_tibble() %>% 
  arrange(-dropout_loss,variable)  -> plt

plt$variable <- factor(plt$variable, levels = order)


#partial dependence----
rf_pd = model_profile(ranger_exp,
                      variables = df %>% dplyr::select(all_of(lst)) %>% colnames())$agr_profiles
rf_pd  %>% 
  as_tibble()  %>% 
  set_names(c("vname", "rflabel", "x", "yhat", "ids")) -> plotdata


#extract cp profile----
rf_cp = predict_profile(explainer = ranger_exp,
                        new_observation = df.train)
rf_cp %>% 
  as_tibble() %>%
  dplyr::select(all_of(lst), "_yhat_", "_vname_","_ids_", "_label_")  %>% 
  set_names(c(lst, "yhat", "vname","ids", "label")) %>% 
  dplyr::filter(vname  %in% lst) -> plt2

F2 = function(x){
  plt2 %>% 
  dplyr::filter(vname == x) %>% 
  dplyr::select(all_of(c(x, "yhat", "vname", "ids"))) %>% 
  dplyr::rename(metal = x)
}
plotdata2 = purrr::map_dfr(lst, F2)

plotdata2  %>% 
  dplyr::filter(vname=="Zn_2") %>% 
  ggplot(aes(x = metal, y = yhat, group = ids)) +
  geom_line(color = "grey", alpha = 0.3) +
  geom_smooth(data = (plotdata  %>% dplyr::filter(vname=="Zn_2")) , aes(x = x, y = yhat), se = T) + 
  scale_y_continuous(limits = c(0, 0.9), breaks = c(0,0.3,0.6,0.9),
                     expand=expansion(add = c(0.01, 0.01))) + 
  scale_x_continuous(limits = c(-2.5, 2.5), breaks = c(-2,-1,0, 1, 2),
                     expand=expansion(add = c(0.01, 0.01))) + 
  theme_bw()+
  theme(strip.text.x = element_text(size = 8),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        panel.grid = element_blank())


ggsave(str_c(outpath, "/Fig4C2.", "Zn_2", ".png"),
       height = 6,
       width = 6,
       dpi = 600)



#------------Fig S8B----------------------
df = df_hair.metal
lst = hair4.list.bkmr
label = "Hair#4"



# 处理离群值
df %>% 
  dplyr::select(all_of(lst)) %>% 
  colnames() -> elements
con_l <- vector()
con_r <- vector()

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
# generate train dataset
df %>% 
  dplyr::select(all_of(VarY),
                all_of(C %>% as_tibble() %>% colnames)) %>% 
  set_names(c("Y", "Age_2Cat", "BMI_3Cat", "Region", "IVF_oocyte", "M2_mature", "PN2", "ET_number", "hCG_endo")) %>% 
  cbind(scale(df %>% dplyr::select(all_of(lst)))) %>% 
  as_tibble()   -> df.train
# set task
df.train %>%
  as_task_classif(target = "Y") -> task
# learner ranger: initial
learner_rf <- lrn(str_c("classif",".ranger"),
                  predict_type = "prob",
                  predict_sets = c("train", "test"),
                  id = "random forest")
# auto-tune range for learner_rf
set.seed(1)
at_rf = AutoTuner$new(
  learner = learner_rf,
  resampling = rsmp("cv",folds = 5),
  measure = msr("classif.acc"),
  search_space = ps(
    max.depth = p_int(lower = 6, upper = 100),
    mtry.ratio = p_dbl(lower = 0.3, upper = 0.7),
    num.trees = p_int(lower = 10, upper = 500)
  ),
  terminator = trm("evals", n_evals = 20),
  tuner = tnr("random_search")
)
# autotune for learner_rf
ddpcr::quiet(
  at_rf$train(task)
)
# set parameters for learner_rf
learner_rf$param_set$values <- at_rf$model$tuning_instance$result_learner_param_vals
learner_rf$param_set$values$importance <- "permutation"
# train rf model
learner_rf$train(task)

# explain using DALEX
df.train$Y = as.numeric(df.train$Y)
set.seed(1)
ranger_exp = explain_mlr3(learner_rf,
                          data = df.train[,-1],
                          y = df.train$Y,
                          label = "Ranger RF",
                          colorize = F)                        
rf_imp = model_parts(ranger_exp)

#plotting----
fills = c("#B03060", "#008B8B","#818181","#2A5522","#BF9895", "#E07E35", "#F2CCA0", "#A9C4E6", "#D1392B")
#order
rf_imp %>% 
  dplyr::filter(variable  %in% lst)  %>% as_tibble() %>% 
  arrange(dropout_loss,variable)  %>% 
  group_by(variable) %>% 
  dplyr::summarize(mean = mean(dropout_loss)) %>% 
  arrange(mean) %>% 
  dplyr::select(variable) %>% 
  unique() %>% as.matrix %>% as.vector  -> order
rf_imp %>% 
  dplyr::filter(variable  %in% lst)  %>% as_tibble() %>% 
  arrange(-dropout_loss,variable)  -> plt

plt$variable <- factor(plt$variable, levels = order)


#partial dependence----
rf_pd = model_profile(ranger_exp,
                      variables = df %>% dplyr::select(all_of(lst)) %>% colnames())$agr_profiles
rf_pd  %>% 
  as_tibble()  %>% 
  set_names(c("vname", "rflabel", "x", "yhat", "ids")) -> plotdata


#extract cp profile----
rf_cp = predict_profile(explainer = ranger_exp,
                        new_observation = df.train)
rf_cp %>% 
  as_tibble() %>%
  dplyr::select(all_of(lst), "_yhat_", "_vname_","_ids_", "_label_")  %>% 
  set_names(c(lst, "yhat", "vname","ids", "label")) %>% 
  dplyr::filter(vname  %in% lst) -> plt2

F2 = function(x){
  plt2 %>% 
  dplyr::filter(vname == x) %>% 
  dplyr::select(all_of(c(x, "yhat", "vname", "ids"))) %>% 
  dplyr::rename(metal = x)
}
plotdata2 = purrr::map_dfr(lst, F2)

plotdata2  %>% 
  dplyr::filter(vname=="Co_H4") %>% 
  ggplot(aes(x = metal, y = yhat, group = ids)) +
  geom_line(color = "grey", alpha = 0.3) +
  geom_smooth(data = (plotdata  %>% dplyr::filter(vname=="Co_H4")) , aes(x = x, y = yhat), se = T) + 
  scale_y_continuous(limits = c(0, 0.8), breaks = c(0,0.2,0.4,0.6,0.8),
                     expand=expansion(add = c(0.01, 0.01))) + 
  scale_x_continuous(limits = c(-1, 2), breaks = c(-1,0, 1, 2),
                     expand=expansion(add = c(0.05, 0.05))) + 
  theme_bw()+
  theme(strip.text.x = element_text(size = 8),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        panel.grid = element_blank())


ggsave(str_c(outpath, "/FigS8B", ".Co_H4", ".png"),
       height = 6,
       width = 6,
       dpi = 600)

#------------Fig S8C----------------------
df = df_serum1.metal
lst = serum1.list.bkmr
label = "Serum#1"



# 处理离群值
df %>% 
  dplyr::select(all_of(lst)) %>% 
  colnames() -> elements
con_l <- vector()
con_r <- vector()

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
# generate train dataset
df %>% 
  dplyr::select(all_of(VarY),
                all_of(C %>% as_tibble() %>% colnames)) %>% 
  set_names(c("Y", "Age_2Cat", "BMI_3Cat", "Region", "IVF_oocyte", "M2_mature", "PN2", "ET_number", "hCG_endo")) %>% 
  cbind(scale(df %>% dplyr::select(all_of(lst)))) %>% 
  as_tibble()   -> df.train
# set task
df.train %>%
  as_task_classif(target = "Y") -> task
# learner ranger: initial
learner_rf <- lrn(str_c("classif",".ranger"),
                  predict_type = "prob",
                  predict_sets = c("train", "test"),
                  id = "random forest")
# auto-tune range for learner_rf
set.seed(1)
at_rf = AutoTuner$new(
  learner = learner_rf,
  resampling = rsmp("cv",folds = 5),
  measure = msr("classif.acc"),
  search_space = ps(
    max.depth = p_int(lower = 6, upper = 100),
    mtry.ratio = p_dbl(lower = 0.3, upper = 0.7),
    num.trees = p_int(lower = 10, upper = 500)
  ),
  terminator = trm("evals", n_evals = 20),
  tuner = tnr("random_search")
)
# autotune for learner_rf
ddpcr::quiet(
  at_rf$train(task)
)
# set parameters for learner_rf
learner_rf$param_set$values <- at_rf$model$tuning_instance$result_learner_param_vals
learner_rf$param_set$values$importance <- "permutation"
# train rf model
learner_rf$train(task)

# explain using DALEX
df.train$Y = as.numeric(df.train$Y)
set.seed(1)
ranger_exp = explain_mlr3(learner_rf,
                          data = df.train[,-1],
                          y = df.train$Y,
                          label = "Ranger RF",
                          colorize = F)                        
rf_imp = model_parts(ranger_exp)

#plotting----
fills = c("#B03060", "#008B8B","#818181","#2A5522","#BF9895", "#E07E35", "#F2CCA0", "#A9C4E6", "#D1392B")
#order
rf_imp %>% 
  dplyr::filter(variable  %in% lst)  %>% as_tibble() %>% 
  arrange(dropout_loss,variable)  %>% 
  group_by(variable) %>% 
  dplyr::summarize(mean = mean(dropout_loss)) %>% 
  arrange(mean) %>% 
  dplyr::select(variable) %>% 
  unique() %>% as.matrix %>% as.vector  -> order
rf_imp %>% 
  dplyr::filter(variable  %in% lst)  %>% as_tibble() %>% 
  arrange(-dropout_loss,variable)  -> plt

plt$variable <- factor(plt$variable, levels = order)


#partial dependence----
rf_pd = model_profile(ranger_exp,
                      variables = df %>% dplyr::select(all_of(lst)) %>% colnames())$agr_profiles
rf_pd  %>% 
  as_tibble()  %>% 
  set_names(c("vname", "rflabel", "x", "yhat", "ids")) -> plotdata


#extract cp profile----
rf_cp = predict_profile(explainer = ranger_exp,
                        new_observation = df.train)
rf_cp %>% 
  as_tibble() %>%
  dplyr::select(all_of(lst), "_yhat_", "_vname_","_ids_", "_label_")  %>% 
  set_names(c(lst, "yhat", "vname","ids", "label")) %>% 
  dplyr::filter(vname  %in% lst) -> plt2

F2 = function(x){
  plt2 %>% 
  dplyr::filter(vname == x) %>% 
  dplyr::select(all_of(c(x, "yhat", "vname", "ids"))) %>% 
  dplyr::rename(metal = x)
}
plotdata2 = purrr::map_dfr(lst, F2)

plotdata2  %>% 
  dplyr::filter(vname=="Fe_1") %>% 
  ggplot(aes(x = metal, y = yhat, group = ids)) +
  geom_line(color = "grey", alpha = 0.3) +
  geom_smooth(data = (plotdata  %>% dplyr::filter(vname=="Fe_1")) , aes(x = x, y = yhat), se = T) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0,0.25,0.5,0.75,1),
                     expand=expansion(add = c(0.01, 0.01))) + 
  scale_x_continuous(limits = c(-2, 2), breaks = c(-2,-1,0, 1, 2),
                     expand=expansion(add = c(0.07, 0.07))) + 
  theme_bw()+
  theme(strip.text.x = element_text(size = 8),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        panel.grid = element_blank())


ggsave(str_c(outpath, "/FigS8C", ".Fe_1", ".png"),
       height = 6,
       width = 6,
       dpi = 600)
