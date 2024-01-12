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


# export results for BKMR models------------------------------------------
load(str_c(outpath, "/3.BKMR_final_20231226.Rdata"))
outpath4 = str_c(outpath, "/4.Associations.BKMR")
if(!file.exists(outpath4)){dir.create(outpath4)}

# hair1
ExtractPIPs(fitkm1)
# #Partial dependence
# pred.resp.univar <- PredictorResponseUnivar(fit = fitkm1, q.fixed = 0.5)
# pred.resp.univar
# ggplot(pred.resp.univar, aes(z, est, ymin = est - 1.96*se,
#                                ymax = est + 1.96*se)) +
#     geom_smooth(stat = "identity") +
#     facet_wrap(~variable, ncol =8) +
#     xlab("scaled concentrations") +
#     ylab("h(exposures)") +
#     theme_bw() + 
#     theme(panel.grid = element_blank()) +
#     geom_hline(yintercept = 0, linetype=2,color="red")


# ggsave(str_c(outpath4, "/PD.Hair1.png"), height = 4, width = 12, dpi = 600)
# # mixture effect
# risks.overall <- OverallRiskSummaries(fit = fitkm1, qs = seq(0.1, 0.9, by = 0.05), q.fixed = 0.5)
# risks.overall
# ggplot(risks.overall, aes(quantile, est, ymin = est - 1.96*sd, 
#                           ymax = est + 1.96*sd)) + 
#     geom_hline(yintercept = 0, lty = 2, col = "brown") +
#     theme_bw() + 
#     theme(panel.grid = element_blank()) +
#     geom_pointrange() 
# ggsave(str_c(outpath4, "/MixEffect.Hair1.png"), height = 4, width = 6, dpi = 600)
#Single exposure estimate
risks.singvar <- SingVarRiskSummaries(
    fit = fitkm1, qs.diff = c(0.25,0.75), 
    q.fixed = c(0.25,0.5,0.75))
ggplot(risks.singvar, aes(variable, est, ymin = est - 1.96*sd, ymax = est + 1.96*sd, 
                          color = q.fixed, shape=q.fixed)) +
    geom_pointrange(position = position_dodge(width = 0.75)) + 
    geom_hline(yintercept = 0, linetype=2,color="red") +
    scale_y_continuous(limits = c(-0.42, 0.42),
                       breaks = c(-0.4, -0.2,0,0.2,0.4),
                       expand=expansion(add = c(0.01, 0.01)))+
    theme_bw() +
    theme(panel.grid = element_blank()) 
ggsave(str_c(outpath4, "/Single.risk.p25vsp75.Hair1.png"), height = 4, width = 6, dpi = 600)



# hair2
ExtractPIPs(fitkm2)
# #Partial dependence
# pred.resp.univar <- PredictorResponseUnivar(fit = fitkm2, q.fixed = 0.5)
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

# ggsave(str_c(outpath4, "/PD.Hair2.png"), height = 8, width = 12, dpi = 600)

# risks.overall <- OverallRiskSummaries(fit = fitkm2, qs = seq(0.1, 0.9, by = 0.05), q.fixed = 0.5)
# risks.overall
# ggplot(risks.overall, aes(quantile, est, ymin = est - 1.96*sd, 
#                           ymax = est + 1.96*sd)) + 
#     geom_hline(yintercept = 0, lty = 2, col = "brown") +
#     theme_bw() + 
#     theme(panel.grid = element_blank()) +   
#     geom_pointrange()
# ggsave(str_c(outpath4, "/MixEffect.Hair2.png"), height = 4, width = 6, dpi = 600)
#Single exposure estimate
risks.singvar <- SingVarRiskSummaries(
    fit = fitkm2, qs.diff = c(0.25,0.75), 
    q.fixed = c(0.25,0.5,0.75))
ggplot(risks.singvar, aes(variable, est, ymin = est - 1.96*sd, ymax = est + 1.96*sd, 
                          color = q.fixed, shape=q.fixed)) +
    geom_pointrange(position = position_dodge(width = 0.75)) + 
    geom_hline(yintercept = 0, linetype=2,color="red") +
    scale_y_continuous(limits = c(-1.2, 1.7),
                       breaks = c(-1,-0.5,0,0.5,1,1.5),
                       expand=expansion(add = c(0.03, 0.03)))+    
    theme_bw() +
    theme(panel.grid = element_blank()) 
ggsave(str_c(outpath4, "/Single.risk.p25vsp75.Hair2.png"), height = 4, width = 6, dpi = 600)


# hair3
ExtractPIPs(fitkm3)
# #Partial dependence
# pred.resp.univar <- PredictorResponseUnivar(fit = fitkm3, q.fixed = 0.5)
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

# ggsave(str_c(outpath4, "/PD.Hair3.png"), height = 8, width = 12, dpi = 600)

# risks.overall <- OverallRiskSummaries(fit = fitkm3, qs = seq(0.1, 0.9, by = 0.05), q.fixed = 0.5)
# risks.overall
# ggplot(risks.overall, aes(quantile, est, ymin = est - 1.96*sd, 
#                           ymax = est + 1.96*sd)) + 
#     geom_hline(yintercept = 0, lty = 2, col = "brown") +
#     theme_bw() + 
#     theme(panel.grid = element_blank()) + 
#     geom_pointrange()
# ggsave(str_c(outpath4, "/MixEffect.Hair3.png"), height = 4, width = 6, dpi = 600)
#Single exposure estimate
risks.singvar <- SingVarRiskSummaries(
    fit = fitkm3, qs.diff = c(0.25,0.75), 
    q.fixed = c(0.25,0.5,0.75))
ggplot(risks.singvar, aes(variable, est, ymin = est - 1.96*sd, ymax = est + 1.96*sd, 
                          color = q.fixed, shape=q.fixed)) +
    geom_pointrange(position = position_dodge(width = 0.75)) + 
    geom_hline(yintercept = 0, linetype=2,color="red") +
    scale_y_continuous(limits = c(-0.63, 0.63),
                       breaks = c(-0.6,-0.3,0,0.3,0.6),
                       expand=expansion(add = c(0, 0)))+   
    theme_bw() +
    theme(panel.grid = element_blank()) 
ggsave(str_c(outpath4, "/Single.risk.p25vsp75.Hair3.png"), height = 4, width = 6, dpi = 600)


# hair4
ExtractPIPs(fitkm4)
# #Partial dependence
# pred.resp.univar <- PredictorResponseUnivar(fit = fitkm4, q.fixed = 0.5)
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
# ggsave(str_c(outpath4, "/PD.Hair4.png"), height = 8, width = 12, dpi = 600)

# risks.overall <- OverallRiskSummaries(fit = fitkm4, qs = seq(0.1, 0.9, by = 0.05), q.fixed = 0.5)
# risks.overall
# ggplot(risks.overall, aes(quantile, est, ymin = est - 1.96*sd, 
#                           ymax = est + 1.96*sd)) + 
#     geom_hline(yintercept = 0, lty = 2, col = "brown") +
#     geom_pointrange()+
#     theme_bw() +
#     theme(panel.grid = element_blank()) 
# ggsave(str_c(outpath4, "/MixEffect.Hair4.png"), height = 4, width = 6, dpi = 600)
#Single exposure estimate
risks.singvar <- SingVarRiskSummaries(
    fit = fitkm4, qs.diff = c(0.25,0.75), 
    q.fixed = c(0.25,0.5,0.75))
ggplot(risks.singvar, aes(variable, est, ymin = est - 1.96*sd, ymax = est + 1.96*sd, 
                          color = q.fixed, shape=q.fixed)) +
    geom_pointrange(position = position_dodge(width = 0.75)) + 
    geom_hline(yintercept = 0, linetype=2,color="red") +
    scale_y_continuous(limits = c(-1, 1),
                       breaks = c(-1,-0.5,0,0.5,1),
                       expand=expansion(add = c(0.03, 0.03)))+   
    theme_bw() +
    theme(panel.grid = element_blank()) 
ggsave(str_c(outpath4, "/Single.risk.p25vsp75.Hair4.png"), height = 4, width = 6, dpi = 600)

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
# ggsave(str_c(outpath4, "/PD.Serum1.png"), height = 8, width = 12, dpi = 600)
# # mixture effect
# risks.overall <- OverallRiskSummaries(fit = fitkm.serum1, qs = seq(0.1, 0.9, by = 0.05), q.fixed = 0.5)
# risks.overall
# ggplot(risks.overall, aes(quantile, est, ymin = est - 1.96*sd, 
#                           ymax = est + 1.96*sd)) + 
#     geom_hline(yintercept = 0, lty = 2, col = "brown") +
#     geom_pointrange() +
#     theme_bw() +
#     theme(panel.grid = element_blank()) 
# ggsave(str_c(outpath4, "/MixEffect.Serum1.png"), height = 4, width = 6, dpi = 600)
#Single exposure estimate
risks.singvar <- SingVarRiskSummaries(
    fit = fitkm.serum1, qs.diff = c(0.25,0.75), 
    q.fixed = c(0.25,0.5,0.75))
ggplot(risks.singvar, aes(variable, est, ymin = est - 1.96*sd, ymax = est + 1.96*sd, 
                          color = q.fixed, shape=q.fixed)) +
    geom_pointrange(position = position_dodge(width = 0.75)) + 
    geom_hline(yintercept = 0, linetype=2,color="red") +
    scale_y_continuous(limits = c(-0.55, 0.55),
                       breaks = c(-0.5,-0.25,0,0.25,0.5),
                       expand=expansion(add = c(0.0, 0.0)))+   
    theme_bw() +
    theme(panel.grid = element_blank()) 
ggsave(str_c(outpath4, "/Single.risk.p25vsp75.Serum1.png"), height = 4, width = 6, dpi = 600)


# serum2
ExtractPIPs(fitkm.serum2)
# #Partial dependence
# pred.resp.univar <- PredictorResponseUnivar(fit = fitkm.serum2, q.fixed = 0.5)
# pred.resp.univar
# ggplot(pred.resp.univar, aes(z, est, ymin = est - 1.96*se,
#                                ymax = est + 1.96*se)) +
#     geom_smooth(stat = "identity") +
#     facet_wrap(~variable, ncol =4) +
#     xlab("scaled concentrations") +
#     ylab("h(exposures)") +
#     geom_hline(yintercept = 0, linetype=2,color="red")+
#     theme_bw() +
#     theme(panel.grid = element_blank()) 
# ggsave(str_c(outpath4, "/PD.Serum2.png"), height = 8, width = 12, dpi = 600)
# #mixture effect
# risks.overall <- OverallRiskSummaries(fit = fitkm.serum2, qs = seq(0.1, 0.9, by = 0.05), q.fixed = 0.5)
# risks.overall
# ggplot(risks.overall, aes(quantile, est, ymin = est - 1.96*sd, 
#                           ymax = est + 1.96*sd)) + 
#     geom_hline(yintercept = 0, lty = 2, col = "brown") +
#     geom_pointrange()+
#     theme_bw() +
#     theme(panel.grid = element_blank()) 
# ggsave(str_c(outpath4, "/MixEffect.Serum2.png"), height = 4, width = 6, dpi = 600)
#Single exposure estimate
risks.singvar <- SingVarRiskSummaries(
    fit = fitkm.serum2, qs.diff = c(0.25,0.75), 
    q.fixed = c(0.25,0.5,0.75))
ggplot(risks.singvar, aes(variable, est, ymin = est - 1.96*sd, ymax = est + 1.96*sd, 
                          color = q.fixed, shape=q.fixed)) +
    geom_pointrange(position = position_dodge(width = 0.75)) + 
    geom_hline(yintercept = 0, linetype=2,color="red") +
    # scale_y_continuous(limits = c(-1.2, 1.7),
    #                    breaks = c(-1,-0.5,0,0.5,1,1.5),
    #                    expand=expansion(add = c(0.03, 0.03)))+   
    theme_bw() +
    theme(panel.grid = element_blank()) 
ggsave(str_c(outpath4, "/Single.risk.p25vsp75.Serum2.png"), height = 4, width = 6, dpi = 600)


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
# ggsave(str_c(outpath4, "/PD.ff.png"), height = 8, width = 12, dpi = 600)
# #mix effect
# risks.overall <- OverallRiskSummaries(fit = fitkm.ff, qs = seq(0.1, 0.9, by = 0.05), q.fixed = 0.5)
# risks.overall
# ggplot(risks.overall, aes(quantile, est, ymin = est - 1.96*sd, 
#                           ymax = est + 1.96*sd)) + 
#     geom_hline(yintercept = 0, lty = 2, col = "brown") +
#     geom_pointrange() +
#     theme_bw() +
#     theme(panel.grid = element_blank()) 
# ggsave(str_c(outpath4, "/MixEffect.ff.png"), height = 4, width = 6, dpi = 600)
#Single exposure estimate
risks.singvar <- SingVarRiskSummaries(
    fit = fitkm.ff, qs.diff = c(0.25,0.75), 
    q.fixed = c(0.25,0.5,0.75))
ggplot(risks.singvar, aes(variable, est, ymin = est - 1.96*sd, ymax = est + 1.96*sd, 
                          color = q.fixed, shape=q.fixed)) +
    geom_pointrange(position = position_dodge(width = 0.75)) + 
    geom_hline(yintercept = 0, linetype=2,color="red") +
    scale_y_continuous(limits = c(-1.25, 1.25),
                       breaks = c(-1,-0.5,0,0.5,1),
                       expand=expansion(add = c(0.01, 0.01)))+ 
    theme_bw() +
    theme(panel.grid = element_blank()) 
ggsave(str_c(outpath4, "/Single.risk.p25vsp75.ff.png"), height = 4, width = 6, dpi = 600)

