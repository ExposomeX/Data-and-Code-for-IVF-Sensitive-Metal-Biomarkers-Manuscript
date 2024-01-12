#obtain crude RR of log-binomial models

Ex.RR = function(
    Y,#dataframe containing outcome
    X,#dataframe containing exposures
    C) #dataframe containing covariates
{
    df <- dplyr::bind_cols(Y,X,C)
    name <- colnames(X)
    #loop function
    F1 = function(
        expo,#exposure
        name #name of exposure
    ){
        #1. cRR and CI
        fit2 <- glm(df[,1] ~ expo,
                    family = binomial(link = "log"),
                    start = c(log(mean(df[,1])), rep(0,1)),
                    data = df)
        tmp <- summary(fit2)$coefficients
        
        tmp1 <- cbind(cRR = exp(tmp[2,1]),
                      LCI_c = exp(tmp[2,1]) + qnorm(0.05/2)*tmp[2,2],
                      UCI_c = exp(tmp[2,1]) - qnorm(0.05/2)*tmp[2,2], 
                      Pvalue_c = tmp[2,4])
        #2. aRR and CI
        fit4 <- glm(df[,1] ~ expo + 年龄_2Cat + BMI_3Cat + 病因_3Cat + 治疗方案_3Cat + X1移植胚胎数量 + X1新鲜冷冻, 
                    family = binomial(link = "log"), 
                    start = c(log(mean(df[,1])), rep(0,9)),
                    data = df)
        tmp <- summary(fit4)$coefficients
        
        tmp2 <- cbind(aRR = exp(tmp[2,1]),
                      LCI_a = exp(tmp[2,1]) + qnorm(0.05/2)*tmp[2,2],
                      UCI_a = exp(tmp[2,1]) - qnorm(0.05/2)*tmp[2,2], 
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
