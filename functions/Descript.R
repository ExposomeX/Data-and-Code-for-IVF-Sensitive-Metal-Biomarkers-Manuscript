##自定义描述统计量
Descript = function(df){
    #Self designed functions
    min_ = function(x){min(x, na.rm = TRUE)}
    max_ = function(x){max(x, na.rm = TRUE)}
    mean_ = function(x){mean(x, na.rm = TRUE)}
    sd_ = function(x){sd(x, na.rm = TRUE)}
    median_ = function(x){median(x, na.rm = TRUE)}
    p10 = function(x){quantile(x, probs = 0.10, na.rm = TRUE)}
    p25 = function(x){quantile(x, probs = 0.25, na.rm = TRUE)}
    p75 = function(x){quantile(x, probs = 0.75, na.rm = TRUE)}
    p90 = function(x){quantile(x, probs = 0.90, na.rm = TRUE)}
    DoR = function(x){(1-(sum(x==0)/length(x)))*100}
    #Calculate descriptive stats
    df %>%
        dplyr::mutate(across(everything(), list(mean_, sd_, p10, p25, median_, p75, p90, min_, max_, DoR))) %>%
        dplyr::select(matches("((\\d+)|F)_\\d+$")) %>% 
        slice(1) %>% 
        pivot_longer(cols = everything(), names_to = "Stats", values_to = "Value")  %>% 
        mutate(Pollu = str_extract(Stats, pattern = "\\w+(?=\\_\\d+)"),
               tmp = str_extract(Stats, pattern = "((?<=F\\_)\\d+)|((?<=H\\d\\_)\\d+)|((?<=2\\_)\\d+)|((?<=1\\_)\\d+)")) %>% 
        group_by(Pollu) %>%
        select(-Stats) %>%
        pivot_wider(names_from = tmp, values_from = Value) %>%
        rename_with(~c("Mean", "Sd", "P10", "P25", "Median", "P75", "P90", "Min", "Max", "DoR_%"), matches("\\d+")) -> outdf
    return(outdf)
}


