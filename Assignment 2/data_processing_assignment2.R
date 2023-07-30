library(tidyverse)


ess10 = read_csv("C:/Users/doduc/OneDrive - KU Leuven/My knowledge/Master of Statistics_KUL/Collecting big data/data/ESS10/ESS10.csv")


## Check NA value 

check_na_ratio = data.frame()
for (i in c(1:ncol(ess10))){
  check_na_ratio = rbind(check_na_ratio,
                         data.frame(col_name = names(ess10)[i],
                                    na_ratio = sum(is.na(ess10[,i, drop = T]))*100/nrow(ess10)))
}


View(check_na_ratio %>% 
  arrange(desc(na_ratio)))

unique(ess10$domain)


ess10 %>% filter(cntry == "GB" & is.na(rlgdngb)) %>% nrow()


sum(is.na(ess10$rlgdngb))


length(unique(ess10$rlgdngb))
