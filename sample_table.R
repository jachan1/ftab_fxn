require(dplyr)
ex_df <- data_frame(var = "widget", title="Widget Category", 
                    levels = 1:4, labels = paste0("widget_", letters[1:4]))

n <- 987
ds <- data_frame(widget=sample(c(1,1,1,2,2,3,4), n, replace=T), 
                 colors=sample(c(1,2,3,"1, 2", "1, 3", "2, 3", -8), n, replace=T),
                 grp=sample(letters[1:3], n, replace=T))

source("I:/Analytics/R Tools/R_functions/Table_Maker/stab_fxn.R")
## basic tables for single and multi select vars
(tab1 <- ftab_fxn(ex_df, ds))
## ignore not recorded
ex_df_m <- data_frame(var = "colors", title="Widget Color", 
                      levels = c(1:3), labels = c("Red", "Green", "Blue"))
(tab2 <- ftab_fxn(ex_df_m, ds))
## account for not recorded
ex_df_m1 <- data_frame(var = "colors", title="Widget Color", 
                      levels = c(1:3, -8), labels = c("Red", "Green", "Blue", "Not Recorded"))
(tab2.1 <- ftab_fxn(ex_df_m1, ds))
## leave not recorded out of percentages
ex_df_m2 <- data_frame(var = "colors", title="Widget Color", 
                       levels = c(1:3, -8), labels = c("Red", "Green", "Blue", "Not Recorded"),
                       exclude=ifelse(levels==-8, T, F))
(tab2.2 <- ftab_fxn(ex_df_m2, ds))

## adding in a grouping factor
require(tidyr)
(tab3 <- ftab_fxn(ex_df, ds, grp_fcts="grp"))
tab3 %>% select(f_var, grp, cpct) %>% spread(grp, cpct)

## adding in a grouping factor and overall group
(tab4 <- ftab_fxn(ex_df, ds, grp_fcts="grp", overall=T))
tab4 %>% select(f_var, grp, cpct) %>% spread(grp, cpct)



