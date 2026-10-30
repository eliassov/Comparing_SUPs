library(dplyr)
library(purrr)
#prepare results
#GAUSS
tab_gauss <- tab_gauss %>% 
  rename(primary_GAUSS = primary,
         suppressed_GAUSS = suppressed,
         lo_GAUSS = lo,
         up_GAUSS = up) %>% 
  select(starts_with("var"),response, n_contr,primary_GAUSS, suppressed_GAUSS,
         lomax_response, upmin_response, lo_GAUSS, up_GAUSS, mean_n_at)
#MODULAR
out_tau_2 <- out_tau_2 %>% 
  rename(primary_modular = primary,
         suppressed_modular = suppressed,
         lo_modular = lo,
         up_modular = up) %>% 
  select(starts_with("var"),primary_modular, suppressed_modular,
         lo_modular, up_modular)
#Simple
out_simple_2 <- out_simple_2 %>% 
  rename(primary_simple = primary,
         suppressed_simple = suppressed,
         lo_simple = lo,
         up_simple = up) %>% 
  select(starts_with("var"),primary_simple, suppressed_simple,
         lo_simple, up_simple)

#Merge the three results ("tab_gauss", "out_tau_2", "out_simple_2")
result_list <- list(tab_gauss, out_tau_2, out_simple_2)
#merge with reduce()
df_merged <- reduce(result_list, left_join, by = vars)

##Analyse:
#secondary suppressed cells for each method:
#GAUSS
secondary_GAUSS <- df_merged %>% 
  filter(suppressed_GAUSS=="TRUE" & primary_GAUSS=="FALSE")
#MODULAR
secondary_modular <- df_merged %>% 
  filter(suppressed_modular=="TRUE" & primary_modular=="FALSE")
#Simple
secondary_simple <- df_merged %>% 
  filter(suppressed_simple=="TRUE" & primary_simple=="FALSE")

nrow(secondary_GAUSS)
nrow(secondary_modular)
nrow(secondary_simple)
#suppressed response value for each method:



#add importance score for each cell
df_merged <- df_merged %>% 
            mutate(importance_score = response / 10^(5 * mean_n_at))
#suppressions by importance score to evaluate hierarchical damage to table

#Protectedness:
#For how many primary suppressed cells [lo, up] is OK, i.e  
#[lomax_response, upmin_response] is covered by [lo, up]?

