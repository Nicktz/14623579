
# Count fruit references with this function:
Fruit_extractor <- function(dff_adj, froots){
    library(xtable)

    df_sav <- dff_adj %>% group_by(country) %>%
        summarise(Ref_froot = sum( grepl(froots, description)) / n()) %>%
        mutate(Fruit = froots)

df_sav


}
