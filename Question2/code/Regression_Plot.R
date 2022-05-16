
Regression_Plot <- function(df_SA){

dfreg <- df_SA %>%
    select(taster_name, points, price, province, variety)

Reglist <-
    dfreg %>% group_by(taster_name) %>%
    do(reg = lm(as.formula("points ~ price + province + variety"), data = .)) %>%
    # Make each taster a list so we can use map:
    group_split(taster_name)


g <- Reglist %>% map(~Reg_Graph_Creator(.)) %>% cowplot::plot_grid(plotlist = ., ncol = 1)

g

}



