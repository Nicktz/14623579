Reg_Graph_Creator <- function(Reglist){

    # TIP: For building function, uncomment the following:
    # Reguse <- Reglist[[1]]
    Reguse <- Reglist
    # Let's be fancy and grab the R squared as well...
    Rsqd <- Reguse %>% pull(reg) %>% .[[1]] %>% broom::glance() %>% pull(r.squared) %>% round(.,3)*100
    Reguse %>% pull(reg) %>% .[[1]] %>% broom::augment() %>%
        ggplot()  +
        geom_point(aes(.fitted, price)) +
        geom_smooth(aes(.fitted, price), method = "lm", se = FALSE, color = "lightgrey") +
        labs(x = "Actual", y = "Fitted",
             title =  glue::glue("Regression Type: {Reguse$taster_name}"),
             subtitle = glue::glue("R squared: {Rsqd}%")) +
        theme_bw()
}
