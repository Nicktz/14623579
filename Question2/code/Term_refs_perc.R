
Term_refs_perc <- function(dff_adj, Terms = c("tannins", "Tannins", "Smoke", "Smokey", "Smoke", "smoke", "smokey", "smoke", "ash", "Ash")){

    Term_ref_freq <-
        dff_adj %>%
        group_by(country) %>% summarise(Percent = sum( grepl( paste(Terms, collapse = "|"), description )) / n()) %>%
        mutate(Term = glue::glue_collapse(Terms, sep = ", ", last = " and ") )

    Term_ref_freq

}

