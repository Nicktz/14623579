Term_refs_table <- function(dff_adj, Latex = TRUE){
library(xtable)
    # NB to remove xtable's comments as (https://stackoverflow.com/questions/24400308/how-to-remove-the-lines-in-xtable-table-output-by-knitr):
    options(xtable.comment = FALSE)

    Result <-
    bind_rows(
        Term_refs_perc(dff_adj, Terms = c("tannins")),
        Term_refs_perc(dff_adj, Terms = c("smoke", "smokey", "ash"))
    ) %>% mutate(Percent = paste0(round(Percent*100, 3), "%"))

    # If using Texevier, use this:
    # addtorow <- Tab_Span_Page()

    if(Latex){

        Tab <- xtable(Result, caption = "Long Table Example  \\label{tab1}")

        # If using Texevier (check):
    # Tab <-
    # print.xtable(table,
    #              tabular.environment = "longtable",
    #              floating = FALSE, # Leave this as is.
    #              table.placement = 'H', # Leave this as is.
    #              booktabs = T, # Aesthetics
    #              include.rownames = FALSE,  # Typically you don't want this in a table.
    #              add.to.row = addtorow, # For adding the Continued on next page part...
    #              comment = FALSE,
    #              caption.placement = 'top',  # Where do you want the caption?
    #              size="\\fontsize{12pt}{13pt}\\selectfont"  # Size of text in table..
    # )

    } else {

Tab <-
    knitr::kable(Result)
    }

    Tab
}