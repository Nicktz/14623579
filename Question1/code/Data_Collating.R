Data_Collating <- function(Datroot){

library(tidyverse)

    # let's create a silent read function first (as it prints a load of nonsense if you use read_csv directly):
    silentread <- function(x){
        hushread <- purrr::quietly(read_csv)
        df <- hushread(x)
        df$result
    }

    datcolat <-
        list.files(Datroot, full.names = T, recursive = T) %>%
        # Ensure you only load the csv's, not the README.txt.
        .[!grepl(".txt", .)] %>%
        as.list() %>%
        map(~silentread(.)) %>% bind_rows()
        # equivalent to using map_df

    datcolat

}