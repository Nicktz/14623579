Data_Prep_Function <- function(Lst, Tournament_Inputs = c("G", "M", "F")){

    read_file_func <- function(Lst){
        # listchoose <- Lst[[10]]
        listchoose <- Lst

        shhcsv <- function(x, ...) {
            sjrd <- purrr::quietly(read_csv)
            df <- sjrd(x, ... )
            df$result
        }
        result <- suppressWarnings(shhcsv(listchoose))
        result <-
            result %>%
            mutate(across(.cols = -c(contains("tourney"), contains("name"), contains("ioc"), contains("round"), contains("score"), contains("surface")),
                          .fns = ~as.numeric(.))) %>%
            mutate(across(.cols = c(contains("tourney"), contains("name"), contains("ioc"), contains("round"), contains("score"), contains("surface")),
                          .fns = ~as.character(.)))
    }

    result <- Lst %>% map_df(~read_file_func(.))

    # Tons of data, might require some filtering.



    # result$tourney_date %>% unique

    result <-
        result %>% mutate(date = ymd(tourney_date)) %>%
        select(date, everything())
    # Let's only consider big events - the small ones don't matter...
    # see
    # tourney_level
    # - For men: 'G' = Grand Slams, 'M' = Masters 1000s, 'A' = other tour-level events, 'C' = Challengers, 'S' = Satellites/ITFs, 'F' = Tour finals and other season-ending events, and 'D' = Davis Cup
    # - For women, there are several additional tourney_level codes, including 'P' = Premier, 'PM' = Premier Mandatory, and 'I' = International. The various levels of ITFs are given by the prize money (in thousands), such as '15' = ITF $15,000. Other codes, such as 'T1' for Tier I (and so on) are used for older WTA tournament designations. 'D' is used for Federation/Fed/Billie Jean King Cup, and also for Wightman Cup and Bonne Bell Cup.

    # result %>% select(tourney_id, tourney_level, tourney_name) %>% unique

    result_Large_events <-
        result %>% mutate(date = ymd(tourney_date)) %>% select(date, everything()) %>%
        filter(tourney_level %in% Tournament_Inputs)

    result_Large_events

}