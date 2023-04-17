Winsummary_Function_Compiler <- function(result_Large_events,
                                         Player_Name = "Rafael Nadal",
                                         playerinfo, Silent = TRUE,
                                         Top_N = 10){

    if(!Silent) message(glue::glue("You are now looking at {Player_Name}"))

    if( result_Large_events %>%
        filter(grepl(Player_Name, winner_name) | grepl(Player_Name, loser_name)) %>% nrow() < 10) {
        warning(glue::glue("Player: {Player_Name} played too few games.... ignored"))
        return(NULL)
    }

    games_df_raw <-
        result_Large_events %>%
        mutate(Result = ifelse(grepl(Player_Name, winner_name), "Win",
                               ifelse(grepl(Player_Name, loser_name), "Loss", NA))) %>% filter(!is.na(Result))

    # Determine opponent's hand:

    games_df <-
        games_df_raw %>%
        mutate(player_id = ifelse(Result == "Win", loser_id,
                                  ifelse(Result == "Loss", winner_id, NA))) %>%
        mutate(Opp_Rank = ifelse(Result == "Win", loser_rank,
                                 ifelse(Result == "Loss", winner_rank, NA))) %>%
        mutate(Type_Opponet = ifelse(Opp_Rank <= Top_N, glue::glue("Top {Top_N}"),
                                     ifelse(Opp_Rank >= 50, "Weak Player", NA))) %>%
        left_join(., playerinfo, by = "player_id")


    # Exploring phase:
    Result_Win <-

        bind_rows(
            games_df %>% summarise(Win_Percentage = sum(Result == "Win") / n()) %>%
                mutate(Type = "All"),

            games_df %>% group_by(surface) %>%
                summarise(Win_Percentage = sum(Result == "Win") / n()) %>%
                rename(Type = surface),


            games_df %>%
                group_by(Type_Opponet) %>%
                summarise(Win_Percentage = sum(Result == "Win") / n()) %>%
                mutate(Type_Opponet = coalesce(Type_Opponet, "SlightlyAboveAvg")) %>%
                rename(Type = Type_Opponet),

            games_df %>%
                group_by(hand) %>%
                summarise(Win_Percentage = sum(Result == "Win") / n()) %>%
                rename(Type = hand),

            games_df %>% mutate(Ht = ifelse(height >= 189, "Tall_Height", ifelse(height < 189, "Medium_Height", NA))) %>%
                filter(!is.na(Ht)) %>%
                group_by(Ht) %>%
                summarise(Win_Percentage = sum(Result == "Win") / n()) %>%
                rename(Type = Ht))

    Result_N <-

        bind_rows(
            games_df  %>% summarise(N = n()) %>%
                mutate(Perc_N = N / sum(N)) %>%
                mutate(Type = "All"),

            games_df %>% group_by(surface) %>%
                summarise(N = n()) %>%
                ungroup() %>% mutate(Perc_N = N / sum(N)) %>%
                rename(Type = surface),


            games_df %>%
                group_by(Type_Opponet) %>%
                summarise(N = n()) %>%
                ungroup() %>% mutate(Perc_N = N / sum(N)) %>%
                mutate(Type_Opponet = coalesce(Type_Opponet, "SlightlyAboveAvg")) %>%
                rename(Type = Type_Opponet),

            games_df %>%
                group_by(hand) %>%
                summarise(N = n()) %>%
                ungroup() %>% mutate(Perc_N = N / sum(N)) %>%
                rename(Type = hand),

            games_df %>% mutate(Ht = ifelse(height >= 189, "Tall_Height", ifelse(height < 189, "Medium_Height", NA))) %>%
                filter(!is.na(Ht)) %>%
                group_by(Ht) %>%
                summarise(N = n()) %>%
                ungroup() %>% mutate(Perc_N = N / sum(N)) %>%
                rename(Type = Ht),

            games_df %>% mutate(Round_Type = ifelse(!round %in% c("QF", "SF", "F"), "Early Rounds", "Final Rounds")) %>%
                group_by(Round_Type) %>%
                summarise(N = n()) %>%
                ungroup() %>% mutate(Perc_N = N / sum(N)) %>%
                rename(Type = Round_Type)

            )

    Result_Time <-

        bind_rows(
            games_df %>% summarise(TTaken = mean(minutes, na.rm=T)) %>%
                mutate(Type = "All"),

            games_df %>%
                group_by(surface) %>%
                summarise(TTaken = mean(minutes, na.rm=T)) %>%
                rename(Type = surface),

            games_df %>%
                group_by(Type_Opponet) %>%
                summarise(TTaken = mean(minutes, na.rm=T)) %>%
                mutate(Type_Opponet = coalesce(Type_Opponet, "SlightlyAboveAvg")) %>%
                rename(Type = Type_Opponet),

            games_df %>%
                group_by(hand) %>%
                summarise(TTaken = mean(minutes, na.rm=T)) %>%
                rename(Type = hand),

            games_df %>% mutate(Ht = ifelse(height >= 189, "Tall_Height", ifelse(height < 189, "Medium_Height", NA))) %>%
                filter(!is.na(Ht)) %>%
                group_by(Ht) %>%
                summarise(TTaken = mean(minutes, na.rm=T)) %>%
                rename(Type = Ht),

            # Check rounds:
            # games_df$round %>% unique

            games_df %>% mutate(Round_Type = ifelse(!round %in% c("QF", "SF", "F"), "Early Rounds", "Final Rounds")) %>%
                group_by(Round_Type) %>%
                summarise(TTaken = mean(minutes, na.rm=T)) %>%
                rename(Type = Round_Type)
        )



    # The following is a great summary in how to split strings that are not vanilla...
    # Aim: we want to split the score into the amount of games played - in order to assess how frequently e.g. aces are hit.
    # Naturally, more sets mean more aces, and tournaments with more sets mean more aces - so let's first count the amount of serving games - dividing it by two to crudely proxy for amount of services games played...
    # You'd typically start by trying to 'str_split' a vector until it works, and try and get a tough one.
    # E.g. 7-6(3) states that the set was won in a tie breaker, so we want to drop the brackets to sum the games...

    # After some playing around (make sure you can unscramble the following to make sense of it), the following can now be made a function, and then be used in a map...
    # str_split("6-2(3) 6-3", " ") %>% .[[1]] %>% substr(., 1, 3) %>% str_split(., "-") %>% reduce(c) %>% as.numeric() %>% sum()str_split("6-2(3) 6-3", " ") %>% .[[1]] %>% substr(., 1, 3) %>% str_split(., "-") %>% reduce(c) %>% as.numeric() %>% sum()    str_split("6-2(3) 6-3", " ") %>% .[[1]] %>% substr(., 1, 3) %>% str_split(., "-") %>% reduce(c) %>% as.numeric() %>% sum()
    # Checking the data - I also notice some scores have RET, for retired, and W/O for walkover... let's ignore those...

    str_split_func <- function(x) {

        if(grepl("RET|W/O", x)) return(NA)
        str_split(x, " ") %>% .[[1]] %>% substr(., 1, 3) %>% str_split(., "-") %>% reduce(c) %>% as.numeric() %>% sum()

    }

    # I will now do a rowwise operation for this one:
    games_df_sumgames <-
            games_df %>%
                dplyr::rowwise() %>%
                mutate(games_in_match = str_split_func(score)) %>% select(games_in_match, score, everything()) %>% ungroup() %>%
                mutate(Total_Aces = ifelse(Result == "Win", w_ace, ifelse(Result == "Loss", w_ace, NA))) %>%
                mutate(Total_BP_Saved = ifelse(Result == "Win", w_bpSaved, ifelse(Result == "Loss", l_bpSaved, NA))) %>%
        mutate(across( c(Total_Aces, Total_BP_Saved), ~coalesce(., 0)))

    Result_Aces <-

        bind_rows(
            games_df_sumgames %>% summarise(Aces_Perc = sum(Total_Aces, na.rm=T)/sum(games_in_match, na.rm=T)) %>%
                mutate(Type = "All"),

            games_df_sumgames %>% group_by(surface) %>%
                summarise(Aces_Perc = sum(Total_Aces, na.rm=T)/sum(games_in_match, na.rm=T)) %>%
                rename(Type = surface),

            games_df_sumgames %>%
                group_by(Type_Opponet) %>%
                summarise(Aces_Perc = sum(Total_Aces, na.rm=T)/sum(games_in_match, na.rm=T)) %>%
                mutate(Type_Opponet = coalesce(Type_Opponet, "SlightlyAboveAvg")) %>%
                rename(Type = Type_Opponet),

            games_df_sumgames %>%
                group_by(hand) %>%
                summarise(Aces_Perc = sum(Total_Aces, na.rm=T)/sum(games_in_match, na.rm=T)) %>%
                rename(Type = hand),

            games_df_sumgames %>% mutate(Ht = ifelse(height >= 189, "Tall_Height", ifelse(height < 189, "Medium_Height", NA))) %>%
                filter(!is.na(Ht)) %>%
                group_by(Ht) %>%
                summarise(Aces_Perc = sum(Total_Aces, na.rm=T)/sum(games_in_match, na.rm=T)) %>%
                rename(Type = Ht),

            # Check rounds:
            # games_df$round %>% unique

            games_df_sumgames %>% mutate(Round_Type = ifelse(!round %in% c("QF", "SF", "F"), "Early Rounds", "Final Rounds")) %>%
                group_by(Round_Type) %>%
                summarise(Aces_Perc = sum(Total_Aces, na.rm=T)/sum(games_in_match, na.rm=T)) %>%
                rename(Type = Round_Type)
        )


    Result_BPSaved <-

        bind_rows(
            games_df_sumgames %>% summarise(BreakPoints_Saved = sum(Total_BP_Saved, na.rm=T)/sum(games_in_match, na.rm=T)) %>%
                mutate(Type = "All"),

            games_df_sumgames %>% group_by(surface) %>%
                summarise(BreakPoints_Saved = sum(Total_BP_Saved, na.rm=T)/sum(games_in_match, na.rm=T)) %>%
                rename(Type = surface),

            games_df_sumgames %>%
                group_by(Type_Opponet) %>%
                summarise(BreakPoints_Saved = sum(Total_BP_Saved, na.rm=T)/sum(games_in_match, na.rm=T)) %>%
                mutate(Type_Opponet = coalesce(Type_Opponet, "SlightlyAboveAvg")) %>%
                rename(Type = Type_Opponet),

            games_df_sumgames %>%
                group_by(hand) %>%
                summarise(BreakPoints_Saved = sum(Total_BP_Saved, na.rm=T)/sum(games_in_match, na.rm=T)) %>%
                rename(Type = hand),

            games_df_sumgames %>% mutate(Ht = ifelse(height >= 189, "Tall_Height", ifelse(height < 189, "Medium_Height", NA))) %>%
                filter(!is.na(Ht)) %>%
                group_by(Ht) %>%
                summarise(BreakPoints_Saved = sum(Total_BP_Saved, na.rm=T)/sum(games_in_match, na.rm=T)) %>%
                rename(Type = Ht),

            # Check rounds:
            # games_df$round %>% unique

            games_df_sumgames %>% mutate(Round_Type = ifelse(!round %in% c("QF", "SF", "F"), "Early Rounds", "Final Rounds")) %>%
                group_by(Round_Type) %>%
                summarise(BreakPoints_Saved = sum(Total_BP_Saved, na.rm=T)/sum(games_in_match, na.rm=T)) %>%
                rename(Type = Round_Type)
        )

Result <-
    full_join(Result_Win,Result_Time, by = "Type") %>%
    full_join(.,Result_N, by = "Type") %>%
        full_join(., Result_Aces, by = "Type") %>%
        full_join(., Result_BPSaved, by = "Type")

Result %>% mutate(Player = Player_Name) %>% select(Player, Type, N, Perc_N, everything())

}
