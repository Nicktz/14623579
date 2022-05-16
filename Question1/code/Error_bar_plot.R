Error_bar_plot <- function(datcolat, xaxis_size = 5, xaxis_rows = 3){

dfplot <-
    datcolat %>%
    group_by(`Regional indicator`) %>%
    # summarise(across(c(`Ladder score`, ends_with("whisker")),
    # list(median), .names = "{col}.{fn}") )
    summarise_at( vars(c(`Ladder score`, ends_with("whisker")) ), ~median(.))

# Life expectancy
HE <-
    datcolat %>% group_by(`Regional indicator`) %>% summarise_at(vars(`Healthy life expectancy`), ~median(.)) %>%
    # For ease of naming:
    rename(HE = `Healthy life expectancy`) %>% mutate(HE = round(HE, 1))

# Join LE to plot, so that we have y-coordinates for LE label:
dfplot <-
    left_join(dfplot,
              HE,
              by = "Regional indicator")

# Adjust ordering as per gist:
order <- dfplot %>% arrange(HE) %>% pull(`Regional indicator`)
dfplot <- dfplot %>% plot_orderset(., Column = "Regional indicator", Order = order)

g <-
    dfplot %>%
    ggplot() +
    geom_point(aes(x = `Regional indicator`, y = `Ladder score`, color = `Regional indicator`), size = 4, alpha = 0.8) +
    geom_errorbar(aes(x = `Regional indicator`,
                      ymin = lowerwhisker, ymax = upperwhisker, color = `Regional indicator`)) +
    geom_text(aes(`Regional indicator`, y = upperwhisker, label = HE), vjust = 0) +


    theme_bw() +
    scale_x_discrete(guide = guide_axis(n.dodge = xaxis_rows)) +
    labs(title = "Happiness Index", subtitle = "Some subtitle", caption = "Data source: World Happiness Index", x = "", y = "Happiness Score") +
    theme(legend.position = "top", legend.title = element_blank()) +
    theme(plot.title = element_text(size = 14),
          plot.subtitle = element_text(size = 12),
          axis.text.x = element_text(size = xaxis_size)) +
    guides(color = F)
# If you wanted to make the x-axis vertical:
# theme(axis.text.x=element_text(angle = 90, hjust = 1))

g


}