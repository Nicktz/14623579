First_fruits_plot <- function(fruit_count, xaxis_rows = 2, xaxis_size = 5){

fruitsplot <- fruit_count %>% group_by(Fruit) %>% summarise(largest = sum(Ref_froot)) %>% arrange(desc(largest)) %>% head(5) %>%  pull(Fruit)
g <-
fruit_count %>% filter(Fruit %in% fruitsplot) %>%
    plot_orderset(., Column = "Fruit", Order = fruitsplot) %>%
    plot_orderset(., Column = "country", Order = CountryList) %>%
    ggplot() +
    geom_bar(aes(Fruit, Ref_froot, fill = country), stat = "identity", position = "dodge") +
    theme_bw() +
    # y axis percentage:
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    scale_fill_brewer(palette="Dark2") +
    scale_x_discrete(guide = guide_axis(n.dodge = xaxis_rows)) +
    labs(title = "Fruit References for Wine Reviews", subtitle = "Some subtitle", caption = "Data source: WineMag.com", x = "", y = "Fruit Reference Percentages") +
    theme(legend.position = "top", legend.title = element_blank()) +
    theme(plot.title = element_text(size = 14),
          plot.subtitle = element_text(size = 12),
          axis.text.x = element_text(size = xaxis_size))

g

}