
# Great function for ordering columns when using ggplot. I use this exact function all the time.

plot_orderset <- function(df, Column, Order){

    df[,Column][[1]] <- factor(df[,Column][[1]], levels = Order)

    df

}