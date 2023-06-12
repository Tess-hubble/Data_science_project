plotter <- function(data, x_value, y_value){

    auction_plotter <- ggplot(data, aes(x=x_value, y=y_value))+
        geom_line()
    return(auction_total_plot)
}
