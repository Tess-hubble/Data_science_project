unlogged_distribution <- function(input, graph_title, x_title ){

   unlogged_plot <-  ggplot(model_df, aes(x=input))+
    geom_density()+
    labs(title = graph_title,
         x = x_title, y = "Density") +
    theme(plot.title = element_text(hjust = 0.5, vjust = 2, face="bold"))

   return(unlogged_plot)
}


scatter_plotter <- function(predictor, graph_title, y_title){
    scatter <- ggplot(trial_plots, aes(x=predictor, y=auction_tot))+
        geom_point()+
        labs(title = graph_title,
             x = "Auction total value",
             y = y_title) +
        theme(plot.title = element_text(hjust = 0.5, vjust = 2, face = "bold"))
    return(scatter)
}

timeline_plot <- function(data){
    timeline <- ggplot(data, aes(x = year, y = auction_tot)) +
    geom_point() +
    labs(title = "Total auction value over time",
         x = "Year", y = "Auction total (Rix-dollars)") +
    scale_x_continuous(breaks = custom_breaks, labels = custom_labels) +
    theme_minimal() +
    ggplot2::theme(plot.title = element_text(hjust = 0.5, vjust = 2, face="bold"),
                   axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
    return(timeline)
}

attendance_plotter <- function(data, x_value, y_value, filler){


    attendance_df <- plotting_df %>%
        select(men_total, women_tot, year) %>%
        gather(title, number, men_total, women_tot)

    attendance_plot <- ggplot(data, aes(x=x_value, y=y_value, fill=filler))+
        geom_bar(stat="identity")+
        labs(title="Attendance by titled men and women per auction",
             x="Date", y="Number of titled attendees")+
        theme(plot.title = element_text(hjust = 0.5, vjust=2))+
        theme(axis.title.y = element_text(margin = margin(r = 10)))+
        theme(axis.title.y = element_text(hjust = 0.5))+
        theme(axis.title.x = element_text(margin = margin(t = 10)))+
        theme(axis.title.x = element_text(hjust = 0.5))+
        theme(plot.margin = margin(15, 10, 15, 10))+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

    return(attendance_plot)

}