plotter <- function(y_value, title_graph, axis_label_y){

    auction_plot <- ggplot(plotting_df, aes(year, y_value))+
        geom_point()+
        labs(title=title_graph,
             x="Date", y=axis_label_y)+
        theme(plot.title = element_text(hjust = 0.5, vjust=2))+
        theme(axis.title.y = element_text(margin = margin(r = 10)))+
        theme(axis.title.y = element_text(hjust = 0.5))+
        theme(axis.title.x = element_text(margin = margin(t = 10)))+
        theme(axis.title.x = element_text(hjust = 0.5))+
        theme(plot.margin = margin(15, 10, 15, 10))+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

    return(auction_plot)
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