tourney_transformation <- function(data){

    tournament_winners <- data %>%
        dplyr::select(tourney_name, winner_name, round) %>%
        mutate(final_wins=ifelse(round=="F", 1, 0)) %>%
        group_by(tourney_name, winner_name) %>%
        summarise(tourney_winner=sum(final_wins)) %>%
        filter(tourney_winner>3)
    return(tournament_winners)
}

plotter <- function(data, title_x, title_y, indicator, y_label, title_name){

    the_greats <- ggplot(data, aes(x=title_x, y=title_y, fill=indicator))+
        geom_bar(stat="identity", position="dodge")+
        labs(title=title_name,
             x="Tournament",
             y= y_label,
             fill= "Player")+
        theme(plot.title = element_text(hjust = 0.5, vjust=3))+
        theme(axis.title.y = element_text(margin = margin(r = 10)))+
        theme(axis.title.y = element_text(hjust = 0.5))+
        theme(axis.title.x = element_text(margin = margin(t = 10)))+
        theme(axis.title.x = element_text(hjust = 0.5))+
        theme(plot.margin = margin(15, 10, 15, 10))+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

    return(the_greats)
}

winning_success <- function(data){

    winners <- data %>%
        group_by(tourney_name, winner_name) %>%
        summarise(wins=n()) %>%
        rename(name=winner_name)

    losers <-  data %>%
        group_by(tourney_name, loser_name) %>%
        summarise(losses=n()) %>%
        rename(name=loser_name)

    success <- inner_join(winners, losers, c("tourney_name", "name")) %>%
        mutate(matches=wins+losses) %>%
        group_by(tourney_name, name) %>%
        mutate(success=(wins/matches)*100) %>%
        filter(name=="Novak Djokovic"| name=="Rafael Nadal"|name=="Roger Federer")
    success <- success[-10, ]

    return(success)

}



