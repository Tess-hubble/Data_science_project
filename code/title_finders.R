titledwomen_locator <- function(titled_women, vector_input){
    titled_women <- ifelse(regexpr("(([Mm]e)?[jJ]uff?r?r?o?u?:?r?w?|[Jj]:[rw])(?= )|(d[e:’'
                        ]?n? )?\\b[Ww]([edw]+:?|[ed]*:?[duwe]+|:)(?= )",
                                   vector_input, perl=TRUE)==TRUE, 1,0)
    return(titled_women)
}


titledmen_locator <- function(titled_men, vector_input) {
    titled_men <- ifelse(
        regexpr("[Mm]ons?:r?s?|[Mm]onsr:?|[Mm]ons(?= )|([Mm]:[rs](?= )^(?!.*oude)|[Mm]r?:s)",
                vector_input, perl = TRUE) == TRUE,
        1,
        ifelse(
            grepl("([Jj]onge?)|[Jj]unior|[Jj]:r|[Jj]un:?r?", vector_input) == TRUE,
            1,
            ifelse(
                regexpr("([Hh](:|:r|eer))(?= )|((d[e:’']n? ?|)[Ee](:|[erwsz:]+))(?= )", vector_input, perl = TRUE) == TRUE,
                1,
                ifelse(
                    regexpr("([Mm]:heer|[Mm]:nh:r|[Mm]ijnheer|[Mm]ijnh:r)|([Mm](ijn|:)|[Mm]e)(?= )",
                            vector_input, perl = TRUE) == TRUE,
                    1,
                    ifelse(
                        regexpr("([Mm]eester)(?=)", vector_input, perl = TRUE) == TRUE,
                        1,
                        ifelse(
                            grepl("[Oo][u:]de", vector_input, perl = TRUE) == TRUE,
                            1,
                            ifelse(
                                regexpr("(d[e:’']?n? ?[Mm](:|(anh:)t?e?)|[Mm](anh:)t?e?|(anh?t?e?))(?= )",
                                        vector_input, perl = TRUE) == TRUE,
                                1,
                                ifelse(
                                    regexpr("([Ss]:r)", vector_input, perl = TRUE) == TRUE,
                                    1,
                                    0
                                )
                            )
                        )
                    )
                )
            )
        )
    )

    return(titled_men)
}

title_counter <- function(purchases) {
        purchases_titles <- purchases %>%
            group_by(mooc_id) %>%
            summarise(women_tot=sum(women, na.rm=TRUE),
                   men_total=sum(men, na.rm=TRUE))

    return(purchases_titles)
}




