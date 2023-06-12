data_loader <- function(data_dest){

    library(tidyverse)
    library(purrr)

    reader <- function(x){
        hushread <- purrr::quietly(read_csv)
        df <- hushread(x)
        df$result %>% dplyr::select(mooc_id, date, type, bundle)
    }

    data_merger <-
        list.files(data_dest, recursive=TRUE, full.names=TRUE) %>%
        .[!grepl("Full_Rec_Mooc", .)] %>%
        as.list() %>%
        map(~reader(.)) %>%
        bind_rows()

        return(data_merger)
}

merger <- function(item_df){
    goods <- left_join(item_df, auctions, by=c("mooc_id", "date"))
    return(goods)
}


