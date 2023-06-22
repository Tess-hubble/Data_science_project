# loading all the good_type datasets and binding them together

data_loader <- function(data_dest){

    library(tidyverse)
    library(purrr)

    reader <- function(x){
        hushread <- purrr::quietly(read_csv)
        df <- hushread(x)
        df$result %>%
            dplyr::select(mooc_id, date, type, count,
                                    unit_price, bundle) %>%
            mutate(count = as.numeric(count))
    }

    data_merger <-
        list.files(data_dest, recursive=TRUE, full.names=TRUE) %>%
        .[!grepl("Full_Rec_Mooc", .)] %>%
        as.list() %>%
        map(~reader(.)) %>%
        bind_rows()

        return(data_merger)
}


# joining the goods dataset to the auction dataset

merger <- function(data1, data2){
    goods <- left_join(data1, data2 , by=c("mooc_id", "date"))
    return(goods)
}

# loading all the good_type datasets and binding them together

data_auction <- function(data_dest){

    library(tidyverse)
    library(purrr)

    reader <- function(x){
        hushread <- purrr::quietly(read_csv)
        df <- hushread(x)
        df$result %>%
            dplyr::select(mooc_id,purchase_id, date, type, count,
                          unit_price, bundle) %>%
            mutate(count = as.numeric(count)) %>%
            filter(mooc_id=="MOOC10/10.1")
    }

    data_merger <-
        list.files(data_dest, recursive=TRUE, full.names=TRUE) %>%
        .[!grepl("Full_Rec_Mooc", .)] %>%
        as.list() %>%
        map(~reader(.)) %>%
        bind_rows()

    return(data_merger)
}

