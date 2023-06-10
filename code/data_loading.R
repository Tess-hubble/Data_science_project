
item_runner <- function(n, item){
    item <- loaded_data[[n]] %>% dplyr::select(purchase_id, mooc_id, date, focal_good, type, unit_price, bundle)
    return(item)
}

merger <- function(item_df){
    goods <- inner_join(auctions, item_df, by=c("mooc_id", "date"))
    return(goods)
}
