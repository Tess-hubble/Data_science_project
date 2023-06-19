good_counter <- function(merged_df, good_type, goods_total) {
    for (i in seq_along(good_type))
    merged_df <- merged_df %>%
        group_by(mooc_id) %>%
        mutate(goods = ifelse(type == good_type[i], 1, 0)) %>%
        mutate(!!goods_total[i] := sum(goods))

    return(merged_df)
}

total_purchase_value <- function(goods= c("books", "art_products", "scales",
                                                   "beds", "cattle", "chairs", "china",
                                                   "coatrack", "cups", "guns", "hammers",
                                                   "horse_cart", "horses", "irons", "jewelry",
                                                   "lamps", "oven", "plates", "pot", "sheep",
                                                   "slaves", "tables", "utensils", "wagons")){

    total_purch <- merged_df %>%
        mutate(total_value = count * unit_price) %>%
        group_by(mooc_id, type) %>%
        summarise(total_value = sum(total_value), .groups = "drop")

    return(total_purch)

}
