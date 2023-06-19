
# summing the price of all items within a good type per auction

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

# summing the number of items within a good type per auction

good_counter<- function(goods= c("books", "art_products", "scales",
                                          "beds", "cattle", "chairs", "china",
                                          "coatrack", "cups", "guns", "hammers",
                                          "horse_cart", "horses", "irons", "jewelry",
                                          "lamps", "oven", "plates", "pot", "sheep",
                                          "slaves", "tables", "utensils", "wagons")){

    total_good_types <- merged_df %>%
        dplyr::group_by(mooc_id, type) %>%
        mutate(total_goods= ifelse(type %in% goods, sum(count), 0))

    return(total_good_types)

}
