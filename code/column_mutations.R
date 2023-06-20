
# summing the price of all items within a good type per auction

total_purchase_value <- function(goods= c("books", "art_products", "balance", "beds",
                                          "cattle", "chairs", "china", "clocks",
                                          "coatrack", "cups", "guns", "hammers",
                                          "horse_cart", "horses", "irons",
                                          "jewlery", "lamps", "oven", "plates", "pot",
                                          "plough", "sheep", "slaves", "tables",
                                          "utensils", "wagons")){

    total_purch <- merged_df %>%
        mutate(total_value = count * unit_price) %>%
        group_by(mooc_id, type) %>%
        summarise(total_value = sum(total_value), .groups = "drop")

    return(total_purch)

}

# summing the number of items within a good type per auction

good_counter <- function(goods = c("books", "art_products", "balance", "beds",
                                   "cattle", "chairs", "china", "clocks",
                                   "coatrack", "cups", "guns", "hammers",
                                   "horse_cart", "horses", "irons",
                                   "jewlery", "lamps", "oven", "plates", "pot",
                                   "plough", "sheep", "slaves", "tables",
                                   "utensils", "wagons")) {

    total_goods <- merged_df %>%
        group_by(mooc_id, type) %>%
        mutate(total_goods = ifelse(type %in% goods, sum(count), 0)) %>%
        mutate(goods_id = paste0("total_", type))

    return(total_goods)
}


