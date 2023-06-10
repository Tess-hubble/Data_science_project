good_counter <- function(merged_df, good_type, goods_total) {
    for (i in seq_along(good_type))
    merged_df <- merged_df %>%
        group_by(mooc_id) %>%
        mutate(goods = ifelse(type == good_type[i], 1, 0)) %>%
        mutate(!!goods_total[i] := sum(goods))

    return(merged_df)
}

