create_summary_tables <- function(df, yearly = "", monthly = "", weekly = "") {
    smr_m <- df  %>%
        ddply(.(month, category), summarise,
            cost = sum(amount))  %>%
        mutate(cumulative_spend = cumsum(cost))
    smr_w <- df  %>%
        ddply(.(week, category), summarise,
            cost = sum(amount))  %>%
        mutate(cumulative_spend = cumsum(cost))
    smr_y <- df  %>%
        ddply(.(year, category), summarise,
            cost = sum(amount))  %>%
        mutate(cumulative_spend = cumsum(cost))
    if (yearly == "Y" | yearly == "y") {
    yearly_table <<- smr_y  %>%
        dplyr::select(-cumulative_spend) %>%
        tidyr::pivot_wider(names_from = category, values_from = cost) %>%
        as.data.frame()
    }
    if (monthly == "M" | monthly == "m") {
    monthly_table <<- smr_m  %>%
        dplyr::select(-cumulative_spend) %>%
        tidyr::pivot_wider(names_from = category, values_from = cost) %>%
        as.data.frame()
    }
    if (weekly == "W" | weekly == "w") {
    weekly_table <<- smr_w  %>%
        dplyr::select(-cumulative_spend) %>%
        tidyr::pivot_wider(names_from = category, values_from = cost) %>%
        as.data.frame()
    }
}

create_summary_tables(data, "n", "n", "w")
