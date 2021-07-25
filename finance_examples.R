# New code
create_summary_tables <- function(df, yearly = "", monthly = "", weekly = "") {
    summary_function <- function(df, ...) {
        dots <- enquos(...)
        df %>%
        dplyr::group_by(!!!dots, category) %>%
        dplyr::summarise(cost = sum(amount)) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(!!!dots) %>% 
        dplyr::mutate(cumulative_spend = cumsum(cost)) %>%
        as.data.frame()
    }
    smr_m <- summary_function(data, month)
    smr_w <- summary_function(data, week)
    smr_y <- summary_function(data, year)
    table_function <- function(x) {
        x %>%
        dplyr::select(-cumulative_spend) %>%
        tidyr::pivot_wider(names_from = category, values_from = cost) %>%
        as.data.frame()
    }
    yearly_table <- table_function(smr_y)
    monthly_table <- table_function(smr_m)
    weekly_table <- table_function(smr_y)
    if (yearly == "Y" | yearly == "y" | monthly == "Y" |
        monthly == "y" | weekly == "Y" | weekly == "y") {
    yearly_table <<- yearly_table
    }
    if (yearly == "M" | yearly == "m" | monthly == "M" |
         monthly == "m" | weekly == "M" | weekly == "m")  {
    monthly_table <<- monthly_table
    }
    if (yearly == "W" | yearly == "w" | monthly == "W" |
         monthly == "w" | weekly == "W" | weekly == "w") {
    weekly_table <<- weekly_table
    }
}

create_summary_tables(data, "y", "m", "w")


# Old code
#create_summary_tables <- function(df, yearly = "", monthly = "", weekly = "") {
#     smr_m <- df  %>%
#         ddply(.(month, category), summarise,
#             cost = sum(amount))  %>%
#         mutate(cumulative_spend = cumsum(cost))
#     smr_w <- df  %>%
#         ddply(.(week, category), summarise,
#             cost = sum(amount))  %>%
#         mutate(cumulative_spend = cumsum(cost))
#     smr_y <- df  %>%
#         ddply(.(year, category), summarise,
#             cost = sum(amount))  %>%
#         mutate(cumulative_spend = cumsum(cost))
#     if (yearly == "Y" | yearly == "y") { 
# #         easy mode solution is for each if, to do
# #         if (yearly == "Y" | yearly == "y") | yearly == "M" | yearly == "m" | yearly == "W" | yearly == "w" {
#     yearly_table <<- smr_y  %>%
#         dplyr::select(-cumulative_spend) %>%
#         tidyr::pivot_wider(names_from = category, values_from = cost) %>%
#         as.data.frame()
#     }
#     if (monthly == "M" | monthly == "m") {
#     monthly_table <<- smr_m  %>%
#         dplyr::select(-cumulative_spend) %>%
#         tidyr::pivot_wider(names_from = category, values_from = cost) %>%
#         as.data.frame()
#     }
#     if (weekly == "W" | weekly == "w") {
#     weekly_table <<- smr_w  %>%
#         dplyr::select(-cumulative_spend) %>%
#         tidyr::pivot_wider(names_from = category, values_from = cost) %>%
#         as.data.frame()
#     }
# }

create_summary_tables(data, "n", "n", "w")
