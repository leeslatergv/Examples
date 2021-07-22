pacman::p_load(tidyverse)

df <- data.frame(Q1_Org_Type = as.factor(c("Provider", "Provider", "Other", "Other")),
                 Q2_No_of_Employees = as.factor(c("50 to 99", "1 to 10", "", "")))

Q1_Table <- df %>% 
  filter(Q1_Org_Type != "Other") %>% 
  group_by(Q1_Org_Type) %>% 
  count(Q2_No_of_Employees) # this is the part which will vary, the filter and group_by will stay the same.

Q1_Table

###########################
# Function that does this #
employer_count <- function(df, col_y){
  df <- df %>%filter(Q1_Org_Type != "Other")
  df <- df %>%  
    add_count(Q1_Org_Type, df[[col_y]])
  return(df)   
}

Q1_Table_function <- employer_count(df, "Q2_No_of_Employees")

Q1_Table
Q1_Table_function
str(df)



