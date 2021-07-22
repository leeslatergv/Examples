pacman::p_load(tidyverse)

df <- data.frame(Q1_Org_Type = as.factor(c("Provider", "Provider", "Other", "Other")),
                 Q2_No_of_Employees = as.factor(c("50 to 99", "1 to 10", "", "")))

Q1_Table <- df %>% 
  filter(Q1_Org_Type != "Other") %>% 
  group_by(Q1_Org_Type) %>% 
  count(Q2_No_of_Employees) # this is the part which will vary, the filter and group_by will stay the same.

# How to create a function that does this?
function (df, x) {} # what argument do I feed it, the column name?
employee_count <- function (df, x=df$Q1_Org_Type, y){
  filter(df, df$Q1_Org_Type != "Other")
  group_by(x)
  count(y)
}
employee_count(df, y=`Q2_No_of_Employees`) # This returns an error that filter can't apply to logical class
# Do I need to use if else?


