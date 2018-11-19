library(tidyverse)
library(dplyr)
library(magrittr)
library(readxl)
library(mFilter)
library(stats)
## links 
# https://stackoverflow.com/questions/37594411/convert-classes-tbl-df-tbl-and-data-frame-into-dataframe-with-r
# http://rcompanion.org/handbook/I_12.html
# https://blog.rstudio.com/2016/03/24/tibble-1-0-0/
# https://howisonlab.github.io/datawrangling/Handling_multi_indexes.html#a-tidyverse-solution

nl$`total_working hours`

nl <- read_excel("mpa_nl_data.xlsx", sheet = 1)
View(nl)

nl_data <- nl %>% 
  filter(Year>1968) %>%
  select(Year, net_capital_stock:real_investment) 

?ts
View(nl_data)
nl

class(nl_data) # why I get result "tbl_df"     "tbl"        "data.frame", convert it to data_frame
consumption_ts = ts(nl$consumption, start=c(1960,1), end=c(2017,1), frequency=1)
investment_ts = ts(nl$investment, start=c(1960,1), end=c(2017,1), frequency=1)
nominal_gdp_ts = ts(nl$nominal_gdp, start=c(1960,1), end=c(2017,1), frequency=1)
net_capital_stock_ts = ts(nl$net_capital_stock, start=c(1960,1), end=c(2017,1), frequency=1)
gdp_deflator_ts = ts(nl$gdp_deflator, start=c(1960,1), end=c(2017,1), frequency=1)
total_working_hours_ts = ts(nl$`total_working hours`, start=c(1960,1), end=c(2017,1), frequency=1)
real_gdp_ts = ts(nl$real_gdp, start=c(1960,1), end=c(2017,1), frequency=1)
real_consumption_ts = ts(nl$real_consumption, start=c(1960,1), end=c(2017,1), frequency=1)
real_investment_ts = ts(nl$real_investment, start=c(1960,1), end=c(2017,1), frequency=1)

consumption_ts
plot(consumption_ts)
