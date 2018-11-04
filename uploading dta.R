if (!require("foreign")) install.packages("foreign")
require("foreign")

# example how to upload .dta file into R
# mainpath contains link to the working directory where data is stored
mainpath <- "C:/Users/admin/Desktop/mae_ta_2017/HW_1"

variables<- read.dta(file.path(mainpath,"CPS_for_HW_1.dta"))

names(variables)