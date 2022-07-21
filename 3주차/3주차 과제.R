options(repos = c(CRAN = "http://cran.rstudio.com")) # set a mirror site
install.packages("readxl")

search()
library(readxl)
getwd()

ex= read_excel(path = "C:\\Users\\qorgh2akfl\\Desktop\\µ¥»ç±â\\weight-height.xlsx")
str(ex)


