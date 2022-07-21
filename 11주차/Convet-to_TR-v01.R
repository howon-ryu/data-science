# Convert DF to transaction data

# Source: https://cran.r-project.org/web/packages/arules/vignettes/arules.pdf 5.2. Example 2: Preparing and mining a questionnaire data set

# Data: AdultUCI
# The data originates from the U.S.
# census bureau database and contains 48842 instances with 14 attributes like age, work class,
# education, etc. In the original applications of the data, the attributes were used to predict
# the income level of individuals. We added the attribute income with levels small and large,
# representing an income of ?? USD 50,000 and > USD 50,000, respectively. This data is
# included in arules as the data set AdultUCI.

# Example
install.packages("arules")

library(arules)
data("AdultUCI")
dim(AdultUCI)
head(AdultUCI, 3)

# Delete unnecessary columns
AdultUCI[["fnlwgt"]] <- NULL
AdultUCI[["education-num"]] <- NULL
head(AdultUCI, 3)
# Convert numeric type into ordered factor type 
AdultUCI[[ "age"]] <- ordered(cut(AdultUCI[[ "age"]], c(15,25,45,65,100)),
                              labels = c("Young", "Middle-aged", "Senior", "Old"))
AdultUCI[[ "hours-per-week"]] <- ordered(cut(AdultUCI[[ "hours-per-week"]],c(0,25,40,60,168)),
                                         labels = c("Part-time", "Full-time", "Over-time", "Workaholic"))
AdultUCI[[ "capital-gain"]] <- ordered(cut(AdultUCI[[ "capital-gain"]], 
                                           c(-Inf,0,median(AdultUCI[[ "capital-gain"]][AdultUCI[[ "capital-gain"]]>0]),Inf)),
                                       labels = c("None", "Low", "High"))
AdultUCI[[ "capital-loss"]] <- ordered(cut(AdultUCI[[ "capital-loss"]],
                                           c(-Inf,0,median(AdultUCI[[ "capital-loss"]][AdultUCI[[ "capital-loss"]]>0]),Inf)),
                                       labels = c("none", "low", "high"))

head(AdultUCI, 3)
# Now, the data can be automatically recoded as a binary incidence matrix 
# by coercing the data set to transactions.
Adult <- as(AdultUCI, "transactions")
Adult

