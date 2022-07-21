# Search R Packages for ROC Curves

# https://rviews.rstudio.com/2019/03/01/some-r-packages-for-roc-curves/

library(tidyverse)  # for data manipulation
library(dlstats)    # for package download stats
library(pkgsearch)  # for searching packages

# Search packages in CRAN 
rocPkg <-  pkg_search(query="ROC",size=200)

# Filter the search results.
# narrowed down the field to 46 packages 
# by filtering out orphaned packages and 
# packages with a score less than 190.
rocPkgShort <- rocPkg %>% 
  filter(maintainer_name != "ORPHANED", score > 190) %>%
  select(score, package, downloads_last_month) %>%
  arrange(desc(downloads_last_month))
head(rocPkgShort)

# Download statistics
shortList <- c("pROC","precrec","ROCit", "PRROC","ROCR","plotROC")
downloads <- cran_stats(shortList)
windows()
ggplot(downloads, aes(end, downloads, group=package, color=package)) +
  geom_line() + geom_point(aes(shape=package)) +
  scale_y_continuous(trans = 'log2')

