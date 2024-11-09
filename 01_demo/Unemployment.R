# Unemployment 
# import libraries

library("rio")
library("zoo")
library("data.table")

unemp <- import("Unemployment/unemployment.xlsx")


# convert to long
long_unemp <- unemp %>% 
  pivot_longer(
    cols = 2:13,
    names_to = "Months",
    values_to = "Rate"
  )

# change data frame to data.table
long_unemp <- data.table(long_unemp)

# convert column to date
long_unemp[, Year := as.Date(Year)]
setkey(long_unemp, Year)


## generate data set where data is randomly missing
rand.long_unemp.idx <- sample(1:nrow(long_unemp), .1*nrow(long_unemp))
rand.long_unemp <- long_unemp[-rand.long_unemp.idx]


## generate data set where data is more likely 
## to be missing when unemployment is high
high.long_unemp.idx <- which(long_unemp$Rate > 8)
num.to.select <- .2 * length(high.long_unemp.idx)

high.long_unemp.idx <- sample(high.long_unemp.idx,)
bias.long_unemp <- long_unemp[-high.long_unemp.idx]

##
all.dates <- seq(long_unemp$Year[1], tail(long_unemp$Year, 1), "months")

rand.long_unemp = rand.long_unemp[J(all.dates), roll=0]
bias.long_unemp = bias.long_unemp[J(all.dates), roll=0]
rand.long_unemp[, rpt := is.na(Rate)]
## here we label the missing data for easy plotting



rand.long_unemp[, impute.ff := na.locf(Rate, na.rm = FALSE)]
bias.long_unemp[, impute.ff := na.locf(Rate, na.rm = FALSE)]


long_unemp[350:400, plot(Year, Rate,
                         col = 1, lwd=2,type='b')]

rand.long_unemp[350:400, lines(Year, impute.ff,
                         col = 2, lwd=2,lty=2)]

rand.long_unemp[350:400][rpt== TRUE, points(Year, impute.ff,
                                            col=2, pch=6, cex =2)]

### 
# where a moving average data imputation is a better fit for
# the task than a forward fill. For example, if the data is noisy,

rand.long_unemp[, impute.rm.nolookahead := rollapply(c(NA,NA,Rate),3,
                                                     function(x){
                                                       if(!is.na(x[3]))x[3] else mean(x, na.rm = T)
                                                     })]
















