setwd("C:/Users/abina/iCloudDrive/MSBA Spring Classes/Marketing Analytics/Marketing Model Project/SanMiguel")
library(readxl)
library(lubridate)

#week numbers for holiday file
holiday <- read.csv("holiday.csv", header = TRUE)

holiday$week_number <- lubridate::week(ymd(holiday$tran_wk))
holiday[1,3] <- holiday[1,3]-51
holiday[2:17,3] <- holiday[2:17,3]+1
holiday[18:36,3] <- holiday[18:36,3]+53

write.csv(holiday, file = "holiday1.csv")

#week numbers for promo ad file
promo_ad <- read.csv("promo_ad.csv", header = TRUE)

promo_ad$week_number <- lubridate::week(ymd(promo_ad$tran_wk))
promo_ad[1:3,6] <- promo_ad[1:3,6]-51
promo_ad[4:89,6] <- promo_ad[4:89,6]+1
promo_ad[90:188,6] <- promo_ad[90:188,6]+53

write.csv(promo_ad, file = "promo_ad1.csv")

#week numbers for seasonality file
seasonality <- read.csv("seasonality.csv", header = TRUE)

seasonality$week_number <- lubridate::week(ymd(seasonality$tran_wk))
seasonality[1,3] <- seasonality[1,3]-51
seasonality[2:53,3] <- seasonality[2:53,3]+1
seasonality[54:106,3] <- seasonality[54:106,3]+53

write.csv(seasonality, file = "seasonality1.csv")

#week numbers for transaction_table_supp file
tran_supp <- read.csv("transaction_table_supp.csv", header = TRUE)
tran_supp <- tran_supp[order(tran_supp$tran_dt),]
rownames(tran_supp) <- NULL

tran_supp$week_number <- lubridate::isoweek(ymd(tran_supp$tran_dt))
tran_supp[1:10,13] <- tran_supp[1:10,13]-52
tran_supp[11:17,13] <- tran_supp[11:17,13]-52
tran_supp[18:4746,13] <- tran_supp[18:4746,13]+1
tran_supp[4747:8879,13] <- tran_supp[4747:8879,13]+52

write.csv(tran_supp, file = "transaction_table_supp1.csv")
