### data preparation

library(dplyr)

#tran #8879
colnames(columns) <- c("tran_dt", 'week')
tran$tran_dt <- as.Date(tran$tran_dt)
columns$tran_dt <- as.Date(columns$tran_dt)

tran <- tran %>% inner_join(columns, by="tran_dt")
#8845
write.csv(tran, "tran_new.csv")

#promo
colnames(promo)[1] <- c("tran_dt")
promo$tran_dt <- as.Date(promo$tran_dt)

promo <- promo %>% inner_join(columns, by="tran_dt")
length(unique(promo$week))
write.csv(promo, "promo_new.csv")

#seasonality
colnames(seasonality)[1] <- c("tran_dt")
seasonality$tran_dt <- as.Date(seasonality$tran_dt)

seasonality <- seasonality %>% inner_join(columns, by="tran_dt")
length(unique(seasonality$week))
write.csv(seasonality, "seasonality_new.csv")

#holiday
colnames(holiday)[1] <- c("tran_dt")
holiday$tran_dt <- as.Date(holiday$tran_dt)

holiday <- holiday %>% inner_join(columns, by="tran_dt")
length(unique(holiday$week))
write.csv(holiday, "holiday_new.csv")

plot(tran1 %>% group_by(week) %>% summarize(product_1_sale =sum(tran_prod_sale_qty)))
# 3 models
#tran1
tran1 <- tran_new %>% filter(prod_id == "138936951")
tran1_sale <- tran1 %>% group_by(week) %>% summarize(sum(tran_prod_sale_qty))
plot(tran1_sale)
#tran2
tran2 <- tran_new %>% filter(prod_id == "138936952")
tran2_sale <- tran2 %>% group_by(week) %>% summarize(sum(tran_prod_sale_qty))
plot(tran2_sale)
#tran3
tran3 <- tran_new %>% filter(prod_id == "138936953")
tran3_sale <- tran3 %>% group_by(week) %>% summarize(sum(tran_prod_sale_qty))
plot(tran3_sale)


#TV Radio to Reach
tv <- final_table %>% select(`TV (values)`, `TV_reach (blank)`)
radio <- final_table %>% select(`radio (values)`, `radio_reach (blank)`)
a1 = 1 - 0.5^(1/8)
a2 = 1 - 0.5^(1/4)
#tv
#adstock
colnames(tv) <- c("a", "b")
tv$b[1]=0
for (i in 2:104) {
  tv$b[i] = a1*tv$a[i] + (1-a1)*tv$b[i-1]
}
plot(tv$b)
#reach
tv$c[1]=0
for (i in 1:104) {
  tv$c[i] = 0.95*(1 - exp(-0.02*tv$b[i]))
}
plot(tv$c)
colnames(tv) <- c("a", "b", "tv")

#radio
#adstock
colnames(radio) <- c("a", "b")
radio$b[1]=0
for (i in 2:104) {
  radio$b[i] = a1*radio$a[i] + (1-a1)*radio$b[i-1]
}
plot(radio$b)
#reach
radio$c[1]=0
for (i in 1:104) {
  radio$c[i] = 0.95*(1 - exp(-0.02*radio$b[i]))
}
plot(radio$c)
colnames(radio) <- c("a", "b", "radio")

#product 1 preparation
final_table$`email (binary)` <- ifelse(final_table$`email (binary)` == 0 , 0 , 200000)
prod1 <- cbind.data.frame(tran1_sale$`sum(tran_prod_sale_qty)`, final_table$`Store_prod1 (binary)`, final_table$`Flyer_prod1 (binary)`, final_table$`email (binary)`, final_table$`paid (values)`, final_table$`web (value)`, tv$tv, radio$radio)
tran1_price <- tran1 %>% group_by(week) %>% summarise(price = sum(tran_prod_sale_qty*prod_unit_price)/sum(tran_prod_sale_qty))
tran1_discount <- tran1 %>% group_by(week) %>% summarise(discount = mean(tran_prod_discount_amt/tran_prod_sale_qty))
holiday_new = holiday_new[!duplicated(holiday_new$week),]
tran1_sale <- left_join(tran1_sale, holiday_new, by="week")
tran1_sale[is.na(tran1_sale)] <- 0
tran1_sale$holiday <- ifelse(tran1_sale$holiday == 0, 0, 1)
prod1 <- cbind.data.frame(prod1, tran1_price$price, tran1_discount$discount, seasonality_new$seas_index, tran1_sale$holiday)

colnames(prod1) <- c("sales", "store", "flyer", "email", "paid_search", "web_search", "tv", "radio", "price", "discount", "seasonality", "holiday")
write.csv(prod1, "prod1.csv", row.names = FALSE)

#model 1
library(glmnet)
library(MLmetrics)
model1 <- glm(sales ~ store + flyer + email + paid_search + web_search + tv + radio + price + discount + seasonality + holiday, data = prod1)
summary(model1)
sd(prod1$sales) #38.377
mean(prod1$sales) #219.298
RMSE(model1$fitted.values, prod1$sales) #28.693
MAPE(model1$fitted.values, prod1$sales) #0.1095
R2_Score(model1$fitted.values, prod1$sales) #0.4356
plot(model1$fitted.values,prod1$sales,
     xlab="predicted",ylab="actual")
abline(a=0,b=1)

model2 <- lm(sales ~ store + flyer + email + paid_search + web_search + tv + radio + price + discount + seasonality + holiday, data = prod1)
RMSE(model2$fitted.values, prod1$sales) #28.693
MAPE(model2$fitted.values, prod1$sales) #0.1095
R2_Score(model2$fitted.values, prod1$sales) #0.4356

#product 2 preparation
final_table$`email (binary)` <- ifelse(final_table$`email (binary)` == 0 , 0 , 200000)
prod2 <- cbind.data.frame(tran2_sale$`sum(tran_prod_sale_qty)`, final_table$`Store_prod2 (binary)`, final_table$`Flyer_prod2 (binary)`, final_table$`email (binary)`, final_table$`paid (values)`, final_table$`web (value)`, tv$tv, radio$radio)
tran2_price <- tran2 %>% group_by(week) %>% summarise(price = sum(tran_prod_sale_qty*prod_unit_price)/sum(tran_prod_sale_qty))
tran2_discount <- tran2 %>% group_by(week) %>% summarise(discount = mean(tran_prod_discount_amt/tran_prod_sale_qty))
holiday_new = holiday_new[!duplicated(holiday_new$week),]
tran2_sale <- left_join(tran2_sale, holiday_new, by="week")
tran2_sale[is.na(tran2_sale)] <- 0
tran2_sale$holiday <- ifelse(tran2_sale$holiday == 0, 0, 1)
prod2 <- cbind.data.frame(prod2, tran2_price$price, tran2_discount$discount, seasonality_new$seas_index, tran2_sale$holiday)

colnames(prod2) <- c("sales", "store", "flyer", "email", "paid_search", "web_search", "tv", "radio", "price", "discount", "seasonality", "holiday")
write.csv(prod2, "prod2.csv", row.names = FALSE)

library(tidyr)
#product 3 preparation
final_table$`email (binary)` <- ifelse(final_table$`email (binary)` == 0 , 0 , 200000)
tran3_sale <- tran3_sale %>% right_join(tran1_price) %>% select(week, "sum(tran_prod_sale_qty)")
prod3 <- cbind.data.frame(tran3_sale$`sum(tran_prod_sale_qty)`, final_table$`Store_prod3 (binary)`, final_table$`Flyer_prod3 (binary)`, final_table$`email (binary)`, final_table$`paid (values)`, final_table$`web (value)`, tv$tv, radio$radio)
tran3_price <- tran3 %>% group_by(week) %>% summarise(price = sum(tran_prod_sale_qty*prod_unit_price)/sum(tran_prod_sale_qty))
tran3_price <- tran3_price %>% right_join(tran1_sale) %>% select(week, price)
tran3_discount <- tran3 %>% group_by(week) %>% summarise(discount = mean(tran_prod_discount_amt/tran_prod_sale_qty))
tran3_discount <- tran3_discount %>% right_join(tran1_price) %>% select(week, discount)
holiday_new = holiday_new[!duplicated(holiday_new$week),]
holiday_new <- left_join(tran3_sale, holiday_new, by="week")
holiday_new[is.na(holiday_new)] <- 0
holiday_new$holiday <- ifelse(holiday_new$holiday == 0, 0, 1)
prod3 <- cbind.data.frame(prod3, tran3_price$price, tran3_discount$discount, seasonality_new$seas_index, holiday_new$holiday)
prod3[is.na(prod3)] <- 0
colnames(prod3) <- c("sales", "store", "flyer", "email", "paid_search", "web_search", "tv", "radio", "price", "discount", "seasonality", "holiday")
write.csv(prod3, "prod3.csv", row.names = FALSE)


### Substitute and Complimentary
trans_prod <- transaction_table %>% left_join(product_table,by='prod_id')
# extract week value
columns <- unique(tran_new[c("week", "tran_dt")])
write.csv(columns, "columns.csv", row.names = FALSE)
columns <- read_csv("columns.csv", col_types = cols(tran_dt = col_date(format = "%m/%d/%Y")))
trans_prod <- inner_join(trans_prod, columns, by="tran_dt")

#substitute
#category
main <- trans_prod %>% filter(category_desc_eng %in% "BEER WITH ALCOHOL")
table(main$sub_category_desc)
table(main$brand_desc)
sub <- main %>% filter(sub_category_desc %in% "CERVEJA EST C/ALCOOL")
sub_sale <- sub %>% group_by(brand_desc, prod_id) %>% summarise(sale = sum(tran_prod_sale_qty), price = mean(prod_unit_price)) 

#find the substitute products of each 3 items by their unit price
mean(tran1$prod_unit_price) #0.746 
# 999176967 999942124 999682638 999939106
mean(tran2$prod_unit_price) #4.285
# 999721011 999446019 999947565
mean(tran3$prod_unit_price) #15.641
# 999266067 999159921

# for item 1 2
sub12_sale <- sub %>% filter(prod_id %in% c("999176967", "999942124", "999682638", "999939106", "999721011", "999446019", "999947565")) %>% group_by(week) %>% summarise(sub12 = sum(tran_prod_sale_qty))
sub12_price <- sub %>% filter(prod_id %in% c("999176967", "999942124", "999682638", "999939106", "999721011", "999446019", "999947565")) %>% group_by(week) %>% summarise(price12 = sum(tran_prod_sale_qty*prod_unit_price)/sum(tran_prod_sale_qty))
#for item 3
sub3_sale <- sub %>% filter(prod_id %in% c("999266067", "999159921")) %>% group_by(week) %>% summarise(sub3 = sum(tran_prod_sale_qty))
sub3_price <- sub %>% filter(prod_id %in% c("999266067", "999159921")) %>% group_by(week) %>% summarise(price3 = sum(tran_prod_sale_qty*prod_unit_price)/sum(tran_prod_sale_qty))
sub3_sale <- sub3_sale %>% right_join(sub12_sale, by = "week") %>% select(week, sub3)
sub3_sale[is.na(sub3_sale)] <- 0
sub3_price <- sub3_price %>% right_join(sub3_sale, by = "week") %>% select(week, price3)
sub3_price[is.na(sub3_price)] <- 0

write.csv(sub12_sale, "sub12_sale.csv")
write.csv(sub12_price, "sub12_price.csv")
write.csv(sub3_sale, "sub3_sale.csv")
write.csv(sub3_price, "sub3_price.csv")









