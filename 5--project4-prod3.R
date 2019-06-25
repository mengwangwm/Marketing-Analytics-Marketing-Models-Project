# preprare data for regression
prod3$flyer <- factor(prod3$flyer)
prod3$holiday <- factor(prod3$holiday)
prod3$store <- factor(prod3$store)
prod3$email <- factor(prod3$email)
sub3_price <- sub3_price %>% select(-week)
sub3_sale <- sub3_sale %>% select(-week)
prod3 <- cbind.data.frame(prod3, sub3_price, sub3_sale)

hist(prod3$sales) # need to standardize sales

# multiplicative model
out <- glm(log(sales + 0.01) ~ flyer + email + paid_search + web_search + tv + radio + holiday + seasonality + price + discount + store + price3 + sub3, data = prod3)
summary(out)
RMSE(exp(predict(out,prod3) + 0.01),prod3$sales) # 8.110972
MAE(exp(predict(out,prod3) + 0.01),prod3$sales) # 5.311971
MAPE(exp(predict(out,prod3) + 0.01),prod3$sales) # Inf
dwtest(out)
vif(out)

# linear model
out <- glm(sales ~ flyer + email + paid_search + web_search + tv + radio + holiday + seasonality + price + discount + store + price3 + sub3, data = prod3)
summary(out)
RMSE(predict(out,prod3),prod3$sales) # 7.792062
MAE(predict(out,prod3),prod3$sales) # 5.739204
MAPE(predict(out,prod3),prod3$sales) # Inf
dwtest(out)
vif(out)


# multiplicative model with interaction, using step-wise to chose important interactions
# get upper formula for step-wise
Vars <- names(prod3)
tt <- as.data.frame(combn(Vars[2:14],2))
inter <- c()
for (i in 1:ncol(tt)) {
  c <- paste(tt[1,i],tt[2,i],sep = "*")
  inter <- append(inter,c)
}
print(paste(Vars[1],"~",paste(paste(Vars[2:14],collapse=" + "),paste(inter,collapse=" + "),sep = " + "),sep=" "))

BigFm <- formula(log(sales + 0.01) ~ store + flyer + email + paid_search + web_search + tv + radio + price + discount + seasonality + holiday + price3 + sub3 + store*flyer + store*email + store*paid_search + store*web_search + store*tv + store*radio + store*discount + store*seasonality + store*holiday + store*price3 + store*sub3 + flyer*email + flyer*paid_search + flyer*web_search + flyer*tv + flyer*radio + flyer*discount + flyer*holiday + flyer*price3 + flyer*sub3 + email*paid_search + email*web_search + email*radio + email*price + email*discount + email*seasonality + email*holiday + email*price3 + email*sub3 + paid_search*web_search + paid_search*tv + paid_search*discount + paid_search*holiday + paid_search*sub3 + web_search*tv + web_search*radio + web_search*price + web_search*discount + web_search*seasonality + web_search*holiday + web_search*price3 + web_search*sub3 + tv*discount + tv*holiday + radio*discount + radio*holiday + radio*sub3 + price*holiday + price*sub3 + discount*seasonality + discount*holiday + discount*price3 + discount*sub3 + seasonality*holiday + holiday*price3 + holiday*sub3)

SmallFm <- log(sales + 0.01) ~ flyer + email + paid_search + web_search + tv + radio + holiday + seasonality + price + discount + store + price3 + sub3
OutSmall <- glm(SmallFm,data=prod3)
sc <- list(lower=SmallFm,upper=BigFm)
out <- step(OutSmall,scope=sc,direction="both")
summary(out)
RMSE(exp(predict(out,prod3) + 0.01),prod3$sales) # 6.3875
MAE(exp(predict(out,prod3) + 0.01),prod3$sales) # 4.384059
MAPE(exp(predict(out,prod3) + 0.01),prod3$sales) # Inf
dwtest(out)
vif(out)


# linear model with interaction, using step-wise to chose important interactions
BigFm <- formula(sales ~ store + flyer + email + paid_search + web_search + tv + radio + price + discount + seasonality + holiday + price3 + sub3 + store*flyer + store*email + store*paid_search + store*web_search + store*discount + store*holiday + store*sub3 + flyer*email + flyer*web_search + flyer*discount + flyer*holiday + flyer*price3 + flyer*sub3 + email*paid_search + email*web_search + email*radio + email*price + email*discount + email*holiday + email*price3 + email*sub3 + paid_search*web_search + paid_search*radio + paid_search*price + paid_search*discount + paid_search*holiday + paid_search*price3 + paid_search*sub3 + web_search*tv + web_search*radio + web_search*price + web_search*discount + web_search*holiday + web_search*sub3 + tv*holiday + radio*holiday + price*holiday + discount*sub3 + holiday*price3 + holiday*sub3)

SmallFm <- sales ~ flyer + email + paid_search + web_search + tv + radio + holiday + seasonality + price + discount + store + price3 + sub3

OutSmall <- glm(SmallFm,data=prod3)

sc <- list(lower=SmallFm,upper=BigFm)
out <- step(OutSmall,scope=sc,direction="both")
summary(out)
RMSE(predict(out,prod3),prod3$sales) # 5.510381
MAE(predict(out,prod3),prod3$sales) # 4.120232
MAPE(predict(out,prod3),prod3$sales) # Inf
dwtest(out)
vif(out)



# prepare table for calculating DueTos
coef <- out$coefficients
dueto <- data.frame(matrix(, nrow=104, ncol=0))
prod3$flyer <- as.numeric(trimws(prod3$flyer))
prod3$holiday <- as.numeric(trimws(prod3$holiday))
prod3$email <- as.numeric(prod3$email) -1
prod3$store <- as.numeric(prod3$store) -1
dueto$base <- coef[1] + coef[8] * prod3$holiday + coef[9] * prod3$seasonality
dueto$flyer <- coef[2]*prod3$flyer
dueto$email <- coef[3]* prod3$email
dueto$paid_search <- coef[4]* prod3$paid_search
dueto$web_search <- coef[5]* prod3$web_search
dueto$tv <- coef[6]* prod3$tv
dueto$radio <- coef[7]* prod3$radio
dueto$price <- coef[10]* prod3$price
dueto$discount <- coef[11]* prod3$discount
dueto$store <- coef[12]* prod3$store
dueto$competition <- coef[13]* prod3$price3 + coef[14]* prod3$sub3
dueto$flyer_email <- coef[15]* prod3$flyer * prod3$email
dueto$websearch_comp <- coef[16]* prod3$web_search * prod3$sub3
dueto$store_comp <- coef[17]* prod3$store * prod3$sub3
dueto$flyer_comp <- coef[18]* prod3$flyer * prod3$sub3 + coef[21]* prod3$flyer * prod3$price3
dueto$paidsearch_store <- coef[19]* prod3$paid_search * prod3$store
dueto$websearch_radio <- coef[20]* prod3$web_search * prod3$radio


write.csv(dueto, file = "dueto3.csv")
plot(dueto$base)


