# preprare data for regression
prod1 <- prod1 %>% select(-store)
prod1$flyer <- factor(prod1$flyer)
prod1$holiday <- factor(prod1$holiday)
prod1$email <- factor(prod1$email)
sub12_price <- sub12_price %>% select(-week)
sub12_sale <- sub12_sale %>% select(-week)
prod1 <- cbind.data.frame(prod1, sub12_price, sub12_sale)

# linear model
out <- glm(sales ~ flyer + email + paid_search + web_search + tv + radio + holiday + seasonality + price + discount + price12 + sub12, data = prod1)
summary(out)
RMSE(predict(out,prod1),prod1$sales) # 28.22731
MAPE(predict(out,prod1),prod1$sales) # 0.1072526
dwtest(out)
vif(out)

# multiplicative model
out <- glm(log(sales) ~ flyer + email + paid_search + web_search + tv + radio + holiday + seasonality + price + discount + price12 + sub12, data = prod1)
summary(out)
RMSE(exp(predict(out,prod1)),prod1$sales) # 28.46888
MAPE(exp(predict(out,prod1)),prod1$sales) # 0.1072157
dwtest(out)
vif(out)


# linear model with interaction, using step-wise to chose important interactions
# get upper formula for step-wise
Vars <- names(prod1)
tt <- as.data.frame(combn(Vars[2:13],2))
inter <- c()
for (i in 1:ncol(tt)) {
  c <- paste(tt[1,i],tt[2,i],sep = "*")
  inter <- append(inter,c)
}
print(paste(Vars[1],"~",paste(paste(Vars[2:13],collapse=" + "),paste(inter,collapse=" + "),sep = " + "),sep=" "))

BigFm <- formula(sales ~ flyer + email + paid_search + web_search + tv + radio + price + discount + seasonality + holiday + price12 + flyer*email + flyer*web_search + flyer*price + flyer*discount + flyer*holiday + flyer*price12 + flyer*sub12 + email*paid_search + email*web_search + email*discount + email*holiday + email*sub12 + paid_search*web_search + paid_search*tv + paid_search*radio + paid_search*price + paid_search*discount + paid_search*holiday + paid_search*sub12 + web_search*tv + web_search*radio + web_search*price + web_search*discount + web_search*seasonality + web_search*holiday + web_search*price12 + web_search*sub12 + tv*radio + tv*price + tv*discount + tv*seasonality + tv*holiday + tv*price12 + tv*sub12 + radio*price + radio*discount + radio*seasonality + radio*holiday + radio*price12 + radio*sub12 + price*discount + price*price12 + discount*price12 + discount*sub12 + seasonality*sub12 + holiday*price12 + holiday*sub12)

SmallFm <- sales ~ flyer + email + paid_search + web_search + tv + radio + holiday + seasonality + price + discount + price12

OutSmall <- glm(SmallFm,data=prod1)

sc <- list(lower=SmallFm,upper=BigFm)
out <- step(OutSmall,scope=sc,direction="both") # best model for product 1
summary(out)
RMSE(predict(out,prod1),prod1$sales) # 26.99039
MAE(predict(out,prod1),prod1$sales) # 20.48553
MAPE(predict(out,prod1),prod1$sales) # 0.09892532
dwtest(out)
vif(out)



# multiplicative model with interaction, using step-wise to chose important interactions
BigFm <- formula(log(sales) ~ flyer + email + paid_search + web_search + tv + radio + price + discount + seasonality + holiday + price12 + sub12 + flyer*email + flyer*paid_search + flyer*web_search + flyer*tv + flyer*price + flyer*discount + flyer*seasonality + flyer*holiday + flyer*price12 + flyer*sub12 + email*paid_search + email*web_search + email*tv + email*discount + email*seasonality + email*holiday + email*sub12 + paid_search*web_search + paid_search*tv + paid_search*radio + paid_search*price + paid_search*discount + paid_search*holiday + paid_search*sub12 + web_search*radio + web_search*price + web_search*discount + web_search*seasonality + web_search*holiday + web_search*price12 + web_search*sub12 + tv*radio + tv*price + tv*discount + tv*holiday + tv*price12 + tv*sub12 + radio*price + radio*discount + radio*seasonality + radio*holiday + radio*price12 + radio*sub12 + price*discount + price*seasonality + price*holiday + price*price12 + discount*price12 + discount*sub12 + seasonality*holiday + seasonality*sub12 + holiday*price12 + holiday*sub12)

SmallFm <- log(sales) ~ flyer + email + paid_search + web_search + tv + radio + holiday + seasonality + price + discount + price12 + sub12

OutSmall <- glm(SmallFm,data=prod1)

sc <- list(lower=SmallFm,upper=BigFm)
out <- step(OutSmall,scope=sc,direction="both")
summary(out)
RMSE(exp(predict(out,prod1)),prod1$sales) # 26.89486
MAPE(exp(predict(out,prod1)),prod1$sales) # 0.1018229
dwtest(out)
vif(out)


# prepare table for calculating DueTos
coef <- out$coefficients
dueto <- data.frame(matrix(, nrow=104, ncol=0))
prod1$flyer <- as.numeric(trimws(prod1$flyer))
prod1$holiday <- as.numeric(trimws(prod1$holiday))
prod1$email <- as.numeric(prod1$email) -1
dueto$base <- coef[1] + coef[8] * prod1$holiday + coef[9] * prod1$seasonality
dueto$flyer <- coef[2]*prod1$flyer
dueto$email <- coef[3]* prod1$email
dueto$paid_search <- coef[4]* prod1$paid_search
dueto$web_search <- coef[5]* prod1$web_search
dueto$tv <- coef[6]* prod1$tv
dueto$radio <- coef[7]* prod1$radio
dueto$price <- coef[10]* prod1$price
dueto$discount <- coef[11]* prod1$discount
dueto$competition <- coef[12]* prod1$price12
dueto$radio_disc <- coef[13]* prod1$radio * prod1$discount
dueto$email_paidsearch <- coef[14]* prod1$email * prod1$paid_search

write.csv(dueto, file = "dueto1.csv")
plot(dueto$base)
