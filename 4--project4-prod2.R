
library(car)
## overall data table for product 2 including sales, flyer, email,paid_seach, web_search, tv, radio, price, discount, seasonality, holiday, sub_sale and sub_price. 
setwd("C:/Users/Meng Wang/Desktop/MKT_Analysis/Project/New folder")
prod2 <- read.csv("prod2.csv")
price <- read.csv("sub12_price.csv")
price <- price[,3]
sale <- read.csv("sub12_sale.csv")
sale <- sale[,3]
prod2<- cbind(prod2,sale,price)
colnames(prod2) <- c("sales","store","flyer","email","paid_search","web_search","tv","radio","price","discount","seasonality","holiday","sub_sale","sub_price")

# coding the following four variables into factor for linear regression later. 
prod2$flyer <- factor(prod2$flyer)
prod2$holiday <- factor(prod2$holiday)
prod2$store <- factor(prod2$store)
prod2$email <- factor(prod2$email)

hist(prod2$sales)

# log_price: doing a simple lieanr regression respect to log form of sales
out <- glm(log(sales) ~ flyer + email + paid_search + web_search + tv + radio + holiday + seasonality + price + discount + store+sub_price+sub_sale, data = prod2)
summary(out) # summary of the regression result   
RMSE(exp(predict(out,prod2)),prod2$sales) # RMSE: 17.3174
MAPE(exp(predict(out,prod2)),prod2$sales) # MAPE:0.1285695
dwtest(out) #2.0872
vif(out) # used for filter high collinearity terms

# price: doing a simple lieanr regression respect to sles
out <- glm(sales ~ flyer + email + paid_search + web_search + tv + radio + holiday + seasonality + price + discount + store+sub_price+sub_sale, data = prod2)
summary(out)
RMSE(predict(out,prod2),prod2$sales) # 17.3329
MAPE(predict(out,prod2),prod2$sales) # 0.1318087
dwtest(out) #1.9978
vif(out)

# generate all possible the interactions terms of two variables. 
Vars <- names(prod2)
tt <- as.data.frame(combn(Vars[2:14],2))
inter <- c()
for (i in 1:ncol(tt)) {
  c <- paste(tt[1,i],tt[2,i],sep = "*")
  inter <- append(inter,c)
}
BigFm <- paste(Vars[1],"~",paste(paste(Vars[2:13],collapse=" + "),paste(inter,collapse=" + "),sep = " + "),sep=" ")
BigFm <- formula(BigFm)

# sales: the final upper formular for step wise after filtering our high collineairty term using vip. 
BigFm <- formula(sales ~ store + flyer + email + paid_search + web_search + tv + 
                   radio + price + discount + seasonality + holiday + sub_sale + 
                   store * flyer + store * email + store * paid_search + store * 
                   web_search + store * tv + store * radio + store * price + 
                   store * discount + store * seasonality + store * holiday + 
                   store * sub_sale + flyer * email + flyer * 
                   paid_search + flyer * web_search + 
                   flyer * price + flyer * discount +  
                   flyer * holiday + flyer * sub_sale + flyer * sub_price + 
                   email * web_search + email * tv + email * discount + 
                   email * holiday + email * sub_sale + 
                   paid_search * web_search + paid_search * tv + paid_search * 
                   radio  + paid_search * discount + paid_search * 
                   seasonality + paid_search * holiday + paid_search * sub_sale + 
                   paid_search * sub_price  + 
                   + web_search * price + web_search * discount 
                 + web_search * holiday  + 
                   web_search * sub_price + tv * radio  + tv * discount + 
                   tv * seasonality + tv * holiday + tv * sub_sale + tv * sub_price + 
                   radio * discount + radio * seasonality + 
                   radio * holiday + radio * sub_sale + radio * sub_price + 
                   price * discount + price * seasonality + price * holiday + 
                   price * sub_sale + price * sub_price + 
                   discount * holiday + discount * sub_sale + discount * sub_price + 
                   seasonality * holiday  + holiday * sub_sale + holiday * sub_price)
# simple linear formualr for the lower regression and run a step-wise to find the best linear model result
SmallFm <- sales ~ flyer + email + paid_search + web_search + tv + radio + holiday + seasonality + price + discount + store+sub_price+sub_sale
OutSmall <- glm(SmallFm,data=prod2)
sc <- list(lower=SmallFm,upper=BigFm)
out <- step(OutSmall,scope=sc,direction="both")
# summary of teh best model reuslt
summary(out)
RMSE((predict(out,prod2)),prod2$sales) # 15.45297
MAE((predict(out,prod2)),prod2$sales) # 11.61643
MAPE((predict(out,prod2)),prod2$sales) # 0.1186129
dwtest(out) #1.9001
vif(out) 




######## calculate the dueto for product 2 ########
coef <- out$coefficients
dueto <- data.frame(matrix(, nrow=104, ncol=0))
prod2$flyer <- as.numeric(trimws(prod2$flyer))
prod2$holiday <- as.numeric(trimws(prod2$holiday))
prod2$email <- as.numeric(prod2$email) -1
prod2$store <- as.numeric(prod2$store) -1

dueto$base <- coef[1] + coef[8] * prod2$holiday + coef[9] * prod2$seasonality
dueto$flyer <- coef[2]*prod2$flyer
dueto$email <- coef[3]* prod2$email
dueto$paid_search <- coef[4]* prod2$paid_search
dueto$web_search <- coef[5]* prod2$web_search
dueto$tv <- coef[6]* prod2$tv
dueto$radio <- coef[7]* prod2$radio
dueto$price <- coef[10]* prod2$price
dueto$discount <- coef[11]* prod2$discount
dueto$store <- coef[12]* prod2$store
dueto$competition <- coef[13]* prod2$sub_price + coef[14]* prod2$sub_sale
dueto$paidsearch_seasonality <- coef[15]* prod2$paid_search * prod2$seasonality
dueto$paidsearch_holiday <- coef[16]* prod2$paid_search * prod2$holiday
dueto$flyer_paiddsearch <- coef[17]* prod2$flyer * prod2$paid_search
dueto$tv_radio <- coef[18]* prod2$tv * prod2$radio
dueto$discount_store <- coef[19]* prod2$discount * prod2$store
dueto$flyer_discount <- coef[20]* prod2$flyer * prod2$discount


write.csv(dueto, file = "dueto2.csv", row.names = FALSE)
plot(dueto$base)

######## calculate the dueto for product 2 




# natural log of sales: the final upper formular for step wise after filtering our high collineairty term using vip. 
BigFm <- formula(log(sales) ~ store + flyer + email + paid_search + web_search + tv + 
                   radio + price + discount + seasonality + holiday + sub_sale + 
                   store * flyer + store * email + store * paid_search + store * 
                   web_search + store * tv + store * radio + store * price + 
                   store * discount + store * seasonality + store * holiday + 
                   store * sub_sale + store * sub_price + flyer * email + flyer * 
                   paid_search + flyer * web_search + flyer * tv + flyer * radio + 
                   flyer * price + flyer * discount + flyer * seasonality + 
                   flyer * holiday + flyer * sub_sale + flyer * sub_price + 
                   email * paid_search + email * web_search + email * tv +
                   email * discount +  
                   email * holiday + email * sub_sale  + 
                   paid_search * web_search + paid_search * tv + paid_search * 
                   radio  + paid_search * discount + paid_search * 
                   seasonality + paid_search * holiday + paid_search * sub_sale + 
                   paid_search * sub_price  + web_search * discount  + web_search * holiday + web_search * sub_sale + 
                   web_search * sub_price + tv * radio  + tv * discount + 
                   tv * seasonality + tv * holiday + tv * sub_sale + tv * sub_price + 
                   radio * discount + radio * seasonality + 
                   radio * holiday + radio * sub_sale + radio * sub_price + 
                   price * discount + price * seasonality + price * holiday + 
                   price * sub_sale + price * sub_price + discount * seasonality + 
                   discount * holiday + discount * sub_sale + discount * sub_price + 
                   seasonality * holiday + seasonality * sub_sale + seasonality * 
                   sub_price + holiday * sub_sale + holiday * sub_price + sub_sale * 
                   sub_price)
# simple linear formualr for the lower regression and run a step-wise to find the best linear model result
SmallFm <- log(sales) ~ flyer + email + paid_search + web_search + tv + radio + holiday + seasonality + price + discount + store+sub_price+sub_sale
OutSmall <- glm(SmallFm,data=prod2)
sc <- list(lower=SmallFm,upper=BigFm)
out <- step(OutSmall,scope=sc,direction="both")
# summary of teh best model reuslt
summary(out)
RMSE(exp(predict(out,prod2)),prod2$sales) # 15.75185
MAE(exp(predict(out,prod2)),prod2$sales) # 11.6488
MAPE(exp(predict(out,prod2)),prod2$sales) # 0.1160463
dwtest(out) #2.0763
vif(out)


#### the best model for product two is additive model with interaction terms. 
