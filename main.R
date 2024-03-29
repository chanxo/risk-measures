# Title     : Market Risk Measures using crytocurrency data
# Created by: Francisco Prado Moreno
# Created on: 09/03/2021
# Last edit : 20/04/2022

# Importing useful libraries ####
library("quantmod")
library("Hmisc")
library("PerformanceAnalytics")

crypto_to_usd = new.env()

# Importing crypto ####
# 13(12) Largest Market Caps

# Bitcoin USD ("BTC-USD")
# Ethereum USD ("ETH-USD")
# BinanceCoin USD ("BNB-USD")
# Cardano USD ("ADA-USD")
# Tether USD ("USDT-USD")
# Polkadot USD ("DOT1-USD") -> will not be used in the end, data not spanning long enough
# XRP USD ("XRP-USD")
# Litecoin USD ("LTC-USD")
# Chainlink USD "LINK-USD")
# BitcoinCash USD ("BCH-USD")
# Stellar USD ("XLM-USD")
# USDCoin USD ("USDC-USD") -> also lack of data (will be removed)
# Dogecoin USD ("DOGE-USD")

cryto_as_list =  c("BTC-USD", "ETH-USD", "BNB-USD", "ADA-USD",
                   "USDT-USD", "XRP-USD", "LTC-USD",
                   "LINK-USD", "BCH-USD", "XLM-USD",
                    "DOGE-USD")
n = length(cryto_as_list)
# For simplicity we will only use the adjusted prices

cryto_var_names =  c("btc_usd", "eth_usd", "bnb_usd", "ada_usd",
                   "usdt_usd", "xrp_usd", "ltc_usd",
                   "link_usd", "bch_usd", "xlm_usd",
                    "doge_usd")

# Getting data in a loop, for an arbitrary number of symbols

for (currency in 1:length(cryto_as_list)) {
  var_all = getSymbols(cryto_as_list[currency], verbose = FALSE, warnings = FALSE,
           src = "yahoo",
           env = crypto_to_usd, auto.assign = FALSE)
  dispersion_var_ =  (var_all[,2] - var_all[,3])/var_all[,6]
  # high (2nd) minus low (3rd) difference over the adj closing,
  # this is done so the dispersion is in the same space as the price return
  # later on (normalized)
  # this will serve to approximate, given the lack of intra-day data.
  var_ = var_all[,6] # the 6th variable is the Adjusted prices
  colnames(dispersion_var_) = paste("high_low_diff", cryto_var_names[currency], sep = "_")
  assign(paste("diff", cryto_var_names[currency], sep = "_"), dispersion_var_)
  colnames(var_) = paste(cryto_var_names[currency],"adj", sep = "_")
  assign(paste(cryto_var_names[currency]), var_)
}
# Housekeeping
rm(var_all, var_, dispersion_var_)

# Main data
df = do.call(
  cbind, lapply(cryto_var_names, get) # argument in do.call has to be a list.
)

# Dispersion data
diff_df = do.call(
  cbind, lapply(paste("diff", cryto_var_names, sep = "_"), get) # argument in do.call has to be a list.
)

# Analysis since 2019
# Determining the returns
# ?apply()
r_df = do.call(cbind, apply(df, MARGIN = 2, FUN = dailyReturn, type = "arithmetic"))["2019/"]
colnames(r_df) = paste("r", cryto_var_names, sep = "_")

diff_df = diff_df["2019/"]
diff_df = na.omit(diff_df)

# Providing portfolio weights
set.seed(123)
portfolio_weights_ = runif(n)
portfolio_weights = portfolio_weights_/sum(portfolio_weights_)
# sum(portfolio_weights) # 1
portfolio_weights
w = portfolio_weights


# Portfolio performance
portfolio_returns = sweep(r_df, MARGIN = 2, portfolio_weights, "*")



# Figuring out distributions
# ?hist.data.frame()
hist.data.frame(as.data.frame(r_df), mtit= '(daily) Return distribution')
# After observing the distributions of the long-term returns for each
# crypto-currency, normality is not too far, save some tails which most likely
# come from days of particular losses/gains.

# Since we do not have intra-day data, for simplicity we will assume that the
# Normality in the intra-day distribution of the returns
# if we did not assume normality we would have to determine the convolution
# of the daily distribution for all the assets in the portfolio.

# In a normal distribution:
# Range             Expected fraction of population inside range
# mu +- 7*sigma 	0.999999999997440

# Computing daily P&L
time_span = dim(r_df)[1] # number of rows
n_sim = 2000
pl_distribution = NULL
for (t in 1:time_span) {
  pl_distribution = rbind(pl_distribution,
                          sort(rnorm(n_sim, mean = sum(w * r_df[t,]), sd = sqrt( sum(w^2 * (diff_df[t,]/7)^2)) )))
  # we assume normals are iid, otherwise we cannot just sum their sds, we would need their covariances
  # too, which again we do not have due the lack of intraday data.
  # The reason why the sum of sds is divided by 7 is because the diff_df contains the "full range" (a proxy really) of
  # the distribution for each crypto for each day, since we saw in the graphs, that most returns were
  # centered around 0, the full distribution contains around 7 sigmas.
}
pl_distribution = xts(pl_distribution, order.by = index(r_df))

# Defining VAR (Value at Risk) and ES (Expected Shortfall)

Value_at_risk = function(pnl_distribution, alpha=0.05) {
  return(quantile(pnl_distribution, p = alpha))
}

#?apply() # using apply(x, 1, fun) should suffice.

VAR_t_5p = xts(unlist(apply(pl_distribution, 1, Value_at_risk)), order.by = index(r_df))
colnames(VAR_t_5p) = "VaR_005"

ES = function(pnl_distribution, alpha = 0.05) {
  return(sum(pnl_distribution[which(pnl_distribution <= Value_at_risk(pnl_distribution, alpha))])/alpha)
}

ES_t_5p = xts(unlist(apply(pl_distribution, 1, ES)), order.by = index(r_df))
colnames(ES_t_5p) = "ES_005"


# Let's talk about the results
# ----------------------------
# VaR (Value at Risk)
# ----------------------------
# hist(VAR_t_5p, breaks=floor(length(VAR_t_5p)/10), main = "Distribution of VaR_t (5%) since 2019")
mean(VAR_t_5p < 0)  # 0.4925373
# From the histogram we can not fully appreciate how many of the days in the sample the VaR was negative
# from a quick look at the sample we see that ~49% of the times the VaR was negative. We can, therefore,
# say that in about 49% of the days the random portfolio would not lose money (have negative return).
# ----------------------------
# ES (Expected Shortfall)
# ----------------------------
# Even though the portfolio loses money in only ~49% of the days, it could be that the loss in those day was
# substantial. Here the expected shortfall could give us a better view.
# hist(ES_t_5p, breaks=floor(length(VAR_t_5p)/10), main = "Distribution of ES_t (5%) since 2019")
min(ES_t_5p[which(VAR_t_5p < 0)])  # -776.0609
max(ES_t_5p[which(VAR_t_5p < 0)])  # -1.011143
# Here we see that the mininum (daily) Expected Shortfall is ~ -776 and the maximum is about ~ -1
# when the VaR was negative. In other words, the worst (daily) expected loss in the worst 5% of the cases when the VaR
# was negative was ~776 (i.e., losing 776 times the initial investment). Analogously, in the best scenario the expected
# loss in the worst 5% of the cases when the VaR was negative was ~1 (i.e., losing the initial investment).
