# Title     : Market Risk Measures using crytocurrency data
# Created by: Francisco Prado Moreno
# Created on: 09/03/2021

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
hist.data.frame(as.data.frame(r_df))
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
                          rnorm(n_sim, mean = sum(w * r_df[t,]), sd = sum((w/7) * diff_df[t,])))
}
pl_distribution = xts(pl_distribution, order.by = index(r_df))

# TODO add VaR and ES