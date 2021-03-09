# Title     : TODO
# Objective : TODO
# Created by: chanx
# Created on: 09/03/2021

# Importing useful libraries ####
library("quantmod")


crypto_to_usd = new.env()

# Importing crypto ####
# 13 Largest Market Caps

# Bitcoin USD
# Ethereum USD
# BinanceCoin USD
# Cardano USD
# Tether USD
# Polkadot USD
# XRP USD
# Litecoin USD
# Chainlink USD
# BitcoinCash USD
# Stellar USD
# USDCoin USD
# Dogecoin USD

cryto_as_list =  c("BTC-USD", "ETH-USD", "BNB-USD", "ADA-USD",
                   "USDT-USD", "DOT1-USD", "XRP-USD", "LTC-USD",
                   "LINK-USD", "BCH-USD", "XLM-USD", "USDC-USD",
                    "DOGE-USD")
# For simplicity we will only use the adjusted prices

cryto_var_names =  c("btc_usd", "eth_usd", "bnb_usd", "ada_usd",
                   "usdt_usd", "dot1_usd", "xrp_usd", "ltc_usd",
                   "link_usd", "bch_usd", "xlm_usd", "usdc_usd",
                    "doge_usd")


for (currency in 1:length(cryto_as_list)) {
  var_ = getSymbols(cryto_as_list[currency], verbose = FALSE, warnings = FALSE,
           src = "yahoo",
           env = crypto_to_usd, auto.assign = FALSE)[,6]
  # the 6th variable is the Adjustted prices
  colnames(var_) = paste(cryto_var_names[currency],"adj", sep = "_")
  assign(paste(cryto_var_names[currency]), var_)
}
