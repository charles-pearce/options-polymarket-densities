# returns a list with the calibration coefficients per stock and combined in the correct format
get_calibration_coefficients = function(parquet_loc, price_tolerance = 0.05){
  require(arrow)
  require(data.table)
  
  # only read needed columns
  parquet = setDT(read_parquet(parquet_loc, col_select = c("market", "DTE", "price_yes", "outcome_yes")))

  
  # remove observations we don't need
  parquet = parquet[DTE < 5 & DTE > 0]
  
  parquet[,price_yes := price_yes * 24*60 / sum(price_yes), by = .(market, DTE)]
  
  parquet = parquet[price_yes > price_tolerance & price_yes < 1-price_tolerance]
  
  parquet[, c("stock", "week") := tstrsplit(market, "-week-", fixed = TRUE)]
  
  parquet[, logit_price := log(price_yes / (1 - price_yes))]
  
  
  setkey(parquet, stock, DTE)
  
  
  coefs = parquet[,
    {
      fit = glm(outcome_yes ~ logit_price,
                family = binomial(link = "logit"),
                data = .SD)
      as.list(coef(fit))
    },
    .SDcols = c("outcome_yes", "logit_price"),
    by = .(stock, DTE)
  ]
  
  coefs = coefs[, stock := toupper(stock)]
  
  comb_coef = parquet[,
                      {
                        fit = glm(outcome_yes ~ logit_price,
                                  family = binomial(link = "logit"),
                                  data = .SD)
                        as.list(coef(fit))
                      },
                      .SDcols = c("outcome_yes", "logit_price"),
                      by = .(DTE)
                      ]
  comb_coef[, stock := "COMBINED"]
  
  coefs = rbind(coefs,comb_coef)
  
  setorder(coefs, stock, DTE)
  
  calibration_coeff = split(coefs, coefs[, "stock"]) |>
    lapply(function(df) unname(df[, "logit_price"]))
  
  #make sure the paquet file is not kept
  rm(parquet)
  gc()
  
  return(calibration_coeff)
}

