

# Terminal Value

g <- 0.0295   # long-term growth rate (2.5%)

fcff_last <- tail(forecast_tbl$fcff, 1)

terminal_value <- fcff_last * (1 + g) / (wacc - g)


# Discount Terminal Value to today 

pv_terminal <- terminal_value / (1 + wacc)^5


# Enterprise Value 

enterprise_value <- sum(forecast_tbl$pv_fcff) + pv_terminal


# Equity Value

cash <- tail(financials_clean$cash, 1)
debt <- tail(financials_clean$total_debt, 1)

equity_value <- enterprise_value + cash - debt

# Intrinstic Price

shares_outstanding <- tail(financials_clean$shares_outstanding, 1)

shares_outstanding_hardcode <- 14.7e9

intrinsic_price <- equity_value / shares_outstanding_hardcode

print(intrinsic_price)

tail(financials_clean$shares_outstanding)

