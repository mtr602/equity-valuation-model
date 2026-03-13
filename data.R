
library(tidyverse)
library(tidyquant)
library(quantmod)
library(ggplot2)
library(lubridate)
library(PerformanceAnalytics)
library(dplyr)
library(httr)
library(jsonlite)

# Initial Market Data 

ticker <- "AAPL"

getSymbols(ticker, src = "yahoo")

head(AAPL)

aapl_prices <- AAPL %>%
  fortify.zoo() %>%
  rename(
    date = Index,
    open = AAPL.Open,
    high = AAPL.High,
    low = AAPL.Low,
    close = AAPL.Close,
    volume = AAPL.Volume,
    adjusted = AAPL.Adjusted
  )

head(aapl_prices)

# Income Statement Data - Part 1 


api_key <- "F0jFBa7ExdDeFC1G02DlLLk27eCShLbP"

url <- paste0(
  "https://financialmodelingprep.com/stable/income-statement?symbol=AAPL&apikey=",
  api_key
)

response <- GET(url)

income_data <- fromJSON(content(response, "text"))


# Cash FLow System - Part 2 


api_key <- "F0jFBa7ExdDeFC1G02DlLLk27eCShLbP"

url <- paste0(
  "https://financialmodelingprep.com/stable/cash-flow-statement?symbol=AAPL&apikey=",
  api_key
)

response <- GET(url)

cashflow_data <- fromJSON(content(response, "text"))

head(cashflow_data)

# Balance Sheet - part 3 


api_key <- "F0jFBa7ExdDeFC1G02DlLLk27eCShLbP"

url <- paste0(
  "https://financialmodelingprep.com/stable/balance-sheet-statement?symbol=AAPL&apikey=",
  api_key
)

response <- GET(url)

balance_data <- fromJSON(content(response, "text"))


# Clean data table 

# Select Required Columns

income_clean <- income_data %>%
  select(
    fiscalYear,
    revenue,
    operatingIncome,
    incomeTaxExpense,
    depreciationAndAmortization,
    weightedAverageShsOut
  ) %>%
  rename(
    year = fiscalYear,
    ebit = operatingIncome,
    tax_expense = incomeTaxExpense,
    depreciation = depreciationAndAmortization,
    shares_outstanding = weightedAverageShsOut
  )

cashflow_clean <- cashflow_data %>%
  select(
    fiscalYear,
    capitalExpenditure,
    changeInWorkingCapital
  ) %>%
  rename(
    year = fiscalYear,
    capex = capitalExpenditure,
    change_wc = changeInWorkingCapital
  )

balance_clean <- balance_data %>%
  select(
    fiscalYear,
    cashAndCashEquivalents,
    totalDebt,
    totalCurrentAssets,
    totalCurrentLiabilities
  ) %>%
  rename(
    year = fiscalYear,
    cash = cashAndCashEquivalents,
    total_debt = totalDebt,
    current_assets = totalCurrentAssets,
    current_liabilities = totalCurrentLiabilities
  )

# Merge in one table

financials_clean <- income_clean %>%
  left_join(cashflow_clean, by = "year") %>%
  left_join(balance_clean, by = "year")

names(financials_clean)


# Terminal Value

g <- 0.025   # long-term growth rate (2.5%)

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

intrinsic_price <- equity_value / shares_outstanding

print(intrinsic_price)

tail(financials_clean$shares_outstanding)

