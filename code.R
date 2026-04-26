require(zoo)
require(data.table)
require(forecast)
require(tseries)
require(stargazer)
require(DataCombine)
require(knitr)
library(kableExtra)
require(tibble)
library(xts)
library(readxl)
library(glue)
library(dplyr)
library(ggplot2)
require(fitdistrplus)
library(lmtest)
library("TSA")
library(imputeTS)
library(glue)
library(Metrics)
library(corrplot)
library(fGarch)
library(rugarch)
library(bsts)
library(lubridate)
library(tidyr)

## Initialize data


interpolate_ts <- function(df, class, show_plot=FALSE){
  df$DateOfSale <- as.Date(df$DateOfSale, format = "%m/%d/%Y")
  df$YearMonth <- as.Date(paste0(df$Year, "-", df$Month, "-01"), format = "%Y-%m-%d")
  df_yearmonth <- df %>% group_by(YearMonth) %>% 
    summarise(mean_price = mean(price,na.rm=T)) 
  
  all_months <- data.frame(YearMonth = seq(from = as.Date("2001-01-01"), 
                                           to = as.Date("2013-12-01"), 
                                           by = "month"))
  
  df_full <- merge(all_months, df_yearmonth, by = "YearMonth", all.x = TRUE)
  
  ## Backward Fill
  # df_full$mean_price_filled <- na_locf(df_full$mean_price, option = "locf", 
  #                                       na_remaining = "rev") 
  ## KNN Imputation
  # library(DMwR)
  # df_full$mean_price_filled <- knnImputation(df_full$mean_price, k = 5)
  
  ## Spline Interpolation
  df_full$mean_price_filled <- na.spline(df_full$mean_price)
  
  if (show_plot == TRUE){
    ## Code for plotting:
    # Only keep the interpolated prices
    df_full$only_filled <- ifelse(is.na(df_full$mean_price), 
                                  df_full$mean_price_filled, NA)
    # Convert to time series format
    df_full_ts <- ts(df_full$mean_price, 
                     start = c(2001, 1), frequency = 12)
    ggplot() + 
      geom_line(data=df_full, aes(x=YearMonth,y=mean_price_filled,color="Interpolated Data"),
                size=1) +
      geom_point(data=df_full[!is.na(df_full$only_filled), ], 
                 aes(x=YearMonth,y=only_filled,color="Interpolated Data"),
                 size=2) +
      geom_line(data=df_full, aes(x=YearMonth,y=mean_price,color="Original"),size=1) +
      labs(title = glue("CME Seat Prices Sold - {class}"),x="Year",y="Seat Price") +
      scale_color_manual(values = c("Original" = "black", "Interpolated Data" = "red")) +
      theme_minimal() 
  } else {
    return(df_full)
  }
}

contracts_volume <- read.csv("Contracts_Volume.csv")
contracts_classification <- read.csv("Contracts_Classification.csv")
cmes <- read.csv("cmeS.csv")
imms <- read.csv("immS.csv")
ioms <- read.csv("iomS.csv")

cmes_interpolate <- interpolate_ts(cmes, "CMES")
cmes_interpolate <- cmes_interpolate %>% dplyr::select(-(mean_price)) %>% 
  rename(Date=YearMonth)
imms_interpolate <- interpolate_ts(imms, "IMMS")
imms_interpolate <- imms_interpolate %>% dplyr::select(-(mean_price)) %>% 
  rename(Date=YearMonth)
ioms_interpolate <- interpolate_ts(ioms, "IOMS")
ioms_interpolate <- ioms_interpolate %>% dplyr::select(-(mean_price)) %>% 
  rename(Date=YearMonth)

#contracts_volume
contracts_volume$Date <- as.Date(contracts_volume$Date, format = "%m/%d/%Y")
contracts_volume$Electronic.Volume <- 
  as.numeric(gsub(",","", contracts_volume$Electronic.Volume))
kable(head(contracts_volume,4),caption = "Sample of `contracts_volume` Data")

#contracts_classification
kable(head(contracts_classification,4),caption = "Sample of `contracts_classification` Data")


## Data Preparation


filter_volume <-
  function(.data,
           .division,
           .date = list(floor = "2001-01-01", ceiling = "2012-12-01"),
           .date_format = "%Y-%m-%d",
           test_set = FALSE) {
    dates <- map(.date, ~ lubridate::as_date(.x, .date_format))
    if (.division %in% c("iom", "imm")) {
      .data <- .data %>% filter(division == .division)
    }
    
    if (.division == "cme") {
      .data <- .data %>% 
        mutate(division = "cme") %>% 
        distinct()
    }
    
    if (test_set) {
      .data <- .data %>% filter(date >= dates)
    } else {
      .data <- .data %>% filter(between(date, dates$floor, dates$ceiling))
    }
    .data
  }
aggregate_volume <- function(.data) {
  .data %>% 
    group_by(date) %>%
    summarize(
      total_volume = sum(total_volume) %>% as.double(),
      electronic_volume = sum(electronic_volume) %>% as.double(),
      floor_volume = sum(floor_volume) %>% as.double()
    ) %>% 
    mutate(year_month = yearmonth(date)) %>% 
    select(year_month, everything(), -date)
}
transform_by_division <- function(.division, .data, ...) {
  .data %>%
    filter_volume(.division, ...) %>%
    aggregate_volume()
}

divisions <-
  c(
    cme = "cme",
    imm = "imm",
    iom = "iom"
  )

contract_volume_divisions <- contract_volume_divisions %>%
  mutate(date = as.Date(date, format="%m/%d/%Y"))

train_volumes <-
  map(divisions, transform_by_division, .data = contract_volume_divisions)

test_volumes <-
  map(
    divisions,
    ~ transform_by_division(
      .division = .x,
      .data = contract_volume_divisions,
      .date = "2013-01-01",
      test_set = TRUE
    )
  )

# calculate the Floor volume:
contracts_volume$Floor.Volume = contracts_volume$Total.Volume - contracts_volume$Electronic.Volume
# add division for each commodity code
contracts <- merge(contracts_volume, contracts_classification, 
                   by.x = "Commodity.Indicator", 
                   by.y = "Commodity.Code", 
                   all.x = TRUE)

# prevent duplicates for CME since CME is all
contracts_classification_distinct <- 
  contracts_classification %>% distinct(Commodity.Code, .keep_all = TRUE)
contracts_distinct <- merge(contracts_volume, contracts_classification_distinct, 
                            by.x = "Commodity.Indicator", 
                            by.y = "Commodity.Code", 
                            all.x = TRUE)

# sort out of the volume data, those commodities that are relevant for the particular badge
cme_volume <- contracts_distinct
imm_volume <- contracts[contracts$Division=="IMM",]
iom_volume <- contracts[contracts$Division=="IOM",]

cme_monthly <- cme_volume %>%
  group_by(Date) %>%
  summarize(
    Elec.Vol = sum(Electronic.Volume, na.rm = TRUE),
    Tot.Vol = sum(Total.Volume, na.rm = TRUE),
    Flo.Vol = sum(Floor.Volume, na.rm = TRUE))
imm_monthly <- imm_volume %>%
  group_by(Date) %>%
  summarize(
    Elec.Vol = sum(Electronic.Volume, na.rm = TRUE),
    Tot.Vol = sum(Total.Volume, na.rm = TRUE),
    Flo.Vol = sum(Floor.Volume, na.rm = TRUE))
iom_monthly <- iom_volume %>%
  group_by(Date) %>%
  summarize(
    Elec.Vol = sum(Electronic.Volume, na.rm = TRUE),
    Tot.Vol = sum(Total.Volume, na.rm = TRUE),
    Flo.Vol = sum(Floor.Volume, na.rm = TRUE))

cme_final <- left_join(cmes_interpolate,cme_monthly,by="Date")
kable(head(cme_final,4),caption = "Sample of CME Data")

imm_final <- left_join(imms_interpolate,imm_monthly,by="Date")
kable(head(imm_final,4),caption = "Sample of IMM Data")

iom_final <- left_join(ioms_interpolate,iom_monthly,by="Date")
kable(head(iom_final,4),caption = "Sample of IOM Data")


## Exploratory Data Analysis


par(mfrow = c(1, 3))
plot.ts(cme_final$Date,cme_final$Tot.Vol,type='l',ylab = "Volume",
        xlab="Date",ylim=c(0,max(cme_final$Tot.Vol)),main="CME Volume Breakdown")
lines(cme_final$Date,cme_final$Elec.Vol,col = "red")
lines(cme_final$Date,cme_final$Flo.Vol,col = "blue")
legend("topleft", legend = c("Total","Electric","Floor"), col = c("black","red","blue"), 
       lty = c(2,1,1), lwd = 2)

plot.ts(imm_final$Date,imm_final$Tot.Vol,type='l',ylab = "Volume",
        xlab="Date",ylim=c(0,max(imm_final$Tot.Vol)),main="IMM Volume Breakdown")
lines(imm_final$Date,imm_final$Elec.Vol,col = "red")
lines(imm_final$Date,imm_final$Flo.Vol,col = "blue")
legend("topleft", legend = c("Total","Electric","Floor"), col = c("black","red","blue"), 
       lty = c(2,1,1), lwd = 2)

plot.ts(iom_final$Date,iom_final$Tot.Vol,type='l',ylab = "Volume",
        xlab="Date",ylim=c(0,max(iom_final$Tot.Vol)),main="IOM Volume Breakdown")
lines(iom_final$Date,iom_final$Elec.Vol,col = "red")
lines(iom_final$Date,iom_final$Flo.Vol,col = "blue")
legend("topleft", legend = c("Total","Electric","Floor"), col = c("black","red","blue"), 
       lty = c(2,1,1), lwd = 2)

par(mfrow = c(1, 3))
corrplot(cor(cme_final[,-1]), method = 'square',type="upper", addCoef.col = 'black')
title("CME Correlation")
corrplot(cor(imm_final[,-1]), method = 'square',type="upper", addCoef.col = 'black')
title("IMM Correlation")
corrplot(cor(iom_final[,-1]), method = 'square',type="upper", addCoef.col = 'black')
title("IOM Correlation")


## Linear Regression

linear_regression <- function(data, label){
  if (label == "IOM"){
    model <- lm(mean_price_filled ~ Flo.Vol, data = data)
  } else {
    model <- lm(mean_price_filled ~ Tot.Vol, data = data)
  }
  return(model)
}

checkresiduals(linear_regression(cme_final,"CME"),test=F)
checkresiduals(linear_regression(imm_final,"IMM"),test=F)
checkresiduals(linear_regression(iom_final,"IOM"),test=F)


## Linear regression with ARMA errors

lm_arma_params <- rlang::list2(
  cme = list(price = quo(price), total_volume = quo(total_volume)),
  imm = list(price = quo(price), total_volume = quo(total_volume)),
  iom = list(price = quo(price), floor_volume = quo(floor_volume))
)

lm_arma_models <-
  map2(lm_arma_params, model_price_volume,
       function(.params, .data) {
         .data <- .data %>% 
           ungroup()
         
         price <- .data %>% 
           select(!!.params[[1]]) %>% 
           as.ts(frequency = 12)
         
         xreg <- .data %>% 
           select(!!.params[[2]]) %>% 
           as.ts(frequency = 12)
         
         auto.arima(price, xreg = xreg)
       })

linear_arma <- function(data,label){
  data_ts <- ts(data$mean_price_filled, start = c(2001, 1), frequency = 12)
  if (label == "IOM"){
    xreg <- ts(data$Flo.Vol, start = c(2001, 1), frequency = 12)
  } else {
    xreg <- ts(data$Tot.Vol, start = c(2001, 1), frequency = 12)
  }
  model <- auto.arima(data_ts, xreg = xreg)
  return(model)
}

checkresiduals(linear_arma(cme_final,"CME"),test=F)
checkresiduals(linear_arma(imm_final,"IMM"),test=F)
checkresiduals(linear_arma(iom_final,"IOM"),test=F)


## Holt Winters


holt_winters <- function(data){
  data_ts <- ts(data$mean_price_filled, start = c(2001, 1), frequency = 12)
  model <- HoltWinters(data_ts,seasonal="mult")
  return(model)
}

checkresiduals(holt_winters(cme_final),test=F)
checkresiduals(holt_winters(imm_final),test=F)
checkresiduals(holt_winters(iom_final),test=F)


## ARIMA


arima_default <- function(data){
  data_ts <- ts(data$mean_price_filled, start = c(2001, 1), frequency = 12)
  model <- auto.arima(data_ts,seasonal=FALSE)
  return(model)
}

checkresiduals(arima_default(cme_final),test=F)
checkresiduals(arima_default(imm_final),test=F)
checkresiduals(arima_default(iom_final),test=F)


## Seasonal ARIMA (SARIMA)


arima_seasonal <- function(data){
  data_ts <- ts(data$mean_price_filled, start = c(2001, 1), frequency = 12)
  model <- auto.arima(data_ts,D=1)
  return(model)
}

checkresiduals(arima_seasonal(cme_final),test=F)
checkresiduals(arima_seasonal(imm_final),test=F)
checkresiduals(arima_seasonal(iom_final),test=F)


## Fractional ARIMA (ARFIMA)


arima_fractional <- function(data){
  data_ts <- ts(data$mean_price_filled, start = c(2001, 1), frequency = 12)
  model <- forecast::arfima(data_ts)
  return(model)
}

checkresiduals(arima_fractional(cme_final),test=F)
checkresiduals(arima_fractional(imm_final),test=F)
checkresiduals(arima_fractional(iom_final),test=F)


## ARMA and GARCH combination

fit_garch <-
  function(.ts) {
    arima_model <- auto.arima(.ts, seasonal = FALSE)
    .spec <- ugarchspec(
      variance.model = list(
        mean.model = list(
          armaOrder = arimaorder(arima_model),
          include.mean = T
        ),
        distribution.model = "std"
      )
    )
    ugarchfit(spec = .spec, data = .ts)
  }

garch_models <-
  map(model_price_volume, function(.data) {
    fit_garch(.data %>% select(price, year_month) %>% as.ts(frequency = 12))
  })

# using fGarch
garch <- function(data){
  data$mean_price_filled <- scale(data$mean_price_filled)
  data_ts <- ts(data$mean_price_filled, start = c(2001, 1), frequency = 12)
  arma_model <- auto.arima(data_ts, seasonal = FALSE)
  garch_model <- garchFit(~ garch(1,1), data = residuals(arma_model), trace = FALSE)
  return(garch_model) 
}
# using rugarch
#garch <- function(data){
#  data$mean_price_filled <- scale(data$mean_price_filled)
#  data_ts <- ts(data$mean_price_filled, start = c(2001, 1), frequency = 12)
#  arima_model <- auto.arima(data_ts, seasonal = FALSE)
#  spec <- ugarchspec(variance.model = 
#                       list(mean.model = list(armaOrder = arimaorder(arima_model),
#                                              include.mean = T),
#                            distribution.model = "std"))
#  garch_fit <- ugarchfit(spec = spec, data = residuals(arima_model))
#  return(garch_fit)
#}

checkresiduals(residuals(garch(cme_final)),test=F)
checkresiduals(residuals(garch(imm_final)),test=F)
checkresiduals(residuals(garch(iom_final)),test=F)


## Model Comparison


model_evaluation <- function(data, label){
  train <- data[data$Date < '2013-01-01',]
  test <- data[data$Date >= '2013-01-01',]
  compute_rmse <- function(actual, predicted) {
    return(rmse(actual, predicted))
  }
  pred_linear <- predict(linear_regression(train, "CME"), test)
  if (label == "IOM"){
    xreg <- ts(train$Flo.Vol, start = c(2001, 1), frequency = 12)
  } else {
    xreg <- ts(train$Tot.Vol, start = c(2001, 1), frequency = 12)
  }
  pred_arma <- forecast(linear_arma(train, "CME"), xreg = xreg)$mean
  pred_hw <- forecast(holt_winters(train), h = nrow(test))$mean
  pred_arima_def <- forecast(arima_default(train), h = nrow(test))$mean
  pred_arima_seas <- forecast(arima_seasonal(train), h = nrow(test))$mean
  pred_arima_frac <- forecast(arima_fractional(train), h = nrow(test))$mean
  pred_garch <- predict(garch(train), n.ahead = nrow(test))$meanForecast
  # Compute RMSE for each model
  rmse_linear <- compute_rmse(test$mean_price_filled, pred_linear)
  rmse_arma <- compute_rmse(test$mean_price_filled, pred_arma)
  rmse_hw <- compute_rmse(test$mean_price_filled, pred_hw)
  rmse_arima_def <- compute_rmse(test$mean_price_filled, pred_arima_def)
  rmse_arima_seas <- compute_rmse(test$mean_price_filled, pred_arima_seas)
  rmse_arima_frac <- compute_rmse(test$mean_price_filled, pred_arima_frac)
  rmse_garch <- compute_rmse(test$mean_price_filled, pred_garch)
  rmse_results <- data.frame(
    Model = c("Linear Regression", "ARMA", "Holt-Winters", "ARIMA Default", 
              "ARIMA Seasonal", "ARIMA Fractional", "GARCH"),
    RMSE = c(rmse_linear, rmse_arma, rmse_hw, rmse_arima_def, 
             rmse_arima_seas, rmse_arima_frac, rmse_garch)
  )
  return(rmse_results)
}

kable(model_evaluation(cme_final,"CME"))
kable(model_evaluation(imm_final,"IMM"))
kable(model_evaluation(iom_final,"IOM"))


## Commodity Prices


sts_data_2023 <- read.csv("sts_data_2023.csv")
sts_data_2023$Date <- as.Date(sts_data_2023$Date, format = "%m/%d/%y")
sts_data_2023$Date <- as.Date(paste0(ifelse(as.numeric(format(sts_data_2023$Date, "%Y")) > 2024, 
                                            as.numeric(format(sts_data_2023$Date, "%Y")) - 100, 
                                            format(sts_data_2023$Date, "%Y")), 
                                     "-", format(sts_data_2023$Date, "%m-%d")))
sts_data_2023 <- na.omit(sts_data_2023)
kable(head(sts_data_2023,4),caption = "Sample of `sts_data_2023` Data")

par(mfrow = c(1, 2))
plot.ts(sts_data_2023$Mean_CL, main = "Mean CL Over Time",ylab = "Mean CL")
plot.ts(sts_data_2023$Mean_GC, main = "Mean GC Over Time",ylab = "Mean GC")

#data <- sts_data_2023
#date1<- "6/1979"
bsts_forecasts <- function(data, date1){
  fit_end <- ceiling_date(as.Date(paste0("01/", date1), format="%d/%m/%Y"),"month") 
  data_fit <- data %>% filter(Date <= fit_end)
  
  # CL: Define state space specification
  data_fit_cl <- data_fit %>% 
    mutate(
      GC_Lag1 = lag(Mean_GC, 1),
      CPI_Lag1 = lag(Mean_CPI, 1),
      SPX_Lag1 = lag(Mean_SPX, 1)
    ) %>%
    drop_na() # Remove NA values after lagging
  
  ss1_cl <- list()
  ss1_cl <- AddLocalLevel(ss1_cl, y = data_fit_cl$Mean_CL)
  
  ss2_cl <- list()
  ss2_cl <- AddLocalLinearTrend(ss2_cl, y = data_fit_cl$Mean_CL)
  
  ss3_cl <- list()
  ss3_cl <- AddLocalLinearTrend(ss3_cl, y = data_fit_cl$Mean_CL) 
  ss3_cl <- AddSeasonal(ss3_cl, y = data_fit_cl$Mean_CL, nseasons = 4)
  
  sink("/dev/null")
  # Fit BSTS model with lags 
  model1_cl <- bsts(
    Mean_CL ~ GC_Lag1 + CPI_Lag1 + SPX_Lag1, 
    state.specification = ss1_cl,
    data = data_fit_cl,
    niter = 1000
  )
  model2_cl <- bsts(
    Mean_CL ~ GC_Lag1 + CPI_Lag1 + SPX_Lag1, 
    state.specification = ss2_cl,
    data = data_fit_cl,
    niter = 1000
  )
  model3_cl <- bsts(
    Mean_CL ~ GC_Lag1 + CPI_Lag1 + SPX_Lag1,
    state.specification = ss3_cl,
    data = data_fit_cl,
    niter = 1000
  )
  sink()
  
  
  # GC: Define state space specification
  data_fit_gc <- data_fit %>%
    mutate(
      CL_Lag1 = lag(Mean_CL, 1), 
      CPI_Lag1 = lag(Mean_CPI, 1), 
      SPX_Lag1 = lag(Mean_SPX, 1)
    ) %>%
    drop_na() # Remove NA values after lagging
  
  ss1_gc <- list()
  ss1_gc <- AddLocalLevel(ss1_gc, y = data_fit_gc$Mean_GC)
  
  ss2_gc <- list()
  ss2_gc <- AddLocalLinearTrend(ss2_gc, y = data_fit_gc$Mean_GC)
  
  ss3_gc <- list()
  ss3_gc <- AddLocalLinearTrend(ss3_gc, y = data_fit_gc$Mean_GC) 
  ss3_gc <- AddSeasonal(ss3_gc, y = data_fit_gc$Mean_GC, nseasons = 4)
  
  # Fit BSTS model with lags
  sink("/dev/null")
  model1_gc <- bsts(
    Mean_GC ~ CL_Lag1 + CPI_Lag1 + SPX_Lag1,
    state.specification = ss1_gc,
    data = data_fit_gc,
    niter = 1000
  )
  model2_gc <- bsts(
    Mean_GC ~ CL_Lag1 + CPI_Lag1 + SPX_Lag1,
    state.specification = ss2_gc,
    data = data_fit_gc,
    niter = 1000
  )
  model3_gc <- bsts(
    Mean_GC ~ CL_Lag1 + CPI_Lag1 + SPX_Lag1,
    state.specification = ss3_gc,
    data = data_fit_gc,
    niter = 1000
  ) 
  sink()
  
  horizon <- 5
  
  # CL prediction
  last_values_cl <- tail(data_fit_cl, 1) # Take last available values 
  newdata_cl <- data.frame(
    GC_Lag1 = rep(last_values_cl$GC_Lag1, horizon),
    CPI_Lag1 = rep(last_values_cl$CPI_Lag1, horizon),
    SPX_Lag1 = rep(last_values_cl$SPX_Lag1, horizon)
  )
  # Predict using BSTS
  predictions1_cl <- predict(model1_cl, horizon = horizon, newdata = newdata_cl) 
  predictions2_cl <- predict(model2_cl, horizon = horizon, newdata = newdata_cl) 
  predictions3_cl <- predict(model3_cl, horizon = horizon, newdata = newdata_cl)
  
  actual_cl <- data %>% filter(Date >= fit_end-months(2)) %>% head(5) %>% 
    dplyr::select(Mean_CL)
  
  # GC prediction
  last_values_gc <- tail(data_fit_gc, 1) # Take last available values 
  newdata_gc <- data.frame(
    CL_Lag1 = rep(last_values_gc$CL_Lag1, horizon),
    CPI_Lag1 = rep(last_values_gc$CPI_Lag1, horizon),
    SPX_Lag1 = rep(last_values_gc$SPX_Lag1, horizon)
  )
  # Predict using BSTS
  predictions1_gc <- predict(model1_gc, horizon = horizon, newdata = newdata_gc)
  predictions2_gc <- predict(model2_gc, horizon = horizon, newdata = newdata_gc)
  predictions3_gc <- predict(model3_gc, horizon = horizon, newdata = newdata_gc)
  
  actual_gc <- data %>% filter(Date >= fit_end-months(2)) %>% head(5) %>% 
    dplyr::select(Mean_GC)
  
  if (date1=="12/2022"){
    cat(" RMSE (CL Model 1):",Metrics::rmse(actual_cl$Mean_CL,
                                            predictions1_cl$mean[1:2]),"\n", 
        "RMSE (CL Model 2):",Metrics::rmse(actual_cl$Mean_CL,
                                           predictions2_cl$mean[1:2]),"\n", 
        "RMSE (CL Model 3):",Metrics::rmse(actual_cl$Mean_CL,
                                           predictions3_cl$mean[1:2]),"\n", 
        "RMSE (GC Model 1):",Metrics::rmse(actual_gc$Mean_GC,
                                           predictions1_gc$mean[1:2]),"\n", 
        "RMSE (GC Model 2):",Metrics::rmse(actual_gc$Mean_GC,
                                           predictions2_gc$mean[1:2]),"\n", 
        "RMSE (GC Model 3):",Metrics::rmse(actual_gc$Mean_GC,
                                           predictions3_gc$mean[1:2]))
  } else {
    cat(" RMSE (CL Model 1):",Metrics::rmse(actual_cl$Mean_CL,predictions1_cl$mean),"\n",
        "RMSE (CL Model 2):",Metrics::rmse(actual_cl$Mean_CL,predictions2_cl$mean),"\n", 
        "RMSE (CL Model 3):",Metrics::rmse(actual_cl$Mean_CL,predictions3_cl$mean),"\n", 
        "RMSE (GC Model 1):",Metrics::rmse(actual_gc$Mean_GC,predictions1_gc$mean),"\n", 
        "RMSE (GC Model 2):",Metrics::rmse(actual_gc$Mean_GC,predictions2_gc$mean),"\n", 
        "RMSE (GC Model 3):",Metrics::rmse(actual_gc$Mean_GC,predictions3_gc$mean))
  }
  # Plots
  par(mfrow = c(2, 3))
  plot(predictions1_cl, main = "CL: Dynamic regression of other 3 variables") 
  plot(predictions2_cl, main = "CL: Different trend component for each model") 
  plot(predictions3_cl, main = "CL: State specification of Our Choice (Seasonal)") 
  plot(predictions1_gc, main = "GC: Dynamic regression of other 3 variables") 
  plot(predictions2_gc, main = "GC: Different trend component for each model") 
  plot(predictions3_gc, main = "GC: State specification of Our Choice (Seasonal)")
  
  # Inclusion Probability Plots
  par(mfrow = c(2, 3))
  plot(model1_cl,"coef", main = "CL: Dynamic regression of other 3 variables")
  plot(model2_cl,"coef", main = "CL: Different trend component for each model")
  plot(model3_cl,"coef", main = "CL: State specification of Our Choice (Seasonal)")
  plot(model1_gc,"coef", main = "GC: Dynamic regression of other 3 variables")
  plot(model2_gc,"coef", main = "GC: Different trend component for each model")
  plot(model3_gc,"coef", main = "GC: State specification of Our Choice (Seasonal)")
  mtext(" ", outer = TRUE, cex = 1.5, line = -1)
}

bsts_forecasts(sts_data_2023,"6/1979")
bsts_forecasts(sts_data_2023,"6/2004")
bsts_forecasts(sts_data_2023,"12/2022")



## EXTRA


#data <- sts_data_2023
#date1<- "6/2004"
bsts_forecasts <- function(data, date1){
  fit_end <- ceiling_date(as.Date(paste0("01/", date1), format="%d/%m/%Y"),"month")
  data_fit <- data %>% filter(Date <= fit_end)
  
  
  # CL: Define state space specification
  data_fit_cl <- data_fit %>%
    mutate(
      GC_Lag1 = lag(Mean_GC, 1),
      CPI_Lag1 = lag(Mean_CPI, 1),
      SPX_Lag1 = lag(Mean_SPX, 1)
    ) %>%
    drop_na()  # Remove NA values after lagging
  
  ss1_cl <- list()
  ss1_cl <- AddLocalLevel(ss1_cl, y = data_fit_cl$Mean_CL)
  ss1_cl <- AddDynamicRegression(ss1_cl,
                                 formula = Mean_CL~CPI_Lag1+SPX_Lag1,
                                 data = data_fit_cl)
  
  ss2_cl <- list()
  ss2_cl <- AddLocalLinearTrend(ss2_cl, y = data_fit_cl$Mean_CL)
  ss2_cl <- AddDynamicRegression(ss2_cl,
                                 formula = Mean_CL~CPI_Lag1+SPX_Lag1,
                                 data = data_fit_cl)
  
  ss3_cl <- list()
  ss3_cl <- AddLocalLinearTrend(ss3_cl, y = data_fit_cl$Mean_CL)  
  ss3_cl <- AddSeasonal(ss3_cl, y = data_fit_cl$Mean_CL, nseasons = 4) 
  ss3_cl <- AddDynamicRegression(ss3_cl,
                                 formula = Mean_CL~CPI_Lag1+SPX_Lag1,
                                 data = data_fit_cl)
  
  #sink("/dev/null")
  # Fit BSTS model with lags
  model1_cl <- bsts(
    data_fit_cl$Mean_CL, 
    state.specification = ss1_cl,
    niter = 1000
  )
  model2_cl <- bsts(
    data_fit_cl$Mean_CL, 
    state.specification = ss2_cl,
    niter = 1000
  )
  model3_cl <- bsts(
    data_fit_cl$Mean_CL, 
    state.specification = ss3_cl,
    niter = 1000
  )
  #sink()
  
  
  # GC: Define state space specification
  data_fit_gc <- data_fit %>%
    mutate(
      CL_Lag1 = lag(Mean_CL, 1),
      CPI_Lag1 = lag(Mean_CPI, 1),
      SPX_Lag1 = lag(Mean_SPX, 1)
    ) %>%
    drop_na()  # Remove NA values after lagging
  
  ss1_gc <- list()
  ss1_gc <- AddLocalLevel(ss1_gc, y = data_fit_gc$Mean_GC)
  ss1_gc <- AddDynamicRegression(ss1_gc,
                                 formula = Mean_GC~CPI_Lag1+SPX_Lag1,
                                 data = data_fit_gc)
  
  ss2_gc <- list()
  ss2_gc <- AddLocalLinearTrend(ss2_gc, y = data_fit_gc$Mean_GC)
  ss2_gc <- AddDynamicRegression(ss2_gc,
                                 formula = Mean_GC~CPI_Lag1+SPX_Lag1,
                                 data = data_fit_gc)
  
  ss3_gc <- list()
  ss3_gc <- AddLocalLinearTrend(ss3_gc, y = data_fit_gc$Mean_GC)  
  ss3_gc <- AddSeasonal(ss3_gc, y = data_fit_gc$Mean_GC, nseasons = 4)  
  ss3_gc <- AddDynamicRegression(ss3_gc,
                                 formula = Mean_GC~CPI_Lag1+SPX_Lag1,
                                 data = data_fit_gc)
  
  # Fit BSTS model with lags
  #sink("/dev/null")
  model1_gc <- bsts(
    data_fit_cl$Mean_GC, 
    state.specification = ss1_gc,
    niter = 1000
  )
  model2_gc <- bsts(
    data_fit_cl$Mean_GC, 
    state.specification = ss2_gc,
    niter = 1000
  )
  model3_gc <- bsts(
    data_fit_cl$Mean_GC, 
    state.specification = ss3_gc,
    niter = 1000
  )
  #sink()
  
  horizon <- 5
  
  # CL prediction
  last_values_cl <- tail(data_fit_cl, 1)  # Take last available values
  newdata_cl <- data.frame(
    GC_Lag1 = rep(last_values_cl$GC_Lag1, horizon),
    CPI_Lag1 = rep(last_values_cl$CPI_Lag1, horizon),
    SPX_Lag1 = rep(last_values_cl$SPX_Lag1, horizon)
  )
  # Predict using BSTS
  predictions1_cl <- predict(model1_cl, horizon = horizon, newdata = newdata_cl)
  predictions2_cl <- predict(model2_cl, horizon = horizon, newdata = newdata_cl)
  predictions3_cl <- predict(model3_cl, horizon = horizon, newdata = newdata_cl)
  
  actual_cl <- data %>% filter(Date >= fit_end-months(2)) %>% head(5) %>% 
    dplyr::select(Mean_CL)
  
  # GC prediction
  last_values_gc <- tail(data_fit_gc, 1)  # Take last available values
  newdata_gc <- data.frame(
    CL_Lag1 = rep(last_values_gc$CL_Lag1, horizon),
    CPI_Lag1 = rep(last_values_gc$CPI_Lag1, horizon),
    SPX_Lag1 = rep(last_values_gc$SPX_Lag1, horizon)
  )
  # Predict using BSTS
  predictions1_gc <- predict(model1_gc, horizon = horizon, newdata = newdata_gc)
  predictions2_gc <- predict(model2_gc, horizon = horizon, newdata = newdata_gc)
  predictions3_gc <- predict(model3_gc, horizon = horizon, newdata = newdata_gc)
  
  actual_gc <- data %>% filter(Date >= fit_end-months(2)) %>% head(5) %>% 
    dplyr::select(Mean_GC)
  
  
  if (date1=="12/2022"){
    cat(" RMSE (CL Model 1):",Metrics::rmse(actual_cl$Mean_CL,
                                            predictions1_cl$mean[1:2]),"\n",
        "RMSE (CL Model 2):",Metrics::rmse(actual_cl$Mean_CL,
                                           predictions2_cl$mean[1:2]),"\n",
        "RMSE (CL Model 3):",Metrics::rmse(actual_cl$Mean_CL,
                                           predictions3_cl$mean[1:2]),"\n",
        "RMSE (GC Model 1):",Metrics::rmse(actual_gc$Mean_GC,
                                           predictions1_gc$mean[1:2]),"\n",
        "RMSE (GC Model 2):",Metrics::rmse(actual_gc$Mean_GC,
                                           predictions2_gc$mean[1:2]),"\n",
        "RMSE (GC Model 3):",Metrics::rmse(actual_gc$Mean_GC,
                                           predictions3_gc$mean[1:2]))
  } else {
    cat(" RMSE (CL Model 1):",Metrics::rmse(actual_cl$Mean_CL,predictions1_cl$mean),"\n",
        "RMSE (CL Model 2):",Metrics::rmse(actual_cl$Mean_CL,predictions2_cl$mean),"\n",
        "RMSE (CL Model 3):",Metrics::rmse(actual_cl$Mean_CL,predictions3_cl$mean),"\n",
        "RMSE (GC Model 1):",Metrics::rmse(actual_gc$Mean_GC,predictions1_gc$mean),"\n",
        "RMSE (GC Model 2):",Metrics::rmse(actual_gc$Mean_GC,predictions2_gc$mean),"\n",
        "RMSE (GC Model 3):",Metrics::rmse(actual_gc$Mean_GC,predictions3_gc$mean))
  }
  
  
  # Plots
  par(mfrow = c(2, 3))
  plot(predictions1_cl, main = "CL: Dynamic regression of other 3 variables")
  plot(predictions2_cl, main = "CL: Different trend component for each model")
  plot(predictions3_cl, main = "CL: State specification of Our Choice (Seasonal)")
  plot(predictions1_gc, main = "GC: Dynamic regression of other 3 variables")
  plot(predictions2_gc, main = "GC: Different trend component for each model")
  plot(predictions3_gc, main = "GC: State specification of Our Choice (Seasonal)")
  
  # Inclusion Probability Plots
  par(mfrow = c(2, 3))
  
  plot(model1_cl$dynamic.regression.coefficients[,,1][,1],
       main = "CL Model 1 Inclusion Probabilities",xlab = "Iteration",
       ylab = "Inclusion Probability",type="l",col="blue")
  lines(model1_cl$dynamic.regression.coefficients[,,1][,2],col="red")
  legend("topright", legend = c("CPI Lag","SPX Lag"), col = c("blue","red"),lty = c(1,1))
  
  plot(model2_cl$dynamic.regression.coefficients[,,1][,1],
       main = "CL Model 2 Inclusion Probabilities",xlab = "Iteration",
       ylab = "Inclusion Probability",type="l",col="blue")
  lines(model2_cl$dynamic.regression.coefficients[,,1][,2],col="red")
  legend("topright", legend = c("CPI Lag","SPX Lag"), col = c("blue","red"),lty = c(1,1))
  
  plot(model3_cl$dynamic.regression.coefficients[,,1][,1],
       main = "CL Model 3 Inclusion Probabilities",xlab = "Iteration",
       ylab = "Inclusion Probability",type="l",col="blue")
  lines(model3_cl$dynamic.regression.coefficients[,,1][,2],col="red")
  legend("topright", legend = c("CPI Lag","SPX Lag"), col = c("blue","red"),lty = c(1,1))
  
  
  plot(model1_gc$dynamic.regression.coefficients[,,1][,1],
       main = "GC Model 1 Inclusion Probabilities",xlab = "Iteration",
       ylab = "Inclusion Probability",type="l",col="blue")
  lines(model1_gc$dynamic.regression.coefficients[,,1][,2],col="red")
  legend("topright", legend = c("CPI Lag","SPX Lag"), col = c("blue","red"),lty = c(1,1))
  
  plot(model2_gc$dynamic.regression.coefficients[,,1][,1],
       main = "GC Model 2 Inclusion Probabilities",xlab = "Iteration",
       ylab = "Inclusion Probability",type="l",col="blue")
  lines(model2_gc$dynamic.regression.coefficients[,,1][,2],col="red")
  legend("topright", legend = c("CPI Lag","SPX Lag"), col = c("blue","red"),lty = c(1,1))
  
  plot(model3_gc$dynamic.regression.coefficients[,,1][,1],
       main = "GC Model 3 Inclusion Probabilities",xlab = "Iteration",
       ylab = "Inclusion Probability",type="l",col="blue")
  lines(model3_gc$dynamic.regression.coefficients[,,1][,2],col="red")
  legend("topright", legend = c("CPI Lag","SPX Lag"), col = c("blue","red"),lty = c(1,1))
  
  mtext(" ", outer = TRUE, cex = 1.5, line = -1)
}









