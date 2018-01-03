library(hash)
library(forecast)

set.seed(Sys.time()+Sys.getpid())

g2_price <- hash()
g2_price[["us-west-2"]] = 0.65
g2_price[["us-west-1"]] = 0.702
g2_price[["us-east-1"]] = 0.65
g2_price[["ap-northeast-1"]] = 0.898
g2_price[["ap-southeast-1"]] = 1.0
g2_price[["ap-southeast-2"]] = 0.898
g2_price[["eu-central-1"]] = 0.772
g2_price[["eu-west-1"]] = 0.702

c3_price <- hash()
c3_price[["ap-northeast-1"]] = 0.562
c3_price[["ap-southeast-1"]] = 0.529
c3_price[["ap-southeast-2"]] = 0.529
c3_price[["eu-west-1"]] = 0.478
c3_price[["us-east-1"]] = 0.42
c3_price[["us-west-1"]] = 0.478
c3_price[["us-west-2"]] = 0.42

m4_price <- hash()
m4_price[["ap-northeast-1"]] = 0.556
m4_price[["ap-southeast-1"]] = 0.533
m4_price[["ap-southeast-2"]] = 0.538
m4_price[["eu-west-1"]] = 0.475
m4_price[["us-east-1"]] = 0.431
m4_price[["us-west-1"]] = 0.503
m4_price[["us-west-2"]] = 0.431

i2_price <- hash()
i2_price[["ap-northeast-1"]] = 2.001
i2_price[["ap-southeast-1"]] = 2.035
i2_price[["ap-southeast-2"]] = 2.035
i2_price[["eu-west-1"]] = 1.876
i2_price[["us-east-1"]] = 1.705
i2_price[["us-west-1"]] = 1.876
i2_price[["us-west-2"]] = 1.705

getOndemandPrice <- function(filename) {
  region = convertFileNameToRegion(filename)
  type = getInstanceType(filename)
  if (type == "g2.2xlarge") pt = g2_price
  else if (type == "c3.2xlarge") pt = c3_price
  else if (type == "m4.2xlarge") pt = m4_price
  else if (type == "i2.2xlarge") pt = i2_price
  
  get(region,pt)
}

getInstanceType <- function(filename) {
  dir_sep = strsplit(filename, "/")[[1]]
  dir_sep = dir_sep[length(dir_sep)]
  strsplit(dir_sep, "_")[[1]][2]
}

convertFileNameToRegion <- function (filename) {
  dir_sep = strsplit(filename, "/")[[1]]
  dir_sep = dir_sep[length(dir_sep)]
  az = strsplit(dir_sep, "_")[[1]][1]
  substr(az, 1, nchar(az)-1)
}

readAsTimeseries <- function(filename) {
  ondemand_price = getOndemandPrice(filename)
  price = read.table(filename,sep=",")$V2
  price = price[-1]
  price = price[-length(price)]
  time_series = ts(sapply(price, function(x) min(1.0,x/ondemand_price)), frequency = 24)
  time_series
}

buildTrainWindow <- function(price_history, day, modeling_period) {
  window(price_history, start=c(day-modeling_period, 1), end=c(day-1, 23))
}

buildTestWindow <- function(price_history, day, hour, modeling_period) {
  window(price_history, start=c(day-modeling_period, 1), end=c(day, hour))
}

buildEvalWindow <- function(price_history, day, hour, modeling_period) {
  window(price_history, start=c(day-modeling_period, 1), end=c(day+1, hour))
}

runEdbSimulation <- function(price_log_path, modeling_period, use_period) {
  predict_hours = 24
  ts_price = readAsTimeseries(price_log_path)
  end_day = end(ts_price)[1]
  model_use_days = 100000
  test_rmse <- vector()
  train_rmse <- vector()
  for (d in seq(modeling_period, end_day-5)) {
    if(model_use_days > use_period) {
      train_time = buildTrainWindow(ts_price, d, modeling_period)
      model = auto.arima(train_time)
      model_use_days = 0
    }
    for (h in seq(0, 23)) {
      test_time = buildTestWindow(ts_price, d, h, modeling_period)
      eval_time = buildEvalWindow(ts_price, d, h, modeling_period)
      predict = forecast(test_time, predict_hours, model=model)
      acc = accuracy(predict, eval_time)
      vec_index = (d - modeling_period) * 24 + h
      test_rmse[vec_index] = acc[4]
      train_rmse[vec_index] = acc[3]
    }
    print(paste(mean(test_rmse), mean(train_rmse)))
    model_use_days = model_use_days + 1
  }
  print(paste("arima", price_log_path, mean(test_rmse), mean(train_rmse)))
}

