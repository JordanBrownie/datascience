
ratio_data <- na.omit(feature_data, cols = c("manufacturer","age"))
ratio_data <- ratio_data[usd_sale_price < usd_list_price | is.na(usd_sale_price) | is.na(usd_list_price)]

dt_make_model <- unique(ratio_data[, list(make_model,manufacturer)])

ratio_auction_bounds <- c(0.40, 0.92)
ratio_asking_bounds <-c(1.02, 1.6)

auction_price_bounds <- c(1750, 250000)
aftersale_price_bounds <- c(2000, 275000)
asking_price_bounds <- c(2500, 300000)

tree_formula <- paste0("usd_full_price~data_type+make_model+manufacturer+age")
rx_pred_price <- train_rx_fast_trees(data = ratio_data[age %between% c(0, 1)], 
                                     formula = tree_formula, 
                                     rx_options = list(numTrees = 300, numLeaves = 50,reportProgress = 3))
print("post pred price")
##### Remove some outliers #####
ratio_data <- recipe(ratio_data) %>%
  add_prediction(model = rx_pred_price, new_col = "pred_price") %>%
  step_dt(NULL, pred_error := pred_price - usd_full_price) %>%
  filter(pred_error > (quantile(pred_error, probs = 0.05, na.rm = TRUE) - 1*IQR(pred_error, na.rm = TRUE))) %>%
  filter(pred_error < (quantile(pred_error, probs = 0.95, na.rm = TRUE) + 1*IQR(pred_error, na.rm = TRUE))) %>%
  trained()


##### Build make model values with a spec you can impute by make model #####
rx_mm_value <- train_rx_fast_trees(data = ratio_data[age %between% c(0, 1)], 
                                   formula = tree_formula, 
                                   rx_options = list(numTrees = 300, numLeaves = 50))

# look <- ratio_data[, pred_mm_value := rxPredict(rx_mm_value, data = ratio_data, verbose = 0)$Score]
# plot(look$sc_full_price, look$pred_mm_value)
#cor(look$sc_full_price, look$pred_mm_value)


##### Build data table with year, make model, and data type #####
age_vector <- as.numeric(c(seq(0,10.5,.5),seq(11,24,1),seq(25,45,5)))
dt_year_mm <- CJ(make_model = ratio_data$make_model, age = age_vector, data_type = ratio_data$data_type, unique = TRUE)

##### Apply rx model to make model data table #####
dt_year_mm <- recipe(dt_year_mm, info = NULL) %>%
  step_merge(y = dt_make_model,by = c("make_model"),all.x = FALSE) %>%
  step_dt(NULL, age := as.numeric(age)) %>%
  step_dt(NULL, `:=`(usd_full_price = NA_real_, condition = "used")) %>%
  trained()


##### Smooth auction, aftersale, and asking values by age #####
dt_year_mm <- dt_year_mm[,`:=`(pred_mm_value = rxPredict(rx_mm_value, data = dt_year_mm, verbose = 0)$Score),
                         ][,`:=`(make_model_value = loess("pred_mm_value~age", data = .SD, span = 0.20, 
                                                          degree = 1)$fitted), 
                           by = list(make_model, data_type)
                           ][,c("make_model","manufacturer","data_type","age","make_model_value")]

dt_year_mm <- dcast(dt_year_mm, make_model+manufacturer+age ~ str_c("mmv_", data_type), 
                    value.var = "make_model_value")


# a <- ratio_data[, .N, make_model][order(-N)]
# n <- 1
# library("ggplot2", "personal")
# ggplot(dt_year_mm[make_model == a[n, make_model] & age < 40], aes(age)) +
#   ylab("USD") +
#   ggtitle(str_c(a[n, make_model], " make_model_values by data type")) +
#   geom_line(aes(y = mmv_asking, colour = "Asking")) +
#   geom_line(aes(y = mmv_aftersale, colour = "Aftersale")) +
#   geom_line(aes(y = mmv_auction, colour = "Auction"))


##### Use smoothed make model values to get auction and asking ratios #####
##### apply business rule bounds #####

dt_year_mm_final <- recipe(dt_year_mm, info = NULL) %>%
  step_bound(cutoffs = list("mmv_auction" = auction_price_bounds,
                            "mmv_aftersale" = aftersale_price_bounds,
                            "mmv_asking" =  asking_price_bounds)) %>%
  step_dt(NULL, `:=`(ratio_auction = mmv_auction/mmv_aftersale,
                     ratio_asking = mmv_asking/mmv_aftersale)) %>%
  step_bound(cutoffs = list(ratio_auction = ratio_auction_bounds,
                            ratio_asking = ratio_asking_bounds)) %>%
  step_dt(NULL, `:=`(mmv_auction = mmv_aftersale*ratio_auction,
                     mmv_asking = mmv_aftersale*ratio_asking)) %>%
  trained()

# quantile(dt_year_mm_final$ratio_auction, seq(0, 1, 0.01))
# quantile(dt_year_mm_final$ratio_asking, seq(0, 1, 0.01))


df_ratio_bounds <- dt_year_mm_final[,list(ratio_auction_lower = quantile(ratio_auction, probs = 0.1, na.rm = TRUE, names = FALSE),
                                          ratio_auction_upper = quantile(ratio_auction, probs = 0.9, na.rm = TRUE, names = FALSE),
                                          ratio_asking_lower = quantile(ratio_asking, probs = 0.1, na.rm = TRUE, names = FALSE),
                                          ratio_asking_upper = quantile(ratio_asking, probs = 0.9, na.rm = TRUE, names = FALSE)),
                                    by = list(age, manufacturer)]


dt_year_mm_final <- df_ratio_bounds[dt_year_mm_final, on = c("manufacturer", "age")]

# ggplot(data = dt_year_mm_final, aes(age_bucket, color = manufacturer)) +
#   geom_line(aes(y = ratio_auction_lower)) +
#   geom_line(aes(y = ratio_auction_upper))

dt_year_mm_final[ratio_auction < ratio_auction_lower, ratio_auction := ratio_auction_lower]
dt_year_mm_final[ratio_auction > ratio_auction_upper, ratio_auction := ratio_auction_upper]
dt_year_mm_final[ratio_asking < ratio_asking_lower, ratio_asking := ratio_asking_lower]
dt_year_mm_final[ratio_asking > ratio_asking_upper, ratio_asking := ratio_asking_upper]


# quantile(dt_year_mm_final$ratio_auction, probs = seq(0, 1, 0.01), na.rm = TRUE)
# quantile(dt_year_mm_final$ratio_asking, probs = seq(0, 1, 0.01), na.rm = TRUE)

dt_year_mm_final[,c("manufacturer", "r_message", "ratio_auction_lower", "ratio_auction_upper", "ratio_asking_lower", 
                    "ratio_asking_upper") := NULL]

# a <- ratio_data[, .N, make_model][order(-N)]
# n <- 7
# library("ggplot2", "personal")
# ggplot(dt_year_mm_final[make_model == a[n, make_model] & age < 40], aes(age)) +
#  ylab("USD") +
#  ggtitle(str_c(a[n, make_model], " make_model_values by data type")) +
#  geom_line(aes(y = mmv_asking, colour = "Asking")) +
#  geom_line(aes(y = mmv_aftersale, colour = "Aftersale")) +
#  geom_line(aes(y = mmv_auction, colour = "Auction"))

