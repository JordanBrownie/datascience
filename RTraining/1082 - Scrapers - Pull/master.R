options(contrasts = rep("contr.treatment", 2))

# suppressWarnings({
  
  library("shconfig2")
  set_wd()
  load_packages("2017-10-15")
  db_restore_check(category_id = 1082)
  
  set_auction_bounds(1750, 250000)
  set_retail_bounds(2500, 300000)
  
  source("custom_functions.R")

  today <- lubridate::today()
  yesterday <- today-1 
  max_week <- year(today) + (week(today)-1)/52
  max_month <- year(today) + (month(today)-1)/12
  max_year <- year(today)
  
  #### Get Data
  con <- create_server_connection()
  all_data <- get_all_data(con = con, 
                           category_id = 1082, 
                           selections = c(),
                           archive = TRUE,
                           invalid_aftersale = TRUE)
  
  all_data <- add_specs(con = con, 
                        data = all_data, 
                        selections = "sup_engine_rebuilt, sup_transmission_rebuilt") 
  #sup_year, sup_hours, sup_does_not_run, sup_hours_meter_inaccurate, 
  
  region_data <- region_table(con = con, category_id = "1082")
  region_data <- region_data[region %in% c("us east", "us west", "us central", "canada"),]
  
  odbcCloseAll()
  
  continuous_cutoffs <- list(age = c(7.5,14))
  
  all_data <- recipe(all_data, info = NULL) %>%
    add_date() %>%  
    format_character() %>%
    format_numeric() %>% 
    format_yes_no(category_id = 1082) %>% 
    step_replace_na(with = c("sup_")) %>% 
    add_age() %>%
    add_region(region_dt = region_data) %>%
    trained()
  
  if (interactive()) {  
    original_current_asking <- all_data[current_asking()]
  }
  
  all_data <- mm_check(all_data)
  all_data <- rm_duplicates(all_data)
  print(all_data[,.N])

  options(error = function() traceback())
  filtered_data <- recipe(all_data, info = NULL) %>%
    filter_company_id() %>%  
    filter_review_status_id() %>% 
    filter_event_type_id() %>% 
    filter_sale_currency() %>% 
    filter_sale_status() %>% 
    filter_sale_type() %>% 
    filter_exclude_hi_lo_avg() %>% 
    filter_auction_exclusions() %>% 
    filter_aftersale_exclusions() %>% 
    filter_year() %>% 
    filter_country() %>% 
    filter_sale_date() %>% 
    filter(odc_status == "active") %>% 
    filter(condition %in% c("new","used")) %>% 
    filter(usd_sale_price %between% c(1750, 250000) | is.na(usd_sale_price)) %>% 
    filter(usd_list_price %between% c(2500, 300000) | is.na(usd_list_price)) %>% 
    filter(usd_sale_price <= usd_list_price | is.na(usd_sale_price) | is.na(usd_list_price)) %>% 
    step_dt(asking(), usd_full_price := usd_list_price) %>% 
    step_dt(auction() | aftersale(), usd_full_price := usd_sale_price) %>% 
    filter(!is.na(usd_full_price)) %>% 
    filter_sup_conditions() %>% 
    filter(sale_date < today) %>% 
    filter(sale_week <= max_week) %>%
    trained()
  

#   ### hold out data used for testing
# #  set.seed(2231)
# #  filtered_data <- filtered_data[,random_num := ceiling(runif(.N, 0, 10)), by = data_type]
# #  holdout <- copy(filtered_data)[random_num == 7]
# #  saveRDS(holdout, file = "holdout.RDS")
# #  holdout <- readRDS("holdout.RDS")
# #  filtered_data <- filtered_data[!(ref_id %in% holdout$ref_id)]
  
#   feature_data <- recipe(filtered_data, info = NULL) %>%
#     add_continuous_group(cutoffs = continuous_cutoffs) %>% 
#     add_wmqy() %>% 
#     filter(!is.na(region)) %>%
#     trained()
  
#   source("auction_asking_ratios.R")
  
#   training_data <- recipe(ratio_data, info = NULL) %>% 
#     step_roll_join(x = dt_year_mm_final, on = c("make_model", "age"), roll = 'nearest', rollends = c(FALSE,TRUE)) %>%
#     step_dt(auction(), `:=`(usd_full_price_adjusted = usd_full_price/ratio_auction)) %>%
#     step_dt(aftersale(), `:=`(usd_full_price_adjusted = usd_full_price)) %>%
#     step_dt(asking(), `:=`(usd_full_price_adjusted = usd_full_price/ratio_asking)) %>%
#     add_count(by = list(make_model)) %>% 
#     filter(make_model_count >= 5) %>% 
#     trained()

#   final_formula <- paste0("usd_full_price_adjusted~",
#                           "+manufacturer+mmv_aftersale+region")
  
#   final_formula_200 <- paste0("usd_full_price_adjusted~make_model+region")
  
#   models <- training_data %>%
#     purrr::map(.x = c(final_formula, final_formula_200), .f = glm, data = .)
  
#   bad_factor_levels <- check_slope_coefficients(training_data, models[[1]], ratio_bounds = c(-0.15, 0), 
#                                                 n_bounds = c(3, 50))

#   final_training_data <- export_data(data = training_data)
  
#   source("export.R")
  
# })

# if (interactive() && FALSE) {
  
#   ### test individual listings
#   dev <- shapi:::API$new(env = 'dev', api = 'EquipmentEvaluation')
#   dev_1 <- shapi:::as.data.table.InventoryEvaluation_GetEvaluation(dev$methods$InventoryEvaluation_GetEvaluation(150001952226,
#                                                                                                                  "DSInventoryLookupID",
#                                                                                                      IncludeInvalidEvaluations = TRUE))
#   dev_res_1 <- score_models(dev_1, aftersale_recipe = fe_recipe, auction_recipe = NULL, strict = FALSE) 
#   dev_res_1
  
#   stg <- shapi:::API$new(env = 'stg', api = 'EquipmentEvaluation')
#   stg_1 <- shapi:::as.data.table.InventoryEvaluation_GetEvaluation(stg$methods$InventoryEvaluation_GetEvaluation(150002569353,
#                                                                                                                  "DSInventoryLookupID",
#                                                                                                      IncludeInvalidEvaluations = TRUE))
#   stg_res_1 <- score_models(stg_1, aftersale_recipe = fe_recipe, auction_recipe = NULL, strict = FALSE) 
#   stg_res_1
  
#   live <- shapi:::API$new(env = 'live', api = 'EquipmentEvaluation')
#   live_1 <- shapi:::as.data.table.InventoryEvaluation_GetEvaluation(live$methods$InventoryEvaluation_GetEvaluation(150013999964,
#                                                                                                                    "DSInventoryLookupID",
#                                                                                                        IncludeInvalidEvaluations = TRUE))
#   live_res_1 <- score_models(live_1, aftersale_recipe = fe_recipe, auction_recipe = NULL, strict = FALSE) 
#   live_res_1
  
#   ### custom test
#   #    test <- setDT(data.frame(Year = 2016, Model = "313fl", Manufacturer = "caterpillar", State = NA, Country = "united kingdom", 
#   #    Hours = 5275, Thumb = "no", HydrAux = "yes", QuickAttach = "yes", Condition = "used"))
#   #    results <-suppressWarnings(score_models(test, auction_recipe = NULL, aftersale_recipe = scoring_one, strict = TRUE))
  
#   ### Holdout Testing and Formatting
#   holdout <- holdout[!is.na(make_model) & !is.na(region)][
#     auction() | aftersale() | (asking() & historical_asking)]
  
#   holdout_test <- score_models(data = holdout, aftersale_recipe = fe_recipe, auction_recipe = NULL, strict = FALSE)
#   #  holdout_test_old <- copy(holdout_test)[data_type == EvaluationType | (data_type == "aftersale" & EvaluationType == "retail")
#   #                                       ][auction() | aftersale(), price_col := USDSalePrice
#   #                                         ][asking(), price_col := USDListPrice
#   #                                           ][PriorityOrder == 200, ':='(Evaluation = NA, PriorityOrder = NA)
#   #                                             ][, old_eval := Evaluation][]
#   holdout_test_new <- copy(holdout_test)[data_type == EvaluationType | (data_type == "aftersale" & EvaluationType == "retail")
#                                          ][auction() | aftersale(), price_col := USDSalePrice
#                                            ][asking(), price_col := USDListPrice
#                                              ][PriorityOrder >= 200, ':='(Evaluation = NA, PriorityOrder = NA)
#                                                ][, new_eval := Evaluation][]
#   #  saveRDS(holdout_test_old, file = "holdout_data_old.RDS")
  
#   #  holdout_test_old <- readRDS("holdout_data_old.RDS")
#   dfAuctionHoldout <- readRDS("old_auction_holdout.RDS")
#   dfRetailHoldout <- readRDS("old_retail_holdout.RDS")
  
#   old_auction <- copy(setDT(dfAuctionHoldout))[,ReferenceID := ObjectHistoryID
#                                                ][,EvaluationType := "auction"
#                                                  ][,c("ObjectHistoryID", "Link", "Auction") := NULL]
#   old_retail <- copy(setDT(dfRetailHoldout))[,ReferenceID := PostSaleID
#                                              ][,EvaluationType := "retail"
#                                                ][,c("PostSaleID", "Retail") := NULL]
#   holdout_test_old <- rbind(old_auction, old_retail, fill = TRUE)[,':='(old_eval = Pred)]
  
#   holdout_test_2 <- merge(holdout_test_new, holdout_test_old[,c("ReferenceID", "EvaluationType", "old_eval")], 
#                           by = c("ReferenceID", "EvaluationType"), all = TRUE)
#   holdout_test_3 <- holdout_test_2[((auction() | aftersale()) & !is.na(USDSalePrice) & !is.na(new_eval) & !is.na(old_eval)) |
#                                      (asking() & !is.na(USDListPrice) & !is.na(new_eval))][]
  
#   #  holdout_test_2 <- merge(holdout_test_new, holdout_test_old[,c("ReferenceID", "data_type", "old_eval")], 
#   #                          by = c("ReferenceID", "data_type"), all = TRUE)
#   #  holdout_test_3 <- holdout_test_2[((auction() | aftersale()) & !is.na(USDSalePrice) & !is.na(new_eval) & !is.na(old_eval)) |
#   #                                     (asking() & !is.na(USDListPrice) & !is.na(new_eval) & !is.na(old_eval))][]
  
#   overall_old_results <- metrics(holdout_test_old, pred = "old_eval", value = "SoldPrice", by = list(EvaluationType))
#   #  overall_old_results <- metrics(holdout_test_old, pred = "old_eval", value = "price_col", by = list(data_type))
#   overall_new_results <- metrics(holdout_test_new, pred = "new_eval", value = "price_col", by = list(data_type))
#   compare_old_results <- metrics(holdout_test_3, pred = "old_eval", value = "price_col", by = list(data_type))
#   compare_new_results <- metrics(holdout_test_3, pred = "new_eval", value = "price_col", by = list(data_type))
  
#   overall_old_results <- dcast(melt(overall_old_results, id.vars = "EvaluationType"), variable ~ EvaluationType)
#   overall_new_results <- dcast(melt(overall_new_results, id.vars = "data_type"), variable ~ data_type)
#   compare_old_results <- dcast(melt(compare_old_results, id.vars = "data_type"), variable ~ data_type)
#   compare_new_results <- dcast(melt(compare_new_results, id.vars = "data_type"), variable ~ data_type)
  
#   shtrain:::shdata$dependencies
#   att_data
  
#   coefficient_check <- setDT(data.frame(coefficient_name = names(coef(models[[1]])), coefficient = coef(models[[1]])))
  
#   # score current asking
#   asking_score <- unique(original_current_asking[,eval_type := NULL])
#   asking <- score_models(asking_score, auction_recipe = NULL, aftersale_recipe = fe_recipe, strict = FALSE)
#   names(asking) <- change_names(names(asking), from = "sql")
  
#   asking_auction <- copy(asking)[eval_type == "auction", c("ref_id", "evaluation"), with = FALSE]
#   asking_aftersale <- copy(asking)[eval_type == "retail", c("ref_id", "evaluation"), with = FALSE]
#   asking_asking <- copy(asking)[eval_type == "asking"][,evaluation_asking := evaluation][,evaluation := NULL]
#   all_asking <- merge(asking_aftersale, asking_auction, by = "ref_id", all.x = TRUE, suffixes = c("_retail", "_auction"))
#   all_asking <- merge(asking_asking, all_asking, by = "ref_id", all.x = TRUE)
  
#   all_asking[evaluation_auction < floor_price("auction"), "evaluation_auction"] <- floor_price("auction")
#   all_asking[evaluation_retail < floor_price("retail"), "evaluation_retail"] <- floor_price("retail")
#   all_asking[evaluation_asking < floor_price("retail"), "evaluation_asking"] <- floor_price("retail")
#   all_asking[priority_order != 1, c("evaluation_auction", "evaluation_retail", "evaluation_asking") := NA]
  
#   all_asking[,c("auction > retail", "auction > asking", "auction > list", "retail > asking", "retail > list", "asking > list") := 0]
#   all_asking[priority_order == 1 & evaluation_auction >= evaluation_retail, ("auction > retail") := 1]
#   all_asking[priority_order == 1 & evaluation_auction >= evaluation_asking, ("auction > asking") := 1]
#   all_asking[priority_order == 1 & evaluation_retail >= evaluation_asking, ("retail > asking") := 1]
#   all_asking[priority_order == 1 & !is.na(usd_list_price) & evaluation_auction >= usd_list_price, ("auction > list") := 1]
#   all_asking[priority_order == 1 & !is.na(usd_list_price) & evaluation_retail >= usd_list_price, ("retail > list") := 1]
#   all_asking[priority_order == 1 & !is.na(usd_list_price) & evaluation_asking >= usd_list_price, ("asking > list") := 1]
  
#   auc_ret <- sum(all_asking[,"auction > retail"]) / all_asking[!is.na(evaluation_auction) & !is.na(evaluation_retail), .N]
#   auc_ask <- sum(all_asking[,"auction > asking"]) / all_asking[!is.na(evaluation_auction) & !is.na(evaluation_asking), .N]
#   ret_ask <- sum(all_asking[,"retail > asking"]) / all_asking[!is.na(evaluation_retail) & !is.na(evaluation_asking), .N]
#   auc_lp <- sum(all_asking[,"auction > list"]) / all_asking[!is.na(evaluation_auction) & !is.na(usd_list_price), .N]
#   ret_lp <- sum(all_asking[,"retail > list"]) / all_asking[!is.na(evaluation_retail) & !is.na(usd_list_price), .N]
#   ask_lp <- sum(all_asking[,"asking > list"]) / all_asking[!is.na(evaluation_asking) & !is.na(usd_list_price), .N]
  
#   asking_metrics <- setDT(data.frame(auc_ret, auc_ask, ret_ask, auc_lp, ret_lp, ask_lp))
#   setnames(asking_metrics, 
#            old = names(asking_metrics), 
#            new = c("auction > retail", "auction > asking", "retail > asking", "auction > list", "retail > list", "asking > list"))
  
#   base::library(openxlsx)
#   wb <- createWorkbook()
#   addWorksheet(wb, "summary")
#   addWorksheet(wb, "current_asking")
#   addWorksheet(wb, "auction_holdout")
#   addWorksheet(wb, "aftersale_holdout")
#   addWorksheet(wb, "asking_holdout")
#   addWorksheet(wb, "coef")
#   writeData(wb, "summary", compare_new_results, xy = c(2, 3))
#   writeData(wb, "summary", compare_old_results, xy = c(7, 3))
#   writeData(wb, "summary", overall_new_results, xy = c(2, 15))
#   writeData(wb, "summary", overall_old_results, xy = c(7, 15))
#   writeData(wb, "summary", asking_metrics, xy = c(12, 3))
#   writeDataTable(wb, "current_asking", all_asking, tableName = "current_asking")
#   writeDataTable(wb, "auction_holdout", holdout_test_3[auction()], tableName = "auction_holdout")
#   writeDataTable(wb, "aftersale_holdout", holdout_test_3[aftersale()], tableName ="aftersale_holdout")
#   writeDataTable(wb, "asking_holdout", holdout_test_3[asking()], tableName ="asking_holdout")
#   writeDataTable(wb, "coef", coefficient_check, tableName ="coef")
#   saveWorkbook(wb, "1082_pull_scrapers_report_comparison.xlsx", overwrite = TRUE)
  
# }
