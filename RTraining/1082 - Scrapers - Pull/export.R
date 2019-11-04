cache_dependency(source = "make_model", derived = c("mmv_aftersale"))

fe_info <- gather_info(final_training_data, models)
included_but_not_required <- c("year")
fe_info$used_vars <- union(fe_info$used_vars, included_but_not_required)

fe_recipe <- recipe(final_training_data, info = fe_info) %>%
  step_shref() %>%
  step_name(from = "sql") %>%
  format_class() %>%
  format_character() %>%
  add_region(region_dt = region_data) %>%
  add_date() %>%
  add_make_model() %>%
  step_replace_na(with = c("sup_")) %>%
  add_age() %>%
  add_continuous_group(continuous_cutoffs) %>%
  step_roll_join(x = dt_year_mm_final, on = c("make_model","age"), roll = 'nearest',
                 rollends = c(FALSE,TRUE)) %>%
  add_evaluations(type = "evaluation_aftersale", models, priorities = c(1, 200)) %>%
  step_dt(str_detect(make_model, "mobile track solutions me12"), `:=` (evaluation_aftersale = NA_real_, priority_order = NA_real_,
                                                             skip_checks = TRUE)) %>% 
  add_ratio_evaluations() %>%
  check_requirements(year > 1950 & age > 1, priority_order = c(1)) %>%
  check_required_fields() %>%
  check_country(allowed = c("usa", "canada")) %>%
  check_state() %>%
  check_insufficient_data(bad_levels = bad_factor_levels) %>%
  check_condition() %>%
  step_bound_evaluations(bounds = c(retail_floor(), retail_ceiling()), percent = 50, amount = 30000) %>%
  add_json() %>%
  step_name(from = "r") %>% 
  prep()

captured <- copy_package()
att_data <- attr_data(final_training_data, fe_info)

sql_object <- fleet_evaluator_model(category_id = 1082, recipe = fe_recipe, exports = captured, attributes = att_data)

post_recipe(sql_object)
