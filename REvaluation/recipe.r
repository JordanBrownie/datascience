require(data.table)
require(stringr)
require(jsonlite)

ColumnList = 'ReferenceID,ReferenceTypeID,EvaluationType,PriorityOrder,RequiredFieldsError,InvalidDataError,InsufficientDataError,Evaluation,AcceptableEvalLower,AcceptableEvalUpper,ExceedEvalBound,SpecsUsedinModel,MessageFromR,PredictionModelID'
### serialized_model <- shapi:::GetActivePredictionModel(category_id = 1082, env = "dev", product = "fe")
serialized_model <- readRDS("1082_serialized_model.RDS")
### listing <- shapi:::GetTestListings(1082, FALSE, "dev")
# listing <- data.table(DSLookupID = 150001952226, Condition = "Used", Year = 2017, Manufacturer = "ASHLAND", Model = "2500SS", ModelGroup = "2500", ValidMakeModel = 1, Country = "USA", State = "Washington", CategoryID = 1082)

list2env(unserialize(memDecompress(serialized_model$SerializedModel[[1]], "gzip")), envir = globalenv())
list2env(ExportedObjects, envir = globalenv())

# outputs <- score_models(listing, aftersale_recipe = RetailModel, auction_recipe = AuctionModel)

# set(x = outputs,
#     i = NULL,
#     j = "PredictionModelID",
#     value = 1)

# # results <- rbindlist(outputs, use.names = TRUE, fill = TRUE)
# results <- copy(outputs)
# OutputDataSet <- results[results[, .I[best_eval(PriorityOrder)], by = c("ReferenceID", "EvaluationType")]$V1]
# expected_columns <- str_split(ColumnList, ",\\s?")[[1]]
# missing <- setdiff(expected_columns, names(OutputDataSet))
# for (k in seq_along(missing)) {
#   set(x = OutputDataSet,
#       i = NULL,
#       j = missing[k],
#       value = NA)
# }
# not_needed <- setdiff(names(OutputDataSet), expected_columns)
# for (k in seq_along(not_needed)) {
#   set(x = OutputDataSet,
#       i = NULL,
#       j = not_needed[k],
#       value = NULL)
# }

# OutputDataSet
