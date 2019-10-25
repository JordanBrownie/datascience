source("recipe.r")
library("jsonlite")
#* @get /predict_petal_length
get_predict_length <- function(petal_width){
    # convert the input to a number
    petal_width <- as.numeric(petal_width)
    # create the prediction data frame
    input_data <- data.frame(Petal.Width=as.numeric(petal_width))
    # create the prediction
    predict(model,input_data)
}

#* @post /testing
function(req){
    TESTING <- fromJSON(req$postBody,simplifyVector=TRUE)
    TESTING
}

#* @post /scoring
function(req){
    
listing <- fromJSON(req$postBody,simplifyVector=TRUE)
outputs <- score_models(listing, aftersale_recipe = RetailModel, auction_recipe = AuctionModel)
# listings <- shapi:::GetTestListings(category_id = 1082, customer_owned = FALSE, env = "live") 
# outputs <- score_models(listings, aftersale_recipe = RetailModel, auction_recipe = AuctionModel)

set(x = outputs,
    i = NULL,
    j = "PredictionModelID",
    value = 1)

# results <- rbindlist(outputs, use.names = TRUE, fill = TRUE)
results <- copy(outputs)
OutputDataSet <- results[results[, .I[best_eval(PriorityOrder)], by = c("ReferenceID", "EvaluationType")]$V1]
expected_columns <- str_split(ColumnList, ",\\s?")[[1]]
missing <- setdiff(expected_columns, names(OutputDataSet))
for (k in seq_along(missing)) {
  set(x = OutputDataSet,
      i = NULL,
      j = missing[k],
      value = NA)
}
not_needed <- setdiff(names(OutputDataSet), expected_columns)
for (k in seq_along(not_needed)) {
  set(x = OutputDataSet,
      i = NULL,
      j = not_needed[k],
      value = NULL)
}

OutputDataSet
}