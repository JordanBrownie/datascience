##### custom functions #####
#Function to minimize an rx object
minimize.rx_fast_trees <- function(object, ...) {
  object$call$data<- NULL
  object
}

#Function to minimize an rpart object
minimize.rpart <- function(object, ...) {
  object$xlevels <- attr(object, "xlevels") 
  attr(object$terms, ".Environment") = NULL
  object$cptable <- NULL
  object$y <- NULL
  object$where<- NULL
  object$variable.importance<- NULL
  object$call<- NULL
  object
}

train_rpart <- function(data, formula, rpart_options){
  
  data_1 <- copy(data)
  results <- do.call(rpart, args = c(list(formula = formula, data = data_1), rpart_options))
  results <- minimize.rpart(results)
  
}

train_rx_fast_trees <- function(data, formula, rx_options){
  
  data_1 <- copy(data)
  vars <- all.vars(as.formula(formula))[-1]
  df_factors <- vars[vapply(vars, function(x) is.character(data_1[[x]]), FUN.VALUE = logical(1L))]
  results <- do.call(rxFastTrees, args = c(list(formula = formula, data = data_1, type = "regression", verbose = 0, mlTransforms = categorical(df_factors)), rx_options))
  results <- minimize.rx_fast_trees(results)
  
}

