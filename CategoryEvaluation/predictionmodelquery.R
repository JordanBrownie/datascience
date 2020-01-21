library(stringr)
library(httr)
library(glue)
library(jsonlite)
library(odbc)
library(data.table)


cat_id <- Sys.getenv("cat_id")
env <-  Sys.getenv("env")
product <- Sys.getenv("product")
status <-  Sys.getenv("status")
max_models <-  Sys.getenv("max_models")

if (env == "staging") {
  env <- "stg"
}
print(product)
if (product == "fe") {
  historical <- FALSE
} else if (product == "bm") {
  historical == TRUE
} else {
  stop("'product' must be either 'fe' or 'bm'.")
}

api_name <- "EquipmentEvaluation"
api_key <- "B45DE5615B1AE7B6172D85BF2C3B91CD"
base_url <- switch(env,
                   "dev" = "http://devApiDS.dsdev.ext",
                   "stg" = "http://stgApiDS.dsstag.ext",
                   "live" = "http://ApiDS.DealerServices.dmz")
api_url <- stringr::str_c(base_url, api_name, api_key, sep = "/")

response1 <-
  httr::POST(
    url = stringr::str_c(api_url, "/api/PredictionModel/Search"),
    body = stringr::str_c(
      "{'SearchProperties':{},",
      "'FilterList':[{'Field':'CategoryID','Operator':'Equals','Argument':",
      cat_id,
      "},{'Field':'IsHistoricalModel','Operator':'Equals','Argument':",
      tolower(historical),
      "},{'Field':'Status','Operator':'Equals','Argument':'",
      tolower(status),
      "'}],'OrderByList':[],'AndOr':'And','CheckSum':null,'Message':null}"
    ),
    httr::content_type_json()
  )

if (httr::status_code(response1) != 200) {
  stop(glue::glue("API method in returned an unsuccessful HTTP status."), call. = FALSE)
}

model_info <- jsonlite::fromJSON(httr::content(response1, "text"))
model_ids <- model_info$PredictionModelID
model_ids_sql <- stringr::str_c("(", glue::collapse(glue::glue("'{model_ids}'"), sep = ", "), ")")

con <- odbc::dbConnect(
  odbc::odbc(),
  .connection_string = stringr::str_c(
    "driver={ODBC Driver 17 for SQL Server};",
    "database=dbEquipmentEvaluationModel;",
    "server=AGLIDSEVALSTG1.dsstag.ext;",
    " integrated security=SSPI;",
    "persist security info=False;",
    "Trusted_Connection=Yes;",
    "MultiSubnetFailover=Yes;"
  )
)

# models <- data.table::setDT(odbc::dbGetQuery(con,
#                                  glue::glue("SELECT TOP {max_models} CreatedOnUTCDateTime, SerializedModel from dbEquipmentEvaluationModel.dbo.PredictionModel \\
#                                       where PredictionModelID IN {model_ids_sql} Order by CreatedOnUTCDateTime DESC")))

con <- odbc::dbConnect(
  odbc::odbc(),
  .connection_string = stringr::str_c(
    "driver={ODBC Driver 17 for SQL Server};",
    "database=dbDataScientist;",
    "server=sisqldwreadstg.sandhills.int;",
    "uid=RUser;",
    " pwd=Jg:DXL2xgUvc@+Pjz4N!;"
  )
)

models <- data.table::setDT(odbc::dbGetQuery(con,
                                 glue::glue("SELECT TOP {max_models} [ClientID],[ClientName],[Status],[CreatedOnUTCDateTime],[ModifiedOnUTCDateTime] from [dbDataScientist].[Reporting].[Client] \\
                                      where Status IN ('ACTIVE') Order by CreatedOnUTCDateTime DESC")))

print(models)
odbc::dbDisconnect(con)
