library(data.table)
minicheias <- fread("data-raw/cheias.csv")
minicheias <- minicheias[posto %in% c("ernestina", "barra_grande")]
minicheias <- melt(minicheias, id.vars = c("posto", "ano"))
minicheias <- dcast(minicheias, ano ~ posto + variable, value.var = "value")
minicheias <- minicheias[, -1]

usethis::use_data(minicheias)
