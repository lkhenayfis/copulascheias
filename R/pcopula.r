#' Inferencia Em Modelos De Copula
#' 
#' Calcula a probabilidade de um evento `expr` de acordo com a copula estimada `modelo`
#' 
#' @param expr string descrevendo o evento cuja probabilidade deve ser calculada
#' @param modelo modelo de copula como retornado por [`fit_modelo_cheia()`]
#' 
#' @export

pcopula <- function(expr, modelo) {
    inference <- parse_inference(expr, modelo)
    run_inference(inference, modelo)
}

# INTERNAS -----------------------------------------------------------------------------------------

#' Executa Inferencia
#' 
#' Funcao interna para integracao de PDfs estimadas
#' 
#' @param inference objeto inferenca

run_inference <- function(inference, modelo) UseMethod("run_inference", inference)

run_inference.simple_inference <- function(inference, modelo, nsims = 2e6, threads = 1) {
    bounds <- simple_inference2bounds(inference, modelo)

    vars <- modelo$vines$names
    sim  <- sapply(vars, function(var) runif(nsims, bounds[[1]][var], bounds[[2]][var]))
    norm <- prod(sapply(vars, function(var) bounds[[2]][var] - bounds[[1]][var]))

    prob <- norm * mean(RVinePDF(sim, modelo$vines))
    return(prob)
}
