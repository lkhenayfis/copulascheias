#' Inferencia Em Modelos De Copula
#' 
#' Calcula a probabilidade de um evento `expr` de acordo com a copula estimada `modelo`
#' 
#' `expr` e o principal argumento desta funcao. Atraves dele sao informadas expressoes simbolicas
#' que qualificam um evento cuja probabilidade sera calculada. Ha duas partes destas expressoes.
#' 
#' Primeiro, devem ser descritos os eventos unitarios. Estes sao da forma `"var <= b"`, em que
#' `"var"` e uma das variaveis representadas em `modelo` e `b` um valor de bound. Sao suportadas
#' expressoes das formas `"var <= b"`, `"var >= b"` e `"a <= var <= b"`.
#' 
#' Eventos unitarios podem ser combinados por intersecao, utilizando os simbolos `&` e `|`
#' respectivamente. Por exemplo, uma expressao de dois eventos seria "(var1 <= b1) & (var2 >= b2)".
#' Nao ha qualquer limite no numero de eventos.
#' 
#' @param expr string descrevendo o evento cuja probabilidade deve ser calculada. Veja Detalhes
#' @param modelo modelo de copula como retornado por [`fit_modelo_cheia()`]
#' 
#' @return escalar indicando a probabilidade do evento `expr`
#' 
#' @examples 
#' 
#' # utilizando parte do dado interno do pacote
#' data <- minicheias[, 4:6]
#' mod <- fit_modelo_cheia(data)
#' 
#' # evento simples, considerando apenas uma variavel
#' pcopula("ernestina_pico >= 500", mod)
#' 
#' # considerando agora duas
#' pcopula("(ernestina_pico >= 500) & (ernestina_volume >= 150)", mod)
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
#' @param modelo modelo ajustado no qual computar inferencia
#' @param nsims numero de amostras de Monte Carlo

run_inference <- function(inference, modelo, nsims) UseMethod("run_inference", inference)

#' @rdname run_inference

run_inference.simple_inference <- function(inference, modelo, nsims = 2e6) {
    bounds <- simple_inference2bounds(inference, modelo)

    vars <- modelo$vines$names
    sim  <- sapply(vars, function(var) runif(nsims, bounds[[1]][var], bounds[[2]][var]))
    norm <- prod(sapply(vars, function(var) bounds[[2]][var] - bounds[[1]][var]))

    prob <- norm * mean(RVinePDF(sim, modelo$vines))
    return(prob)
}
