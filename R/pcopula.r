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
#' @param nsims numero de amostras de Monte Carlo
#' @param modo um de `c("default", "lhs")` indicando se a simulacao de Monte Carlo deve ser feita
#'     por amostragem comum ou hipercubo latino
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
#' pcopula("ernestina_pico >= 500", mod, nsims = 1000)
#' 
#' # considerando agora duas
#' pcopula("(ernestina_pico >= 500) & (ernestina_volume >= 150)", mod, nsims = 1000)
#' 
#' @export

pcopula <- function(expr, modelo, nsims = 2e6, modo = c("default", "lhs")) {
    modo <- match.arg(modo)
    if ((modo == "lhs") && !requireNamespace("lhs", quietly = TRUE)) {
        stop("Simulacao por hipercubo latino precisa do pacote 'lhs'")
    }
    inference <- parse_inference(expr, modelo)
    run_inference(inference, modelo, nsims, modo)
}

# INTERNAS -----------------------------------------------------------------------------------------

#' Executa Inferencia
#' 
#' Funcao interna para integracao de PDfs estimadas
#' 
#' @param inference objeto inferenca
#' @param modelo modelo ajustado no qual computar inferencia
#' @param nsims numero de amostras de Monte Carlo
#' @param modo um de `c("default", "lhs")` indicando se a simulacao de Monte Carlo deve ser feita
#'     por amostragem comum ou hipercubo latino

run_inference <- function(inference, modelo, nsims, modo) UseMethod("run_inference", inference)

#' @rdname run_inference

run_inference.simple_inference <- function(inference, modelo, nsims, modo) {
    bounds <- inference2bounds(inference, modelo)

    vars <- modelo$vines$names
    norm <- prod(sapply(vars, function(var) bounds[[2]][var] - bounds[[1]][var]))

    sim_fun <- ifelse(modo == "default", rmunif, rmunifLHS)
    sim <- sim_fun(nsims, length(vars), bounds[[1]], bounds[[2]])

    prob <- norm * mean(RVinePDF(sim, modelo$vines))
    return(prob)
}

#' @rdname run_inference

run_inference.complex_inference <- function(inference, modelo, nsims, modo) {
    ecdfs <- attr(modelo, "ecdfs")
    inv_ecdfs <- attr(modelo, "inv_ecdfs")

    bounds_x <- inference2bounds(inference, modelo)
    bounds_u <- lapply(bounds_x, function(v) mapply(FUN = function(f, x) f(x), ecdfs, v))

    vars <- modelo$vines$names
    norm <- prod(sapply(vars, function(var) bounds_u[[2]][var] - bounds_u[[1]][var]))

    sim_fun <- ifelse(modo == "default", rmunif, rmunifLHS)
    h <- build_h_function(inference, modelo)

    sim_u <- sim_fun(nsims, length(vars), bounds_u[[1]], bounds_u[[2]])
    sim_x <- mapply(inv_ecdfs, seq_len(ncol(sim_u)), FUN = function(f, i) f(sim_u[, i]), SIMPLIFY = FALSE)
    belong <- c(h(sim_x))

    prob <- norm * mean(belong * RVinePDF(sim_u, modelo$vines))

    return(prob)
}

build_h_function <- function(inference, modelo) {
    inference <- Filter(function(e) inherits(e, "unitary_event_m"), inference)
    list_g <- lapply(inference, function(i) event2function(i))
    list_g <- unlist(list_g)
    h <- function(x) run_ineqs(x, modelo$vines$names, list_g) <= 0
    return(h)
}

rmunif <- function(n, d, min, max) {
    sapply(seq_len(d), function(i) runif(n, min[i], max[i]))
}

rmunifLHS <- function(n, d, min, max) {
    sim_lhs <- lhs::randomLHS(n, d)
    sapply(seq_len(d), function(i) qunif(sim_lhs[, i], min[i], max[i]))
}
