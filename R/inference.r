#' Parse De Expressoes De Inferencia
#' 
#' Interpreta uma expressao de inferencia e retorna objeto indicando tipo de integracao necessario
#' 
#' @param expr uma string indicando a inferencia desejada no modelo. Veja Exemplos e Detalhes
#' @param modelo o modelo no qual sera realizada a inferencia

parse_inference <- function(expr, modelo) {

    seps <- split_separadores(expr)

    units <- split_eventos_unitarios(expr)
    units <- lapply(units, parse_unitary_expr, modelo)

    is_square <- all(seps == "&") && all(sapply(units, inherits, "unitary_event_u"))
    if (is_square) {
        new_simple_inference(units)
    } else {
        new_complex_inference()
    }
}

new_simple_inference <- function(unitary_events) {
    new <- structure(unitary_events, class = c("simple_inference", "list"))
    return(new)
}

new_complex_inference <- function(unitary_events, separators) {
    stop("Inferencias complexas (conjuntos nao quadrados) ainda nao suportadas")
}

# HELPERS ------------------------------------------------------------------------------------------

split_eventos_unitarios <- function(expr) {
    trimws(strsplit(expr, "(&|\\|)")[[1]])
}

split_separadores <- function(expr) {
    regmatches(expr, gregexpr("(&|\\|)", expr))[[1]]
}

# METODOS ------------------------------------------------------------------------------------------

simple_inference2bounds <- function(inference, modelo = NULL) {
    lower <- sapply(inference, function(i) attr(i, "bounds_u")[1])
    upper <- sapply(inference, function(i) attr(i, "bounds_u")[2])
    names(lower) <- names(upper) <- unclass(inference)

    if (!is.null(modelo)) {
        model_vars <- modelo$vines$names
        lower <- fillvec(lower, model_vars, 0)
        upper <- fillvec(upper, model_vars, 1)
    }

    bounds <- list(lower, upper)
    return(bounds)
}

fillvec <- function(vec, names, fill = 1) {
    full <- structure(rep(fill, length(names)), names = names)
    full[match(names(vec), names(full))] <- vec
    return(full)
}