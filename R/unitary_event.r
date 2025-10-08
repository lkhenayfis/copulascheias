#' Interpreta Evento Unitario
#' 
#' Transforma string `x` em objeto para posterior construcao dos volumes de integracao
#' 
#' @param x string no formato `"lhs (>=|<=) rhs` indicando um evento para inferencia
#' @param modelo modelo com referencia ao qual o evento sera interpretado

parse_unitary_event <- function(x, modelo) {

    vars <- get_event_vars(x)
    validate_vars(vars, modelo)

    lower <- get_lower(x, modelo)
    upper <- get_upper(x, modelo)

    is_multivar <- grepl("(\\+|\\-)", vars)
    if (is_multivar) {
        new_unitary_event_m(lower, upper, vars, modelo)
    } else {
        new_unitary_event_u(lower, upper, vars, modelo)
    }
}

get_event_vars <- function(x) {
    x <- gsub("(\\(|\\))", "", x)
    x <- sub("^[[:digit:]]+(\\.[[:digit:]]*)? ?<= ?", "", x)
    x <- sub(" ?>= ?[[:digit:]]+(\\.[[:digit:]]*)?$", "", x)
    x <- sub(" ?<= ?[[:digit:]]+(\\.[[:digit:]]*)?$", "", x)
    return(x)
}

get_lower <- function(x, modelo) {
    # procura bound da forma b <= x (presente em expressoes do tipo a <= x <= b)
    val <- regmatches(x, regexpr("([[:digit:]]+(\\.[[:digit:]]*)? )(?=(<=))", x, perl = TRUE))
    if (length(val) == 0) {
        # caso nao ache, procura x >= b
        val <- regmatches(x, regexpr("(?<=(>=))( ?[[:digit:]]+(\\.[[:digit:]]*)?)", x, perl = TRUE))
    }
    val <- as.numeric(val)
    if (identical(val, numeric(0))) NA else val
}

get_upper <- function(x, modelo) {
    val <- regmatches(x, regexpr("(?<=(<=))( ?[[:digit:]]+(\\.[[:digit:]]*)?)", x, perl = TRUE))
    val <- as.numeric(val)
    if (identical(val, numeric(0))) NA else val
}

# CONSTRUTORES INTERNOS ----------------------------------------------------------------------------

new_unitary_event_u <- function(lower, upper, var, modelo) {

    ecdf <- attr(modelo, "ecdfs")[[var]]
    lower_u <- ecdf(lower)
    upper_u <- ecdf(upper)

    new <- structure(var, class = "unitary_event_u")
    attr(new, "bounds_x") <- c(lower, upper)
    attr(new, "bounds_u") <- c(lower_u, upper_u)

    return(new)
}

new_unitary_event_m <- function(lower, upper, var, modelo) {
    stop("Eventos unitarios multivariados ainda nao sao suportados")
}

# VALIDACAO ----------------------------------------------------------------------------------------

validate_vars <- function(vars, modelo) {
    vars <- strsplit(vars, " ?\\+ ?")[[1]]
    has_all_vars <- all(vars %in% modelo$vines$names)
    if (!has_all_vars) stop("Algumas variaveis de inferencia nao existem no modelo")
}

# METODOS ------------------------------------------------------------------------------------------

#' Extrai Bounds De Evento Unitario
#' 
#' @param x evento unitario
#' @param ... demais parametros de cada metodo
#' @param mode um de `c("x", "u")`, indicando a escala em que bounds serao retornados

event2bounds <- function(x, ...) UseMethod("event2bounds")

#' @rdname event2bounds

event2bounds.unitary_event_u <- function(x, mode = c("x", "u"), ...) {
    mode <- match.arg(mode)
    mode <- paste0("bounds_", mode)
    bounds <- attr(x, mode)
    if (is.na(bounds[1])) bounds[1] <- 0
    if (is.na(bounds[2])) bounds[2] <- ifelse(mode == "bounds_x", Inf, 1)
    return(bounds)
}

#' @rdname event2bounds

event2bounds.unitary_event_m <- function(x) {
    stop("Eventos unitarios multivariados ainda nao sao suportados")
}
