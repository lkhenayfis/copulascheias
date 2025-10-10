#' Parse De Expressoes De Inferencia
#' 
#' Interpreta uma expressao de inferencia e retorna objeto indicando tipo de integracao necessario
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
#' @param expr uma string indicando a inferencia desejada no modelo. Veja Detalhes
#' @param modelo o modelo no qual sera realizada a inferencia
#' 
#' @return objeto `simple_inference` ou `complex_inference`

parse_inference <- function(expr, modelo) {

    seps <- split_separadores(expr)

    units <- split_eventos_unitarios(expr)
    units <- lapply(units, parse_unitary_event, modelo)

    only_intersect <- all(seps == "&")
    all_univariate <- all(sapply(units, inherits, "unitary_event_u"))

    if (only_intersect && all_univariate) {
        new_simple_inference(units)
    } else if (only_intersect) {
        new_complex_inference(units)
    }
}

#' Construtor Interno De Objetos De Inferencia
#' 
#' Gera um objeto para inferencia, uma lista de eventos unitarios interpretados
#' 
#' @param unitary_events lista de eventos unitarios ja interpretados por [parse_unitary_event()]
#' 
#' @return argumento `unitary_events` com classe adicionada `simple_inference`
#' 
#' @name inference_constructors
NULL

#' @rdname inference_constructors

new_simple_inference <- function(unitary_events) {
    new <- structure(unitary_events, class = c("simple_inference", "list"))
    return(new)
}

#' @rdname inference_constructors

new_complex_inference <- function(unitary_events) {
    new <- structure(unitary_events, class = c("complex_inference", "list"))
    return(new)
}

new_void_inference <- function(unitary_events) {
    new <- structure(unitary_events, class = c("void_inference", "list"))
    return(new)
}

simplify_complex_inference <- function(inference) {
    univars <- Filter(function(e) inherits(e, "unitary_event_u"), inference)
    if (length(univars) == 0) {
        new_void_inference(univars)
    } else {
        new_simple_inference(univars)
    }
}

# HELPERS ------------------------------------------------------------------------------------------

#' Separadores De Trechos De Expressoes
#' 
#' Separam uma expressao de inferencia entre expressoes de eventos unitarios e operadores de combinacao
#' 
#' Ambas as funcoes recebem uma expressao do tipo `"(var1 <= b1) & (var2 >= b2) ..."`.
#' `split_eventos_unitarios` retorna um vetor `c("var1", "var2", ...)` enquanto `split_separadores`
#' retorna os operadores `c("&, ...")`. O numero de operadores e sempre o numero de eventos menos 1.
#' 
#' @param expr uma string de evento para inferencia. Veja [parse_inference()]
#' 
#' @return vetor de variaveis de evento unitario ou operadores de combinacao. Veja Detalhes

split_eventos_unitarios <- function(expr) {
    trimws(strsplit(expr, "(&|\\|)")[[1]])
}

#' @rdname split_eventos_unitarios

split_separadores <- function(expr) {
    regmatches(expr, gregexpr("(&|\\|)", expr))[[1]]
}

# METODOS ------------------------------------------------------------------------------------------

#' Extrai Limites De Integracao Retangulares
#' 
#' Gera lista de dois vetores: lower e upper bounds de integracao
#' 
#' O comportamento desta funcao depende do tipo de `inference`. No caso de `simple_inference`, a
#' funcao simplesmente retorna os bounds de cada evento unitario, preenchendo com 0 e 1 para
#' as variaveis que nao aparecem em `inference`.
#' 
#' No caso de `complex_inference`, a funcao identifica a bounding box que contem o evento complexo e
#' retorna os limites desta caixa.
#' 
#' E recomendado sempre passar este argumento, do contrario erros podem decorrer.
#' 
#' @param inference um objeto do tipo `simple_inference`
#' @param modelo modelo ajustado por [fit_modelo_cheia()] para identificacao das variaveis que nao
#'     aparecem em `inference`
#' 
#' @return lista de dois vetores: lower e upper bounds de integracao

inference2bounds <- function(inference, modelo, ...) UseMethod("inference2bounds", inference)

inference2bounds.simple_inference <- function(inference, modelo, mode = "u", ...) {
    bounds <- lapply(inference, function(i) event2bounds(i, mode = mode))
    lower <- sapply(bounds, function(b) b[1])
    upper <- sapply(bounds, function(b) b[2])
    names(lower) <- names(upper) <- unclass(inference)

    upper <- upper[!is.infinite(upper)]
    model_vars <- modelo$vines$names

    lower <- fillvec(lower, model_vars, 0)
    upper <- fillvec(upper, model_vars,
        if (mode == "u") 1 else sapply(attr(modelo, "inv_ecdf"), function(f) f(1)))

    bounds <- list(lower, upper)
    return(bounds)
}

inference2bounds.complex_inference <- function(inference, modelo, ...) {
    box_only <- simplify_complex_inference(inference)
    box_bounds <- inference2bounds(box_only, modelo, mode = "x")

    inference <- Filter(function(e) inherits(e, "unitary_event_m"), inference)
    list_g <- lapply(inference, function(i) event2function(i))
    list_g <- unlist(list_g)

    vars <- modelo$vines$names
    bounding_box <- lapply(vars, min_max_single_var, ineqs = list_g, bounds = box_bounds, allvars = vars)
    names(bounding_box) <- vars

    bounding_box <- lapply(seq_len(2), function(i) sapply(bounding_box, function(x) x[i]))
    return(bounding_box)
}

inference2bounds.void_inference <- function(inference, modelo, mode = "u", ...) {
    model_vars <- modelo$vines$names
    lower <- structure(rep(0, length(model_vars)), names = model_vars)
    if (mode == "u") {
        upper <- structure(rep(1, length(model_vars)), names = model_vars)
    } else {
        upper <- structure(sapply(attr(modelo, "inv_ecdf"), function(f) f(1)), names = model_vars)
    }

    bounds <- list(lower, upper)
    return(bounds)
}

#' Preenche Vetor Com Valores Padrao
#' 
#' Preenche `vec` com valores `fill` para todas as posicoes em `names` que nao existam em `vec`
#' 
#' @param vec vetor nomeado a ser preenchido
#' @param names vetor de nomes que `vec` deve ter
#' @param fill valor com o qual preencher posicoes faltantes
#' 
#' @return vetor `vec` preenchido com `fill` para todas as posicoes em `names` faltantes em `vec`

fillvec <- function(vec, names, fill = 1) {
    full <- structure(rep(fill, length.out = length(names)), names = names)
    full[match(names(vec), names(full))] <- vec
    return(full)
}

min_max_single_var <- function(var, ineqs, bounds, allvars) {
    start <- structure((bounds[[1]] + bounds[[2]]) / 2, names = allvars)
    eval_g <- function(x) run_ineqs(x, allvars, ineqs)

    ind <- which(allvars == var)

    obj <- function(x) x[ind]
    min <- nloptr::nloptr(start, obj,
        lb = bounds[[1]], ub = bounds[[2]], eval_g_ineq = eval_g,
        opts = list(algorithm = "NLOPT_LN_COBYLA", maxeval = 5e3))
    min <- min$solution[ind]

    obj <- function(x) -x[ind]
    max <- nloptr::nloptr(start, obj,
        lb = bounds[[1]], ub = bounds[[2]], eval_g_ineq = eval_g,
        opts = list(algorithm = "NLOPT_LN_COBYLA", maxeval = 5e3))
    max <- max$solution[ind]

    out <- c(min, max)
    return(out)
}

run_ineqs <- function(x, x_names, ineqs) {
    names(x) <- x_names
    sapply(ineqs, function(f) {
        cc <- c(list(f), as.list(x))
        eval(as.call(cc), parent.frame())
    })
}

run_deriv_ineqs <- function(x, ineqs) {
    sapply(ineqs, function(f) {
        cc <- c(list(f), as.list(x))
        eval(as.call(cc), parent.frame())
    })
}

remove_dots <- function(fun) {
    formals(fun) <- formals(fun)[-length(formals(fun))]
    fun
}