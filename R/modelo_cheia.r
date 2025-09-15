
#' Ajuste De Modelo De Cheias
#' 
#' Estima uma vine copula considerando todos os aspectos de cheia em `data`
#' @param data `data.table` de dados de cheias
#' @param postos vetor de postos a serem modelados
#' @param anos vetor indicando quais anos devem ser considerados para o modelo; `NULL` usa todos os
#'     anos disponiveis em `data`
#' @param vine_params lista de argumentos para estimacao da vine copula, pode ser composta de
#'     qualquer argumento da funcao [VineCopula::RVineStructureSelect()]
#' 
#' @return objeto `modelo_cheias`
#' 
#' @export

fit_modelo_cheia <- function(data, postos, anos = NULL, vine_params = default_vine_params()) {
    data <- data[posto %in% postos]
    if (!is.null(anos)) data <- data[ano %in% anos]

    data <- melt(data, id.vars = c("posto", "ano"))
    data <- dcast(data, ano ~ posto + variable)[, -1]

    ecdfs <- lapply(data, ecdf)

    data_u <- mapply(function(v, f) f(v), data, ecdfs)

    cc <- as.call(c(list(quote(VineCopula::RVineStructureSelect), data = quote(data_u)), vine_params))
    vines <- eval(cc)

    new_modelo_cheia(vines, ecdfs, postos, anos)
}

default_vine_params <- function() {
    list(familyset = c(1, 2, 3), rotations = TRUE, selectioncrit = "logLik")
}

new_modelo_cheia <- function(vines, ecdfs, postos, anos) {
    num_postos <- length(postos)
    num_anos   <- length(anos)

    new <- structure(list(vines = vines), class = c("modelo_cheia", "list"))
    attr(new, "ecdfs")  <- ecdfs
    attr(new, "postos") <- postos
    attr(new, "anos") <- anos

    return(new)
}

# METODOS ------------------------------------------------------------------------------------------

#' @export

print.modelo_cheia <- function(x, ...) {
    cat("objeto 'modelo_cheia' com vines: \n\n")
    print(x$vines)
}