#' Ajuste De Modelo De Cheias
#' 
#' Estima uma vine copula considerando todos os aspectos de cheia em `data`
#' 
#' O argumento `data` deve ser um `data.table`-like em que cada coluna corresponde a uma variavel
#' da distribuicao multivariada a ser estimada. Os nomes das colunas sao de grande importancia neste
#' pacote pois serao posteriormente utilizadas para interpretacao dos eventos de inferencia (veja
#' [pcopula()]).
#' 
#' @param data `data.table` de dados em que cada coluna representa uma variavel. Veja Detalhes
#' @param vine_params lista de argumentos para estimacao da vine copula, pode ser composta de
#'     qualquer argumento da funcao [VineCopula::RVineStructureSelect()]
#' 
#' @return objeto `modelo_cheias`, uma lista de apenas um elemento `"vines"` contendo o modelo de
#'     `VineCopula` estimado
#' 
#' @examples
#' 
#' # utilizando parte do dado interno do pacote
#' data <- minicheias[, 1:3]
#' mod <- fit_modelo_cheia(data)
#' 
#' @export

fit_modelo_cheia <- function(data, vine_params = default_vine_params()) {
    ecdfs <- lapply(data, ecdf)
    data_u <- mapply(function(v, f) f(v), data, ecdfs)

    cc <- as.call(c(list(quote(RVineStructureSelect), data = quote(data_u)), vine_params))
    vines <- eval(cc)

    new_modelo_cheia(vines, ecdfs)
}

default_vine_params <- function() {
    list(familyset = c(1, 2, 3), rotations = TRUE, selectioncrit = "logLik")
}

new_modelo_cheia <- function(vines, ecdfs, postos, anos) {
    new <- structure(list(vines = vines), class = c("modelo_cheia", "list"))
    attr(new, "ecdfs")  <- ecdfs

    return(new)
}

# METODOS ------------------------------------------------------------------------------------------

#' @export

print.modelo_cheia <- function(x, ...) {
    cat("objeto 'modelo_cheia' com vines: \n\n")
    print(x$vines)
}