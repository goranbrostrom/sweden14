#' Extract the net in-migration in Sweden by age, year, and sex.
#'
#' @param sex "all", "males", or "females"
#' @param years years to include, numeric vector.
#' @param ages ages to include, numeric vector.
#' @param aggrYear logical, if TRUE aggregate ove years.
#' @param aggrAge logical, if TRUE aggregate over ages.
#'
#' @return A matrix, a vector, or a real number, depending on aggregate level.
#'
#' @examples
#' getInmig()
#' getInmig(sex = "males", years = 1969:1978, aggrYear = TRUE)

getInmig <- function(sex = c("all", "males", "females"),
                   years = 1969:2014,
                   ages = 0:100,
                   aggrYear = FALSE,
                   aggrAge = FALSE){
    sex <- sex[1]
    if (!(sex %in% c("all", "males", "females"))){
        stop("Argument 'sex' has wrong value")
    }

    inmig <- sweden14::inmig

    if (sex == "females") {
        res <- inmig$females
    }else{
        if (sex == "males"){
            res <- inmig$males
        }else{
            res <- inmig$males + inmig$females
        }
    }
    ## Cut out the age/year selection
    res <- res[rownames(res) %in% ages,
               colnames(res) %in% years]
    if (aggrYear){
        if (is.matrix(res)){
            res <- rowSums(res)
        }else{
            res <- sum(res)
        }
        if (aggrAge){
            res <- sum(res) # Just a number!
        }
    }else if (aggrAge){
        if (is.matrix(res)){
            res <- colSums(res)
        }else{
            res <- sum(res)
        }
    }

    res
}

