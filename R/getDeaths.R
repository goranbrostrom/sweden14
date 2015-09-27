#' Extract the number of deaths in Sweden by age, year, and sex.
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
#' getDeaths()
#' getDeaths(sex = "males", years = 1969:1978, aggrYear = TRUE)

getDeaths <- function(sex = c("all", "males", "females"),
                      years = 1969:2014,
                      ages = 0:100,
                      aggrYear = FALSE,
                      aggrAge = FALSE){
    sex <- sex[1]
    if (!(sex %in% c("all", "males", "females"))){
        stop("Argument 'sex' has wrong value")
    }

    ##data("deaths")
    load("../data/deaths.rda") # Temporary!!

    if (sex == "females") {
        res <- deaths$females
    }else{
        if (sex == "males"){
            res <- deaths$females
        }else{
            res <- deaths$males + deaths$females
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

