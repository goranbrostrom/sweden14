#' Calculates the percentile of an age distribution
#'
#'
#' @param age The quantile.
#' @param pop A matrix of population data.
#' @param below logical, if true the lower tail is calculated.
#' @param ages A vector of ages to consider.
#'
#' @return A (vector of) percentile(s).
#'
#' @examples
#' men <- getPop(sex = "males")
#' prop(65, men, below = FALSE)

prop <- function(age, pop, below = TRUE, ages = 0:100){
    ## Calculates the proportion of the population below or above
    ## the exact age 'age'.

    ## 'pop' is a matrix with ages in rows and years in columns.
    
    age.d <- floor(age)
    age.r <- age - age.d

    if (length(age) > 1)
      stop("This is not a vectorizing function in 'age'")
    if ((age <= 0) || (age >= 100)) stop("'age' must be in (0, 100)")
    ##if (!is.numeric(pop)) stop("'pop' must be numeric") 
    if (!is.matrix(pop)){
        if (is.vector(pop)) {
            pop <- matrix(pop, ncol = 1)
        }else{
            if (is.data.frame(pop)){
                pop <- as.matrix(pop)
            }else stop("'pop' is a not allowed object")
        }
    }

    if (length(ages) != NROW(pop)) stop("'pop' and 'ages' do not match")

    res <- numeric(NCOL(pop))
    for (j in 1:NCOL(pop)){
        year <- pop[, j]
        n <- sum(year)
        if (age.d <= 0.1) n.d <- 0
        else n.d <- sum(year[1:age.d])
        res[j] <- (n.d + age.r * pop[age.d + 1]) / n
    }

    if (below) 100 * res
    else 100 * (1 - res)
}
    
