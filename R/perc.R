perc <- function(value, pop, ages = 0:100){
    ## Assuming that 'pop' is a matrix of population by age and year,
    ## with age in rows and years (or other groups) in columns,
    ## this function calculates the 'value' percentile. 'value' must lie
    ## between 0 and 100. Note that the last age class is open, so very high
    ## percentiles will be inaccurate.

    ## Warning: 'ages' must be in one-year intervals!!
    
    if (length(value) > 1)
      stop("This is not a vectorizing function in 'value'")
    if ((value <= 0) || (value >= 100)) stop("'value' must be in (0, 100)")
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

    per <- numeric(NCOL(pop))
    for (j in 1:NCOL(pop)){
        year <- pop[, j]
        n <- sum(year)
        orr <- value * n / 100
        ##cat("orr = ", orr, "\n")
        su <- 0
        i <- 0
        while ((su < orr) && (i < length(year))){
            i <- i + 1
            su <- su + year[i]
            ##cat("i = ", i, "su = ", su, "\n")
        }
        ##cat("su - orr = ", su - orr, "\n")
        su <- su - year[i]
        per[j] <- ages[i] + (orr - su) / year[i]
    }
    per
}
