get.inmig <- function(sex = c("all", "males", "females"),
                      years = c(1997, 2007)){
    sex <- sex[1]
    if (!(sex %in% c("all", "males", "females")))
      stop("Parameter 'sex' has wrong value")
    if (length(years) == 1) years <- c(years, years)
    if (length(years) == 2){
        years <- c(min(years), max(years))
        if (years[1] < 1997) years[1] <- 1997
        if (years[2] > 2007) years[2] <- 2007
        start <- years[1] - 1997 + 1
        slut <- years[2] - 1997 + 1
    }else{
        stop ("years has wrong length (should be 1 or 2)")
    }
    ## inmig9707 <- read.csv("inmig9707.csv", sep = ",", header = TRUE)
    ## inmig9707 <- inmig9707[, -c(1, 2)]
    ##return(inmig9707)
    ## for (x in names(inmig9707)) inmig9707[, x] <-
      ## as.numeric(as.character(inmig9707[, x]))
    ## names(inmig9707) <- substr(names(inmig9707), 2, 5)
    ## save(inmig9707, file = "inmig9707.RData")

    data(inmig9707)
    if (sex == "males"){
        res <- inmig9707[seq(1, NROW(inmig9707), 2),
                      start:slut,
                         drop = FALSE]
    }else{
        if (sex == "females"){
            res <- inmig9707[seq(2, NROW(inmig9707), 2),
                             start:slut,
                             drop = FALSE]
        }else{
            males <- inmig9707[seq(1, NROW(inmig9707), 2),
                      start:slut,
                         drop = FALSE]
            females <- inmig9707[seq(2, NROW(inmig9707), 2),
                             start:slut,
                             drop = FALSE]
            res <- males + females
        }
    }

    res <- as.matrix(res)
    res <- rbind(res, matrix(0, nrow = 5, ncol = (slut - start + 1)))
    rownames(res) <- as.character(0:105)
    colnames(res) <- as.character(years[1]:years[2])
    res
}
