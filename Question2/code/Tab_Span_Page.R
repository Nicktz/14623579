# Copied this from Texevier template...
Tab_Span_Page <- function(){

    addtorow          <- list()
    addtorow$pos      <- list()
    addtorow$pos[[1]] <- c(0)
    addtorow$command  <- c(paste("\\hline \n",
                                 "\\endhead \n",
                                 "\\hline \n",
                                 "{\\footnotesize Continued on next page} \n",
                                 "\\endfoot \n",
                                 "\\endlastfoot \n",sep=""))
    addtorow
}