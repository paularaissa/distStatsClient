##########################################
# Combine an entire list of Cum Dist Funcs
##########################################

combineCDFs <- function(CDFlist) {
  if (length(CDFlist) == 1)
    return(CDFlist[[1]])

  return(combine2CDFs(CDFlist[[1]], combineCDFs(CDFlist[2:length(CDFlist)])))
}

