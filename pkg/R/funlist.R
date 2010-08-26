funlist = function() {
    # list all functions in base and utils which we would
    # like to 'know'. Exclude all the as.*, just including
    # "as" once for example. Include non-methods that may
    # look like methods such as 'write.table'.

    cat("Building function list ...");flush.console()    
    xx = sort(c(objects(pos="package:base"), objects(pos="package:utils")))
    xx = xx[-grep("[<][-]",xx)]
    xx = xx[-grep("[?]",xx)]
    defunct = sapply(xx, function(x) {
        thisfun = get(x)
        is.function(thisfun) && length(grep("[.]Defunct", deparse(thisfun)))
    })
    xx = xx[!defunct]
    nodots = xx[grep("^[^.]+$",xx)]
    nodots = nodots[!nodots %in% c("UseMethod","|","||")]
    # exclude the following realmethods, at least
    realmethods = unlist(lapply(nodots, function(x) {
        thisfun = get(x)
        if (is.function(thisfun)) {
            dd = deparse(thisfun)
            if (is.primitive(thisfun) || length(grep("UseMethod",dd))) {
                return(suppressWarnings(tryCatch(methods(x),error=function(e)NULL)))
            }
        }
        NULL
    }))
    if (length(grep("^[^.]*$",realmethods))) stop("some methods don't have any .")
    xx = xx[!xx %in% realmethods]
    excludes = c("^is[.].+", "^as[.].+", "^Summary[.].+", "^Math[.].+", "^Ops[.].+", "^qr[.].+", "^all.equal[.].+")
    for (e in excludes) xx = xx[-grep(e,xx)]
    xx = sort(c(xx,"?","<-","<<-","is","as"))
    cat("done\n"); flush.console()
    xx
}

