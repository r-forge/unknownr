learn=function(funs=get("tolearn"))
{
    options(browser="firefox")  # and make sure to turn off new tab in preferences->tabs
    for (i in funs) {
        eval(parse(text=paste("help('",i,"')",sep="")))
        cat(i)
        scan(quiet=TRUE)
    }
}


