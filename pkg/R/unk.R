unk = function(fnam = file.path(path.expand("~"),".knowns.Rdata")) {
    .unk.i=.unk.lock=.unk.esc=.unk.i=.unk.dlg=.unk.bool=.unk.starting=.unk.qlabel=.unk.qtext=.unk.unknowns=.unk.funlist=NULL
    rm(list=objects(pattern="^[.]unk[.]",all=TRUE))
    require(tcltk)
    if (file.exists(fnam)) {
        tt = load(fnam)
        if (!identical(tt,"knowns")) stop(fnam,"must contain a single character vector 'knowns'")
	cat("Read",length(knowns),"from",fnam,"\n")
    } else {
	cat("First time running unk(). File",fnam,"does not exist\n")
	knowns=NULL
    }
    if (!exists(".unk.funlist",.GlobalEnv)) {
        .unk.funlist <<- funlist()
    } else {
        # the funlist won't have changed in this R session. Restart R (or rm(.unk.unknowns)) to build the list again.
    }
    .unk.unknowns <<- .unk.funlist[!.unk.funlist %in% knowns]
    large = tkfont.create(family="courier",size=40,weight="bold")
    .unk.dlg <<- tktoplevel()
    tkwm.title(.unk.dlg,"unknownR")
    tkgrid(tklabel(.unk.dlg,text="Do you know? :"))
    .unk.qtext <<- tclVar("Press SPACE to start")
    .unk.qlabel <<- tklabel(.unk.dlg,text=tclvalue(.unk.qtext),width=max(nchar(.unk.unknowns)),font=large)
    tkconfigure(.unk.qlabel,textvariable=.unk.qtext)
    tkgrid(.unk.qlabel)
    
    .unk.i <<- 0
    .unk.lock <<- FALSE
    .unk.esc <<- FALSE
    .unk.unknowns <<- sample(.unk.unknowns)
    .unk.bool <<- rep(FALSE,length(.unk.unknowns))
    .unk.starting <<- TRUE
    # These are left in .GlobalEnv because the after function may run Red for example, even after the window has been destroyed.  I did rm() them to clear up and discovered that. .GlobalEnv is used because I don't know any other way to store state since private package variables are sealed.
    
    #ChangeTextButton = tkbutton(dlg,text="Start",command=Next)
    tkbind(.unk.dlg,"<space>", Know)
    tkbind(.unk.dlg,"<Return>", Skip)  # anticipate most people will go space or q to get through it quickly
    tkbind(.unk.dlg,"<Escape>", Esc)
    tkfocus(.unk.dlg)
    tkwait.window(.unk.dlg)
    tkdestroy(.unk.dlg)
    .unk.unknowns  # TO DO subset by bool
}

Know = function() {
    .unk.lock=.unk.starting=.unk.bool=.unk.i=.unk.qlabel=.unk.esc=NULL
    rm(list=objects(pattern="^[.]unk[.]",all=TRUE))
    if (.unk.lock) return()
    if (!.unk.starting) {
        .unk.lock <<- TRUE
        .unk.bool[.unk.i] <<- TRUE
        cat("I know",.unk.i,"\n")
        tkconfigure(.unk.qlabel,fg="sea green")
        tcl("after",500,Next)
        # provides visual feedback to user and prevents accidental double key presses.
    } else {
        .unk.esc <<- FALSE
        Next()  # display the first one
    }
}

DontKnow = function(thisi=get(".unk.i",.GlobalEnv)) {
# runs 2s after red regardless of space being pressed, but if space was pressed i would have incremented and thisi<i
    if (thisi==get(".unk.i",.GlobalEnv) && !get(".unk.lock",.GlobalEnv) && !get(".unk.esc",.GlobalEnv)) {
        cat("Don't know",thisi,"\n")
        Next()
    }
}

Red = function(thisi) {
    .unk.i=.unk.lock=.unk.esc=.unk.qlabel=NULL
    rm(list=objects(pattern="^[.]unk[.]",all=TRUE))
    if (thisi==.unk.i && !.unk.lock && !.unk.esc) {
        tkconfigure(.unk.qlabel,fg="red")
        xx = parse(text=paste("DontKnow(",thisi,")"))
        tcl("after",2000,xx)
    }
}

Unlock = function() {
    assign(".unk.lock",FALSE,.GlobalEnv)
}

Next = function() {
    .unk.i=.unk.starting=.unk.qlabel=.unk.unknowns=.unk.lock=.unk.qtext=NULL
    rm(list=objects(pattern="^[.]unk[.]",all=TRUE))
    .unk.i <<- .unk.i+1
    .unk.starting <<- FALSE
    tkconfigure(.unk.qlabel,fg="black")
    .unk.lock <<- FALSE 
    if (.unk.i > length(.unk.unknowns)) {
        # finished
    } else {
        tclvalue(.unk.qtext) = .unk.unknowns[.unk.i]
        xx = parse(text=paste("function()Red(",.unk.i,")"))
        tcl("after",2000,xx)
        .unk.lock <<- TRUE
        tcl("after",250,Unlock)
        # prevents presses intended for the very end of red which are a little too late counting as a know for the next one. Unexpected that user will see function, recognise, know it and press space all within 250ms. Also prevents holding down space.
    }
}

Esc = function() {
    .unk.esc=.unk.dlg=.unk.qlabel=.unk.lock=.unk.i=.unk.starting=.unk.qtext=NULL
    rm(list=objects(pattern="^[.]unk[.]",all=TRUE))
    if (.unk.esc) {
        tkdestroy(.unk.dlg)
    } else {
        tkconfigure(.unk.qlabel,fg="black")
        .unk.lock <<- FALSE
        .unk.esc <<- TRUE
        .unk.i <<- .unk.i-1
        .unk.starting <<- TRUE
        tclvalue(.unk.qtext) = "Press SPACE to resume"
    }
}

Skip = function() {
    .unk.lock=.unk.starting=.unk.qlabel=NULL
    rm(list=objects(pattern="^[.]unk[.]",all=TRUE))
    if (!.unk.lock && !.unk.starting) {
        .unk.lock <<- TRUE
        tkconfigure(.unk.qlabel,fg="red")
        tcl("after",500,Next)
    }
}



