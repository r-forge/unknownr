unk = function(fnam = file.path(path.expand("~"),".knowns.Rdata")) {
    require(tcltk)
    if (file.exists(fnam)) {
        tt = load(fnam)
        if (!identical(tt,"knowns")) stop(fnam,"must contain a single character vector 'knowns'")
	cat("Read",length(knowns),"from",fnam,"\n")
    } else {
	cat("First time running unk(). File",fnam,"does not exist\n")
	knowns=NULL
    }
    unknowns = funlist()
    unknowns = unknowns[!unknowns %in% knowns]
    large = tkfont.create(family="courier",size=40,weight="bold")
    dlg <<- tktoplevel()
    tkwm.title(dlg,"unknownR")
    done <<- tclVar(0)
    tkgrid(tklabel(dlg,text="Do you know? :"))
    qtext = tclVar("Press SPACE to start")
    qlabel = tklabel(dlg,text=tclvalue(qtext),width=max(nchar(unknowns)),font=large)
    tkconfigure(qlabel,textvariable=qtext)
    tkgrid(qlabel)
    
    i <<- 0
    lock <<- FALSE
    esc <<- FALSE
    unknowns <<- sample(unknowns)
    bool <<- rep(FALSE,length(unknowns))
    starting <<- TRUE
    Know <<- function() {
        if (lock) return()
        if (!starting) {
            lock <<- TRUE
            bool[i] <<- TRUE
            cat("I know",i,"\n")
            tkconfigure(qlabel,fg="sea green")
            tcl("after",1000,Next)
            # provides visual feedback to user and prevents accidental double key presses.
        } else {
            esc <<- FALSE
            Next()  # display the first one
        }
    }
    DontKnow <<- function(thisi=i) {
        # runs 2s after red regardless of space being pressed, but if space was pressed i would have incremented and thisi<i
        if (thisi==i && !lock && !esc && tclvalue(done)==0) {
            cat("Don't know",thisi,"\n")
            Next()
        }
    }
    Red <<- function(thisi) {
        if (thisi==i && !lock && !esc && tclvalue(done)==0) {
            tkconfigure(qlabel,fg="red")
            xx = parse(text=paste("DontKnow(",thisi,")"))
            tcl("after",2000,xx)
        }
    }
    Unlock <<- function() {
        lock <<- FALSE
    }
    Next <<- function() {
        i <<- i+1
        starting <<- FALSE
        tkconfigure(qlabel,fg="black")
        lock <<- FALSE 
        if (i > length(unknowns)) {
            # finished
        } else {
            tclvalue(qtext) = unknowns[i]
            xx = parse(text=paste("function()Red(",i,")"))
            tcl("after",3000,xx)
            lock <<- TRUE
            tcl("after",250,Unlock)
            # prevents presses intended for the very end of red which are a little too late counting as a know for the next one. Unexpected that user will see function, recognise, know it and press space all within 250ms. Also prevents holding down space.
        }
    }
    Esc <<- function() {
        if (esc) {
            tclvalue(done)<-2
            tkdestroy(dlg)
	} else {
            tkconfigure(qlabel,fg="black")
            lock <<- FALSE
            esc <<- TRUE
            i <<- i-1
            starting <<- TRUE
            tclvalue(qtext) = "Press SPACE to resume"
        }
    }
    pressq <<- function() {
        if (!lock && !starting) {
            lock <<- TRUE
            tkconfigure(qlabel,fg="red")
            tcl("after",1000,Next)
        }
    }
    
    #ChangeTextButton = tkbutton(dlg,text="Start",command=Next)
    tkbind(dlg,"<space>", Know)
    tkbind(dlg,"q", pressq)  # anticipate most people will go space or q to get through it quickly
    tkbind(dlg,"<Escape>", Esc)
    #tkbind(dlg,"<Destroy>",function(){tclvalue(done)<<-2;tkdestroy(dlg)})
    #tkbind(dlg,"p",function()tclvalue(done)<<-2)
    #tkgrid(ChangeTextButton)
    tkfocus(dlg)
    # tkwait.variable(done)
    tkwait.window(dlg)
    tkdestroy(dlg)
}

# change q to return
# move functions into namespace so as not to disturb the .GlobalEnv


