unk = function(file = paste(path.expand("~"),"/.unk.Rdata",sep="")) {
    # unknowns = funlist()
    if (file.exists(file)) {
        tt = load(file)
        if (!identical(tt,"knowns")) stop(".unk.Rdata must contain a single character vector 'knowns'")
        unknowns = unknowns[!unknowns %in% knowns]
    }
    require(tcltk)
    large = tkfont.create(family="courier",size=40,weight="bold")
    dlg = tktoplevel()
    tkwm.title(dlg,"unkownR")
    done = tclVar(0)
    tkgrid(tklabel(dlg,text="Do you know? :"))
    qtext = tclVar("Press SPACE to start")
    qlabel = tklabel(dlg,text=tclvalue(qtext),width=max(nchar(unknowns)),font=large)
    tkconfigure(qlabel,textvariable=qtext)
    tkgrid(qlabel)
    
    i <<- 0
    dkcount <<- 0
    redcount <<- 0
    lock <<- FALSE
    esc <<- FALSE
    unknowns <<- sample(unknowns)
    bool <<- rep(FALSE,length(unknowns))
    starting <<- TRUE
    Know = function() {
        if (!lock) {
            if (!starting) {
                lock <<- TRUE # so that a double press of SPACE counts as 1
                bool[i] <<- TRUE
                tkconfigure(qlabel,fg="sea green")
                tcl("after",1000,Next)
            } else {
                esc <<- FALSE
                Next()
            }
        }
    }
    DontKnow = function() {
        # runs 2s later regardless of space being pressed
        dkcount <<- dkcount+1
        if (dkcount==i && !esc && tclvalue(done)==0) Next()
    }
    Red = function() {
        redcount <<- redcount+1
        if (redcount==i && !lock && !esc && tclvalue(done)==0) {
            tkconfigure(qlabel,fg="red")
            tcl("after",2000,DontKnow)
        } else {
            dkcount <<- dkcount+1
        }
    }
    Next = function() {
        i <<- i+1
        starting <<- FALSE
        tkconfigure(qlabel,fg="black")
        lock <<- FALSE
        if (i > length(unknowns)) {
            # finished
        } else {
            tclvalue(qtext) = unknowns[i]
            tcl("after",3000,Red)
        }
    }
    Esc = function() {
        tkconfigure(qlabel,fg="black")
        lock <<- FALSE
        esc <<- TRUE
        i <<- i-1
        dkcount <<- dkcount-1
        redcount <<- redcount-1
        starting <<- TRUE
        tclvalue(qtext) = "Press SPACE to resume"
    }
    pressq = function() {
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
    tkbind(dlg,"<Destroy>",function()tclvalue(done)<-2)
    #tkgrid(ChangeTextButton)
    tkwait.variable(done)
    tkdestroy(dlg)
}

# to do: small bug when pressing space near end of red timeout, next one goes twice.

