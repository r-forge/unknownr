unk = function(fnam = path.expand("~/.knowns.Rdata"),size=20) {
    .unk.i=.unk.lock=.unk.esc=.unk.i=.unk.dlg=.unk.bool=.unk.starting=.unk.qlabel=.unk.qtext=.unk.unknowns=.unk.funlist=NULL
    .unk.knowns=.unk.numall=.unk.numkno=.unk.numunk=.unk.numleft=.unk.timeleft=.unk.num1=.unk.num2=.unk.num3=.unk.num4=.unk.num5=NULL
    .unk.numlabel1=.unk.numlabel2=.unk.numlabel3=.unk.numlabel4=.unk.numlabel5=NULL
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
    .unk.knowns <<- knowns
    .unk.fnam <<- fnam

    large = tkfont.create(family="courier",size=size*2,weight="bold")
    other = tkfont.create(family="ariel",size=size,weight="bold")
    .unk.dlg <<- tktoplevel()
    tkwm.title(.unk.dlg,"unknownR")
     tkgrid(tklabel(.unk.dlg,text=""))
    tkgrid(tklabel(.unk.dlg,text="Do you know? :",font=other),columnspan=4)
    tkgrid(tklabel(.unk.dlg,text=""))
    
    .unk.qtext <<- tclVar("Press SPACE to start")
    .unk.qlabel <<- tklabel(.unk.dlg,text=tclvalue(.unk.qtext),width=max(nchar(.unk.unknowns)),font=large,relief="ridge",bd=10,bg="light yellow")
    tkconfigure(.unk.qlabel,textvariable=.unk.qtext)
    tkgrid(.unk.qlabel,columnspan=4)
    tkgrid(tklabel(.unk.dlg,text=""))
    
    .unk.numall <<- tclVar(0)
    .unk.numkno <<- tclVar(0)
    .unk.numunk <<- tclVar(0)
    .unk.numleft <<- tclVar(0)    
    .unk.timeleft <<- tclVar("")
    
    .unk.num1 <<- tklabel(.unk.dlg,textvariable=.unk.numall,width=10,anchor="e",font=other)
    .unk.num2 <<- tklabel(.unk.dlg,textvariable=.unk.numkno,width=10,anchor="e",fg="blue",font=other)
    .unk.num3 <<- tklabel(.unk.dlg,textvariable=.unk.numunk,width=10,anchor="e",fg="red",font=other)
    .unk.num4 <<- tklabel(.unk.dlg,textvariable=.unk.numleft,width=10,anchor="e",font=other)
    .unk.num5 <<- tklabel(.unk.dlg,textvariable=.unk.timeleft,width=10,anchor="e",font=other)
    .unk.numlabel1 <<- tklabel(.unk.dlg,text="All functions:",font=other)  #relief="groove"
    .unk.numlabel2 <<- tklabel(.unk.dlg,text="Known:",fg="blue",font=other)
    .unk.numlabel3 <<- tklabel(.unk.dlg,text="Unknown:",fg="red",font=other)
    .unk.numlabel4 <<- tklabel(.unk.dlg,text="Remaining:",font=other)
    .unk.numlabel5 <<- tklabel(.unk.dlg,text="Estimated time:",font=other)
    tkgrid(.unk.numlabel1,.unk.num1)
    tkgrid(.unk.numlabel2,.unk.num2,label6<-tklabel(.unk.dlg,text="SPACE : ",fg="blue",font=other),label7<-tklabel(.unk.dlg,text="I know it",fg="blue",font=other))
    tkgrid(.unk.numlabel3,.unk.num3,label8<-tklabel(.unk.dlg,text="ENTER : ",fg="red",font=other),label9<-tklabel(.unk.dlg,text="I don't know it",fg="red",font=other))
    tkgrid(.unk.numlabel4,.unk.num4,label10<-tklabel(.unk.dlg,text="ESC : ",font=other),label11<-tklabel(.unk.dlg,text="Pause/Quit",font=other))
    .unk.backbutton <<- tkbutton(.unk.dlg,text="Back",command=PressedBack,font=other,bd=2,state="disabled")
    tkgrid(.unk.numlabel5,.unk.num5,.unk.backbutton,label12<-tklabel(.unk.dlg,text="Undo last answer",font=other))
    tkgrid.configure(.unk.numlabel1,.unk.numlabel2,.unk.numlabel3,.unk.numlabel4,.unk.numlabel5,label6,label8,label10,.unk.backbutton,sticky="e")
    tkgrid.configure(.unk.num1,.unk.num2,.unk.num3,.unk.num4,.unk.num5,label7,label9,label11,label12,sticky="w")
    tkgrid.columnconfigure(.unk.dlg,0,weight=1)
    tkgrid.columnconfigure(.unk.dlg,1,weight=5)
    tkgrid.columnconfigure(.unk.dlg,2,weight=1)
    tkgrid.columnconfigure(.unk.dlg,3,weight=5)
    tkgrid(tklabel(.unk.dlg,text=""))
    
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
    updatestatus()
    tkwait.window(.unk.dlg)
    if (.unk.i>0 && tclvalue(tkmessageBox(message=paste("Save your ",.unk.i," known/unknown response",if(.unk.i>1)"s"else""," to disk?",sep=""),type="yesno"))=="yes") {
        saveanswers()
    }
    tkdestroy(.unk.dlg)
    tolearn = .unk.unknowns[!.unk.bool]
    assign("tolearn",tolearn,envir=.GlobalEnv)
    cat("Now type learn() to view help for your",length(tolearn),"unknowns. Run unk() again when you know them.\n")
    invisible()
}

saveanswers=function() {
    cat("Added",sum(.unk.bool),"knowns to the",length(.unk.knowns),"in",.unk.fnam,"\n")
    knowns = sort(c(.unk.knowns,.unk.unknowns[.unk.bool]))
    save(list="knowns",file=.unk.fnam)
}

updatestatus = function() {
    .unk.numall=.unk.funlist=.unk.numkno=.unk.knowns=.unk.bool=.unk.numunk=.unk.i=.unk.unknowns=.unk.numleft=.unk.timeleft=NULL
    rm(list=objects(pattern="^[.]unk[.]",all=TRUE))
    tclvalue(.unk.numall) <<- length(.unk.funlist)
    tclvalue(.unk.numkno) <<- length(.unk.knowns)+sum(.unk.bool)
    tclvalue(.unk.numunk) <<- sum(!head(.unk.bool,.unk.i))
    n = length(.unk.unknowns) - .unk.i
    tclvalue(.unk.numleft) <<- n
    s = n*3  # assume 3 sec per function (most users will go much faster and beat estimate)
    h = s%/%3600
    m = (s-h*3600)%/%60
    s = s%%60
    tclvalue(.unk.timeleft) <<- sprintf("%02d:%02d:%02d",h,m,s)
    invisible()
}

PressedBack = function() {
   if (.unk.i>0) {
       .unk.bool[.unk.i]=FALSE
       .unk.i<<-.unk.i-1
   }
   updatestatus()
   if (.unk.i==0) tkconfigure(.unk.backbutton,state="disabled")
}

Know = function() {
    .unk.lock=.unk.starting=.unk.bool=.unk.i=.unk.qlabel=.unk.esc=NULL
    rm(list=objects(pattern="^[.]unk[.]",all=TRUE))
    if (.unk.lock) return()
    if (!.unk.starting) {
        .unk.lock <<- TRUE
        .unk.bool[.unk.i] <<- TRUE
        tkconfigure(.unk.qlabel,fg="blue")
        updatestatus()
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
        updatestatus()
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
    if (.unk.esc) return()  # a very quick ESC following SPACE
    .unk.i <<- .unk.i+1
    .unk.starting <<- FALSE
    tkconfigure(.unk.qlabel,fg="black")
    .unk.lock <<- FALSE 
    if (.unk.i > length(.unk.unknowns)) {
        # finished
    } else {
        tclvalue(.unk.qtext) = .unk.unknowns[.unk.i]
        xx = parse(text=paste("function()Red(",.unk.i,")"))
        tcl("after",3000,xx)
        .unk.lock <<- TRUE
        tcl("after",250,Unlock)
        # lock prevents presses intended for the very end of red which are a little too late counting as a know for the next one. Unexpected that user will see function, recognise, know it and press space all within 250ms. Also prevents holding down space.
        tkconfigure(.unk.backbutton,state="disabled")
    }
}

Esc = function() {
    .unk.esc=.unk.dlg=.unk.qlabel=.unk.lock=.unk.i=.unk.starting=.unk.qtext=NULL
    rm(list=objects(pattern="^[.]unk[.]",all=TRUE))
    if (.unk.esc) {
        if (.unk.i>0) {
            ans = tclvalue(tkmessageBox(message=paste("Save your ",.unk.i," known/unknown response",if(.unk.i>1)"s"else""," to disk?",sep=""),type="yesnocancel"))
            if (ans=="cancel") return()
            if (ans=="yes") saveanswers()
            .unk.i <<- 0  # to not ask again on destroy (see end of unk())
        }
        tkdestroy(.unk.dlg)
    } else {
        tkconfigure(.unk.qlabel,fg="black")
        .unk.lock <<- FALSE
        .unk.esc <<- TRUE
        if (!.unk.bool[.unk.i]) .unk.i <<- .unk.i-1  # Quick ESC following SPACE should not forget the known
        .unk.starting <<- TRUE
        tclvalue(.unk.qtext) = "Press SPACE to resume"
        if(.unk.i>0) tkconfigure(.unk.backbutton,state="active")
    }
}

Skip = function() {
    .unk.lock=.unk.starting=.unk.qlabel=NULL
    rm(list=objects(pattern="^[.]unk[.]",all=TRUE))
    if (!.unk.lock && !.unk.starting) {
        .unk.lock <<- TRUE
        tkconfigure(.unk.qlabel,fg="red")
        updatestatus()
        tcl("after",500,Next)
    }
}



