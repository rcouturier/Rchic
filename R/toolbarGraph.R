toolbarGraph <- function (frame) {


#window <- tktoplevel ( )
#tkwm.title ( window , " Select a color" )
#frame <- ttkframe ( window , padding=4 )
#tkpack ( frame , expand = TRUE , fill = "both" )


spin=list()
cb=list()

value=list(tclVar(99),tclVar(95),tclVar(90),tclVar(85))

cbvalue=list(tclVar(1),tclVar(0),tclVar(0),tclVar(0))


tmp.val=array(0,4)


color_well1 <- tkcanvas ( frame , width = 40 , height = 16 ,
                          background = "#FF0000" ,
                          highlightbackground = "#FF0000" )
tkgrid(color_well1)
spin1<-tk2spinbox(frame, from=50, to=100, increment=1, command=function() changeSpinBox(1),width=3, textvariable=value[[1]])
tkgrid(spin1)
cb1 <- tkcheckbutton(frame)
tkconfigure(cb1,variable=cbvalue[[1]])
tkgrid(cb1)







color_well2 <- tkcanvas ( frame , width = 40 , height = 16 ,
                          background = "#00FF00" ,
                          highlightbackground = "#00FF00" )
tkgrid(color_well2)
spin2<-tk2spinbox(frame, from=50, to=100, increment=1, width=3,command=function() changeSpinBox(2), textvariable=value[[2]])
tkgrid(spin2)
cb2 <- tkcheckbutton(frame)
tkconfigure(cb2,variable=cbvalue[[2]])
tkgrid(cb2)










color_well3 <- tkcanvas ( frame , width = 40 , height = 16 ,
                          background = "#0000FF" ,
                          highlightbackground = "#0000FF" )
tkgrid(color_well3)
spin3<-tk2spinbox(frame, from=50, to=100, increment=1, width=3,command=function() changeSpinBox(3), textvariable=value[[3]])
tkgrid(spin3)
cb3 <- tkcheckbutton(frame)
tkconfigure(cb3,variable=cbvalue[[3]])
tkgrid(cb3)







color_well4 <- tkcanvas ( frame , width = 40 , height = 16 ,
                          background = "#00FFFF" ,
                          highlightbackground = "#00FFFF" )
tkgrid(color_well4)
spin4<-tk2spinbox(frame, from=50, to=100, increment=1, width=3,command=function() changeSpinBox(4), textvariable=value[[4]])
tkgrid(spin4)
cb4 <- tkcheckbutton(frame)
tkconfigure(cb4,variable=cbvalue[[4]])
tkgrid(cb4)






tkbind ( color_well1 , "<Button -1>" , function (W) {changeColor(W)})
tkbind ( color_well2 , "<Button -1>" , function (W) {changeColor(W)})
tkbind ( color_well3 , "<Button -1>" , function (W) {changeColor(W)})
tkbind ( color_well4 , "<Button -1>" , function (W) {changeColor(W)})
# 


changeColor <- function(W) {
  color <- tcl ( "tk_chooseColor" , parent = W ,
                 title = "Set box color" )
  color <- tclvalue ( color )
  print ( color )
  if ( nchar ( color ) )
    tkconfigure ( W , background = color )
}




spin=list(spin1,spin2,spin3,spin4)
cb=list(cb1,cb2,cb3,cb4)

OnOK <- function()
{
  for(i in 1:4) {
    tmp.val[i] <- as.numeric(tclvalue(cbvalue[[i]]))
    print(tmp.val[i])
    tmp.val[i]=as.numeric(tclvalue(value[[i]]))
    print(tmp.val[i])
   
  }
}
OK.but <- tkbutton(frame,text="OK",command=OnOK)
tkgrid(OK.but)
tkfocus(frame)








changeSpinBox <- function(spin.nb)  {
  #print(spin.nb)
  for(i in 1:4) {
    tmp.val[i]=as.numeric(tclvalue(value[[i]]))
    #print(tmp.val[i])
  }
  for(i in 1:4)
    if(i>spin.nb)
      if(tmp.val[spin.nb]<tmp.val[i]+i-spin.nb) {
        tmp.val[i]=tmp.val[spin.nb]-(i-spin.nb)
        if(tmp.val[i]<=50)
          tmp.val[i]=50
        tkset(spin[[i]],tmp.val[i])
      }
  for(i in 1:4)
    if(i<spin.nb)
      if(tmp.val[spin.nb]>tmp.val[i]+i-spin.nb) {
        tmp.val[i]=tmp.val[spin.nb]+spin.nb-i
        if(tmp.val[i]>=100)
          tmp.val[i]=100
        tkset(spin[[i]],tmp.val[i])
      }
}

}