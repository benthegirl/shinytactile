library("devtools")
library("grid")
library("extrafont")
library("rlang")
install_github("benthegirl/BlindR")
library(BlindR)

options(scipen=0)

tactilegrobs<-function(xmin, xmax,y,yaxislabel, xaxislabel,newtitle,pdf=FALSE,points=FALSE){
  dat<-NULL
  x<-seq(xmin,xmax, length.out = 150)
  if(sum(y!="")==0){
    ymin<-xmin
    ymax<-xmax
  }else{
    y<-y[y!=""]
    consts<-suppressWarnings(!is.na(as.numeric(y)))
    if(sum(consts)>0){
      datc<-data.frame(x=rep(x,sum(consts)), y=rep(y[consts], each=length(x)),
                       group=rep(1:sum(consts), each=length(x)))
      ymin<-xmin
      ymax<-xmax
      y<-y[!consts]
    }else{
      datc<-list(group=0)
    }
    if(length(y)>0){
      dat<-data.frame(x=rep(x, length(y)), 
                      y=unlist(lapply(y,function(a){eval(parse(text=a))})),
                      group=rep((max(datc$group)+1):length(y),each=length(x)))
      if(max(datc$group)!=0){
        dat<-rbind(datc,dat)
      }
      ymin<-min(dat$y)
      ymax<-max(dat$y)
      }
  }

  xticks<-grid.pretty(c(xmin, xmax))
  yticks<-grid.pretty(c(ymin, ymax))
  xlabs<-numberfixer(as.character(xticks))
  ylabs<-numberfixer(as.character(yticks))
  if(newtitle=="equation of function"){
    if(is.null(dat)){
      title<-textGrob(x=unit(0,"in"), y=unit(10,'in'),just=c("left","top"),
                      label="",
                      gp=gpar(fontsize=29))
    }else{
      title<-textGrob(x=unit(0,"in"), y=unit(10,'in'),just=c("left","top"),
                      label=nemeth(paste0("y=",y[1])),
                      gp=gpar(fontsize=29))
    }
  }else{
    title<-textGrob(x=unit(0,"in"), y=unit(10,'in'),just=c("left","top"),
                    label=newtitle,
                    gp=gpar(fontsize=29))
  }
  ylabswidth<-max(sapply(ylabs,FUN = nchar))*.333+.7

  rightwidthpad<-nchar(tail(xlabs,1))*.333
  yaxislab<-textGrob(x=unit(ylabswidth,"in")-unit(1.3,"cm"),
                     y=unit(10,'in')-grobHeight(title)-unit(1,"cm"),
                     just=c("left","top"),
                     label=yaxislabel,
                     gp=gpar(fontsize=29, fontfamily="Braille29"))

  xaxislab<-textGrob(x=unit(ylabswidth,"in")-unit(1.3,"cm"),
                     y=unit(.1,'in'),
                     just=c("left","bottom"),
                     label=xaxislabel,
                     gp=gpar(fontsize=29, fontfamily="Braille29"))
  plotareavp<-viewport(x=unit(ylabswidth,"in")-unit(1,"cm"),
                       y=unit(10,'in')-grobHeight(title)-grobHeight(yaxislab)-unit(1.6,"cm"),
                       just=c("left","top"),
                       width=unit(10,"in")-unit(ylabswidth,"in")-unit(rightwidthpad,"in")+unit(1,"cm"),
                       height=unit(10,"in")-grobHeight(title)-grobHeight(yaxislab)-
                         grobHeight(xaxislab)-unit(4.9,"cm"),
                       xscale=c(xmin,xmax), yscale=c(ymin,ymax),
                       clip="on")
  ticklinesx<-polylineGrob(x=rep(xticks,each=2),
                           y=c(rep(c(ymin,ymax),length(xticks))),
                           id=rep(1:length(xticks),each=2),gp=gpar(col="grey85"),default.units="native")
  xaxisline<-linesGrob(y=c(ymin,ymin), x=c(xmin,xmax),default.units="native",gp=gpar(col="black",lwd=2))
  yaxisline<-linesGrob(x=c(xmin,xmin), y=c(ymin,ymax),default.units="native",gp=gpar(col="black",lwd=2))
  ticklinesy<-polylineGrob(y=rep(yticks,each=2),
                           x=c(rep(c(xmin,xmax),length(yticks))),
                           id=rep(1:length(yticks),each=2),gp=gpar(col="grey85"),
                           default.units="native")
  if(is.null(dat)){
    mainlinepng<-NULL
    mainlinepdf<-NULL
  }else{
    mainlinepng<-polylineGrob(y=dat$y, x=dat$x, gp=gpar(lwd=5.67, lty="13"),default.units = "native",id=dat$group)
    mainlinepdf<-polylineGrob(y=dat$y, x=dat$x, gp=gpar(lwd=7.5, lty="13"),default.units = "native",id=dat$group)
  }
  plotareavpclipoff<-viewport(width=1,height=1, xscale=c(xmin,xmax), yscale=c(ymin,ymax),clip = "off")

  xax<-xaxisGrob(at=xticks,
                 label = xlabs,
                 edits=gEditList(gEdit("labels", just=c("left","bottom"),
                                       hjust=.15,
                                       gp=gpar(fontsize=29, fontfamily="Braille29")),
                                 gEdit("ticks",y1=unit(-5,"mm"),y0=unit(5,"mm"))))
  yax<-yaxisGrob(at=yticks,
                 label = ylabs,
                 edits=gEditList(gEdit("labels",
                                       vjust=.35,gp=gpar(fontsize=29, fontfamily="Braille29")),
                                 gEdit("ticks",x1=unit(-5,"mm"),x0=unit(5,"mm"))))
  return(list(dat=dat,title=title, yaxislab=yaxislab, xaxislab=xaxislab,plotareavpclipoff=plotareavpclipoff,
       plotareavp=plotareavp, ticklinesx=ticklinesx, ticklinesy=ticklinesy, mainlinepng=mainlinepng,
       xax=xax, yax=yax, xaxisline=xaxisline, yaxisline=yaxisline, mainlinepdf=mainlinepdf))
}

ui<-pageWithSidebar(
  headerPanel("Lines and Points"),
  sidebarPanel(
    textInput(inputId = "y",
              label = "Y=",
              value = ".05*x^3-.1*x^2-1"),
    actionButton("addfunction", "Add Function"),
    numericInput('xmin', 'xmin', -10),
    numericInput('xmax', 'xmax', 10),

    textInput(inputId = "xaxislabel",
              label = "x-axis label",
              value = "x"),
    textInput(inputId = "yaxislabel",
              label = "y-axis label",
              value = "y"),
    textInput(inputId = "newtitle",
              label = "Change Title",
              value = "equation of function")
  ),
  mainPanel(
    downloadButton("downloadData", "Download Printable PDF"),
    imageOutput("plot",width="720px",height="720px")
  )
)


server<-function(input, output, session) {
  counter <- reactiveValues(countervalue = 1)
  observeEvent(input$addfunction,{
    counter$countervalue <- counter$countervalue + 1

    insertUI(selector = "#addfunction",
             where = "beforeBegin",
             ui =textInput(inputId = paste0("y", counter$countervalue),
                           label = paste0("Y", counter$countervalue,"="),
                           value = "x+1")
    )}
  )


  # reactive({if(input$xmin>=input$xmax){
  # output$plot<-renderImage({minmaxerror.jpeg})
  # }else{
  # 
 plotthings<-reactive({
   req(is.numeric(input$xmin)&is.numeric(input$xmax)&(input$xmin<input$xmax))
   fxns<-names(input)[grep("^y[0-9]|^y$", names(input))]
   myy<-unlist(lapply(fxns, function(a){
     input[[a]]
   }))
   tactilegrobs(xmin=input$xmin, xmax=input$xmax,
                           y=myy, yaxislabel=input$yaxislabel,
                           xaxislabel=input$xaxislabel,
                           newtitle=input$newtitle,pdf=FALSE,points=FALSE)})


  output$plot<-renderImage({
    outfile <- tempfile(fileext='.png')
    png(filename = outfile,width = 828, height=792,family = "Braille29")
    pushViewport(viewport(xscale=c(0,11.5), yscale=c(0,11),default.units = "in",name = "paper"))
    pushViewport(viewport(xscale=c(0,10), yscale=c(0,10),default.units = "in",just = "center",x=unit(5.75,"in"),
                          width=unit(10, "in"), height=unit(10, "in"),
                          y=unit(5.5,"in"), name="backgroundarea"))
    #grid.rect(gp = gpar(col = "grey"))

    grid.draw(plotthings()$title)
    grid.draw(plotthings()$yaxislab)
    grid.draw(plotthings()$xaxislab)

    pushViewport(plotthings()$plotareavp)
    grid.draw(plotthings()$ticklinesx)
    grid.draw(plotthings()$ticklinesy)
    grid.draw(plotthings()$xaxisline)
    grid.draw(plotthings()$yaxisline)
    grid.draw(plotthings()$mainlinepng)
    pushViewport(plotthings()$plotareavpclipoff)
    grid.draw(plotthings()$xax)
    grid.draw(plotthings()$yax)
    dev.off()
    list(src = outfile,
         width = 720,
         height = 720,
         alt = "This is alternate text")
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("tactileplot", Sys.Date(), ".pdf", sep="")
    },
    content = function(file) {
      cairo_pdf(filename = file,width = 11.5, height=11,family = "Braille29")
      pushViewport(viewport(xscale=c(0,11.5), yscale=c(0,11),default.units = "in",name = "paper"))
      pushViewport(viewport(xscale=c(0,10), yscale=c(0,10),default.units = "in",just = "center",x=unit(5.75,"in"),
                            width=unit(10, "in"), height=unit(10, "in"),
                            y=unit(5.5,"in"), name="backgroundarea"))
      
      grid.draw(plotthings()$title)
      grid.draw(plotthings()$yaxislab)
      grid.draw(plotthings()$xaxislab)
      
      pushViewport(plotthings()$plotareavp)
      grid.draw(plotthings()$ticklinesx)
      grid.draw(plotthings()$ticklinesy)
      grid.draw(plotthings()$xaxisline)
      grid.draw(plotthings()$yaxisline)
      grid.draw(plotthings()$mainlinepng)
      pushViewport(plotthings()$plotareavpclipoff)
      grid.draw(plotthings()$xax)
      grid.draw(plotthings()$yax)
      dev.off()
    }, contentType = "image/pdf"
  )

}

shinyApp(ui, server)

