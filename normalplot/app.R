library("devtools")
library("grid")
library("extrafont")
library("rlang")
install_github("benthegirl/BlindR")
library(BlindR)

options(scipen=0)

tactilegrobsnormaldist<-function(mean=0, sd=1, shade="none", 
                                 cutoff=1, newtitle="title",
                                 yaxislabel="ylab", xaxislabel="xlab",
                                 lower=-1, upper=1){
  xmin<-mean-sd*3.5
  xmax<-mean+sd*3.5
  x<-seq(xmin,xmax, length.out = 300)
  dat<-data.frame(x=x, y=dnorm(x = x,mean = mean,sd = sd))
    
      ymin<-min(dat$y)
      ymax<-max(dat$y)
  
  xticks<-grid.pretty(c(xmin, xmax))
  yticks<-grid.pretty(c(ymin, ymax))
  xlabs<-numberfixer(as.character(xticks))
  ylabs<-numberfixer(as.character(yticks))
 
    title<-textGrob(x=unit(0,"in"), y=unit(10,'in'),just=c("left","top"),
                    label=newtitle,
                    gp=gpar(fontsize=29))
  
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
  
  mainlinepng<-linesGrob(y=dat$y, x=dat$x, gp=gpar(lwd=5.67),default.units = "native")
  mainlinepdf<-linesGrob(y=dat$y, x=dat$x, gp=gpar(lwd=7.5),default.units = "native")
  if(shade=="none"){
    shadegrob<-NULL
  }else{
  if(shade=="gt"){
    shadegrob<-polygonGrob(x=c(dat$x[dat$x>=cutoff], rev(dat$x[dat$x>=cutoff])), 
                y=c(dat$y[dat$x>=cutoff], rep(0, length(dat$x[dat$x>=cutoff]))), 
                gp=gpar(fill="grey70"),
                default.units = "native")
  }
    if(shade=="lt"){
      shadegrob<-polygonGrob(x=c(dat$x[dat$x<=cutoff], rev(dat$x[dat$x<=cutoff])), 
                             y=c(dat$y[dat$x<=cutoff], rep(0, length(dat$x[dat$x<=cutoff]))), 
                             gp=gpar(fill="grey70"),
                             default.units = "native")
    }
    
    if(shade=="between"){
      shadegrob<-polygonGrob(x=c(dat$x[dat$x>=lower&dat$x<=upper], rev(dat$x[dat$x>=lower&dat$x<=upper])), 
                             y=c(dat$y[dat$x>=lower&dat$x<=upper], rep(0, length(dat$x[dat$x>=lower&dat$x<=upper]))), 
                             gp=gpar(fill="grey70"),
                             default.units = "native")
    }
    if(shade=="twosided"){
      shadegrob<-polygonGrob(x=c(dat$x[dat$x<=lower], rev(dat$x[dat$x<=lower]), dat$x[dat$x>=upper], rev(dat$x[dat$x>=upper])), 
                             y=c(dat$y[dat$x<=lower], rep(0, length(dat$x[dat$x<=lower])),
                                                          dat$y[dat$x>=upper], rep(0, length(dat$x[dat$x>=upper]))),
                             id.lengths=c(length(c(dat$x[dat$x<=lower], rev(dat$x[dat$x<=lower]))),
                                          length(c(dat$x[dat$x>=upper], rev(dat$x[dat$x>=upper])))),
                             gp=gpar(fill="grey70"),
                             default.units = "native")
    }
    
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
              plotareavp=plotareavp, ticklinesx=ticklinesx,shadegrob=shadegrob, ticklinesy=ticklinesy, mainlinepng=mainlinepng,
              xax=xax, yax=yax, xaxisline=xaxisline, yaxisline=yaxisline, mainlinepdf=mainlinepdf))
}

ui<-pageWithSidebar(
  headerPanel("Normal Distribution Plot"),
  sidebarPanel(
    textOutput("text"),
    numericInput(inputId = "mean",
              label = "mean",0),
    
    numericInput(inputId = "sd",
              label = "sd",1),
    textInput(inputId = "yaxislabel",
              label = "y-axis label",
              value = "f(x)"),
    textInput(inputId = "xaxislabel",
              label = "x-axis label",
              value = "x"),
    textInput(inputId = "newtitle",
              label = "Change Title",
              value = "Normal Distribution"),
     radioButtons(inputId="shade", label="shade", 
                  width = NULL, choiceNames = c("none", ">x", "<x", "two-sided", "between"), 
                  choiceValues = c("none", "gt", "lt", "twosided","between")),
    uiOutput("uiforvals")
  ),
  mainPanel(
    downloadButton("downloadData", "Download Printable PDF"),
    imageOutput("plot",width="720px",height="720px")
  )
)


server<-function(input, output, session) {
  output$uiforvals<-renderUI({
    switch(input$shade,
           "none"=NULL,
           "gt"=numericInput("cutoff", "cutoff", 1),
           "lt"=numericInput("cutoff", "cutoff", 1),
           "between"=tagList(numericInput("lower", "lower", -1),
                             numericInput("upper","uppper",1)),
           "twosided"=tagList(numericInput("lower", "lower", -1),
                             numericInput("upper","uppper",1))
           )
    
  })

  
  
  plotthings<-reactive({
    
    tactilegrobsnormaldist(mean=input$mean, sd=input$sd, yaxislabel=input$yaxislabel,
                 xaxislabel=input$xaxislabel,shade=input$shade,
                 cutoff=ifelse(input$shade=="gt"|input$shade=="lt", input$cutoff,1),
                 lower=ifelse(input$shade=="between"|input$shade=="twosided",input$lower,-1),
                 upper=ifelse(input$shade=="between"|input$shade=="twosided",input$upper,1),
                 newtitle=input$newtitle)})
  
  
  output$plot<-renderImage({
    outfile <- tempfile(fileext='.png')
    png(filename = outfile,width = 828, height=792,family = "Braille29")
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
    if(input$shade!="none"){
      grid.draw(plotthings()$shadegrob)
    }
    grid.draw(plotthings()$mainlinepng)
    pushViewport(plotthings()$plotareavpclipoff)
    grid.draw(plotthings()$xax)
    grid.draw(plotthings()$yax)
    dev.off()
    list(src = outfile,
         width = 828,
         height = 792,
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
      if(input$shade!="none"){
      grid.draw(plotthings()$shadegrob)
      }
      grid.draw(plotthings()$mainlinepng)
      pushViewport(plotthings()$plotareavpclipoff)
      grid.draw(plotthings()$xax)
      grid.draw(plotthings()$yax)
      dev.off()
    }, contentType = "image/pdf"
  )
  
}

shinyApp(ui, server)