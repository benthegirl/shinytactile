
grobsofmine<-tactilegrobs(xmin=-10, xmax=10,
             y="x^2", yaxislabel="yaxis",
             xaxislabel="xaxis",
             newtitle="my new label",pdf=FALSE,points=FALSE)


pushViewport(viewport(xscale=c(0,10), yscale=c(0,10),default.units = "in"))
#grid.rect(gp = gpar(col = "grey"))

grid.draw(grobsofmine$title)
grid.draw(grobsofmine$yaxislab)
grid.draw(grobsofmine$xaxislab)

pushViewport(grobsofmine$plotareavp)
grid.draw(grobsofmine$ticklinesx)
grid.draw(grobsofmine$ticklinesy)
grid.draw(grobsofmine$xaxisline)
grid.draw(grobsofmine$yaxisline)
grid.draw(grobsofmine$mainlinepng)
pushViewport(grobsofmine$plotareavpclipoff)
grid.draw(grobsofmine$xax)
grid.draw(grobsofmine$yax)
