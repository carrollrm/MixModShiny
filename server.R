library(shiny)
library(sp)
library(maptools)
library(fillmap)
pF1U <- read.csv("data/pF1U.csv")
pF2U <- read.csv("data/pF2U.csv")
pF3U <- read.csv("data/pF3U.csv")
pF4U <- read.csv("data/pF4U.csv")
pF1M <- read.csv("data/pF1J.csv")
pF2M <- read.csv("data/pF2J.csv")
pF3M <- read.csv("data/pF3J.csv")
pF4M <- read.csv("data/pF4J.csv")
gofF1U <- read.csv("data/gofF1U.csv",row.names = c("OCPCa","MCaS","LBCa","All"))
gofF2U <- read.csv("data/gofF2U.csv",row.names = c("OCPCa","MCaS","LBCa","All"))
gofF3U <- read.csv("data/gofF3U.csv",row.names = c("OCPCa","MCaS","LBCa","All"))
gofF4U <- read.csv("data/gofF4U.csv",row.names = c("OCPCa","MCaS","LBCa","All"))
gofF1M <- read.csv("data/gofF1M.csv",row.names = c("OCPCa","MCaS","LBCa","All"))
gofF2M <- read.csv("data/gofF2M.csv",row.names = c("OCPCa","MCaS","LBCa","All"))
gofF3M <- read.csv("data/gofF3M.csv",row.names = c("OCPCa","MCaS","LBCa","All"))
gofF4M <- read.csv("data/gofF4M.csv",row.names = c("OCPCa","MCaS","LBCa","All"))
geobugsSC<-readSplus("data/SC_geobugsSPlus.txt")

shinyServer(function(input, output) {
  output$text <- renderText({
    paste("You have selected a",input$data,"model with a",
          ifelse(input$mod=="F1","spatially uncorrelated",ifelse(input$mod=="F2","spatially correlated",
                ifelse(input$mod=="F3","spatially correlated and temporally uncorrelated",
                      "spatially and temporally correlated"))),"mixture parameter.")
  })
  output$map <-renderPlot({
	  data <- switch(
	    if(input$data=="Multivariate" & input$mod=="F1"){
	      print("F1J")
	    } else if(input$data=="Multivariate" & input$mod=="F2"){
	      print("F2J")
	    } else if(input$data=="Multivariate" & input$mod=="F3"){
	      print("F3J")
	    } else if(input$data=="Multivariate" & input$mod=="F4"){
	      print("F4J")
	    } else if(input$data=="Univariate" & input$mod=="F1"){
	      print("F1U")
	    } else if(input$data=="Univariate" & input$mod=="F2"){
	      print("F2U")
	    } else if(input$data=="Univariate" & input$mod=="F3"){
	      print("F3U")
	    } else
	      print("F4U"),
	   "F1J" = pF1M,
		 "F2J" = pF2M,
     "F3J" = pF3M,
		 "F4J" = pF4M,
		 "F1U" = pF1U,
		 "F2U" = pF2U,
		 "F3U" = pF3U,
		 "F4U" = pF4U)
		if(input$mod=="F3"|input$mod=="F4"){
		  par(mar=c(0.1,5,2,0))
		  layout(matrix(c(1:9,10,10,10),nrow=4,ncol=3,byrow=T),heights=c(.3,.3,.3,.1))
		  fillmap(geobugsSC,"1996",data[1,],n.col=5,bk="c",cuts=seq(0,1,.2),legendtxt=NA,main.line = 0)
		  title(ylab="OCPCa",cex.lab=1.5)
		  fillmap(geobugsSC,"2003",data[2,],n.col=5,bk="c",cuts=seq(0,1,.2),legendtxt=NA,main.line = 0)
		  fillmap(geobugsSC,"2009",data[3,],n.col=5,bk="c",cuts=seq(0,1,.2),legendtxt=NA,main.line = 0)
		  fillmap(geobugsSC,"1996",data[4,],n.col=5,bk="c",cuts=seq(0,1,.2),legendtxt=NA,main.line = 0)
		  title(ylab="MCaS",cex.lab=1.5)
		  fillmap(geobugsSC,"2003",data[5,],n.col=5,bk="c",cuts=seq(0,1,.2),legendtxt=NA,main.line = 0)
		  fillmap(geobugsSC,"2009",data[6,],n.col=5,bk="c",cuts=seq(0,1,.2),legendtxt=NA,main.line = 0)
		  fillmap(geobugsSC,"1996",data[7,],n.col=5,bk="c",cuts=seq(0,1,.2),legendtxt=NA,main.line = 0)
		  title(ylab="MCaS",cex.lab=1.5)
		  fillmap(geobugsSC,"2003",data[8,],n.col=5,bk="c",cuts=seq(0,1,.2),legendtxt=NA,main.line = 0)
		  fillmap(geobugsSC,"2009",data[9,],n.col=5,bk="c",cuts=seq(0,1,.2),legendtxt=NA,main.line = 0)
		  par(mai=c(0,0,0,0),mar=c(0.1,0,0,0))
		  plot(1, type = "n", axes=FALSE, xlab="", ylab="")
		  n.col=5
		  br <- seq(0,1,.2)
		  leg.txt<-paste("[",br[n.col],",",br[n.col+1],"]",sep="")
		  for(i in (n.col-1):1){ 
		    leg.txt<-append(leg.txt,paste("[",br[i],",",br[i+1],")",sep="")) }
		  leg.txt<-rev(leg.txt)
		  shading<-gray((n.col-1):0/(n.col-1))
		  legend(x="top",legend=leg.txt,bty="n",fill=shading,inset = 0,ncol=1,
		         horiz=T,cex=1.5,x.intersp=1,xjust=0.5,yjust=0.5,text.width =rep(.1,5))		}
		else{
		  par(mar=c(0.1,5,2,0))
      fillmaps(geobugsSC,c("OCPCa","MCaS","LBCa"),data,main.line = 0,
               leg.common = T,lay.m = matrix(1:4,2,2,T),leg.loc = "center",n.col=5)
      }
	})
  output$tab <-renderTable({
    data <- switch(
      if(input$data=="Multivariate" & input$mod=="F1"){
        print("F1J")
      } else if(input$data=="Multivariate" & input$mod=="F2"){
        print("F2J")
      } else if(input$data=="Multivariate" & input$mod=="F3"){
        print("F3J")
      } else if(input$data=="Multivariate" & input$mod=="F4"){
        print("F4J")
      } else if(input$data=="Univariate" & input$mod=="F1"){
        print("F1U")
      } else if(input$data=="Univariate" & input$mod=="F2"){
        print("F2U")
      } else if(input$data=="Univariate" & input$mod=="F3"){
        print("F3U")
      } else
        print("F4U"),
      "F1J" = gofF1M,
      "F2J" = gofF2M,
      "F3J" = gofF3M,
      "F4J" = gofF4M,
      "F1U" = gofF1U,
      "F2U" = gofF2U,
      "F3U" = gofF3U,
      "F4U" = gofF4U)
  })
})