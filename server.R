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
reF1U <- read.csv("data/reF1U.csv",header=F)
reF2U <- read.csv("data/reF2U.csv",header=F)
reF3U <- read.csv("data/reF3U.csv",header=F)
reF4U <- read.csv("data/reF4U.csv",header=F)
reF1M <- read.csv("data/reF1M.csv",header=F)
reF2M <- read.csv("data/reF2M.csv",header=F)
reF3M <- read.csv("data/reF3M.csv",header=F)
reF4M <- read.csv("data/reF4M.csv",header=F)
LBnew2=read.csv("data/LBCa.csv")
OPnew2=read.csv("data/OCPCa.csv")
melnew2=read.csv("data/MCaS.csv")
geobugsSC<-readSplus("data/SC_geobugsSPlus.txt")

shinyServer(function(input, output) {
  output$text <- renderText({
    paste("You have selected a",ifelse(input$data=="Univariate",
      "univariate model that does not offer shared effects between diseases",
      "multivariate model that offers shared effects between diseases"),"with a",
          ifelse(input$mod=="F1","spatially uncorrelated",ifelse(input$mod=="F2","spatially correlated",
                ifelse(input$mod=="F3","spatially correlated and temporally uncorrelated",
                      "spatially and temporally correlated"))),"mixture parameter. The results displayed
      below include: ",ifelse(input$which=="Summary","a bar plot of cancer incidences over the study time",
                              ifelse(input$which=="Mixture Parameter",
      "a plot of the mixture parameter estimates","a plot of the random effect estimates")),
      "and a table of goodness of fit measures.")
  })
  output$map <-renderPlot({
	  if(input$which=="Mixture Parameter"){
    data <- switch(
	    if(input$data=="Multivariate" & input$mod=="F1"){
	      "F1J"
	    } else if(input$data=="Multivariate" & input$mod=="F2"){
	      "F2J"
	    } else if(input$data=="Multivariate" & input$mod=="F3"){
	      "F3J"
	    } else if(input$data=="Multivariate" & input$mod=="F4"){
	      "F4J"
	    } else if(input$data=="Univariate" & input$mod=="F1"){
	      "F1U"
	    } else if(input$data=="Univariate" & input$mod=="F2"){
	      "F2U"
	    } else if(input$data=="Univariate" & input$mod=="F3"){
	      "F3U"
	    } else
	      "F4U",
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
		  title(ylab="LBCa",cex.lab=1.5)
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
		         horiz=T,cex=1.5,x.intersp=1,xjust=0.5,yjust=0.5,text.width =rep(.1,5))		
		}	else{
		  par(mar=c(0.1,5,2,0))
      fillmaps(geobugsSC,c("OCPCa","MCaS","LBCa"),data,main.line = 0,
               leg.common = T,lay.m = matrix(1:4,2,2,T),leg.loc = "center",n.col=5)
		}
	  } else if(input$which=="Summary"){
      data<-rbind(colSums(OPnew2),colSums(melnew2),colSums(LBnew2))
      par(mar=c(0.5,1.9,0,0))
      layout(m=matrix(1:2,nrow=2,ncol=1),heights=c(.8,.2))
      barplot(data,xlab="Year",axisnames=F,
              ylab="Incidence",main="",col=c("white","grey","black"))#,xlim=c(0,18.5))
      axis(side=1,at=seq(.7,16.3,1.2),as.character(1996:2009))
      par(mar=c(0,0,0,0))
      plot(1, type = "n", axes=FALSE, xlab="", ylab="")
      legend("center",legend=c("OCPCa","MCaS","LBCa"),fill=c("white","grey","black"),
             horiz=T,bty="n",cex=1)    
	  } else {
	    data <- switch(
	      if(input$data=="Multivariate" & input$mod=="F1"){
	        "F1J"
	      } else if(input$data=="Multivariate" & input$mod=="F2"){
	        "F2J"
	      } else if(input$data=="Multivariate" & input$mod=="F3"){
	        "F3J"
	      } else if(input$data=="Multivariate" & input$mod=="F4"){
	        "F4J"
	      } else if(input$data=="Univariate" & input$mod=="F1"){
	        "F1U"
	      } else if(input$data=="Univariate" & input$mod=="F2"){
	        "F2U"
	      } else if(input$data=="Univariate" & input$mod=="F3"){
	        "F3U"
	      } else
	        "F4U",
	      "F1J" = reF1M,
	      "F2J" = reF2M,
	      "F3J" = reF3M,
	      "F4J" = reF4M,
	      "F1U" = reF1U,
	      "F2U" = reF2U,
	      "F3U" = reF3U,
	      "F4U" = reF4U)
      if(input$data=="Multivariate"){
             par(mar=c(0,2.2,2,0))
             cut=quantile(as.vector(as.matrix(data[1:4,])),c(0,.25,.4,.6,.75,1))
             layout(m=matrix(c(1:3,5,4,4,4,5,6,6,6,6,7,7,7,7),ncol=4,nrow=4,byrow=T),heights=c(.3,.3,.3,.1),widths=c(.275,.275,.275,.175))
             fillmap(geobugsSC,"OCPCa",data[1,],n.col=5,bk="c",
                     cuts=cut,
                     legendtxt=NA,main.line = 0)
             title(ylab="u",cex.lab=1.5,line=-.5)
             fillmap(geobugsSC,"MCaS",data[2,],n.col=5,bk="c",
                     cuts=cut,
                     legendtxt=NA,main.line = 0)
             fillmap(geobugsSC,"LBCa",data[3,],n.col=5,bk="c",
                     cuts=cut,
                     legendtxt=NA,main.line = 0)
             fillmap(geobugsSC,"Shared",data[4,],n.col=5,bk="c",
                     cuts=cut,
                     legendtxt=NA,main.line = 0)
             title(ylab="v",cex.lab=1.5,line=-.5)
             par(mar=c(0,0,0,0))
             plot(1, type = "n", axes=FALSE, xlab="", ylab="")
             n.col=5
             br <- round(cut,2)
             leg.txt<-paste("[",br[n.col],",",br[n.col+1],"]",sep="")
             for(i in (n.col-1):1){ 
               leg.txt<-append(leg.txt,paste("[",br[i],",",br[i+1],")",sep="")) }
             leg.txt<-rev(leg.txt)
             shading<-gray((n.col-1):0/(n.col-1))
             legend(x="left",legend=leg.txt,bty="n",fill=shading,inset = 0,ncol=1,
                    cex=1.5,x.intersp=1,xjust=0.5,yjust=0.5,text.width =rep(.1,5))
             #gam
             par(mar=c(1, 3, 4, 2) + 0.1)
             plot(1996:2009,data[5,1:14],type="l",lty=4,ylab="",
                  main=expression(gamma),xlab="",cex.main=2)
             par(mar=c(0,0,0,0))
             plot(1, type = "n", axes=FALSE, xlab="", ylab="")
             legend(x="center",legend=c("Shared"),
                    lty=4,horiz=T, bty="n")
      } else {
        par(mar=c(0,2.2,2,0))
        cut=quantile(as.vector(as.matrix(data[c(1:4,6,7),])),c(0,.25,.4,.6,.75,1))
        layout(m=matrix(c(1:3,7,4:6,7,8,8,8,8,9,9,9,9),ncol=4,nrow=4,byrow=T),heights=c(.3,.3,.3,.1),widths=c(.275,.275,.275,.175))
        fillmap(geobugsSC,"OCPCa",data[1,],n.col=5,bk="c",
                cuts=cut,
                legendtxt=NA,main.line = 0)
        title(ylab="u",cex.lab=1.5,line=-.5)
        fillmap(geobugsSC,"MCaS",data[2,],n.col=5,bk="c",
                cuts=cut,
                legendtxt=NA,main.line = 0)
        fillmap(geobugsSC,"LBCa",data[3,],n.col=5,bk="c",
                cuts=cut,
                legendtxt=NA,main.line = 0)
        fillmap(geobugsSC,"",data[4,],n.col=5,bk="c",
                cuts=cut,
                legendtxt=NA,main.line = 0)
        title(ylab="v",cex.lab=1.5,line=-.5)
        fillmap(geobugsSC,"",data[6,],n.col=5,bk="c",
                cuts=cut,
                legendtxt=NA,main.line = 0)
        fillmap(geobugsSC,"",data[7,],n.col=5,bk="c",
                cuts=cut,
                legendtxt=NA,main.line = 0)
        par(mar=c(0,0,0,0))
        plot(1, type = "n", axes=FALSE, xlab="", ylab="")
        n.col=5
        br <- round(cut,2)
        leg.txt<-paste("[",br[n.col],",",br[n.col+1],"]",sep="")
        for(i in (n.col-1):1){ 
          leg.txt<-append(leg.txt,paste("[",br[i],",",br[i+1],")",sep="")) }
        leg.txt<-rev(leg.txt)
        shading<-gray((n.col-1):0/(n.col-1))
        legend(x="left",legend=leg.txt,bty="n",fill=shading,inset = 0,ncol=1,
               cex=1.5,x.intersp=1,xjust=0.5,yjust=0.5,text.width =rep(.1,5))
        #gam
        par(mar=c(1, 3, 4, 2) + 0.1)
        plot(1996:2009,data[5,1:14],type="l",lty=1,ylab="",
             ylim=c(min(data[c(5,8,9),1:14]),max(data[c(5,8,9),1:14])),
             main=expression(gamma),xlab="",cex.main=2)
        lines(1996:2009,data[8,1:14],type="l",lty=2)
        lines(1996:2009,data[9,1:14],type="l",lty=3)
        par(mar=c(0,0,0,0))
        plot(1, type = "n", axes=FALSE, xlab="", ylab="")
        legend(x="center",legend=c("OCPCa","MCaS","LBCa"),
               lty=1:3,horiz=T, bty="n")
	    }}
    })
  output$tab <-renderTable({
    data <- switch(
      if(input$data=="Multivariate" & input$mod=="F1"){
        "F1J"
      } else if(input$data=="Multivariate" & input$mod=="F2"){
        "F2J"
      } else if(input$data=="Multivariate" & input$mod=="F3"){
        "F3J"
      } else if(input$data=="Multivariate" & input$mod=="F4"){
        "F4J"
      } else if(input$data=="Univariate" & input$mod=="F1"){
        "F1U"
      } else if(input$data=="Univariate" & input$mod=="F2"){
        "F2U"
      } else if(input$data=="Univariate" & input$mod=="F3"){
        "F3U"
      } else
        "F4U",
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