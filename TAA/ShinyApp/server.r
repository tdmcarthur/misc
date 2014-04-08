



library(shiny)
library(ggplot2)
library(MASS)


 historical.df<-read.csv("~/ShinyApps/ShinyApp/HistoricalWageData.csv")



  
  historical.df$full.time.wage <- historical.df$full.time.wage * (historical.df$midwest.cpi[historical.df$year==2013]/historical.df$midwest.cpi)

  historical.df$seg.fee <- historical.df$seg.fee * (historical.df$midwest.cpi[historical.df$year==2013]/historical.df$midwest.cpi)

  historical.df<-historical.df[historical.df$year!=2013, ]
  
  fee.by.credits.v <- c(119.28,
    182.96,
    246.64,
    310.32,
    374.00,
    437.68,
    501.36,
    565.04)
  
rec.sports.increase <- c(0,
0,
0,
8,
8,
8,
97,
97,
102,
102,
102,
102,
102,
102,
102,
102,
102,
102,
102,
102,
102)

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output, session) {
  
#    splashTimer <- reactiveTimer(10000, session)
 
#  observe({
    # Invalidate and re-execute this reactive expression every time the
    # timer fires.
#    splashTimer()
    # Do something each time this is invalidated.
    # The isolate() makes this observer _not_ get invalidated and re-executed
    # when input$n changes.
#  })
  
  
 # output$splashplot <- renderUI({FALSE})
  
#  output$splashplot <- renderUI({
#    splashTimer()
#    TRUE
#  })
  
  
  
  fee.base <-  reactive( fee.by.credits.v[min(input$credits, 8)] )
  
  hist.seg.fee <- reactive( historical.df$seg.fee * fee.base()/max(fee.by.credits.v) )

  
#  fee.series <-  reactive( fee.base() * (1 + input$seg.growth/100)^(0:(as.numeric(input$end.year)-2013)) ) 
  
fee.series <-  reactive( if (input$recsports) {
  (fee.base() + rec.sports.increase[1:(as.numeric(input$end.year)-2012)] * (fee.base()/max(fee.by.credits.v)) ) *
    (1 + input$seg.growth/100)^(0:(as.numeric(input$end.year)-2013))  
  } else {
  fee.base() * (1 + input$seg.growth/100)^(0:(as.numeric(input$end.year)-2013)) 
  } 
) 
  
  hist.take.home.series.no.seg <- reactive( (input$appt.pct/100) * historical.df$full.time.wage * (6/12)  )
  
  hist.take.home.series <- reactive( (input$appt.pct/100) * historical.df$full.time.wage * (6/12) - hist.seg.fee()  )
  
  take.home.series.no.seg <-reactive( ((input$appt.pct/100) * 29492 * (6/12)  )/(1 + input$inflation/100)^(0:(as.numeric(input$end.year)-2013))  )
  
  take.home.series <- reactive( ((input$appt.pct/100) * 29492 * (6/12) - fee.series())/(1 + input$inflation/100)^(0:(as.numeric(input$end.year)-2013))  )
  
  take.home.series.no.seg <-reactive( ((input$appt.pct/100) * 29492 * (6/12)  )/(1 + input$inflation/100)^(0:(as.numeric(input$end.year)-2013))  )
  
  # assume 4-month semester
  # actually, I am confused about this, so new value is 6/12
  # from http://www.ohr.wisc.edu/polproced/utg/SalRng.html#stuasst
  # and http://www.ohr.wisc.edu/polproced/utg/StuAsstApptT.html
  
  take.home.series.no.seg.f<-reactive( c(hist.take.home.series.no.seg(), take.home.series.no.seg()) )
  take.home.series.f <- reactive( c(hist.take.home.series(), take.home.series()))
  
  
  
#  take.home.series <- reactive(  take.home.series()/(1 + inflation/100)^(0:(as.numeric(input$end.year)-2013))     )
  
#  cat(take.home.series())
  
#  zero.y.lim<-reactive({input$zero.y.lim})
  
#  if (zero.y.lim()) {
#    y.lim.graph<- c(0, max(take.home.series()))
#    } else {
#      y.lim.graph<-range(take.home.series())
#  }
  
#  cat(isolate(take.home.series.no.seg()))
  
#print(isolate(data.frame(x=2013:input$end.year, y=take.home.series(), z=take.home.series.no.seg())))
  
  output$take.home.plot <- renderPlot({ 
    if (input$tabledisplay) {
      NULL
    } else {
    
    ylim.render <- c(min(min(take.home.series.f(), 0[input$zero.y.lim | input$seg.line])), max(c(take.home.series.no.seg.f(),take.home.series.f())))
  
    
#    plot(x=2013:input$end.year, y=take.home.series(), type="b", 
#      main="Real take-home pay per semester",
#      yaxt="n",
#      ylab="",
#      xlab="",
#      ylim=ylim.render
#    )
    
    ticks <- pretty(ylim.render)
    labels <- format(ticks, big.mark=",", scientific=FALSE)
 #   axis(2, at = ticks, labels = paste0("$", labels), las = 1, cex.axis=0.9) 
      
    theme_set(theme_gray(base_size = 18))
      
    data.df <-  data.frame(x=2002:input$end.year, y=take.home.series.f(), z=take.home.series.no.seg.f())
      
      expl<-rep("red", length(2002:input$end.year))
      expl[(2002:input$end.year)>2013]<-"blue"
      expl.no.fee<- rep("green", length(2002:input$end.year))
      expl.no.fee[(2002:input$end.year)>2013]<-"orange"
      
    
      end.plot<-ggplot( data=data.frame(x=2002:input$end.year, y=take.home.series.f(), z=take.home.series.no.seg.f(), expl=expl, expl.no.fee=expl.no.fee), 
      aes(x=x, y=y, z=z), xlab="", ylab="" )  +  # , group=x>2013
        geom_line(aes(x=x, y=y, colour = expl), size=2) +  
        geom_line(aes(x=x, y=z, colour = expl.no.fee), size=2) +
        geom_point(aes(x=x, y = y), size=2) + 
        geom_point(aes(x=x, y = z), size=2) +
        xlab("") +
        ylab("") +
        ggtitle("Grad student real take-home pay per semester (2013 dollars)") +
        scale_y_continuous(limits=c(ylim.render), breaks=ticks, labels=paste0("$", labels)) +
        theme(legend.title=element_blank(), legend.position="top") +
        scale_colour_discrete(name  ="",   breaks=c("green", "red", "orange",  "blue", "seg.line" ), labels=c("Gross pay", "Pay minus seg fees", "Full seg fee offset (projection)", "No action (projection)", "Seg Fees"  )) 

    if ( input$seg.line ) {
      end.plot <- end.plot + geom_line(aes(x=x, y=z-y, colour ="seg.line", size=2)) + geom_point(aes(x=x, y = z-y), size=2) 
    }
  
    print( end.plot )
      
    }
  
  })
  
  output$take.home.table <- renderTable({
    if (input$tabledisplay) {
      take.home.pay.df <- data.frame(
      With.Seg.Fees=paste0( "$", format(round(take.home.series.f()),big.mark=",", scientific=FALSE)),
      Without.Seg.Fees=paste0( "$", format(round(take.home.series.no.seg.f()),big.mark=",", scientific=FALSE)),
      Seg.Fees=paste0( "$", format(round(take.home.series.no.seg.f()-take.home.series.f()),big.mark=",", scientific=FALSE))
      )
      
      rownames(take.home.pay.df) <- 2002:input$end.year
      take.home.pay.df
    } else {
      NULL
    }
    
  })
  

  
#     output$distPlot <- renderPlot({
# 
#     # generate an rnorm distribution and plot it
#     dist <- mvrnorm(n = input$obs, mu=c(0,0), Sigma=matrix(c(10,as.numeric(input$correlation)*10,as.numeric(input$correlation)*10,10),2,2))
#     plot(dist, main="More cool graphs", col="red", ylab="Betterness", xlab="Grad student pay", cex.lab=1.5,asp=1)
#   }, width=400, height=400)
#   
#       output$rchartsPlot <- renderChart({
# 
#         data(economics, package = "ggplot2")
#         econ <- transform(economics, date = as.character(date))
#         m1 <- mPlot(x = "date", y = c("psavert", "uempmed"), type = "Line", data = econ)
#         m1$set(pointSize = 0, lineWidth = 1)
# #        m1$print("chart2")
#         m1$addParams(dom = 'rchartsPlot')
#         return(m1)
#         
#   })
  
#  peers.df<-data.frame(school=c("UW", "really long nameeeeeeeeee", letters[1:9]), pay=runif(11), deflator=runif(11), fees=runif(11)/10, stringsAsFactors=FALSE)
  
  
  peers.df<-read.csv("~/ShinyApps/ShinyApp/PeerData.csv")

  
  
#    peer.pay.display<-reactive( ifelse(input$PeerCOLA & input$PeerFees, 
#      (peers.df$pay-peers.df$fees)/peers.df$deflator,
#      peers.df$pay
#      ) )

#    peer.pay.display<-reactive( ifelse(input$PeerCOLA & !input$PeerFees, 
#      (peers.df$pay)/peers.df$deflator,
#      peers.df$pay
#      ) )
      
#    peer.pay.display<-reactive( ifelse(!input$PeerCOLA & input$PeerFees, 
#      (peers.df$pay-peers.df$fees),
#      peers.df$pay
#      ) )
  
        peer.pay.display<-reactive( switch(paste(c(input$PeerCOLA,input$PeerFees), collapse=""), 
          TRUETRUE = (peers.df$pay*(input$appt.pct.peer/100)-peers.df$fees)/peers.df$deflator,
          TRUEFALSE = (peers.df$pay*(input$appt.pct.peer/100))/peers.df$deflator,
          FALSETRUE = (peers.df$pay*(input$appt.pct.peer/100)-peers.df$fees),
          FALSEFALSE = peers.df$pay*(input$appt.pct.peer/100)
        ))


  
    take.home.series.no.seg.f<-reactive( c(hist.take.home.series.no.seg(), take.home.series.no.seg()) )
  # TODO: Why is this line postitioned all the way down here?
  

  
   output$competitivenessPlot <- renderPlot({
     
     df<-data.frame(school=factor(peers.df$school, levels=peers.df$school[order(peer.pay.display())]), peer.pay.display=peer.pay.display())[(!input$PeerGroup=="big10" | peers.df$big10) & (!input$PeerGroup=="salaryPeers" | peers.df$salary.peer), ]
     
     ylim.render <- c(min(min(df$peer.pay.display, 0[input$PeerBarplot])), max(df$peer.pay.display))
     
     ticks <- pretty(ylim.render)
     
     labels <- format(ticks, big.mark=",", scientific=FALSE)

     
     a <- ggplot(df, 
       aes(x = school, y = peer.pay.display)) + 
     labs(title = "Teaching assistant earnings comparison with UW peers", x = NULL, y = NULL, fill = NULL)
     if (input$PeerBarplot) {b <- a + geom_bar(stat="identity", fill=ifelse(df$school[order(df$peer.pay.display)]=="UW-Madison", "red", "black")) + coord_flip() + scale_y_continuous(limits=ylim.render, breaks=ticks, labels=paste0("$", labels))
     } else {
     b <- a +  geom_point(stat="identity", size=7, colour=ifelse(df$school=="UW-Madison", "red", "black"))  + coord_flip() + scale_y_continuous(limits=ylim.render, breaks=ticks, labels=paste0("$", labels))
     }
     
     print(b)
     
     
   }) 
  
# peers.df$school[order(peer.pay.display())]
  # ifelse(school=="UW", "red", "black")
  
  # ifelse(peers.df$school[order(peer.pay.display())][(!input$PeerGroup=="big10" | peers.df$big10) & (!input$PeerGroup=="salaryPeers" | peers.df$salary.peer) ]=="UW", "red", "black")
  
  output$expensesPlot <- renderPlot({
  expenses.df <-  
  data.frame( values=(fee.by.credits.v[min(input$credits2, 8)]*2)/c(input$housing, input$groceries, input$clothing, input$transportation),
  categories=factor(c("Months of Housing", "Months of Groceries", "Months of Clothing", "Months of Transportation"), levels = c("Months of Housing", "Months of Groceries", "Months of Clothing", "Months of Transportation")) )
  
    
  discrete.lines <- data.frame(vals=1:max(ceiling(expenses.df$values*1.15)))
  expenses.df$offset <- -1

  aaa <- ggplot(expenses.df, aes(x=categories, y=values, fill=categories)) +
    geom_bar(stat="identity") +
      geom_hline(data=discrete.lines, aes(yintercept=vals), colour="white") +
      geom_text(aes(label = round(values, digits=1), vjust=offset)) +
      ggtitle("Seg fees consume months of grad student expenses") +
        opts(axis.title.x = theme_blank(),axis.title.y = theme_blank(), legend.position="none")  +
      scale_y_continuous(limits = c(0, max(expenses.df$values)*1.15))
        #, position=data.frame(h=1,w=0)) #
  
  print(aaa)
  })
  

})


#  ggplot( data=data.frame(x=2013:2020, y=1:length(2013:2020) ), 
#      aes(x=x, y=y), xlab="", ylab="" )  + geom_line() + geom_point() + scale_y_continuous(limits#=c(1,20), breaks=1:length(2013:2021), labels=paste0("$", 1:length(2013:2021))) 


#, ymin = ylim.render[1], ymax = ylim.render[2]

#ggplot(test_data, aes(date)) + 

# TODO: vertical line to denote projection starts. Connect the two lines

