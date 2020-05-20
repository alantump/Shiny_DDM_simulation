

  ui=fluidPage(    shinyjs::useShinyjs(),
                    headerPanel("Drift diffusion parameters"),
                   sidebarPanel(sliderInput("ndt_value", label = "Non decision time", min=0.1,max=1, value=0.1, step=0.1),
                                sliderInput("bias_value", label = "Bias", min=0.1,max=0.9, value=0.5, step=0.05),
                   sliderInput("drift_value", label = "Drift", min=-2,max=2, value=0, step=0.1),
                   sliderInput("bs_value", label = "Boundary seperation", min=0.1,max=5, value=2, step=0.2),
                   sliderInput("n_agents", label = "Simulated agents", min=1,max=50, value=10, step=2)),
  #textInput ("value2", label = HTML("<br /><br /><br /><br /><br /> <br /> Bitte nichts eingeben:"))),
                     mainPanel(plotOutput(outputId = "distPlot")))
  
                       
  server=function(input, output, session) {

        
    
    output$distPlot <- renderPlot({
   
      rts = seq(0,22,0.001)
      up=dwiener(rts,as.numeric(input$bs_value),as.numeric(input$ndt_value),as.numeric(input$bias_value),as.numeric(input$drift_value), resp="upper")
      low=dwiener(rts,as.numeric(input$bs_value),as.numeric(input$ndt_value),as.numeric(input$bias_value),as.numeric(input$drift_value), resp="lower")
      
      test=rwiener(1000,as.numeric(input$bs_value),as.numeric(input$ndt_value),as.numeric(input$bias_value),as.numeric(input$drift_value))
      mean(test$resp=="upper")
      sum(up/sum(up))
      
      line_data=data.frame(x=c(sum(rts*(up/(sum(up)))),sum(rts*(up/(sum(up)))),sum(rts*(low/(sum(low)))),sum(rts*(low/(sum(low))))),
                                   y=c(0,max(up),0,(-max(low))),
                                   type=c("up","up","low","low"))
      
      p1 = rbind(data.frame(rts,y=up,type="up"),data.frame(rts,y=(-low),type="low")) %>% ggplot() + geom_line(aes(x=rts,y=y,color=type),size=2)+
        xlim(0,max(c(rts[up>0.005],rts[low>0.005]))) + ylim(-max(low),max(up)) + xlab("RTs") + ylab("Densitiy and means as vertical lines") + geom_hline(yintercept = 0,linetype="dashed") +
        geom_line(data=line_data,aes(x=x,y=y,color=type),size=1.5)+ scale_color_discrete(name="Choice",labels=c("Correct", "Wrong")) + theme( legend.position=c(0.7,.2)) 
     
       p2 = data.frame(y=(up/(low+up)),x=rts) %>% ggplot() + geom_line(aes(y=y,x=x),size=2) +  xlab("RTs") + ylab("Proportion correct choices \n (over time and overall)") +
        geom_hline(yintercept = sum(up)/(sum(low)+sum(up)),size=1.5) + geom_hline(yintercept = 0.5,linetype="dashed") +   xlim(0,max(c(rts[up>0.005],rts[low>0.005]))) 
      
      #plot_grid(p1,p2,nrow=1)
      
       
       #random walk model with time
       nreps <- as.numeric(input$n_agents)
       nsamples <- 1000 * as.numeric(input$bs_value)
       
       drift <- as.numeric(input$drift_value) #noninformative stimulus
       sdrw <- 1 #standard deviation
       criterion <- as.numeric(input$bs_value) /2 #treshold
       initial_bias = criterion*2*(as.numeric(input$bias_value)-0.5)
       h=0.01
       
       latencies <- rep(0 , nreps )
       responses <- rep(0 , nreps )
       evidence <- matrix(0 , nreps , nsamples+1)
       for(i in c( 1 : nreps ) ) { 
         evidence [ i , ] <-   cumsum( c(initial_bias , rnorm( nsamples , drift*h , (sdrw*h^0.5) ) ) )
         p <- which( abs(evidence[ i , ] )>criterion ) [1]
         responses [ i ] <- sign( evidence [ i , p ] )
         latencies[ i ] <- p
       }
       
       
       
             
             latencies <- rep(0 , nreps )
             responses <- rep(0 , nreps )
             evidence <- matrix(0 , nreps , nsamples+1)
             for(i in c( 1 : nreps ) ) { 
               evidence [ i , ] <-   cumsum( c(initial_bias , rnorm( nsamples , drift*h , (sdrw*h^0.5) ) ) )
               p <- which( abs(evidence[ i , ] )>criterion ) [1]
               responses [ i ] <- sign( evidence [ i , p ] )
               latencies[ i ] <- p
             }
             
             
             
             tbpn <- min(nreps )
             evidence2 <- matrix(NA , nreps , nsamples+1)
             for(i in c( 1 : tbpn ) ) { 
               evidence2[i, 1 : ( latencies[ i]) ]<- evidence[ i ,1 : ( latencies[ i]) ]  } 

             
             
             
             #ggplot needs a dataframe
             data <- data.frame(evidence= t(evidence2[  ,1 : max( latencies) ]))
             data$time = seq(h,(max( latencies))*h,h) + as.numeric(input$ndt_value)
             
             #id variable for position in matrix 
             #data$id <- 1:nrow(data) 
             #reshape to long format
             plot_data=tidyr::gather(data, "id","value",1:nreps)
             #plot
             p3 = ggplot(plot_data, aes(x=time,y=value,colour=factor(id))) +
               geom_line(size=0.9) + geom_hline(yintercept = c( criterion, -criterion )) +  theme( legend.position="none")
             
             plot_grid(plot_grid(p1,p2),p3,nrow=2)
             
             
           }, height = 300*2, width = 300*2)
           
           
         }
       
       
       
       
       
  
       
       
       
  
  

  
  
  
  
