#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#



library(shiny)
library(data.table)
library(ggplot2)
library(scales)


lseqBy <- function(from=1, to=100000, by=1, length.out=log10(to/from)+1) {
  tmp <- exp(seq(log(from), log(to), length.out = length.out))
  tmp[seq(1, length(tmp), by)]  
}

limiter <- function(sample_a="", sample_b=""){
  plotmin <- min(as.numeric(df_t[c(1:28), sample_a]),as.numeric(df_t[c(1:28),sample_b]))
  plotmax <- max(as.numeric(df_t[c(1:28), sample_a]),as.numeric(df_t[c(1:28),sample_b]))
  return(list(min=plotmin,max=plotmax))
}

regliner <- function(fold=2, min=0, max=1000){
  logline <- data.frame(x=c(min,max),y=c(min,max))
  logline2 <- data.frame(x=c(min,max),y=c(min*fold,max*fold))
  logline3 <- data.frame(x=c(min,max),y=c(min*(1/fold),max*(1/fold)))
  return(list(center=logline, upper=logline2, lower=logline3))
}

breakers <- function(breaks=11, min_l, max_l){
  x <- lseqBy(1,10^10,1)
  s <- lseqBy(from=x[which.min(abs(x-min_l))],to=x[which.max(abs(x-max_l))],by = 1)
  f <- log10(s)
  k <- seq(min(f),max(f),length.out=breaks)
  l <- c()
  for (x in c(1:length(k))){
    l <- c(l,10^k[x])
  }
  return(l)
}


df <- read.csv(url("https://www.dropbox.com/s/d85am7equi7ka5r/Stat_data_full.csv?dl=1"))
df_t <- data.table::transpose(df)
c.names <- df[["X"]]

for (x in c(1:length(c.names))){
  c.names[x] <- paste("Sample", c.names[x], collapse = ",")
}

names(df_t) <- c.names
df_t <- df_t[-1,]
rownames(df_t) <- names(df[,2:length(df)])

area_level <- levels(df$Terulet)
species_level <- levels(df$Fajta)
plant_tissue <- levels(df$Nov_szerv)

group_vars <- list(Terulet=area_level, Fajta=species_level, Nov_szerv=plant_tissue)

grouper <- function(grouping='Terulet', group='Szomod'){
  l <- rowMeans(sapply(df_t[1:32,df_t[grouping,]==group], as.numeric))
  return(l)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$ScatterPlot <- renderPlot({
    #print(input$x)
    #print(input$y)
    #print(input$group)
    ggplot(df,aes(df[[input$x]], df[[input$y]], color=df[[input$group]])) + 
      geom_point(size=5)+
      scale_color_discrete()+
      guides(colour=guide_legend(title=input$Group, ncol=2))+
      xlab(input$x)+
      ylab(input$y)+
      labs(colour = input$Group)+
      #scale_color_manual(values = c('#999999','#E69F00')) + 
      theme(legend.position=c(0,1), legend.justification=c(0,1))
    
    
    
    
  })
  
  output$ScatterPlot2 <- renderPlot({
    limits <- limiter(input$sample_a, input$sample_b)
    #print(limits)
    reglines <- regliner(input$fold, limits$min, limits$max)
    breaker <- breakers(11, limits$min, limits$max)
    
    ggplot(df_t[c(1:28),],aes(as.numeric(df_t[c(1:28),input$sample_a]), as.numeric(df_t[c(1:28),input$sample_b]))) + 
      geom_point(size=3) + 
      geom_line(aes(x=x, y=y), data = reglines$center, col='blue')+
      geom_line(aes(x=x, y=y), data = reglines$upper, col='red')+
      geom_line(aes(x=x, y=y), data = reglines$lower, col='red')+
      scale_x_continuous(trans = log_trans(),
                         breaks = breaker,
                         labels = function(x) format(x, scientific = TRUE, digits = 2),
                         limits = c(limits$min,limits$max)
      )+
      scale_y_continuous(trans = log_trans(),
                         breaks = breaker,
                         labels = function(x) format(x, scientific = TRUE, digits = 2),
                         limits = c(limits$min*(1/input$fold),limits$max*input$fold)
      )+
      xlab(paste("Concentration in ", input$sample_a))+
      ylab(paste("Concentration in ", input$sample_b))
    
  })
  
  output$grouping = renderUI({
    mydata = input$grouping_var
    tagList(
    selectInput('grouping_var2', 'Group A', group_vars[mydata]),
    selectInput('grouping_var3', 'Group B', group_vars[mydata]))
  })
  
  dataframe <- reactive({
    vars <- group_vars[[input$grouping_var]]
    df_t_temp <- data.frame(matrix(0, nrow = length(df_t[,1]), ncol = length(vars)))
    for (y in 1:length(df_t_temp)){
      for (x in 1:length(df_t[,1])){
        df_t_temp[x,y] <- if (is.na(mean(suppressWarnings(as.numeric(df_t[x,df_t[input$grouping_var,]==vars[y]]))))){0} 
        else{mean(as.numeric(df_t[x,df_t[input$grouping_var,]==vars[y]]))}
      }
    }
    rownames(df_t_temp) <- names(df[,2:length(df)])
    names(df_t_temp) <- vars
    df_t_temp
  })
  
  dataplot <- reactive({
    req(input$grouping_var, input$grouping_var2, input$grouping_var3)
    
    vars <- group_vars[[input$grouping_var]]
    #print(group_vars[[input$grouping_var]])
    
    #df_t_temp <- data.frame(matrix(0, nrow = length(df_t[,1]), ncol = length(vars)))
    df_t_temp <- dataframe()
    
    for (y in 1:length(df_t_temp)){
      for (x in 1:length(df_t[,1])){
        df_t_temp[x,y] <- if (is.na(mean(suppressWarnings(as.numeric(df_t[x,df_t[input$grouping_var,]==vars[y]]))))){0} 
        else{mean(as.numeric(df_t[x,df_t[input$grouping_var,]==vars[y]]))}
      }
    }
    
    rownames(df_t_temp) <- names(df[,2:length(df)])
    names(df_t_temp) <- vars
    
    #print(df_t_temp)
    
    #print(df_t_temp[input$grouping_var2])
    
    limits <- list(min=min(df_t_temp[1:32,]), max=max(df_t_temp[1:32,]))
    #print(limits)
    reglines <- regliner(input$fold, limits$min, limits$max)
    breaker <- breakers(11, limits$min, limits$max)

    #cat(file = stderr(), length(df_t_temp[1:32,1]), " ", length(df_t_temp[1:32,input$grouping_var2]))
    
    req(length(df_t_temp[1:32,1])==length(df_t_temp[1:32,input$grouping_var2]))
    ggplot(df_t_temp[1:32,],aes(df_t_temp[1:32,input$grouping_var2], df_t_temp[1:32,input$grouping_var3]))+
      
      geom_point(size=3) + 
      geom_line(aes(x=x, y=y), data = reglines$center, col='blue')+
      geom_line(aes(x=x, y=y), data = reglines$upper, col='red')+
      geom_line(aes(x=x, y=y), data = reglines$lower, col='red')+
      scale_x_continuous(trans = log_trans(),
                         breaks = breaker,
                         labels = function(x) format(x, scientific = TRUE, digits = 2),
                         limits = c(limits$min,limits$max)
      )+
      scale_y_continuous(trans = log_trans(),
                         breaks = breaker,
                         labels = function(x) format(x, scientific = TRUE, digits = 2),
                         limits = c(limits$min*(1/input$fold),limits$max*input$fold)
      )+
      xlab(paste("Group: ",input$grouping_var2))+
      ylab(paste("Group: ",input$grouping_var3))
  })
  

  
  output$ScatterPlot3 <- renderPlot({
    dataplot()
    
  })
  
  output$hover_info <- renderUI({
    req(input$plot_hover)
    
    verbatimTextOutput("vals")
    #hover <- input$plot_hover
    #print(hover)
    #point <- nearPoints(df_t_temp[1:32,], hover, threshold = 5, maxpoints = 1, addDist = TRUE, xvar=input$grouping_var2, yvar=input$grouping_var3)
    #if (nrow(point) == 0) return(NULL)
  })
  

  output$vals <- renderPrint({
      req(input$plot_hover)
      df_t_temp <- dataframe()
      hover <- input$plot_hover 
      #print(str(hover)) # list
      point <- nearPoints(df_t_temp[1:32,], 
                          hover, 
                          threshold = 5, 
                          maxpoints = 1, 
                          addDist = FALSE, 
                          xvar=input$grouping_var2, 
                          yvar=input$grouping_var3)[, c(input$grouping_var2,input$grouping_var3)]
      req(nrow(point) != 0)
      point
    })

    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    ##left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    ##top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    ##left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    ##top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    ##style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
    ##                "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    # actual tooltip created as wellPanel
    ##wellPanel(
    ##  style = style,
    ##  p(HTML(paste0("<b> Car: </b>", rownames(point), "<br/>")))
    ##)
  ##})
})
