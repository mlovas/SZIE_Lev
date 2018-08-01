#
# This is the user-interface definition of the Shiny web application. The main purpose of the application
# is the evalutation of a chromatographic dataset, which consist peak areas of many variables measured 
# in different Lavender samples.  
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


library(shiny)
library(ggplot2)

df <- read.csv(url("https://www.dropbox.com/s/d85am7equi7ka5r/Stat_data_full.csv?dl=1"))
df_t <- data.table::transpose(df)
c.names <- df[["X"]]

area_level <- levels(df$Terulet)
species_level <- levels(df$Fajta)
plant_tissue <- levels(df$Nov_szerv)

group_vars <- list(Terulet=area_level, Fajta=species_level, Nov_szerv=plant_tissue)

for (x in c(1:length(c.names))){
  c.names[x] <- paste("Sample", c.names[x], collapse = ",")
}

names(df_t) <- c.names
df_t <- df_t[-1,]
rownames(df_t) <- names(df[,2:length(df)])

# Define UI for application that draws a histogram
shinyUI(navbarPage("Untargeted screening: Data evaluation",
                   tabPanel("Variable Analysis",
                            fluidPage(
                              # Application title
                              titlePanel("Scatter plot (Variable vs. Variable)"),
                              
                              # Sidebar with a slider input for number of bins 
                              sidebarLayout(
                                sidebarPanel(
                                  
                                  selectInput('x', 'X-axis', names(df[,2:(ncol(df)-3)])),
                                  
                                  selectInput('y', 'Y-axis', names(df[,2:(ncol(df)-3)])),
                                  
                                  selectInput('group', 'Grouping variable', names(df[,3:ncol(df)-ncol(df)]))
                                          
                                  
                                ),
                                
                                # Show a plot of the generated distribution
                                mainPanel(
                                  plotOutput("ScatterPlot")
                                )
                              )
                            )),
                   tabPanel("Sample vs. Sample",
                            
                            titlePanel("Scatter plot (Sample vs. Sample)"),
                            
                            sidebarLayout(
                              sidebarPanel(
                                
                                selectInput('sample_a', 'Sample A', names(df_t)),
                                
                                selectInput('sample_b', 'Sample B', names(df_t)),
                                
                                sliderInput('fold', h3("Fold"),
                                            min = 2, max = 10, value = 2)
                                
                               
                                
                              ),
                            mainPanel(
                              plotOutput("ScatterPlot2")
                            ))),
                   tabPanel("Group vs. Group",
                            titlePanel("Scatter plot (Group vs. Group)"),
                            
                            sidebarLayout(
                              sidebarPanel(
                                
                                selectInput('grouping_var', 'Grouping variable', names(group_vars)),
                                
                                uiOutput('grouping')
                                
                                #selectInput('group_a', 'Group A', group_vars$Area),
                                
                                #selectInput('group_b', 'Group B', group_vars$Species),
                                
                                #sliderInput('fold2', h3("Fold"),
                                #            min = 2, max = 10, value = 2)
                                
                                
                                
                              ),
                              mainPanel(
                                style = "position:relative",
                                plotOutput("ScatterPlot3", 
                                           hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")),
                                uiOutput("hover_info")

                              ))),
                   tabPanel("D")
                
))

  