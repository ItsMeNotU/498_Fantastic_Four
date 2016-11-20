#

library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(lubridate)
library (ggplot2)
library(scales)


#source("C:\\Users\\SZQ4ZD\\Documents\\Armelle\\454\\shiny\\Load_SRT_Tables.R", local=FALSE)
source("/Users/giovannidelgrosso/Documents/MSPA/498 - Capstone/rCode/DB_reviews_Master_gdv03.R",local=FALSE)

# load_compute_data <- function() {
#   source("/Users/giovannidelgrosso/Documents/MSPA/498 - Capstone/rCode/DB_reviews_Master_gdv03.R",local=FALSE)
# }



shinyServer(function(input, output) {
  
#   observe({
#     withProgress(message = 'Please Wait while loading data ...', value = 0, 
# {
#   load_compute_data()
# }
#     )
#   })

  
  KPI_1<-round(as.numeric(levels(freq.helpful[,5]))[2],0)
  KPI_2<-round(((dim(reviews)[1] - sum(reviews$helpful.nan))/dim(reviews)[1])*100,0)
  KPI_3<-round(((dim(subset(reviews, helpful.bins == "Upper"))[1])/(sum(reviews$helpful.nan)))*100,0)
  KPI_4<-999
  
#   #Chart 1 
  overall.prod.rating<-ggplot(data = reviews.eda,
          aes(x = overall.fac)) +
          geom_bar(fill = "grey50") +
          stat_count(aes(label = ..count..),
                     vjust = 1.6,
                     color = "white",
                     size = 3.5,
                     geom = "text") +
          labs(title = "Barplot of Overall Product Ratings",
               x = "Overall Rating",
               y = "Count")
#    #Chart 2
plot.helpful.reviews<-ggplot(data = reviews.eda,
      aes(x = helpful.bins)) +
      geom_bar(fill = "grey50") +
      stat_count(aes(label = ..count..),
                 vjust = 1.6,
                 color = "white",
                 size = 3.5,
                 geom = "text") +
      labs(title = "Barplot of Product Ratings by Helpful Bins",
           x = "Helpful Bins",
           y = "Count")
#   Output
  output$ratingDistrPlot <- renderPlot({
    overall.prod.rating
  })
  output$helpRevDistr <- renderPlot({
    plot.helpful.reviews
  })
# Table 1 - List of products with helpfull review (need to add description)
  output$tbl.prod.helpful = DT::renderDataTable(
    products.with.helpful.reviews, options = list(lengthChange = FALSE)
  )
# Table 2 - List of products most recommended products (show 10)  
  output$tbl.prod = DT::renderDataTable(
    products.with.helpful.reviews, options = list(pageLength = 10)
  )


# --------------- #  
#   RENDERING     #
# --------------- #  
observe({
        switch(input$kpi_boxtype,
         "InfoBox"={
                    output$KPI_1 <- renderUI({
                                                  if (KPI_1 >input$kpi.1.thr) color="green" else color="red"
                                                  infoBox(
                                                          input$kpi1_title, 
                                                          paste0(round(KPI_1,2),input$kpi1_description), 
                                                          width = 6,
                                                          icon = icon("shopping-cart"),
                                                          color = color
                                                          ) #infoBox KPI 1
                                                  })
                    output$KPI_2 <- renderUI({
                                                  if (KPI_2 >input$kpi.2.thr) {
                                                    color="green" 
                                                    icon = icon("thumbs-up", lib = "glyphicon")
                                                    } else {
                                                            color="red"
                                                            icon = icon("thumbs-down", lib = "glyphicon")
                                                            }
                                                    infoBox(
                                                      input$kpi2_title, 
                                                      paste0(round(KPI_2,2),input$kpi2_description), 
                                                      width = 6,
                                                      icon = icon,
                                                      color = color
                                                    )
                                                })
                    output$KPI_3 <- renderUI({
                                                  if (KPI_3 >input$kpi.3.thr) color="green" else color="red"
                                                  infoBox(
                                                    input$kpi3_title, 
                                                    paste0(round(KPI_3,2),input$kpi3_description ), 
                                                    width = 6,
                                                    icon = icon("list"),
                                                    color = color
                                                  )
                                                })
                    output$KPI_4 <- renderUI({
                                                  if (KPI_4 == 0) color="green" else color="red"
                                                  infoBox(
                                                    input$kpi4_title, 
                                                    paste0(round(KPI_4,2), input$kpi4_description), 
                                                    width = 6,
                                                    icon = icon("list"),
                                                    color = color
                                                  )
                                                })
                }, #Info Box case
         "ValueBox"={
                   output$KPI_1 <- renderUI({
                                                   if (KPI_1 >input$kpi.1.thr) color="green" else color="red"
                                                   valueBox(
                                                     #input$kpi1_title, 
                                                     paste0(round(KPI_1,2),"%"), 
                                                     input$kpi1_title, 
                                                     width = 6,
                                                     icon = icon("shopping-cart"),
                                                     color = color
                                                     )      
                                                  })
                   output$KPI_2 <- renderUI({
                                                   if (KPI_2 >input$kpi.2.thr) {
                                                     color="green" 
                                                     icon = icon("thumbs-up", lib = "glyphicon")
                                                   } else {
                                                     color="red"
                                                     icon = icon("thumbs-down", lib = "glyphicon")
                                                   }
                                                   valueBox(
                                                     paste0(round(KPI_2,2),"%"), 
                                                     input$kpi2_title, 
                                                     width = 6,
                                                     icon = icon,
                                                     color = color
                                                     )
                                                  })
                   output$KPI_3 <- renderUI({
                                                 if (KPI_3 >input$kpi.3.thr) color="green" else color="red"
                                                 valueBox(
                                                   paste0(round(KPI_3,2),"%"),
                                                   input$kpi3_title, 
                                                   width = 6,
                                                   icon = icon("list"),
                                                   color = color
                                                   )
                                                  })
                   output$KPI_4 <- renderUI({
                                                   if (KPI_4 == 0) color="green" else color="red"
                                                   valueBox(
                                                     paste0(round(KPI_4,2),""),
                                                     input$kpi4_title, 
                                                     width = 6,
                                                     icon = icon("list"),
                                                     color = color
                                                     )
                                                    }) 
                   },#Value Box case
         "SimpleBox"={
                   output$KPI_1 <- renderUI({
                                             if (KPI_1 >input$kpi.1.thr) status="success" else status="danger"
                                               box(
                                                 title = input$kpi1_title,
                                                 paste0(round(KPI_1,2), input$kpi1_description), 
                                                 width = 6,
                                                 solidHeader = TRUE,
                                                 collapsible = TRUE,
                                                 icon = icon("shopping-cart"),
                                                 status=status
                                               )
                                              })
                   output$KPI_2 <- renderUI({
                                           if (KPI_2 >input$kpi.2.thr) {
                                                status="success" 
                                               icon = icon("thumbs-up", lib = "glyphicon")
                                             } 
                                           else {
                                               status="danger"
                                               icon = icon("thumbs-down", lib = "glyphicon")
                                             }
                                             box(
                                               title=input$kpi2_title, 
                                               paste0(round(KPI_2,2), input$kpi2_description), 
                                               width = 6,
                                               solidHeader = TRUE,
                                               collapsible = TRUE,
                                               icon = icon,
                                               status=status
                                             )
                                            })
                   output$KPI_3 <- renderUI({
                                           if (KPI_3 >input$kpi.3.thr) status="success" else status="danger"
                                           box(
                                             title=input$kpi3_title, 
                                             paste0(round(KPI_3,2),input$kpi3_description ), 
                                             width = 6,
                                             solidHeader = TRUE,
                                             collapsible = TRUE,
                                             icon = icon("list"),
                                             status=status
                                           )
                                          })
                   output$KPI_4 <- renderUI({
                                            if (KPI_4 == 0) status="success" else status="danger"
                                            box(
                                              title=input$kpi4_title, 
                                              paste0(round(KPI_4,2), input$kpi4_description), 
                                              width = 6,
                                              solidHeader = TRUE,
                                              collapsible = TRUE,
                                              icon = icon("list"),
                                              status=status
                                            )
                                           }) 
          } #Simple Box Case
         ) #Switch
      })#Observe

 dtvis <- reactive({ifelse(input$kpiDet_visible=="Yes",TRUE,FALSE)}) 
 observe({
          output$kpiDetails <- renderUI({ if (dtvis()) {
                                                        fluidRow(
                                                          column(12,br(),
                                                            tabBox(width = 12,
                                                                   title = "Key Performance Indicators - Details",
                                                                   id = "tabset1", 
                                                                   tabPanel("Rating Distribution", plotOutput("ratingDistrPlot")),
                                                                   tabPanel("Products with Helpful Review", DT::dataTableOutput("tbl.prod.helpful")),
                                                                   tabPanel("Helpful Reviews Distribution", plotOutput("helpRevDistr")),
                                                                   tabPanel("10 Products: top/least", DT::dataTableOutput("tbl.prod"))
                                                                   )     # tabbox
                                                            )# Column  
                                                          )#Fluid Row
                                                        }
                                             else {br()}
                                                  })
   
  }) #observe
    


})#Server
