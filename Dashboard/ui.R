
#

library(shiny)
library(shinydashboard)
library(dplyr)
library(lubridate)
library (ggplot2)
library(XLConnect)
library(scales)



header <- dashboardHeader(
                          title = "Review Dashboard", 
#                          dropdownMenu(type = "notifications",
#                                        notificationItem(
#                                          text = "Aging Approvals",
#                                          icon("Binoculars"),
#                                          status = "success"
#                                        ),
#                                        notificationItem(
#                                          text = "Attachment not scanned",
#                                          icon = icon("exclamation-triangle"),
#                                          status = "warning"
#                                        )
#                          ) 
                        dropdownMenuOutput("notificationMenu")
                        )# Header


sidebar <-  dashboardSidebar(
              sidebarMenu(
                menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
#                 menuItem("EDA", tabName = "statistics", icon = icon("bar-chart-o"),
#                   menuSubItem("Sentiment", icon = icon("angle-double-right"), tabName = "sentiment"),
#                   menuSubItem("tf-idf words", icon = icon("angle-double-right"), tabName = "words")
#                   ),
                menuItem("Personalize Dashboard", tabName = "config", icon = icon("line-chart"),badgeLabel = "new", badgeColor = "green")
                      ) #Side bar menu
              ) # Side bar


body <- dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "dashboard",
            fluidRow(
              # Dynamic infoBoxes row 1
              #infoBoxOutput("KPI_1",width = 6),
              #infoBoxOutput("KPI_2",width = 6)
              uiOutput("KPI_1"),
              uiOutput("KPI_2")
            ),
            fluidRow(
              uiOutput("KPI_3"),
              uiOutput("KPI_4")
            ),
            uiOutput("kpiDetails")
    ),
    
    # subitem
    tabItem(tabName = "statistics",
            fluidRow(
              column(12,
            plotOutput("wordPlot")
              )
            )
          ) ,
     tabItem(tabName = "sentiment",
            fluidRow(
              column(12,
                     plotOutput("approverPlot")
              )
            )
          ),
    tabItem(tabName = "words",
            fluidRow(
              column(12,
                     plotOutput("areaPlot")
              )
            )
           ),
    
    # Fourth tab content
    tabItem(tabName = "config",
            h2("Dashboard Configuration"),
            fluidRow(
                  box (
                       title = "KPI 1 Configuration", status = "primary", solidHeader = TRUE,
                       collapsible = TRUE,
                       sliderInput("kpi.1.thr", "Threshold:", min=1, max=100, value=80),
                       textInput("kpi1_title", "Title", value="Products with positive ratings"),
                       textInput("kpi1_description", "Description",value="% of the product have positive ratings (4 or 5)")
#                        radioButtons("kpi1_visible", "Visible",
#                                     choices = c("Yes", "No"),
#                                     selected = "Yes")
                      ), #KPI 1
                  box (
                     title = "KPI 2 Configuration", status = "primary", solidHeader = TRUE,
                     collapsible = TRUE,
                     sliderInput("kpi.2.thr", "Threshold:", min=1, max=100, value=80),
                     textInput("kpi2_title", "Title","Products with a vote on review's helpfuness"),
                     textInput("kpi2_description", "Description","%")
#                      radioButtons("kpi2_visible", "Visible",
#                                   choices = c("Yes", "No"),
#                                   selected = "Yes")
#                     
                    ) #KPI 2
              ),
            
            fluidRow(
              box (
                title = "KPI 3 Configuration", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                sliderInput("kpi.3.thr", "Threshold:", min=1, max=100, value=80),
                textInput("kpi3_title", "Title","Helpful Reviews"),
                textInput("kpi3_description", "Description","%")
#                 radioButtons("kpi3_visible", "Visible",
#                              choices = c("Yes", "No"),
#                              selected = "Yes")
              ), #KPI 3
              box (
                title = "KPI 4 Configuration", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                sliderInput("kpi.4.thr", "KPI 4 set threshold:", min=1, max=100, value=80),
                textInput("kpi4_title", "KPI 4 Title","Placeholder for Title of KPI 4"),                
                textInput("kpi4_description", "KPI 4 Description"," Placeholder for description of KPI 4")
#                 radioButtons("kpi4_visible", "Visible",
#                              choices = c("Yes", "No"),
#                              selected = "Yes")
              ) #KPI 4
            ),
            fluidRow(              
              box (
                  title = "Additional Configuration", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                    radioButtons("kpiDet_visible", "KPI Details Panel - Visible",
                               choices = c("Yes", "No"),
                               selected = "Yes"),
                  selectInput("kpi_boxtype", "Box Type",choices = c("InfoBox", "ValueBox", "SimpleBox"))
                
                ) #KPI Details Panel box
              ) #Fluid Row KPI Details
    )# tab item name=config
  ) # Tab Items
)# Body


dashboardPage(header, sidebar, body)


