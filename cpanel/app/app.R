#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(someR)
library(shiny)
library(shinydashboard)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggthemes)

# Define UI for application that draws a histogram
ui <- # Define UI for application that draws a histogram
  dashboardPage(skin = "black",

    ### HEADER ###
    dbHeader <- dashboardHeader(disable = F, dropdownMenuOutput("ddmenu")),

    ### SIDEBAR ###
    dashboardSidebar(disable = T, collapsed = T),

    ### BODY ###
    dashboardBody(

      # extend screen automatically
      tags$head(tags$style(HTML('.content-wrapper { overflow: auto; }'))),

      # current week
      fluidRow(
        column(width = 12,
               box(title = "Twitter Performance Monitoring", width = 12, height = 54, icon = NULL, background = "black"
               ),
               box(title = "#dkpol last day", width = 12, icon = NULL, collapsible = T,
                   plotOutput("ft_lastday")
               ),
               box(title = "#dkpol last month", width = 12, icon = NULL, collapsible = T,
                   plotOutput("ft_lastmonth")
               ),
               box(title = "#dkpol alltime", width = 12, icon = NULL, collapsible = T,
                   plotOutput("ft_alltime")
               ),
               box(title = "#lighthousesdk last day", width = 12, icon = NULL, collapsible = T,
                   plotOutput("lh_lastday")
               ),
               box(title = "#lighthousesdk last month", width = 12, icon = NULL, collapsible = T,
                   plotOutput("lh_lastmonth")
               ),
               box(title = "#lighthousesdk alltime", width = 12, icon = NULL, collapsible = T,
                   plotOutput("lh_alltime")
               )
        )
      )
    )
  )

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {

  ### DATA ###
  dat <- reactive({

    # folketinget
    dat <- read.csv(
      "/home/kasper/someR/projects/folketinget/logs/visitors.csv"
    )
    dat %>% dplyr::mutate(
      time = as.POSIXct(time),
      app = "folketinget"
    ) -> dat_folketing

    # lighthouses
    dat <- read.csv(
      "/home/kasper/someR/projects/lighthouses/logs/visitors.csv"
    )
    dat %>% dplyr::mutate(
      time = as.POSIXct(time),
      app = "folketinget"
    ) -> dat_lighthouses

    dat <- list(
      "folketing" = dat_folketing,
      "lighthouses" = dat_lighthouses
    )

    return(dat)

  })


  ### FOLKETING ###
  output$ft_lastday <- renderPlot({

    dat_plot <- dat()[["folketing"]]

    dat_plot %>% dplyr::select(
      -app
    ) %>% dplyr::mutate(
      count = 1
    ) -> dat_plot

    # current day
    starttime <- as.POSIXct(paste0(Sys.Date()," 00:00:00 UTC"))
    endtime <- as.POSIXct(paste0(Sys.Date()," 23:59:59 UTC"))

    dat_plot %>% dplyr::filter(
      time > starttime
    ) %>% dplyr::mutate(
      time = as.POSIXct(paste0(substr(time,1,13),":00:00"))
    ) -> dat_plot

    dat_plot %>% dplyr::group_by(
      time
    ) %>% dplyr::summarise(
      count = sum(count)
    ) -> dat_plot

    timeseq <- data.frame(
      "time" = seq(starttime,endtime,60*60)
    )

    dat_plot <- dplyr::left_join(
      timeseq,dat_plot,"time"
    )

    dat_plot %>% dplyr::mutate(
      count = ifelse(is.na(count) == T,0,count)
    ) -> dat_plot

    #
    dat_plot <- reshape2::melt(
      dat_plot,
      id.vars = "time"
    )

    # make plot
    p <- ggplot(dat_plot, aes(x=time, y=value)) +
      geom_bar(stat="identity") +
      ggthemes::theme_economist_white() +
      xlab("") +
      ylab("")

    return(p)

  })

  output$ft_lastmonth <- renderPlot({

    dat_plot <- dat()[["folketing"]]

    dat_plot %>% dplyr::select(
      -app
    ) %>% dplyr::mutate(
      count = 1
    ) -> dat_plot

    # current day
    starttime <- as.POSIXct(paste0(Sys.Date()," 00:00:00 UTC")) - (60*60*24*30)
    endtime <- as.POSIXct(paste0(Sys.Date()," 23:59:59 UTC"))

    dat_plot %>% dplyr::filter(
      time > starttime
    ) %>% dplyr::mutate(
      time = as.POSIXct(paste0(substr(time,1,11),"00:00:00"))
    ) -> dat_plot

    dat_plot %>% dplyr::group_by(
      time
    ) %>% dplyr::summarise(
      count = sum(count)
    ) -> dat_plot

    timeseq <- data.frame(
      "time" = seq(starttime,endtime,60*60*24)
    )

    dat_plot <- dplyr::left_join(
      timeseq,dat_plot,"time"
    )

    dat_plot %>% dplyr::mutate(
      count = ifelse(is.na(count) == T,0,count)
    ) -> dat_plot

    #
    dat_plot <- reshape2::melt(
      dat_plot,
      id.vars = "time"
    )

    # make plot
    p <- ggplot(dat_plot, aes(x=time, y=value)) +
      geom_bar(stat="identity") +
      ggthemes::theme_economist_white() +
      xlab("") +
      ylab("")

    return(p)

  })

  output$ft_alltime <- renderPlot({

    dat_plot <- dat()[["folketing"]]

    dat_plot %>% dplyr::select(
      -app
    ) %>% dplyr::mutate(
      count = 1
    ) -> dat_plot

    # current day
    starttime <- as.POSIXct(paste0(as.Date(min(dat_plot[["time"]])))," 23:59:59 UTC")
    endtime <- as.POSIXct(paste0(Sys.Date()," 23:59:59 UTC"))

    dat_plot %>% dplyr::mutate(
      time = as.POSIXct(paste0(substr(time,1,11),"00:00:00"))
    ) -> dat_plot

    dat_plot %>% dplyr::group_by(
      time
    ) %>% dplyr::summarise(
      count = sum(count)
    ) -> dat_plot

    timeseq <- data.frame(
      "time" = seq(starttime,endtime,60*60*24)
    )

    dat_plot <- dplyr::left_join(
      timeseq,dat_plot,"time"
    )

    dat_plot %>% dplyr::mutate(
      count = ifelse(is.na(count) == T,0,count)
    ) -> dat_plot

    #
    dat_plot <- reshape2::melt(
      dat_plot,
      id.vars = "time"
    )

    # make plot
    p <- ggplot(dat_plot, aes(x=time, y=value)) +
      geom_bar(stat="identity") +
      ggthemes::theme_economist_white() +
      xlab("") +
      ylab("")

    return(p)

  })

  ### LIGHTHOUSES ###
  output$lh_lastday <- renderPlot({

    dat_plot <- dat()[["lighthouses"]]

    dat_plot %>% dplyr::select(
      -app
    ) %>% dplyr::mutate(
      count = 1
    ) -> dat_plot

    # current day
    starttime <- as.POSIXct(paste0(Sys.Date()," 00:00:00 UTC"))
    endtime <- as.POSIXct(paste0(Sys.Date()," 23:59:59 UTC"))

    dat_plot %>% dplyr::filter(
      time > starttime
    ) %>% dplyr::mutate(
      time = as.POSIXct(paste0(substr(time,1,13),":00:00"))
    ) -> dat_plot

    dat_plot %>% dplyr::group_by(
      time
    ) %>% dplyr::summarise(
      count = sum(count)
    ) -> dat_plot

    timeseq <- data.frame(
      "time" = seq(starttime,endtime,60*60)
    )

    dat_plot <- dplyr::left_join(
      timeseq,dat_plot,"time"
    )

    dat_plot %>% dplyr::mutate(
      count = ifelse(is.na(count) == T,0,count)
    ) -> dat_plot

    #
    dat_plot <- reshape2::melt(
      dat_plot,
      id.vars = "time"
    )

    # make plot
    p <- ggplot(dat_plot, aes(x=time, y=value)) +
      geom_bar(stat="identity") +
      ggthemes::theme_economist_white() +
      xlab("") +
      ylab("")

    return(p)

  })

  output$lh_lastmonth <- renderPlot({

    dat_plot <- dat()[["lighthouses"]]

    dat_plot %>% dplyr::select(
      -app
    ) %>% dplyr::mutate(
      count = 1
    ) -> dat_plot

    # current day
    starttime <- as.POSIXct(paste0(Sys.Date()," 00:00:00 UTC")) - (60*60*24*30)
    endtime <- as.POSIXct(paste0(Sys.Date()," 23:59:59 UTC"))

    dat_plot %>% dplyr::filter(
      time > starttime
    ) %>% dplyr::mutate(
      time = as.POSIXct(paste0(substr(time,1,11),"00:00:00"))
    ) -> dat_plot

    dat_plot %>% dplyr::group_by(
      time
    ) %>% dplyr::summarise(
      count = sum(count)
    ) -> dat_plot

    timeseq <- data.frame(
      "time" = seq(starttime,endtime,60*60*24)
    )

    dat_plot <- dplyr::left_join(
      timeseq,dat_plot,"time"
    )

    dat_plot %>% dplyr::mutate(
      count = ifelse(is.na(count) == T,0,count)
    ) -> dat_plot

    #
    dat_plot <- reshape2::melt(
      dat_plot,
      id.vars = "time"
    )

    # make plot
    p <- ggplot(dat_plot, aes(x=time, y=value)) +
      geom_bar(stat="identity") +
      ggthemes::theme_economist_white() +
      xlab("") +
      ylab("")

    return(p)

  })

  output$lh_alltime <- renderPlot({

    dat_plot <- dat()[["lighthouses"]]

    dat_plot %>% dplyr::select(
      -app
    ) %>% dplyr::mutate(
      count = 1
    ) -> dat_plot

    # current day
    starttime <- as.POSIXct(paste0(as.Date(min(dat_plot[["time"]])))," 23:59:59 UTC")
    endtime <- as.POSIXct(paste0(Sys.Date()," 23:59:59 UTC"))

    dat_plot %>% dplyr::mutate(
      time = as.POSIXct(paste0(substr(time,1,11),"00:00:00"))
    ) -> dat_plot

    dat_plot %>% dplyr::group_by(
      time
    ) %>% dplyr::summarise(
      count = sum(count)
    ) -> dat_plot

    timeseq <- data.frame(
      "time" = seq(starttime,endtime,60*60*24)
    )

    dat_plot <- dplyr::left_join(
      timeseq,dat_plot,"time"
    )

    dat_plot %>% dplyr::mutate(
      count = ifelse(is.na(count) == T,0,count)
    ) -> dat_plot

    #
    dat_plot <- reshape2::melt(
      dat_plot,
      id.vars = "time"
    )

    # make plot
    p <- ggplot(dat_plot, aes(x=time, y=value)) +
      geom_bar(stat="identity") +
      ggthemes::theme_economist_white() +
      xlab("") +
      ylab("")

    return(p)

  })

})


# Run the application
shinyApp(ui = ui, server = server)
