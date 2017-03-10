library(dplyr)
library(ggplot2)
library(shiny)
dat <- read.csv(file="Room_Use_Fall_2016_DG_for_R.csv", header=TRUE)
dat$date <- as.Date(dat$Date,"%m/%d/%Y")

dat$day <- weekdays(dat$date)
dat$factday <- factor(dat$day,levels=c("Monday","Tuesday","Wednesday",
                                       "Thursday","Friday","Saturday","Sunday"))
dat$factmonth <- factor(dat$Month,levels=c("7","8","9","10","11","12"))
datt <- dat[,c(5,7,8,9,13,14)]
## To create date column from month day year columns
##temp <- paste(dat$Month,'/',dat$Day,'/',dat$Year)
##temp.date <- as.Date(temp,"%m / %d / %Y")    ## need spaces



## Define the UI
ui <- fluidPage(
        fluidRow(
            column(3,
                   div(style = "font-size: 13px;", selectInput("colum", "Select Column Variable", ''))
            ),
            column(3,
                   div(style = "font-size: 13px;", selectInput("rowvar", label = "Select Row Variable",''))
            )),  ## closes fluidRow
        fluidRow(
            tableOutput('foo')    
            ) ## closes fluidRow
    )
   


server <- function(input, output, session) {

        s <- reactive(
            datt
            )


        observe({
            updateSelectInput(session, "colum", choices = sort(as.character(colnames(s()))))
        })

        observe({
            updateSelectInput(session, "rowvar", choices = sort(as.character(colnames(s()))))
        })

        output$foo <- renderTable({
            with(s(), as.matrix(table(get(input$rowvar), get(input$colum))))
        })
    }

# Return a Shiny app object
shinyApp(ui = ui, server = server)


