library(shiny)
library(ggplot2)
library(reshape2)
library(dplyr)
library(grid)
library(gridExtra)
library(ggthemes)
library(magrittr)


# to get theta_results.csv:
#   DCPO::summarize_dcpo_results(dcpo_input, dcpo_output, "theta") %>% 
#   write_csv("theta_results.csv")
pge <- read.csv("theta_results.csv", as.is = TRUE) %>% 
    group_by(country)

cc <- pge %>% 
    select(country) %>% 
    distinct() %>% 
    arrange(country)

server <- function(input, output, session) {
    
    output$yearControl <- renderUI({
        max_year <- pge %>% 
            pull(year) %>% 
            max()
        sliderInput(inputId = "dates", 
                    label = "Years",
                    min = 1970,
                    max = max(max_year, round(max_year, -1)), 
                    value = c(1975, max_year),
                    step = 1,
                    sep = "")
    })
    
    observe({
        updateSelectInput(session, "country1", choices = cc$country, selected = "Norway")
        
        updateSelectInput(session, "country2", choices = c("none", cc$country), selected = "United States")
        
        updateSelectInput(session, "country3", choices = c("none", cc$country), selected = "Nigeria")
        
        updateSelectInput(session, "country4", choices = c("none", cc$country), selected = "Indonesia")
    })
    
    plotInput <- reactive({
        # Get data for selected countries and series
        s1 <- data.frame(pge[pge$country==input$country1, 
                               c("country", "year", "mean", "sd")])
        
        if(input$country2 != "select") {
            s2 <- data.frame(pge[pge$country==input$country2, 
                                   c("country", "year", "mean", "sd")])
            s1 <- merge(s1, s2, all=T)
        }
        
        if(input$country3 != "select") {
            s3 <- data.frame(pge[pge$country==input$country3, 
                                   c("country", "year", "mean", "sd")])
            s1 <- merge(s1, s3, all=T)
        }
        
        if(input$country4 != "select"){
            s4 <- data.frame(pge[pge$country==input$country4, 
                                   c("country", "year", "mean", "sd")])
            s1 <- merge(s1, s4, all=T)
        }   
        
        s1 <- melt(s1, id.vars=c("country", "year"), na.rm=T)
        s2 <- s1[grepl("sd", s1$variable), c("country", "year", "value")]
        s1 <- s1[!grepl("sd", s1$variable), ]
        s1 <- cbind(s1, s2[, 3])
        names(s1)[5] <- "value_se"
        s1$variable <- gsub("mean", "Public Gender Egalitarianism", s1$variable)
        s1 <- s1[s1$year >= input$dates[1] & s1$year <= input$dates[2], ]
        
        # Modify ylabel and legend title to reflect selected countries
        ylabel <- paste(s1$variable[1])
        s1$series <- s1$country
        c_title <- ""
        
        note1 <- "Note: Solid lines indicate mean estimates; shaded regions indicate the associated 80% uncertainty intervals.\nSource: Public Gender Egalitarianism Dataset v1.0 (Woo, Allemang, and Solt 2021)."
        
        # Basic plot
        p <- ggplot(s1, aes(x=year, y=value, colour=series)) + 
            geom_line() +
            geom_ribbon(aes(ymin = value - qnorm(.9)*value_se,
                            ymax = value + qnorm(.9)*value_se, 
                            fill = series, 
                            linetype = NA),
                        alpha = .25) +
            coord_cartesian(xlim=c(input$dates[1], input$dates[2])) +
            labs(x = "Year", 
                 y = ylabel,
                 caption = note1)
        
        hjust1 <- 0
        hjust2 <- 0
        vjust1 <- .2
        vjust2 <- .03
        
        # Apply themes and add source
        if (input$theme=="light") {
            p + theme_light() + 
                scale_fill_brewer(name = c_title, palette = "Dark2", direction = -1) +
                scale_colour_brewer(name = c_title, palette = "Dark2", direction = -1) +
                theme(plot.caption = element_text(size = 8))
        } else if (input$theme=="classic") {
            p + theme_classic() + 
                scale_fill_discrete(name = c_title) + 
                scale_colour_discrete(name = c_title) +
                theme(plot.caption = element_text(size = 8))
        } else if (input$theme=="tufte") {
            p + theme_tufte() + 
                scale_fill_grey(name = c_title) + 
                scale_colour_grey(name = c_title) +
                theme(plot.caption = element_text(size = 8))
        } else if (input$theme=="econ") {
            p + theme_economist() + 
                scale_fill_economist(name = c_title) + 
                scale_colour_economist(name = c_title) +
                theme(plot.caption = element_text(size = 8))
        } else if (input$theme=="fte") {
            p + theme_fivethirtyeight() + 
                scale_fill_fivethirtyeight(name = c_title) + 
                scale_colour_fivethirtyeight(name = c_title) +
                theme(plot.caption = element_text(size = 8))
        } else if (input$theme=="few") {
            p + theme_few() + 
                scale_fill_few(name = c_title) + 
                scale_colour_few(name = c_title) +
                theme(plot.caption = element_text(size = 8))
        } else if (input$theme=="sol") {
            p + theme_solarized() + 
                scale_fill_solarized("blue", name = c_title) + 
                scale_colour_solarized("blue", name = c_title) +
                theme(plot.caption = element_text(size = 8))
        } else if (input$theme=="stata") {
            p + theme_stata() + 
                scale_fill_stata(name = c_title) + 
                scale_colour_stata(name = c_title) +
                theme(plot.caption = element_text(size = 8))
        } else if (input$theme=="wsj") {
            p + theme_wsj() + 
                scale_fill_wsj(name = c_title) + 
                scale_colour_wsj(name = c_title) +
                theme(plot.caption = element_text(size = 8))
        } else if (input$theme=="hc") {
            p + theme_hc() + 
                scale_fill_hc(name = c_title) + 
                scale_colour_hc(name = c_title) +
                theme(plot.caption = element_text(size = 8))
        } else if (input$theme=="pander") {
            p + theme_pander() + 
                scale_fill_pander(name = c_title) + 
                scale_colour_pander(name = c_title) +
                theme(plot.caption = element_text(size = 8))
        } else {
            p + scale_fill_brewer(name = c_title, palette = "Dark2", direction = -1) +
                scale_colour_brewer(name = c_title, palette = "Dark2", direction = -1) +
                theme(plot.caption = element_text(size = 8))
        }
        
    })
    
    output$plot <- renderPlot({     
        print(plotInput())    
    })
    
    output$downloadPlot <- downloadHandler(
        filename = function() { paste0('pge', '.pdf') },
        content = function(file) {
            pdf(file, width = 7, height = 4)
            print(plotInput())
            dev.off()
        })
    
} 

ui <- shinyUI(fluidPage( 
    helpText(" "),
    tags$head(tags$style(HTML("
        .selectize-input, .selectize-dropdown {
                              font-size: 90%;
                              }
                              "))),
    fluidRow(        
        column(3,
               
               selectInput("country1", label="Countries", "Norway"), 
               
               selectInput("country2", label=NULL, "United States"),
               
               selectInput("country3", label=NULL, "Nigeria"),
               
               selectInput("country4", label=NULL, "Indonesia"),
               
               
               br(),
               
               uiOutput("yearControl"),              
               
               
               br(),
               selectInput("theme", "Theme", 
                           choices = list("Standard" = "none",
                                          "Light" = "light",
                                          "Classic" = "classic",
                                          "Economist" = "econ",
                                          "Few" = "few",
                                          "FiveThirtyEight" = "fte",
                                          "Highcharts" = "hc",
                                          "Pander" = "pander",
                                          "Solarized" = "sol",
                                          "Stata" = "stata",
                                          "Tufte" = "tufte",
                                          "WSJ" = "wsj"),
                           selected = "none"),               
               
               
               br(),
               
               downloadButton('downloadPlot', 'Download PDF')
        ),
        
        column(9,
               plotOutput("plot", height = "600px")
        )
    )
))

shinyApp(ui = ui, server = server)