# shiny application for a student overview

library(shiny)
library(shinydashboard)
library(readxl)
library(tidyverse)
library(DT)
library(shinythemes)
source("R_functions.R")
source("R_uiLayout.R")

# Adjustment parameters
global.input_path <- "E:/Punten_Lln_Joke/ExcelOverzicht"
global.output_path <- "E:/Punten_Lln_Joke"


t_fys <- f_load_xlsx(path = paste0(global.input_path,"/FYS.xlsx"),sheet = "Blad1")
t_che <- f_load_xlsx(path = paste0(global.input_path,"/CHE.xlsx"),sheet = "Blad1")
t_bio <- f_load_xlsx(path = paste0(global.input_path,"/BIO.xlsx"),sheet = "Blad1")
t_nwe <- f_load_xlsx(path = paste0(global.input_path,"/NWE.xlsx"),sheet = "Blad1")

# t_fys$leerling <- as_factor(t_fys$leerling)
# t_che$leerling <- as_factor(t_che$leerling)
# t_bio$leerling <- as_factor(t_bio$leerling)
# t_nwe$leerling <- as_factor(t_nwe$leerling)

t_fys$vak <- "fys"
t_che$vak <- "che"
t_bio$vak <- "bio"
t_nwe$vak <- "nwe"

t_leerdoelen <- bind_rows(t_fys,t_che,t_bio,t_nwe)


 
#####################################################################################
# User interface
ui <- fluidPage(
    theme = shinytheme("yeti"),
    title = "Overzicht leerlingen klassenraden SJB - Mevr. J. Robijns",
    
    navbarPage(
        id = "mainnavbarpage",
        title = "Leerlingenoverzicht klassenraden Juni 2020",
        collapsible = TRUE,
        fluid = TRUE,
        footer = "(c)-2020 Dieter Baets, www.github.com/dbaets",
        tabPanel("Leerling selectie",page_selection, value = "page_selection"),
        tabPanel("Samenvatting", page_summary, value = "page_summary"),
        tabPanel("overzicht", page_overzicht, value = "page_overzicht")
    )
)
    
    


# server side
server <- function(input, output) {
    
    t_selected <- reactive({
        switch (input$vak,
            "Fysica" = t_leerdoelen %>% filter(vak == "fys"),
            "Chemie" = t_leerdoelen %>% filter(vak == "che"), 
            "Biologie" = t_leerdoelen %>% filter(vak == "bio"),
            "Natuurwetenschappen" = t_leerdoelen %>% filter(vak == "nwe"))
    })
    
    t_output <- reactive({
        t_selected() %>% 
            filter(klas == input$klas_selected)
    })
    
    output$loadedTable <- renderDataTable({
        datatable(t_output(), options = list(pageLength = 25)) %>% 
            formatStyle('LPD_tot',
                        backgroundColor = styleInterval(c(0.5,0.6),c('red','orange','none'))) %>%
            formatStyle('LPD_zonderVS',
                        backgroundColor = styleInterval(c(0.5,0.6),c('red','orange','none'))) %>%
            formatStyle('kerst',
                        backgroundColor = styleInterval(c(50,60),c('red','orange','none'))) %>%
            formatStyle('examen',
                        backgroundColor = styleInterval(c(50,60),c('red','orange','none'))) %>%
            formatRound('LPD_tot',3) %>% 
            formatRound('LPD_zonderVS',3)
            
    })
    
    output$klas <- renderUI({
        klassen <- levels(as.factor(t_selected()$klas))
        selectInput("klas_selected",h3("Kies klas"), klassen)
    })
    
    output$vis1 <- renderPlot({
        sel_rows1 <- input$loadedTable_rows_selected
        # t_vis1 <- t_output()[,c(input$xaxis, input$yaxis)]
        ggplot()+
            geom_hline(yintercept=0.5, linetype="dashed", color = "red") + 
            geom_vline(xintercept = 50, linetype = "dashed", color = "red") +
            geom_point(t_output(),mapping = aes(x = kerst, y = LPD_tot))+
            geom_point(t_output()[sel_rows1,], mapping = aes(x = kerst, y = LPD_tot),color = "red", cex = 2) +
            ylim(0,1) + 
            xlim(0,100) + 
            ggtitle(paste0(input$vak," - ",input$klas_selected)) + 
            theme_bw()
        
        
    })
    
    output$vis5 <- renderPlot({
        sel_rows1 <- input$loadedTable_rows_selected
        # t_vis1 <- t_output()[,c(input$xaxis, input$yaxis)]
        ggplot()+
            geom_hline(yintercept=0.5, linetype="dashed", color = "red") + 
            geom_vline(xintercept = 50, linetype = "dashed", color = "red") +
            geom_point(t_output(),mapping = aes(x = examen, y = LPD_tot))+
            geom_point(t_output()[sel_rows1,], mapping = aes(x = examen, y = LPD_tot),color = "red", cex = 2) +
            ylim(0,1) + 
            xlim(0,100) + 
            ggtitle(paste0(input$vak," - ",input$klas_selected)) + 
            theme_bw()
        
        
    })
    
    output$table2 <- DT::renderDataTable({
        sel_rows1 <- input$loadedTable_rows_selected
        datatable(t_output()[sel_rows1,]) %>% 
            formatStyle('LPD_tot',
                        backgroundColor = styleInterval(c(0.5,0.6),c('red','orange','none'))) %>%
            formatStyle('LPD_zonderVS',
                        backgroundColor = styleInterval(c(0.5,0.6),c('red','orange','none'))) %>%
            formatStyle('kerst',
                        backgroundColor = styleInterval(c(50,60),c('red','orange','none'))) %>%
            formatStyle('examen',
                        backgroundColor = styleInterval(c(50,60),c('red','orange','none'))) %>%
            formatRound('LPD_tot',3) %>% 
            formatRound('LPD_zonderVS',3)
        
        
        
        
    })
    
    vars <- reactive({
        setdiff(names(t_output()),c("leerling","klas","vak","opmerking","richting"))
    })
    
    output$summary <- renderPrint({
        # t_summary_output <- t_output() %>% 
        #     summarise(mean_LPD_zonderVS = mean(LPD_zonderVS),
        #               mean_LPD_tot = mean(LPD_tot))
    summary(t_output())
        
            
    })
    
    output$vis2 <- renderPlot({
        sel_rows1 <- input$loadedTable_rows_selected
        ggplot() +
            geom_hline(yintercept = 0.5,linetype = "dashed", color = "red") +
            geom_boxplot(t_selected(),mapping = aes(x = klas, y = LPD_tot), position = position_dodge()) + 
            geom_jitter(t_selected(),mapping = aes(x = klas, y = LPD_tot), colour = "grey50", alpha = 0.5) +
            geom_point(t_output()[sel_rows1,], mapping = aes(x = klas, y = LPD_tot),color = "red", cex = 2) +
            ylim(0,1) + 
            ggtitle(paste0(input$vak," - Totaal behaalde leerplandoelen")) +
            theme_bw()
    })
        
    output$vis3 <- renderPlot({
            sel_rows1 <- input$loadedTable_rows_selected
            ggplot() +
                geom_hline(yintercept = 50,linetype = "dashed", color = "red") +
                geom_boxplot(t_selected(),mapping = aes(x = klas, y = examen), position = position_dodge()) + 
                geom_jitter(t_selected(),mapping = aes(x = klas, y = examen), colour = "grey50", alpha = 0.5) +
                geom_point(t_output()[sel_rows1,], mapping = aes(x = klas, y = examen),color = "red", cex = 2) +
                ylim(0,100) + 
                ggtitle(paste0(input$vak," - examen juni")) +
                theme_bw()
        
    })
    
    output$vis4 <- renderPlot({
        sel_rows1 <- input$loadedTable_rows_selected
        ggplot() +
            geom_hline(yintercept = 50,linetype = "dashed", color = "red") +
            geom_boxplot(t_selected(),mapping = aes(x = klas, y = kerst), position = position_dodge()) + 
            geom_jitter(t_selected(),mapping = aes(x = klas, y = kerst), colour = "grey50", alpha = 0.5) +
            geom_point(t_output()[sel_rows1,], mapping = aes(x = klas, y = kerst),color = "red", cex = 2) +
            ylim(0,100) + 
            ggtitle(paste0(input$vak," - examen kerst")) +
            theme_bw()
        
    })
    
    
    output$vis6 <- renderPlot({
        sel_rows1 <- input$loadedTable_rows_selected
        # t_vis1 <- t_output()[,c(input$xaxis, input$yaxis)]
        ggplot()+
            geom_hline(yintercept=50, linetype="dashed", color = "red") + 
            geom_vline(xintercept = 50, linetype = "dashed", color = "red") +
            geom_point(t_output(),mapping = aes(x = kerst, y = examen))+
            geom_point(t_output()[sel_rows1,], mapping = aes(x = kerst, y = examen),color = "red", cex = 2) +
            ylim(0,100) + 
            xlim(0,100) + 
            ggtitle(paste0(input$vak," - ",input$klas_selected)) + 
            theme_bw()
        
    })
    
    
    # output$xaxis <- renderUI({
    #     selectInput("xaxis",h3("Kies x-as"), vars(), selected = vars()[[3]])
    # })
    # output$yaxis <- renderUI({
    #     selectInput("yaxis",h3("Kies y-as"), vars(), selected = vars()[[2]])
    # })
    
    #output$vis2
    

    
}

# Run the application 
shinyApp(ui = ui, server = server)

