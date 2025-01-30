#   |\      _,,,---,,_             
#   /,`.-'`'    -.  ;-;;,_         riki
#  |,4-  ) )-,_. ,\ (  `'-' 
# '---''(_/--'  `-'\_)
################################################################################
# Load packages & dataset #
################################################################################
# r libraries
library(shiny); library(tidyverse); library(png)
library(patchwork); # package for arranging multiple plots
# IMF fonts
imf_theme <- function() {
  theme(text = ggplot2::element_text(face = "plain", family = "Segoe UI"),
        axis.text = ggplot2::element_text(linewidth = 9.5),
        axis.title = ggplot2::element_text(linewidth = 10.25),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(colour = "#b3b3b3",fill=NA,linewidth = 0.5),
        axis.ticks.x = ggplot2::element_line(colour = "#b3b3b3",linewidth = 0.5),
        axis.ticks.y = ggplot2::element_line(colour = "#b3b3b3",linewidth = 0.5),
        axis.ticks.length=unit(-0.25, "cm"),
        axis.text.y = ggplot2::element_text(margin = unit(c(0,0.1,0,0), "cm")),
        axis.line.x = ggplot2::element_blank(),
        plot.caption = element_text(linewidth = 9.5),
        plot.title = element_text(linewidth = 12)
  )
}
palette_4 <- c("#2d6074","#e6b752","#188c81","#d5624b")

distribution <- read.csv("povcal_master.csv")

#########################################################
# Creating function to calculate Gini coefficient #
#########################################################
gini_calculation <- function (x, correction = FALSE, na.rm = TRUE) 
{
  if (!na.rm && any(is.na(x))) 
    return(NA_real_)
  x <- as.numeric(na.omit(x))
  n <- length(x)
  x <- sort(x)
  G <- sum(x * 1L:n)
  G <- 2 * G/sum(x) - (n + 1L)
  if (correction) 
    G/(n - 1L)
  else G/n
}

################################################################################
# Shiny UI code #
################################################################################
# Define UI for application using a HTML function
ui <- fluidPage(
  
  # Application title
  titlePanel("Poverty & Inequality explorer!"),
  
  
  # Sidebar panel with instructions
  sidebarPanel(
    strong("Welcome to the Poverty & Inequality explorer!"),
    br(),
    p("This R Shiny web-app allows users to explore the World Bank PovcalNet's data & simulate basic redistributive policies."),
    br(),
    p("Ever wonder how redistributive policies affect inequality? With this Shiny app, users can select different countries and conduct simplified simulations of how redistribution affects inequality by adding (transfers) and subtracting (taxes) from mean monthly expenditure/consumption per capita."),
    # p("span does the same thing as div, but it works with",
    #   span("groups of words", style = "color:blue"),
    #   "that appear inside a paragraph."),
    br(),
    p("In the inputs below, add values corresponding to specific deciles. For example, a transfer to the poorest decile (decile 1) would, ceterus paribus, decrease inequality and lower the Gini coefficient."),
    selectInput(
      "country_select",
      label = "Input country here",
      choices = unique(distribution$wb_country),
      selected = NULL,multiple = FALSE,selectize = TRUE,width = NULL,size = NULL),
    strong("Add your transfer/tax value to simulate the analysis!"),
    textInput("transfer_value01","Decile 01:", 25, width = '50%'),
    textInput("transfer_value02","Decile 02:", 25, width = '50%'),
    textInput("transfer_value03","Decile 03:", 25, width = '50%'),
    textInput("transfer_value04","Decile 04:", 0, width = '50%'),
    textInput("transfer_value05","Decile 05:", 0, width = '50%'),
    textInput("transfer_value06","Decile 06:", 0, width = '50%'),
    textInput("transfer_value07","Decile 07:", 0, width = '50%'),
    textInput("transfer_value08","Decile 08:", 0, width = '50%'),
    textInput("transfer_value09","Decile 09:", 0, width = '50%'),
    textInput("transfer_value10","Decile 10:", 0, width = '50%')
  ),
  
    # Main panel with uncompressed and compressed image
    mainPanel(
      tabsetPanel(
        # tabPanel("Chart", plotOutput("plot",  width = "50%")),
        tabPanel("Simulator", plotOutput("plot_simulator", width = "50%"))
        #tabPanel("Table", tableOutput("table"))
      )
    ),

  uiOutput("tab")
)


################################################################################
# Shiny Server code #
################################################################################
# Define server logic custom function
server <- function(input, output) {
  
  # Plots
  output$plot_time <- renderPlot({
    temp_name <- distribution %>%
      filter(wbcountry == input$country_select) %>%
      # Grouping by country
      group_by(wbcountry) %>%
      # Pull out first available observations by country.
      dplyr::mutate(survey_first = first(year, order_by = year),
                    survey_last = last(year, order_by = year)) %>%
      ungroup() %>%
      select(datatype, survey_first, survey_last)

    country_select_welfare <- temp_name$datatype[1]
    country_select_survey_first <- temp_name$survey_first[1]
    country_select_survey_last <- temp_name$survey_last[1]

    temp <- distribution %>%
      filter(wbcountry == input$country_select, year == country_select_survey_first | year == country_select_survey_last)  %>%
      select(year, decile01_ave:decile10_ave) %>%
      pivot_longer(cols = c("decile01_ave":"decile10_ave"), names_to = "decile", values_to = "value") %>%
      dplyr::mutate(year = replace(year, year == country_select_survey_first, "Initial"),
                    year = replace(year, year == country_select_survey_last, "Latest")) %>%
      pivot_wider(names_from = "year", values_from = "value") %>%
      mutate(Diff = Latest - Initial,
             decile = as.factor(decile)) %>%
      select(-Latest) %>%
      pivot_longer(cols = c(-1), names_to = "year", values_to = "value") %>%
      dplyr::mutate(year = replace(year, year == "Initial", country_select_survey_first),
                    year = replace(year, year == "Diff", paste("Diff.", country_select_survey_last))) %>%
      mutate(year = as.factor(year))

    temp1 <- temp %>%
      ggplot(aes(y = value, x = decile, group = fct_rev(year), fill = fct_rev(year))) +
      geom_col() +
      labs(title = input$country_select,
           subtitle = paste("Level change in mean monthly", country_select_welfare ,
                            "by decile, USD PPP between", country_select_survey_first, "to", country_select_survey_last),
           y = "USD 2011 PPP",
           x = "") +
      scale_fill_manual(values = palette_4) +
      scale_x_discrete(labels=c("Decile 1","Decile 2","Decile 3", "Decile 4", "Decile 5",
                                "Decile 6", "Decile 7", "Decile 8", "Decile 9", "Decile 10"))

    temp2 <- distribution %>%
      filter(wbcountry == input$country_select)  %>%
      ggplot(aes(y = gini, x = year)) +
      geom_line(colour = "#a6393e") +
      labs(subtitle = "Gini coefficient over time.",
           y = "Gini",
           x = "Year")



    (temp1/temp2 + plot_layout(guides="collect") )


  }, height = 500, width = 1000)
  
  
  output$plot_simulator <- renderPlot({
    #########################################################
    # Data prep #
    #########################################################
    temp1 <- distribution %>%
      filter(wbcountry == input$country_select) %>%
      # Grouping by country
      group_by(wbcountry) %>%
      # Pull out first available observations by country. 
      dplyr::mutate(survey_last = last(year, order_by = year)) %>%
      ungroup() %>%
      select(datatype, survey_last)
    
    country_select_welfare <- temp1$datatype[1]
    country_select_survey_last <- temp1$survey_last[1]
    
    # Remove/drop old variables
    remove(temp1)
    
    # Create temp df for selected country
    country_select_dat <- distribution %>%
      filter(wbcountry == input$country_select, year == country_select_survey_last)  %>%
      select(year, decile01_ave:decile10_ave) %>%
      pivot_longer(cols = c("decile01_ave":"decile10_ave"), names_to = "decile", values_to = "NetIncomeExp") %>%
      # Add the vector of transfers
      dplyr::mutate(Transfers = 0,
                    Transfers = replace(Transfers, decile == "decile01_ave", as.numeric(input$transfer_value01)),
                    Transfers = replace(Transfers, decile == "decile02_ave", as.numeric(input$transfer_value02)),
                    Transfers = replace(Transfers, decile == "decile03_ave", as.numeric(input$transfer_value03)),
                    Transfers = replace(Transfers, decile == "decile04_ave", as.numeric(input$transfer_value04)),
                    Transfers = replace(Transfers, decile == "decile05_ave", as.numeric(input$transfer_value05)),
                    Transfers = replace(Transfers, decile == "decile06_ave", as.numeric(input$transfer_value06)),
                    Transfers = replace(Transfers, decile == "decile07_ave", as.numeric(input$transfer_value07)),
                    Transfers = replace(Transfers, decile == "decile08_ave", as.numeric(input$transfer_value08)),
                    Transfers = replace(Transfers, decile == "decile09_ave", as.numeric(input$transfer_value09)),
                    Transfers = replace(Transfers, decile == "decile10_ave", as.numeric(input$transfer_value10))) %>%
      dplyr::mutate(decile = as.factor(decile)) 
    
    #########################################################
    # Data prep: Gini #
    #########################################################
    country_select_trans_dat <- country_select_dat %>%
      dplyr::mutate(NetIncomeExpPlusTransfer = NetIncomeExp + Transfers)
    
    PreTransfer <- gini_calculation(country_select_trans_dat$NetIncomeExp, correction = FALSE)
    PostTransfer <- gini_calculation(country_select_trans_dat$NetIncomeExpPlusTransfer, correction = FALSE)
    
    gini_dat <- as_tibble(cbind(PreTransfer, PostTransfer))
    
    #########################################################
    # Data viz: Bar chart #
    #########################################################
    temp1 <- country_select_dat %>%
      pivot_longer(cols = c("Transfers", "NetIncomeExp"), names_to = "welfare", values_to = "value") %>%
      mutate(welfare = as.factor(welfare),
             welfare = factor(welfare, levels = c("Transfers", "NetIncomeExp"))) %>%
      ggplot(aes(y = value, x = decile, group = (welfare), fill = fct_rev(welfare))) +
      geom_col() +
      labs(title = input$country_select,
           subtitle = paste("Mean monthly", country_select_welfare ,
                            "by decile,
USD PPP in", country_select_survey_last, "with USD",
                            input$transfer_value,"mean monthly cash transfers."),
           y = "USD 2011 PPP",
           x = "") +
      scale_fill_manual(values = palette_4) +
      scale_x_discrete(labels=c("D1","D2","D3", "D4", "D5",
                                "D6", "D7", "D8", "D9", "D10")) + theme(legend.title = element_blank())

    # temp2 <- distribution %>%
    #   filter(wbcountry == input$country_select)  %>%
    #   ggplot(aes(y = gini, x = year)) +
    #   geom_line(colour = "#a6393e") +
    #   labs(subtitle = "Gini coefficient over time.",
    #        y = "Gini",
    #        x = "Year")
    
    #########################################################
    # Data viz: Slope chart #
    #########################################################
    temp2 <- gini_dat %>%
      pivot_longer(cols = c("PreTransfer", "PostTransfer"), names_to = "welfare", values_to = "value") %>%
      mutate(welfare = as.factor(welfare),
             welfare = factor(welfare, levels = c("PreTransfer", "PostTransfer"))) %>%
      # Creating line chart using ggplot2 package
      ggplot(aes(x = (welfare),
                 y = value)) +
      geom_point(colour = "#2d6074") +
      geom_line(aes(group = 1), colour = "#188c81") +
      # Title and caption 
      labs(subtitle = "Change in Gini coefficient due to transfers.",
           y = "Gini Coefficient (0-1)",
           x = "",
           caption = "Source: PovcalNet, Personal Calculations")

    ((temp1|temp2) + plot_layout(guides="collect") + theme(legend.position="top", legend.title = element_blank()))
    
    
    
  
}, height = 500, width = 1000)


}

# Run the application 
shinyApp(ui = ui, server = server)










# https://shiny.posit.co/r/articles/build/layout-guide/
