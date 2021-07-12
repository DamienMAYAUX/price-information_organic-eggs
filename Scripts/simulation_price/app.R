#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library("truncnorm")
library("mosaic")

rv = reactiveValues()

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Activism and Price : Simulator"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        
        sidebarPanel(
            
            sliderInput("conduct_parameter",
                        "Conduct parameter",
                        min = 0,
                        max = 1,
                        value = 0.5,
                        step = 0.1),
            
            sliderInput("cost",
                        "Cost difference between organic and other eggs",
                        min = 0,
                        max = 0.3,
                        value = 0.1,
                        step = 0.05),
            
            sliderInput("activist_log_mass",
                        "Mass of activists (opposite of the log base 10)",
                        min = 0,
                        max = 4,
                        value = 1,
                        step = 0.5),
            
            radioButtons(
                "passive_shape",
                "What is the distribution of the WTP among passive consumers ?",
                c(
                    "Lognormal" = "lognormal",
                    "Exponential" = "exponential",
                    "Truncated gaussian" = "truncated_gaussian"
                )
            ),
            
            sliderInput("passive_mean",
                        "Average WTP among passive consumers",
                        min = 0,
                        max = 0.3,
                        value = 0.1,
                        step = 0.01),
            
            sliderInput("passive_sigma",
                        "Standard deviation among passive consumers",
                        min = 0,
                        max = 1,
                        value = 0.5,
                        step = 0.01),
            
            radioButtons(
                "activist_shape",
                "What is the distribution of the WTP in the activist population ?",
                c(
                    "Lognormal" = "lognormal",
                    "Exponential" = "exponential",
                    "Truncated gaussian" = "truncated_gaussian"
                )
            ),
            
            sliderInput("activist_mean",
                        "Average WTP among the activists",
                        min = 0,
                        max = 0.5,
                        value = 0.15,
                        step = 0.01),
        
        sliderInput("activist_sigma",
                    "Standard deviation among the activists",
                    min = 0,
                    max = 1,
                    value = 0.1,
                    step = 0.01),
        
        ), # ENd of the sidebarPanel

        # Show a plot of the generated distribution
        mainPanel(
            
            tableOutput("result_table"),
            plotOutput("density_plot"),
            plotOutput("profit_plot"),
            plotOutput("price_setting_plot"),
            tableOutput("base")
        )
    )
)



# --------------------------------------------------------------------

server <- function(input, output) {
    
    activist_mass = reactive({
        10^(-input$activist_log_mass)
    })
    passive_mass = reactive({
        1-10^(-input$activist_log_mass)
    })
    
    ########### DEMAND FUNCTIONS ###########

    activist_demand = reactive({
        if (input$activist_shape == "lognormal"){
            output_list = list(
                density = function(x){
                    dlnorm(x, meanlog = log(input$activist_mean) - input$activist_sigma^2 / 2,
                           sdlog = input$activist_sigma)},
                cumulative = function(x){
                    plnorm(x, meanlog = log(input$activist_mean) - input$activist_sigma^2 / 2,
                           sdlog = input$activist_sigma, lower.tail = FALSE)}
            )
        } else if (input$activist_shape == "exponential") {
            output_list = list(
                density = function(x){dexp(x, rate = 1/input$activist_mean)},
                cumulative = function(x){pexp(x, rate = 1/input$activist_mean, lower.tail = FALSE)}
            )
        } else if (input$activist_shape == "truncated_gaussian"){
            output_list = list(
                density = function(x){
                    dtruncnorm(a = 0, mean = input$activist_mean, sd = input$activist_sigma)},
                cumulative = function(x){
                    ptruncnorm(a = 0, mean = input$activist_mean, sd = input$activist_sigma, lower.tail = FALSE)}
            )
        } else { stop("Unknown shape for the activist distribution") }
    })
    
    passive_demand = reactive({
        if (input$passive_shape == "lognormal"){
            output_list = list(
                density = function(x){
                    dlnorm(x, meanlog = log(input$passive_mean) - input$passive_sigma^2 / 2,
                           sdlog = input$passive_sigma)},
                cumulative = function(x){
                    plnorm(x, meanlog = log(input$passive_mean) - input$passive_sigma^2 / 2,
                           sdlog = input$passive_sigma, lower.tail = FALSE)}
            )
        } else if (input$passive_shape == "exponential") {
            output_list = list(
                density = function(x){dexp(x, rate = 1/input$passive_mean)},
                cumulative = function(x){pexp(x, rate = 1/input$passive_mean, lower.tail = FALSE)}
            )
        } else if (input$passive_shape == "truncated_gaussian"){
            output_list = list(
                density = function(x){
                    dtruncnorm(a = 0, mean = input$passive_mean, sd = input$passive_sigma)},
                cumulative = function(x){
                    ptruncnorm(a = 0, mean = input$passive_mean, sd = input$passive_sigma, lower.tail = FALSE)}
            )
        } else { stop("Unknown shape for the passive distribution") }
    })
    
    base_dataset = reactive({
        
        activist_list = activist_demand()
        passive_list = passive_demand()
        
        nb_points = 1000
        data.frame(x = as.double(1:as.integer(1 + 20*nb_points*input$cost)/nb_points))%>%
            mutate(
                activist_density = activist_list$density(x),
                activist_cumulative = activist_list$cumulative(x),
                
                passive_density = passive_list$density(x),
                passive_cumulative = passive_list$cumulative(x),
                
                total_density = 
                    passive_mass() * passive_density +
                    activist_mass() * activist_density,
                total_cumulative = 
                    passive_mass() * passive_cumulative +
                    activist_mass() * activist_cumulative,
                
                total_profit = total_cumulative * (x-input$cost),
                passive_profit = passive_cumulative * (x-input$cost),
                activist_profit = passive_profit
                + activist_mass()
            )
    })
    
    # For debugging, this prints the main dataframe
    #output$base = renderTable({base_dataset()})
    
    ########### PRICES ###############
    
    compute_monopolist_price = reactive({

        base_dataset()%>%
            filter(x > input$cost)%>%
            mutate(
                dist = abs(total_profit - max(total_profit))
            )%>%
            filter(dist < max(total_profit) * 10^(-5))%>%
            .$x%>%
            .[1]

    })
    
    compute_total_price = reactive({
        
        base_dataset()%>%
            filter(x < compute_monopolist_price())%>%
            mutate(
                y = (x - input$cost) * total_density / total_cumulative,
                dist = abs(y - input$conduct_parameter)
            )%>%
            filter( abs(dist - min(dist)) < min(dist) * 10^(-5) )%>% .$x%>% .[1]
    })
    
    compute_passive_price = reactive({
        
        base_dataset()%>%
            filter(x < compute_monopolist_price())%>%
            mutate(
                y = (x - input$cost) * passive_density / passive_cumulative,
                dist = abs(y - input$conduct_parameter)
            )%>%
            filter( abs(dist - min(dist)) < min(dist) * 10^(-5) )%>% .$x%>% .[1]
        
    })
    
    compute_activist_price = reactive({
        
        passive_price = compute_passive_price()
        
        base_dataset()%>%
            filter(x < compute_monopolist_price())%>%
            mutate(
                is_passive_price = abs(x - passive_price) < 10^(-5) * passive_price,
                dist = abs(passive_profit + activist_mass() * (x - input$cost) - sum(is_passive_price * passive_profit) ),
            )%>%
            filter( abs(dist - min(dist)) < min(dist) * 10^(-5) )%>% .$x%>% .[1]
        
    })
    

    ########### PLOTS ############
    
    plot_range = reactive({
        base_dataset()%>%
            filter(
                x > input$cost, 
                x < 1.2 * compute_monopolist_price()
                )
    })
    
    output$result_table = renderTable({

        passive_list = passive_demand()
        activist_list = activist_demand()

        base_cost = 0.1
        nb_digits = 5

        data.frame(
            Scenario = c("Total population", "Passive consumers only", "Total population with buycott"),
            "Price of an organic egg (in euros)" = c(compute_total_price(), compute_passive_price(), compute_activist_price()) + base_cost,
            "Change (in percent)" = c(
                paste0(
                    "From 1 to 3 : ",
                    trunc(100 * (compute_activist_price() - compute_total_price()) / (base_cost + compute_total_price()), digits = nb_digits)
                ),
                paste0(
                    "From 1 to 2 : ",
                    trunc(100 * (compute_passive_price() - compute_total_price()) / (base_cost + compute_total_price()), digits = nb_digits)
                ),
                paste0(
                    "From 2 to 3 : ",
                    trunc( 100 * (compute_activist_price() - compute_passive_price()) / (base_cost + compute_passive_price()), digits = nb_digits)
                )
            ),
            "Share of organic egg consumers (in %)" = c(
                100 * (activist_mass() * activist_list$cumulative(compute_total_price()) +
                           passive_mass() * passive_list$cumulative(compute_total_price())),
                100 * passive_list$cumulative(compute_passive_price()),
                100 * (activist_mass() + passive_list$cumulative(compute_activist_price()))
            ),
            check.names = FALSE
        )
    })

    output$density_plot <- renderPlot({

        activist_list = activist_demand()
        #total_list = total_demand()
        passive_list = passive_demand()

        ggplot(
            plot_range()%>%
                mutate(
                    activist_density = activist_mass() * activist_density,
                    passive_density = passive_mass() * passive_density
                    )%>%
                pivot_longer(cols = c("passive_density", "activist_density"),
                             names_to = "population", values_to = "density"),
            aes(x)
        )+
            geom_area(aes(y = density, fill = population), position = "stack")+
            geom_vline(xintercept = compute_total_price(), color = "purple")+
            xlab("Price") + ylab("Density")+
            scale_linetype_discrete(name = "")+
            theme(legend.position = "bottom")

    })

    output$price_setting_plot = renderPlot({

        activist_list = activist_demand()

        ggplot(
            plot_range()%>%
                mutate(
                    lerner = (x - input$cost)/x,
                    total_demand_elasticity = x * total_density / total_cumulative,
                    passive_demand_elasticity = x * passive_density / passive_cumulative,
                    total_conduct = lerner * total_demand_elasticity,
                    passive_conduct = lerner * passive_demand_elasticity
                ),
            aes(x)
        )+
            geom_line(aes(y = lerner, linetype = "Lerner index"), size = 1)+
            #geom_line(aes(x, y = demand_elasticity, color = "Demand elasticity"))+
            #geom_line(aes(x, y = demand_elasticity, color = "Demand elasticity"))+
            geom_line(aes(y = total_conduct, linetype = "Demand-elasticity-adjusted Lerner index"), size = 1, color = "purple")+
            geom_line(aes(y = passive_conduct, linetype = "Demand-elasticity-adjusted Lerner index"), size = 1, color = "blue")+
            geom_vline(xintercept = compute_passive_price(), color = "blue")+
            geom_vline(xintercept = compute_total_price(), color = "purple")+
            geom_hline(yintercept = input$conduct_parameter, color = "brown")+
            geom_hline(yintercept = 0)+
            scale_y_continuous(limits = c(0, 1))+
            xlab("Price") + ylab("Lerner index")+
            theme(legend.position = "bottom")

    })

    output$profit_plot = renderPlot({

        passive_list = passive_demand()

        ggplot( plot_range(), aes(x) )+
            geom_line(aes(y = total_profit, color = "Full population"))+
            geom_line(aes(y = passive_mass() * passive_profit, color = "Passive consumers only"))+
            geom_vline(xintercept = compute_passive_price(), color = "blue")+
            geom_vline(xintercept = compute_activist_price(), color = "red")+
            geom_vline(xintercept = compute_total_price(), color = "purple")+
            geom_segment(
                x = compute_activist_price(),
                y = passive_mass() * passive_list$cumulative(compute_activist_price()) * (compute_activist_price() - input$cost),
                xend = compute_activist_price(),
                yend = 
                    (passive_mass() * passive_list$cumulative(compute_activist_price()) + activist_mass()) 
                * (compute_activist_price() - input$cost), 
                size = 1
                )+
            geom_segment(
                x = compute_activist_price(),
                y = (passive_mass() * passive_list$cumulative(compute_activist_price()) + activist_mass()) 
                * (compute_activist_price() - input$cost),
                xend = compute_passive_price(),
                yend = 
                    (passive_mass() * passive_list$cumulative(compute_activist_price()) + activist_mass()) 
                * (compute_activist_price() - input$cost), 
                size = 1
            )+
            theme(legend.position = "bottom")+
            ylab("Profit")+ xlab("Price")+
            scale_color_discrete("Scenario")
            
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
