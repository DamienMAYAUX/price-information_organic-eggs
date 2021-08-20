

library(shiny)
library(tidyverse)
library("truncnorm")
library("mosaic")

rv = reactiveValues()


ui <- fluidPage(

    # Application title
    titlePanel("Activism and Price : Simulator"),

    
    sidebarLayout(
        
        sidebarPanel(
            
            sliderInput("conduct_parameter",
                        "Conduct parameter",
                        min = 0,
                        max = 1,
                        value = 0.25,
                        step = 0.02),
            
            sliderInput("cost",
                        "Wholesale price of organic eggs",
                        min = 0,
                        max = 0.4,
                        value = 0.2,
                        step = 0.01),
            
            sliderInput("activist_log_mass",
                        "Mass of activists (opposite of the log base 10)",
                        min = 0,
                        max = 4,
                        value = 2,
                        step = 0.5),
            
            sliderInput("activist_mean",
                        "Average WTP among the activists",
                        min = 0,
                        max = 1,
                        value = 0.5,
                        step = 0.01),
            
            sliderInput("activist_sigma",
                        "Standard deviation among the activists",
                        min = 0,
                        max = 1,
                        value = 0.2,
                        step = 0.01),
            
            uiOutput("activist_additional_parameter"),
            
            radioButtons(
                "activist_shape",
                "What is the distribution of the WTP in the activist population ?",
                c(
                    "Lognormal" = "lognormal",
                    "Exponential" = "exponential"
                    )
            ),
            
            sliderInput("passive_mean",
                        "Location parameter for passive consumers",
                        min = 0,
                        max = 5,
                        value = 2.43,
                        step = 0.01),
            
            sliderInput("passive_sigma",
                        "Dispersion parameter for passive consumers",
                        min = 0,
                        max = 1,
                        value = 0.85,
                        step = 0.01),
            
            uiOutput("passive_additional_parameter"),
            
            radioButtons(
                "passive_shape",
                "What is the distribution of the WTP among passive consumers ?",
                c(
                    "Inverse of a lognormal" = "inv_lognormal",
                    "Lognormal" = "lognormal",
                    "Exponential" = "exponential"
                )
            ),
        
        ), # End of the sidebarPanel

        
        mainPanel(
            
            verbatimTextOutput("debug"),
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
    
    
    ###### PRINT FOR DEBUG #######
    
    output$debug = renderText({
        #compute_monopolist_price()
        #20*input$cost
        })
    
    output$base = renderTable({
        base_dataset()
    })
    
    ######## CONSTANTS #########
    
    activist_mass = reactive({
        10^(-input$activist_log_mass)
    })
    passive_mass = reactive({
        1-10^(-input$activist_log_mass)
    })
    
    ########## VARIABLE USER INTERFACE ###########
    
    output$passive_additional_parameter = renderUI({
        
        if (input$passive_shape == "inv_lognormal"){
            sliderInput(
                "consumer_valuation_organic", 
                label = "Consumer valuation of organic eggs",
                min = 0, max = 5, value = 3.66, 
                step = 0.01
                )
        }
        
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
        } else { stop("Unknown shape for the activist distribution") }
    })

    passive_demand = reactive({
        if (input$passive_shape == "lognormal"){
            output_list = list(
                density = function(x){
                    dlnorm(x, meanlog = input$passive_mean,
                           sdlog = input$passive_sigma)},
                cumulative = function(x){
                    plnorm(x, meanlog = input$passive_mean,
                           sdlog = input$passive_sigma, lower.tail = FALSE)}
            )
        } else if (input$passive_shape == "exponential") {
            output_list = list(
                density = function(x){dexp(x, rate = 1/input$passive_mean)},
                cumulative = function(x){pexp(x, rate = 1/input$passive_mean, lower.tail = FALSE)}
            )
        } else if (input$passive_shape == "inv_lognormal"){
            output_list = list(
                density = function(x){
                    dlnorm(input$consumer_valuation_organic / x, meanlog = input$passive_mean,
                       sdlog = input$passive_sigma) * input$consumer_valuation_organic / x^2},
                cumulative = function(x){
                    plnorm(input$consumer_valuation_organic / x, meanlog = input$passive_mean,
                           sdlog = input$passive_sigma)}
                # The absence of lower.tail = FALSE is normal
            )
        } else { stop("Unknown shape for the passive distribution") }
    })

    base_dataset = reactive({

        activist_list = activist_demand()
        passive_list = passive_demand()

        nb_points = 500
        #min_price = 3 * input$cost / 4
        min_price = 0
        max_price = 3 * input$cost
        # data.frame(x = as.double(1:as.integer(1 + 20*nb_points*input$cost)/nb_points))
        
        data.frame(x = min_price + 1:nb_points * (max_price - min_price)/nb_points)%>%
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
    
    

    ########### PRICES ###############
    
    compute_monopolist_price = reactive({

        base_dataset()%>%
            filter(x > input$cost)%>%
            arrange(-total_profit)%>%
            .$x%>%
            .[1]

    })
    
    compute_total_price = reactive({
        
        base_dataset()%>%
            filter(x > input$cost, x < compute_monopolist_price())%>%
            mutate(
                y = (x - input$cost) * total_density / total_cumulative,
                dist = abs(y - input$conduct_parameter)
            )%>%
            arrange(dist)%>% .$x%>% .[1]
    })
    
    compute_passive_price = reactive({
        
        base_dataset()%>%
            filter(x > input$cost, x < compute_monopolist_price())%>%
            mutate(
                y = (x - input$cost) * passive_density / passive_cumulative,
                dist = abs(y - input$conduct_parameter)
            )%>%
            arrange(dist)%>% .$x%>% .[1]
        
    })
    
    compute_activist_price = reactive({
        
        passive_price = compute_passive_price()
        passive_price_profit = base_dataset()%>%
            filter(x == passive_price)%>% .$passive_profit%>% .[1]
        
        base_dataset()%>%
            filter(x > input$cost, x < compute_monopolist_price())%>%
            mutate(
                dist = abs(passive_profit + activist_mass() * (x - input$cost) - passive_price_profit ),
            )%>%
            arrange(dist)%>% .$x%>% .[1]
        
    })
    

    ########### PLOTS ############
    
    plot_range = reactive({
        base_dataset()%>%
            filter(
                x > input$cost, 
                # x < 1.2 * compute_monopolist_price()
                x < 1.2 * compute_total_price()
                )
    })
    
    output$result_table = renderTable({

        passive_list = passive_demand()
        activist_list = activist_demand()

        nb_digits = 2

        data.frame(
            Setting = c("Total population", "Passive consumers only", "Total population with buycott"),
            "Retail price (in €)" = c(compute_total_price(), compute_passive_price(), compute_activist_price()),
            "Passive consumption (in %)" = 100 * c(
                passive_list$cumulative(compute_total_price()),
                passive_list$cumulative(compute_passive_price()),
                passive_list$cumulative(compute_activist_price())
            ),
            "Price change (in %)" = c(
                paste0(
                    "Total effect : ",
                    round( 100 * (compute_activist_price() - compute_total_price()) / compute_total_price(), digits = nb_digits)
                ),
                paste0(
                    "Overvaluation effect : ",
                    round( 100 * (compute_passive_price() - compute_total_price()) / compute_total_price(), digits = nb_digits)
                ),
                paste0(
                    "Boycott effect : ",
                    round( 100 * (compute_activist_price() - compute_passive_price()) / compute_passive_price(), digits = nb_digits)
                )
            ),
            check.names = FALSE
        )
    })

    output$density_plot <- renderPlot({

        passive_list = passive_demand()
        activist_list = activist_demand()

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
            theme(legend.position = "bottom", axis.text.y = element_blank())

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
            geom_line(aes(y = total_conduct, linetype = "Adjusted Lerner index"), size = 1, color = "purple")+
            geom_line(aes(y = passive_conduct, linetype = "Adjusted Lerner index"), size = 1, color = "blue")+
            geom_vline(xintercept = compute_passive_price(), color = "blue")+
            geom_vline(xintercept = compute_total_price(), color = "purple")+
            geom_hline(yintercept = input$conduct_parameter, color = "brown")+
            geom_hline(yintercept = 0)+
            scale_y_continuous(limits = c(0, 1))+
            xlab("Price") + ylab("Lerner index")+
            theme(legend.position = "bottom", axis.text.y = element_blank())

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
            theme(legend.position = "bottom", axis.text.y = element_blank())+
            ylab("Profit")+ xlab("Price")+
            scale_color_discrete("Setting")
            
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
