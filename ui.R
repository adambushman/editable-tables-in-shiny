library('shiny')
library('shinythemes')
library('readxl')
library('rhandsontable')
library('reactablefmtr')

data <- read_xlsx('sim_ex_data.xlsx')

shinyUI(fluidPage(
  theme = shinytheme("united"),

  # Application title
  navbarPage("Pattern Profitability Analysis",

             tabPanel("Begin Here",

                      # Sidebar with a slider input for number of bins
                      sidebarLayout(
                        sidebarPanel(
                          h3("WHY"),
                          p("Preserve TETON's mission of helping others to \"Get Outdoors and Enjoy Life\" by realizing the benefits of a marketplace channel while ensuring required profitability."),
                          h3("HOW"),
                          p("Making correct assumptions that will demonstrate the reality of the channel's profitability in order to discover course correction initiatives."),
                          h3("WHAT"),
                          p("Use the tool's features and output to inform and prepared future negotiations with Pattern, as well as other relevant activities internal to TETON.")
                        ),

                        # Show a plot of the generated distribution
                        mainPanel(
                          h1("Begin Here"),
                          p("TETON's new Marketplace business model is an exciting new chapter but presents challenges in evaluating profitability. No longer can we evaluate the channel's health via top-line measures. We must instead estimate bottom-line values with the proper assumptions. This tool aims to simplify that task."),
                          p("Within this tool, we can perform various analyses to identify which SKUs in Pattern's catalog we must improve. Here's an explanation of the features:"),
                          tags$br(),
                          h3("Changing Assumptions"),
                          p("It's important that we can assess where we are with current reality while also being able to change our assumptions."),
                          p("On the \"Assumptions\" tab, you can alter assumptions on allowances, advertising, operating expenses, and discount percentage off retail."),
                          tags$br(),
                          h3("Evaluating the Catalog"),
                          p("Each SKU contributes to profitability differently. They have different negotiated discounts, costs, volume, and variance. We can evaluate the entire catalog using these values to find where we can improve."),
                          p("On the \"Catalog\" tab, you can evaluate the entire catalog while choosing which profitability perspective and buying method you're interested in."),
                          tags$br(),
                          h3("Item Detail"),
                          p("The items requiring improvement we identify in the \"Catalog\" need greater assessment. We'll need a full picture comparison from top to bottom by buying method and channel."),
                          p("On the \"SKU Detail\" tab, we can filter for a specific item to analyze all the contributing factors and where the levers are."),
                          tags$br(),
                          h3("Simulation"),
                          p("Every change implmeneted at the SKU level and every new assumption will have an effect on the overall profitability of the channel. It's important to evaluate the size of the effect and if it achieves our business goals."),
                          p("The \"Simulation\" tab will use the new values populated on the\"Assumptions\" tab to estimate profitability over a calendar year. We'll be able to identify the likely range of outcomes and determine if it's satisfactory.")
                        )
                      )
             ),
             tabPanel("Assumptions",

                      # Sidebar with a slider input for number of bins
                      sidebarLayout(
                        sidebarPanel(
                          h3("Instructions"),
                          p("On the \"Expenses\" tab, review the assumption values (as percent decimals) by buying method and channel, populating new values if desired"),
                          p("On the \"Discounts'\" tab, review the discount % off retail negotiated with Pattern, populating new values if desired"),
                          tags$br(),
                          p("*NOTE* Changes to these values will not remain for future sessions; please keep a record of your finalized assumptions.")
                        ),

                        # Show a plot of the generated distribution
                        mainPanel(
                          h1("Assumptions"),
                          tabsetPanel(
                            tabPanel("Expenses",
                                     actionButton("update_assump",label = "Save"),
                                     tags$br(),
                                     rHandsontableOutput("assumpTabl")
                                     ),
                            tabPanel("Discount %",
                                     actionButton("update_prices",label = "Save"),
                                     tags$br(),
                                     rHandsontableOutput("priceTabl")
                                     )
                          )
                        )
                      )
             ),
             tabPanel("Catalog",
                      sidebarLayout(
                        sidebarPanel(
                          h3("Inputs"),
                          selectInput("perspective_in", "Profitability Perspective", c("Gross", "Net")),
                          selectInput("buying_in", "Buying Method", c("Direct Import", "Domestic"))
                        ),
                        mainPanel(
                          h1("Catalog"),
                          reactableOutput("tablCatalog")
                        )
                      )
             ),
             tabPanel("Item Detail",
                      sidebarLayout(
                        sidebarPanel(
                          h3("Inputs"),
                          selectInput("itemDetail_in", "Item #", unique(data$item))
                        ),

                        # Show a plot of the generated distribution
                        mainPanel(
                          h1("Item Detail"),
                          tableOutput("tablDetail")
                        )
                      )
             ),
             tabPanel("Simulation",

                      # Sidebar with a slider input for number of bins
                      sidebarLayout(
                        sidebarPanel(
                          h3("Explanation"),
                          p("If we could play out the next calendar year of Pattern sales 100 times, what range of results might we see?"),
                          p("This is the aim of the simulation. By using the values in the \"Assumptions\" tab, we can run scenarios for the future to get an idea of what's possible and likely."),
                          h3("Interpretation"),
                          p("Look at how wide each mountain is. That tells us what profitability is POSSIBLE. Look at how wide and tall the peaks are. That tells us what profitability is LIKELY."),
                          h3("Inputs"),
                          selectInput("perspectiveSim_in", "Profitability Perspective", choices = c("Net", "Gross"))
                        ),

                        # Show a plot of the generated distribution
                        mainPanel(
                          h1("Simulation"),
                          plotOutput("densityResults")
                        )
                      )
             )
  )
))
