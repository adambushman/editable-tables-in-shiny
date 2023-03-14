library('shiny')
library('rhandsontable')
library('readxl')
library('tidyverse')
library('kableExtra')
library('reactablefmtr')

sales <- read_xlsx('sim_ex_data.xlsx')
assumptions <- read_xlsx('sim_ex_assump.xlsx')
price_market <- read_xlsx('sim_ex_pricing-market.xlsx')
price_whole <- read_xlsx('sim_ex_pricing-whole.xlsx')

shinyServer(function(input, output, session){

  # REACTIVE VALUES
  assump_react <- reactiveValues(
    data =
      assumptions %>%
      pivot_wider(names_from = assumption, values_from = value)
  )

  price_react <- reactiveValues(
    data = price_market
  )


  # REACTIVE INPUT TABLES
  output$assumpTabl <- renderRHandsontable({
    rhandsontable(assump_react$data, stretchH = "all") %>%
      hot_cols(columnSorting = TRUE) %>%
      hot_col(c("scenario", "buying"), readOnly = TRUE) %>%
      hot_col(c("Allowance", "Advertising", "Opex"), format = "0.0%", language = "en-US")
  })

  output$priceTabl <- renderRHandsontable({
    rhandsontable(price_react$data, stretchH = "all") %>%
      hot_cols(columnSorting = TRUE) %>%
      hot_col(c("item", "buying"), readOnly = TRUE) %>%
      hot_col("map_market", format = "$0,00.00", language = "en-US") %>%
      hot_col("discount_perc", format = "0.0%", language = "en-US")
  })

  observeEvent(input$update_assump,
               assump_react$data <<- hot_to_r(input$assumpTabl))

  observeEvent(input$update_prices,
               price_react$data <<- hot_to_r(input$priceTabl))


  # CORE TRANSFORMS
  i <- reactive({
    assumps <- assump_react$data
    assumps$id = paste(assumps$scenario, assumps$buying)

    prices <- price_react$data
    prices$scenario = "Marketplace"
    prices$id = paste(prices$item, prices$scenario, prices$buying, sep="-")
    prices$price_market = round((1 - prices$discount_perc) * prices$map_market, 2)

    sales %>%
      mutate(id = paste(item, scenario, buying, sep="-")) %>%
      left_join(prices) %>%
      left_join(
            price_whole %>%
              mutate(scenario = "Wholesale",
                     id = paste(item, scenario, buying, sep="-"))
          ) %>%
      mutate(price = coalesce(price_market, price_whole),
             map = coalesce(map_market, map_whole)) %>%
      select(item, scenario, buying , cost, price, map, units, noise) %>%
      mutate(id = paste(scenario, buying)) %>%
      left_join(assumps) %>%
      mutate(gDollars = price - cost - (price * Allowance),
             gMargin = gDollars / price,
             allowanceP = Allowance,
             advertising = ifelse(scenario == "Marketplace", map * Advertising, price * Advertising),
             advertisingP = Advertising,
             opex = price * Opex,
             opexP = Opex,
             nDollars = gDollars - advertising - opex,
             nMargin = nDollars / price) %>%
      mutate(map = paste("$", round(map, 2), sep=""),
             price = paste("$", round(price, 2), sep=""),
             cost = paste("$", round(cost, 2), sep=""),
             allowanceP = paste(allowanceP * 100, "%", sep=""),
             gDollars = paste("$", round(gDollars, 2), sep=""),
             gMargin = paste(round(gMargin * 100, 1), "%", sep=""),
             advertising = paste("$", round(advertising, 2), sep=""),
             advertisingP = paste(advertisingP * 100, "%", sep=""),
             opex = paste("$", round(opex, 2), sep=""),
             opexP = paste(opexP * 100, "%", sep=""),
             nDollars = paste("$", round(nDollars, 2), sep=""),
             nMargin = paste(round(nMargin * 100, 1), "%", sep=""))
  })





  # TABLE DETAIL
  detailDone <- reactive({
    req(input$itemDetail_in)
    i() %>%
      filter(item == input$itemDetail_in) %>%
      select(map, price, cost, allowanceP, gDollars, gMargin,
             advertising, advertisingP, opex, opexP, nDollars, nMargin) %>%
      t(.)

  })

  output$tablDetail <- function() {
    kbl(detailDone(), col.names = rep(c("Direct Import", "Domestic"), 2)) %>%
      kable_styling(bootstrap_options = c("striped", "hover")) %>%
      add_header_above(c(" " = 1, "Marketplace" = 2, "Wholesale" = 2)) %>%
      pack_rows("Retail", 1, 1) %>%
      pack_rows("Gross", 2, 6) %>%
      pack_rows("Net", 7, 12)
  }

  catalogDone <- reactive({
    req(input$buying_in)
    req(input$perspective_in)
    z <-
      i() %>%
      filter(buying == input$buying_in) %>%
      select(item, scenario, units, noise
             | starts_with(stringr::str_to_lower(stringr::str_sub(input$perspective_in, 1, 1)))) %>%
      rename_at(vars(contains("Dollars")), list(~str_extract(., "Dollars"))) %>%
      rename_at(vars(contains("Margin")), list(~str_extract(., "Margin"))) %>%
      mutate(Dollars = as.numeric(str_sub(Dollars, 2, str_length(Dollars))),
             Margin = as.numeric(str_sub(Dollars, 1, str_length(Dollars)-1))) %>%
      pivot_wider(names_from = scenario, values_from = names(.)[3:6])



    g <- z %>% mutate_at(vars(names(.)[2:5]), ~replace_na(., mean(., na.rm = TRUE)))
    g <- g %>% mutate_at(names(.)[2:length(names(.))], ~(scale(.) %>% as.vector))
    g$scale_M = unlist(as.vector((g[,2] * 0.3) + (g[,4] * 0.15) + (g[,6] * 0.3) + (g[,8] * 0.25)), use.names = FALSE)
    g$scale_W = unlist(as.vector((g[,3] * 0.3) + (g[,5] * 0.15) + (g[,7] * 0.3) + (g[,9] * 0.25)), use.names = FALSE)

    inner_join(
      z,
      g %>%
        mutate(scale_Diff = scale_M - scale_W) %>%
        select(item, scale_M, scale_W, scale_Diff),
      by = c("item" = "item")
    )
  })

  output$tablCatalog <- renderReactable ({
    reactable(
      catalogDone(),
      columnGroups = list(
        colGroup(name = "Units", columns = c("units_Marketplace", "units_Wholesale")),
        colGroup(name = "Noise (StDev)", columns = c("noise_Marketplace", "noise_Wholesale")),
        colGroup(name = paste(input$perspective_in, "Dollars"), columns = c("Dollars_Marketplace", "Dollars_Wholesale")),
        colGroup(name = paste(input$perspective_in, "Margin"), columns = c("Margin_Marketplace", "Margin_Wholesale")),
        colGroup(name = "Evaluation", columns = c("scale_M", "scale_W", "scale_Diff"))
      ),
      columns = list(
        item = colDef(
          name = "Item #"
        ),
        units_Marketplace = colDef(
          name = "Marketplace",
          format = colFormat(separators = TRUE)
        ),
        units_Wholesale = colDef(
          name = "Wholesale",
          format = colFormat(separators = TRUE)
        ),
        noise_Marketplace = colDef(
          name = "Marketplace",
          format = colFormat(separators = TRUE)
        ),
        noise_Wholesale = colDef(
          name = "Wholesale",
          format = colFormat(separators = TRUE)
        ),
        Dollars_Marketplace = colDef(
          name = "Marketplace",
          format = colFormat(prefix = "$", digits = 2, separators = TRUE)
        ),
        Dollars_Wholesale = colDef(
          name = "Wholesale",
          format = colFormat(prefix = "$", digits = 2, separators = TRUE)
        ),
        Margin_Marketplace = colDef(
          name = "Marketplace",
          format = colFormat(suffix = "%", digits = 1, separators = TRUE)
        ),
        Margin_Wholesale = colDef(
          name = "Wholesale",
          format = colFormat(suffix = "%", digits = 1, separators = TRUE)
        ),
        scale_M = colDef(
          name = "Marketplace",
          format = colFormat(digits = 3),
          style = color_scales(catalogDone())
        ),
        scale_W = colDef(
          name = "Wholesale",
          format = colFormat(digits = 3),
          style = color_scales(catalogDone())
        ),
        scale_Diff = colDef(
          name = "Difference",
          format = colFormat(digits = 3),
          style = color_scales(catalogDone())
        )
      )
    )
  })


  output$densityResults <- renderPlot({
    ggplot(assump_react$data %>%
             pivot_longer(cols = 3:5,
                          names_to = "assumption",
                          values_to = "value"),
           aes(assumption, value)) +
      geom_bar(aes(fill = scenario), stat = "identity", position = "dodge") +
      facet_wrap(~buying)
  })

})
