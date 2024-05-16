dashboardSidebar(
  width = 310,
  div(class = "inlay", style = "height:10px;;"),
  div(style = "overflow: visible;width:inherit;padding-left:20px;",
      sidebarMenu(
        # menuItem(
        #   "Stock Symbols Selection",
        #   tabName = "stock_symbol",
        #   icon = icon("dashboard"),
        #   hr(),
          pickerInput(
            inputId = "stickers",
            label = "Select Stock Symbols: ",
            choices = sticker$symbol,
            multiple = TRUE,
            selected = c("AMZN", "AAPL"),
            options = list(
              `max-options` = 8,
              `actions-box` = TRUE,
              `live-search` = TRUE,
              `virtual-scroll` = 10,
              `multiple-separator` = "\n",
              size = 10
            ),
            choicesOpt = list(content = stringr::str_trunc(sticker$choices, width = 30))
          ),
          dateInput(
            "dt_frome",
            "Date from:",
            value = Sys.Date() - 720,
            max = Sys.Date() - 360
          ),
          dateInput("dt_to", "Date to:", value = Sys.Date(), min = Sys.Date() -
                      360),
          actionButton("submit", "Get Symbols", icon("paper-plane"),
                       style = "color: #fff; background-color: #32907c; border-color: #32907c"),
          br(),
          hr()
        )
      )
)
