library(tidyverse)
library(httr)
library(modeest)
library(ggrepel)
library(ggpmisc)
library(gridExtra)
library(extrafont)
library(matrixStats)
library(shiny)
library(shinydashboard)
library(DT)
library(rintrojs)
library(shinyjs)
library(waiter)
library(shinyWidgets)
library(emayili)

# Initialise session
my_url <- "https://www.amazon.com/s?k=reusable+straws"
user_agent <- user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) 
                         AppleWebKit/537.36 (KHTML, like Gecko) Chrome/74.0.3729.169 Safari/537.36")
my_session <- session(my_url, user_agent)

### Functions and variables ###
get_keyword_overview <- function(keyword_link) {
  keyword_page <- my_session %>% session_jump_to(keyword_link)
  
  product_title <- keyword_page %>%
    html_nodes(".a-color-base.a-text-normal") %>%
    html_text() %>%
    str_trim()
  
  price_whole <- keyword_page %>%
    html_nodes(".a-price-whole") %>%
    html_text()
  
  price_fraction <- keyword_page %>%
    html_nodes(".a-price-fraction") %>%
    html_text()
  
  product_prices <- as.numeric(str_c(price_whole, price_fraction))
  
  review_rating <- keyword_page %>%
    html_nodes(".aok-align-bottom") %>%
    html_text() %>%
    str_sub(., 1, 3) %>%
    as.numeric(.)
  
  review_count <- keyword_page %>%
    html_nodes(".a-size-small .a-link-normal .a-size-base") %>%
    html_text() %>%
    gsub(",", "", .) %>%
    as.integer(.)
  
  asin.pattern <- "[B][0].{8}"
  product_ASIN <- keyword_page %>%
    html_nodes(".a-size-base.a-link-normal.a-text-normal") %>%
    html_attr("href") %>%
    str_extract(., asin.pattern)
  
  product_links <- paste0("https://www.amazon.com/dp/", product_ASIN)
  
  max_length <- max(c(length(product_title), length(product_ASIN), length(product_prices), 
                      length(review_rating), length(review_count), length(product_links)))
  length(product_title) <- max_length
  length(product_ASIN) <- max_length
  length(product_prices) <- max_length
  length(review_rating) <- max_length
  length(review_count) <- max_length
  length(product_links) <- max_length
  
  keyword_data <- cbind(product_title, product_ASIN, product_prices, review_rating, review_count, product_links)
  
  return(keyword_data)
}
get_keyword_pages <- function(original_link, number_of_pages) {
  result_df <- data.frame()
  for(page_number in c(1:number_of_pages)) {
    current_url <- paste0(original_link, page_number)
    result_single_page <- data.frame(get_keyword_overview(current_url))
    result_df <- rbind(result_df, result_single_page)
  }
  return(result_df)
}
replace_price_text <- function(text) {
  result <- str_to_upper(str_replace_all(text, "_", " "))
  return(result)
}

theme_hc <- function(){
  theme(
    text                = element_text(family = "Bahnschrift", size = 14),
    plot.title          = element_text(face = "bold"),
    plot.title.position = "panel",
    plot.subtitle       = element_text(face = "italic"),
    axis.text           = element_text(face = "bold"),
    legend.title        = element_blank(),
    legend.position     = "none",
    panel.grid.major.y  = element_line(color = "gray", size = 0.5),
    panel.grid.minor.y  = element_blank(),
    panel.grid.major.x  = element_blank(),
    panel.grid.minor.x  = element_blank(),
    panel.border        = element_blank(),
    panel.background    = element_blank(),
    axis.ticks.y        = element_blank(),
    axis.ticks.x        = element_blank()
  )
}
myFormat <- function(number, dig) {
  result <- formatC(number, digits = dig, big.mark = " ", format = "f")
  return(result)
}
my_colours <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")
myTableTheme <- ttheme_default(core = list(fg_params = list(col = my_colours)))
my_lines <- c("dotted", "dashed", "solid", "dotdash", "dotted")

proper_email <- function(input) {
  ifelse(str_detect(input, ".+\\..+@.+\\..+"), TRUE, FALSE)
}

# Variables
price_analysis_text <- list(
  r_skewed_distr_text <- tags$table(
    tags$tr(
      tags$td(icon("info")), tags$td("Most products related to this keyword are priced close to MEDIAN price")
    ),
    tags$tr(
      tags$td(icon("exclamation")), tags$td("There are some products that are priced differently (pack of X deals 
                                            or premium versions)")
    ),
    tags$tr(
      tags$td(icon("exclamation")), tags$td("Check the full dataset to identify in what sub-niche your product 
                                            belongs.")
    ),
    tags$tr(
      tags$td(icon("check")), tags$td("Otherwise, set your product price close to MEDIAN to be competitive")
    )
  ),
  
  double_peaked_distr_text <- tags$table(
    tags$tr(
      tags$td(icon("info")), tags$td("There are two separate sub-niches of products related to this keyword.")
    ),
    tags$tr(
      tags$td(icon("exclamation")), tags$td("Check the full dataset to identify in what sub-niche your product 
                                            belongs.")
    ),
    tags$tr(
      tags$td(icon("check")), tags$td("Otherwise, use the MOST FREQUENT price as a guide to set your price.")
    )
  ),
  
  edge_peak_distr_text <- tags$table(
    tags$tr(
      tags$td(icon("info")), tags$td("Most products related to this keyword are priced close to MEAN price")
    ),
    tags$tr(
      tags$td(icon("exclamation")), tags$td("There is a clear sub-niche of products that are priced differently. 
                                            Check the full dataset to identify if you should price your product 
                                            in that sub-niche instead.")
    ),
    tags$tr(
      tags$td(icon("check")), tags$td("Otherwise, set your product price close to MEAN to be competitive")
    )
  ),
  
  comb_distr_text <- tags$table(
    tags$tr(
      tags$td(icon("info")), tags$td("There is a high amount of price variation of products related to this 
                                     keyword")
    ),
    tags$tr(
      tags$td(icon("check")), tags$td("Try a more specific keyword or use the MOST FREQUENT price as a guide to 
                                      set your price")
    )
  ),
  
  normal_distr_text <- tags$table(
    tags$tr(
      tags$td(icon("info")), tags$td("Most products related to this keyword are priced close to MEAN price")
    ),
    tags$tr(
      tags$td(icon("check")), tags$td("Set your product price close to MEAN to be competitive")
    )
  )
)

waiting_screen <- tagList(
  spin_heartbeat(),
  h3("The Hunt is on...", style = "color: black; text-align: center")
)

#################################
#################################
# Shiny UI
my.ui <- dashboardPage(skin = "purple",
  header = dashboardHeader(title = "MetricsWolf"),
  sidebar = dashboardSidebar(
    tags$style(HTML("#dropdown-menu-feedback-button {background-color: #e1e1f7 !important;}
                    #feedback-button {background-color: #51a3d6; border: none}")),
    introBox(
      sidebarMenu(
        menuItem("The Price Hunt", tabName = "prices", icon = icon("money-bill-wave")),
        menuItem("The Review Den", tabName = "reviews", icon = icon("comments")),
        menuItem("Keyword Tracking", tabName = "keywords", icon = icon("key")),
        menuItem("Dataset", tabName = "dataset", icon = icon("table")),
        introBox(
          textAreaInput(inputId = "user_keyword", label = "Products page URL", width = "100%", rows = 6,
                        value = "https://www.amazon.com/s?k=reusable+straws", resize = "vertical"),
          actionButton(inputId = "analyzeButton", label = "Analyze", icon = icon("refresh"), 
                       style = "color: #fff; background-color: #605ca8"),
          data.step = 2,
          data.intro = "You can change the URL of the page you wish to analyze here. Just remember to
                        press 'Analyze'!",
          data.position = "right"
        ),
        disabled(
          actionButton(inputId = "tutorial", label = "Tutorial", icon = icon("graduation-cap"),
                       style = "color: #fff; background-color: #51a3d6 ")
        ),
        tags$footer(
          style = "position: absolute; bottom: 5%; left: -3%;",
          dropdownButton(
            inputId = "feedback-button",
            circle = FALSE, status = "info", icon = NULL, margin = NULL, up = TRUE,
            tooltip = FALSE, size = "lg", label = "How can we improve?",
            tags$div(
              style = "color: black !important;",
              h4(style = "padding-left: 15px;", "Your judgement? Give a thumb!"),
              tags$hr(),
              prettyToggle(
                inputId = "functionality",
                label_on = "Great functionality!",
                label_off = "Meh...not useful.",
                value = TRUE,
                plain = TRUE,
                outline = TRUE,
                bigger = TRUE,
                icon_on = icon("thumbs-up"),
                icon_off = icon("thumbs-down")
              ),
              prettyToggle(
                inputId = "visual",
                label_on = "Great design!",
                label_off = "I am confused...",
                value = TRUE,
                plain = TRUE,
                outline = TRUE,
                bigger = TRUE,
                icon_on = icon("thumbs-up"),
                icon_off = icon("thumbs-down")
              ),
              prettyToggle(
                inputId = "purchase",
                label_on = "Would buy the full version!",
                label_off = "Needs a LOT of work...",
                value = FALSE,
                plain = TRUE,
                outline = TRUE,
                bigger = TRUE,
                icon_on = icon("thumbs-up"),
                icon_off = icon("thumbs-down")
              ),
              tags$hr(),
              textAreaInput(inputId = "user_feedback", label = "I have more to say!", width = "100%", rows = 2,
                            value = "", resize = "vertical"),
              textInput(inputId = "user_email", label = "Please enter a valid email", 
                        placeholder = "john.smith@gmail.com"),
              actionButton(inputId = "send_feedback", label = "Make it better", icon = icon("paper-plane"),
                             style = "color: #fff; background-color: #51a3d6 ")
            )
          )
        )
      ),
      data.step = 1,
      data.intro = "The sidebar contains links to tools for analyzing different product metrics like prices,
                    reviews and keywords.",
      data.position = "right"
    )
  ),
  body = dashboardBody(
    introjsUI(),
    useShinyjs(),
    useSweetAlert(),
    use_waitress(),
    tabItems(
      tabItem(tabName = "prices",
          fluidRow(
            column(7,
               introBox(
                 plotOutput(outputId = "price_plot"),
                 data.step = 3,
                 data.intro = "The chart shows the distribution of product prices based on the URL you 
                               are analyzing.",
                 data.position = "bottom"
               )
            ),
            column(5,
              fluidRow(column(12, 
                introBox(   
                  hidden(
                    radioButtons(inputId = "rb", label = "If the selected chart does not match your result,
                                          select a different distribution", inline = TRUE,
                                   choiceNames = list(
                                     img(src = "Right Skewed.png", width = 150, height = 75),
                                     img(src = "Double-Peaked.png", width = 150, height = 75),
                                     img(src = "Edge Peak.png", width = 150, height = 75),
                                     img(src = "Comb.png", width = 150, height = 75),
                                     img(src = "Normal.png", width = 150, height = 75)
                                   ),
                                   choiceValues = list(1, 2, 3, 4, 5)
                             )
                  ),
                  data.step = 4,
                  data.intro = "If the selected distribution does not match the chart on the left, select a better 
                                fitting distribution.",
                  data.position = "bottom"
                )
              )),
              fluidRow(column(12, 
                introBox(
                  valueBoxOutput("vbox", width = 12),
                  data.step = 5,
                  data.intro = "The analysis window will provide important insights into the pricing of products
                                from the URL you are analyzing.",
                  data.position = "top"
                )
              ))
            )
          )
      ),
      tabItem(tabName = "reviews", fluidRow(column(12, h1("Coming soon")))),
      tabItem(tabName = "keywords", fluidRow(column(12, h1("Coming soon")))),
      tabItem(tabName = "dataset", fluidRow(column(12, DT::dataTableOutput(outputId = "final_table"))))
    )
  ),
  tags$head(
    tags$style(
      HTML(
        ".radio-inline { 
                    margin-left: 0px;
                    margin-right: 10px;
                    margin-bottom: 10px;
          }
         .radio-inline+.radio-inline {
                    margin-left: 0px;
                    margin-right: 10px;
                    margin-bottom: 10px;
          }
        "
      )
    ) 
  )
)

my.server <- function(input, output, session) {
  my_waitress <- Waitress$new("#price_plot", theme = "overlay-percent", hide_on_render = TRUE)

  observeEvent(input$analyzeButton, {
    enable("tutorial")
    user_url <- isolate(input$user_keyword)
    keyword_products <- reactive(get_keyword_pages(user_url, 3))
    
    #Improve so that products without reviews still make the cut
    products <- reactive({
      keyword_products() %>% 
        filter(complete.cases(.)) %>%
        mutate(across(c(3:5), as.numeric))
    })
    
    # Extract metrics
    digits.pattern <- "[0-9]+\\.*[0-9]*"
    product_count <- reactive(nrow(products()))
  
    price_metrics0 <- reactive({ products() %>%
      summarise(Min_price = round(min(product_prices, na.rm = TRUE), 2),
                Mean_price = round(mean(product_prices, na.rm = TRUE), 2),
                Median_price = round(median(product_prices, na.rm = TRUE), 2),
                Most_frequent_price = round(mfv1(product_prices, na_rm = TRUE), 2),
                Max_price = round(max(product_prices, na.rm = TRUE), 2)
      )
    })
  
    price_metrics <- reactive({ price_metrics0() %>%
        pivot_longer(cols = everything(), names_to = "Metric", values_to = "Price") %>%
        mutate(across(.cols = Metric, .fns = replace_price_text)) %>%
        arrange(., Price)
    })
  
    # Plots
    output$price_plot <- renderPlot({
      my_waitress$start()
      
      for(i in 1:10){
        my_waitress$inc(10) # increase progress bar by 10% steps
        Sys.sleep(.2)
      }
      
      print(
        ggplot() +
        geom_freqpoly(data = products(), mapping = aes(x = product_prices), bins = 30,
                      colour = "#014209", lwd = 1, pad = TRUE) +
        geom_vline(data = price_metrics(), mapping = aes(xintercept = Price),
                   colour = my_colours, linetype = my_lines, lwd = 1) +
        geom_table(data = price_metrics(), table.theme = myTableTheme,
                   aes(x = Inf, y = Inf, label = list(price_metrics())), hjust = 1, vjust = 1) +
        labs(title = "Price Metrics", subtitle = paste("Based on", product_count(), "products"),
             x = "Price [USD]", y = "Number of products") +
        scale_y_continuous(n.breaks = 10, expand = c(0, 0)) +
        scale_x_continuous(n.breaks = 10, expand = c(0, 0), limits = c(0, NA)) +
        theme_hc()
      )
    })
  
    # Output for GUI
    output$final_table <- DT::renderDataTable({
      datatable(products(), colnames = c("Product Title", "ASIN", "Price, USD", "Review Rating",
                                         "Review Amount", "Product link"))
    })
    output$vbox <- renderValueBox({
      valueBox(value = "Price Analysis", subtitle = price_analysis_text[[as.integer(input$rb)]], 
               icon = icon("dollar-sign"), color = "blue", width = NULL) 
    })
    show("rb")
    
  })
  observeEvent(input$tutorial, introjs(session))
  
  observe({
    toggleState(id = "send_feedback", condition = proper_email(input$user_email))
  })
  
  observeEvent(input$send_feedback, {
    my_msg <- paste(
                 "User", isolate(input$user_email),
                 "Functionality", isolate(input$functionality),
                 "Visual", isolate(input$visual),
                 "Purchase", isolate(input$purchase),
                 "User feedback", isolate(input$user_feedback), sep = "\n"
                 )
    
    my_email <- envelope() %>%
      from("vanja@manborgconsulting.com") %>%
      to("vanja@manborgconsulting.com") %>%
      subject(paste("Feedback from", isolate(input$user_email))) %>%
      text(my_msg)
    
    my_smtp = server(host = "x", port = 587, username = "vanja@manborgconsulting.com",
                   password = "x")
    
    my_smtp(my_email, verbose = TRUE)
    
    sendSweetAlert(
      session = session,
      title = "Feedback sent!",
      text = "Thank you for your suggestions",
      type = "success"
    )
  })
}

shinyApp(ui = my.ui, server = my.server)
