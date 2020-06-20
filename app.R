library(shinydashboard)
library(shiny)
library(rtweet)
library(reactable)
library(glue)
library(stringr)
library(httpuv)
library(dplyr)
library(purrr)
library(rsconnect)

num_tweets_to_download <- 200
pineapple <- "@Pineapple_SA"
naked <- "@Naked_insurance"
outsurance <- "@OUTsurance"
santam <- "@SantamInsurance"
kingprice <- "@KingPriceIns"
miway <- "@miwayinsurance"
iwyze <- "@iwyze"
hollard <- "@Hollard"
discovery <- "@Discovery_SA"
budget <- "@Budgetins"


insurers <- c("@Pineapple_SA", "@Naked_insurance", "@OUTsurance", "@SantamInsurance", "@KingPriceIns", "@miwayinsurance", "@iwyze", "@Hollard", "@Discovery_SA", "@Budgetins")

skin <- Sys.getenv("DASHBOARD_SKIN")
skin <- tolower(skin)
if (skin == "")
    skin <- "blue"


sidebar <- dashboardSidebar(
    numericInput("num_tweets_to_download",
                 "Number of tweets to download:",
                 min = 2,
                 max = 18000,
                 value = 20,
                 step = 50),
    selectInput("handle_to_search",
              "Twitter handle to search:",
              choices = insurers),
    actionButton("get_data", "Get data", class = "btn-primary"),
    br(),br(),
    downloadButton("download_data", "Download data"),

    br(),br(),br(),br(),
    sidebarMenu(
        menuItem("Source code for app", icon = icon("file-code-o"),
                 href = "https://github.com/rstudio/shinydashboard/blob/gh-pages/_apps/sidebar/app.R"
        )
    )
)

body <- dashboardBody(
    reactableOutput("tweet_table")

)

header <- dashboardHeader(
    title = "Insurer Tweets"

)

ui <- dashboardPage(header, sidebar, body, skin = skin)

server <- function(input, output) {

    tweet_df <- eventReactive(input$get_data, {
        search_tweets(input$handle_to_search, n = input$num_tweets_to_download, include_rts = FALSE)
    })

    tweet_table_data <- reactive({
        req(tweet_df())
        tweet_df() %>%
            select(user_id, status_id, created_at, screen_name, text, favorite_count, retweet_count, urls_expanded_url) %>%
            mutate(
                Tweet = glue::glue("{text} <a href='https://twitter.com/{screen_name}/status/{status_id}'>>> </a>"),
                URLs = purrr::map_chr(urls_expanded_url, make_url_html)
            )%>%
            select(DateTime = created_at, User = screen_name, Tweet, Likes = favorite_count, RTs = retweet_count, URLs)
    })

    output$tweet_table <- renderReactable({
        reactable::reactable(tweet_table_data(),
                             filterable = TRUE, searchable = TRUE, bordered = TRUE, striped = TRUE, highlight = TRUE,
                             showSortable = TRUE, defaultSortOrder = "desc", defaultPageSize = 25, showPageSizeOptions = TRUE, pageSizeOptions = c(25, 50, 75, 100, 200),
                             columns = list(
                                 DateTime = colDef(defaultSortOrder = "asc"),
                                 User = colDef(defaultSortOrder = "asc"),
                                 Tweet = colDef(html = TRUE, minWidth = 190, resizable = TRUE),
                                 Likes = colDef(filterable = FALSE, format = colFormat(separators = TRUE)),
                                 RTs = colDef(filterable =  FALSE, format = colFormat(separators = TRUE)),
                                 URLs = colDef(html = TRUE)
                             )
        )
    })

    output$download_data <- downloadHandler(
        filename = function() {
            paste(input$hashtag_to_search, "_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(tweet_table_data(), file, row.names = FALSE)
        }
    )
}

shinyApp(ui, server)
