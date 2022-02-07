library(shiny)
library(shinydashboard)
library(plotly)
library(tidyverse)
library(shinyjs)
library(shinycssloaders)
library(magick)

# Data ------------------------------------------------------------------

df <- read_csv("final_df.csv")

# Dashboard ----------------------------------------------------------------

# add this function to fix SSL error
# https://stackoverflow.com/questions/62957886/problem-with-r-shiny-app-that-was-working-web-scrape-from-https-url-works-in-rs
geturl <- function(url,handle) {
    curl::curl_fetch_memory(url, handle = handle)$content
}

ui <- dashboardPage(skin = "black",
    dashboardHeader(title = "", titleWidth="0"),
    dashboardSidebar(disable = TRUE),
    dashboardBody(tags$head(
        tags$style(HTML("img.small-img {
          max-width: 75px;
          }")),
        tags$style("h3 {font-family: 'Lato'")
        ),
    div(h3("Colors of Tate Art"), align = "center"),  
    div(h5('Each column includes three colors from twenty randomly selected art pieces from that decade'), align ="center"),
    div(h5('Hover over a color to learn more about the piece it is from and preview the art'), align = "center"),
    # plotlyOutput("hoverplot", width="850px", height="450px"),
    div(plotlyOutput("hoverplot", width="850px", height="450px") %>% withSpinner(type=5), align = "center"),
    div(h6('Data from:',
           tags$a(href = "https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-01-12/readme.md", 
                  "Tate Art Museum")), align = "right"),
    div(h6('Created by',
           tags$a(href = "https://github.com/kkakey", 
                  "@kkakey")), align = "right"),
    plotOutput("imageOutput")),
    useShinyjs()
)

# Server  ----------------------------------------------------------------
server <- function(input, output, session) {
    addClass(selector = "body", class = "sidebar-collapse")
    output$hoverplot <- renderPlotly({

        plot <- ggplot(data=df, aes(y,x, fill=color_pal, key=accession_number)) +
            geom_tile(col = NA, height=1, width=9,
                      aes(title=title,year=year,artist=artist, acuisition.year=acuisition.year,
                          medium=medium)
            ) +
            scale_fill_manual(values=df$color_pal) +
            guides(fill = "none") +
            theme_minimal() +
            theme(panel.grid = element_blank(),
                  plot.background = element_rect(fill="white", color="white"),
                  text = element_text(family="Lato"),
                  legend.position = "none",
                  axis.text.y=element_blank()
            ) +
            xlab("") + ylab("") + ggtitle("")

        ggplotly(plot, tooltip=c("title","year","artist","acuisition.year","medium"))
    })
    
    # need for SSL error
    h <- curl::new_handle()
    curl::handle_setopt(h, ssl_verifypeer = 0)
    
    output$imageOutput <- renderPlot({ 
        req(plotly::event_data("plotly_hover"))

        mouse_event <- plotly::event_data("plotly_hover")
        hoverOpts(mouse_event, delay = 5)
        im_df <- df %>%
            dplyr::filter(accession_number==unlist(mouse_event$key)) %>%
            dplyr::select(image_nail) %>% distinct()
        pic = magick::image_read(geturl(im_df$image_nail,h))
        plot.new()
        grid::grid.raster(pic)
        })
    options(shiny.sanitize.errors = F)
}

shinyApp(ui = ui, server = server)
