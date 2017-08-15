if("shiny" %in% rownames(installed.packages()) == FALSE) {install.packages("shiny")}
if("plotrix" %in% rownames(installed.packages()) == FALSE) {install.packages("plotrix")}
if("rvest" %in% rownames(installed.packages()) == FALSE) {install.packages("rvest")}
library(shiny)
library(plotrix)
library(rvest)
#shiny::runGitHub("colormixer", "dusty-turner")

# Define UI for application that draws a histogram
colorurl = "https://www.w3schools.com/colors/colors_names.asp"
scrapecolor <- colorurl %>%
  read_html() %>%
  html_nodes(css = "td:nth-child(2) , th:nth-child(2) , td:nth-child(1) , th:nth-child(1)") %>%
  html_text()
scrapecolordf = as.data.frame(matrix(scrapecolor, ncol = 2, byrow = TRUE))
colnames(scrapecolordf) = c("Color", "HEX")
scrapecolordf = scrapecolordf[-1,]
scrapecolordf$Color = as.character(scrapecolordf$Color)
scrapecolordf$Color = gsub('\\s+', '',scrapecolordf$Color)
scrapecolordf$HEX = as.character(scrapecolordf$HEX)

my_max =2


ui <- fluidPage(

   # Application title
   titlePanel(h1("Color Mixer", align = "center"), windowTitle = "Color Mixer by Dusty Turner"),

   # Sidebar with a slider input for number of bins
    fluidRow(
      column(3,wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 600px",
             radioButtons("colorselect",
                     "Select Color 1",
                     scrapecolordf[,1])
      )),
      column(6, align = "center",
          # verbatimTextOutput("cs"), verbatimTextOutput("cs2"),
          # verbatimTextOutput("mix"),
         plotOutput("mixedplot", height = 300, width = 425), 
         plotOutput("plot1", height = 300, width = 425)
      ),
      column(3,wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 600px",
               radioButtons("colorselect2",
                                  "Select Color 2",
                                  scrapecolordf[,1])
      ))
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  mixedcolor = reactiveValues()

  coldf = reactive({
     scrapecolordf[which(scrapecolordf$Color == input$colorselect),][2]
     })

  coldf2 = reactive({
    scrapecolordf[which(scrapecolordf$Color == input$colorselect2),][2]
  })
  
  first = reactive({
    gsub('\\s+', '',as.character(scrapecolordf[which(scrapecolordf$Color == input$colorselect),][2]))
  })
  second = reactive({
    gsub('\\s+', '',as.character(scrapecolordf[which(scrapecolordf$Color == input$colorselect2),][2]))
  })
  
  newEntry <- reactive({
    web1 = paste("https://www.w3schools.com/colors/colors_mixer.asp?colortop=%23", gsub('#',"",first()), "&NULL=Submit&colorbottom=%23", gsub('#',"",second()), sep = "")
    # web1 = "https://www.w3schools.com/colors/colors_mixer.asp?colortop=%23A52A2A&NULL=Submit&colorbottom=%23F0F8FF"
    scrape <- web1 %>%
      read_html() %>%
      html_nodes(css = "tr:nth-child(11) td") %>%
      html_text()
    mixedcolor = gsub('\\s+', '',scrape[2])
    gsub('\\s+', '',scrape[2])
  })
 
     output$cs <- renderPrint({
      # print(scrapecolordf[which(scrapecolordf$Color == input$colorselect),])
      print(first())
   })
   
   output$cs2 <- renderPrint({
     # print(scrapecolordf[which(scrapecolordf$Color == input$colorselect2),])
     print(second())
   })
   
   output$mix <- renderPrint({
     print(newEntry())
   })

      output$mixedplot = renderPlot({
        plot(c(0, 100), c(0, 100), type= "n", xlab = "", ylab = "", main = "Mixed Colors",axes=FALSE, frame.plot=FALSE)
        # rect(0, 0, 100, 100, col = as.character(substring(newEntry(),2)))  ## attempt at online publishing
        rect(0, 0, 100, 100, col = newEntry())  
      # }, width = 400, height = 150)
      })
      output$plot1 = renderPlot({
        plot(c(0, 200), c(0, 200), type= "n", xlab = "", ylab = "", main = "Color 1 and Color 2",axes=FALSE, frame.plot=FALSE)
        rect(0, 0, 100, 200, col = first())
        rect(100, 0, 200, 200, col = second())
      })

}

# Run the application
shinyApp(ui = ui, server = server)

