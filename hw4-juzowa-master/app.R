#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(ggbiplot)
library(shiny)
library(MASS)
# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Iris data visualize"),
    
    tabsetPanel(               
        tabPanel("PCA",
                 sidebarLayout(
                     sidebarPanel(
                         radioButtons("rd",label = "Select first PCA value:(X軸)",choices = list("PC1" = 1,"PC2" = 2,"PC3" = 3,"PC4" = 4)),
                         radioButtons("rr",label = "Select first PCA value:(Y軸)",choices = list("PC1" = 1,"PC2" = 2,"PC3" = 3,"PC4" = 4))
                     ),
                     mainPanel(
                         plotOutput("PCA",width = "100%", height = "900px")
                     )
                 )
        ),
        tabPanel("CA",
            mainPanel(titlePanel("CA Visualize"),
                plotOutput("CA",width = "100%", height = "500px")
            )
         ),
        tabPanel("Iris data attribute",
                 mainPanel(titlePanel("species 特徵分布圖"),
                           plotOutput("HIST",width = "50%", height = "900px")
                 ),
                 mainPanel(titlePanel("Scree plot"),
                           plotOutput("Variance",width = "50%", height = "200px")
                 )
        )
    )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
    data(iris)
    # log transform
    log.ir <- log(iris[, 1:4])
    ir.species <- iris[, 5]
    # apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
    ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)
    output$PCA <- renderPlot({
        qq<-as.numeric(input$rd)
        gg<-as.numeric(input$rr)
        g <- ggbiplot(ir.pca,choices = c(qq,gg) ,obs.scale = 1, var.scale = 1, groups = ir.species)
        g <- g + scale_color_discrete(name = '')
        g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
        print(g)
    })
    output$CA <- renderPlot({
        set.seed(5220)
        iris2 <- scale(iris[, 1:4])
        model <- kmeans(iris[, 1:4], centers = 3)
        table(iris$Species, model$cluster)
        camodel <- corresp(iris$Species, model$cluster, nf = 2)
        plot(camodel)
    })
    output$HIST <- renderPlot({
    attach(mtcars)
    par(mfrow=c(4,1))
    hist(iris$Sepal.Length, breaks = 20)
    hist(iris$Sepal.Width, breaks = 20)
    hist(iris$Petal.Length, breaks = 20)
    hist(iris$Petal.Width, breaks = 20)
    })
    output$Variance <- renderPlot({
    plot(ir.pca, type='l')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
