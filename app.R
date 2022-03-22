library(shiny)
library(shinyjs)
library(bslib)
library(tibble)
library(thematic)

thematic_shiny(font="auto")

df <- read.csv("chess.csv", row.names=1, check.names=FALSE)
description <- readLines("description.txt")

n <- nrow(df)
age_min <- min(df$Age)
age_max <- max(df$Age)
age_mean <- round(mean(df$Age), 1)
df$Age <- NULL

measures <- list(  # group measures into five sections, numbers correspond to column indices
    "Motivation"=c(
        "Motivation"=1
    ),
    "Abilities"=c(
        "Numerical ability"=2,
        "Figural-spatial ability"=3,
        "Arithmetic competence"=4
    ),
    "Emotional intelligence"=c(
        "Cognitive reappraisal"=5,
        "Expressive suppression"=6,
        "Emotion masking"=7
    ),
    "Personality"=c(
        "Extraversion"=8,
        "Agreeableness"=9,
        "Conscientiousness"=10,
        "Negative emotionality"=11,
        "Open-mindedness"=12
    ),
    "Practice"=c(
        "Official hours"=13,
        "Unofficial hours"=14,
        "Training hours"=15,
        "Indirect hours"=16
    )
)

ui <- fluidPage(
    theme=bs_theme(bootswatch="flatly"),
    titlePanel("Development of Chess Expertise"),
    sidebarLayout(
        sidebarPanel(
            useShinyjs(),
            disabled(textInput("id", "ID", value="")),
            selectInput("measure", "Measure", choices=measures),
            htmlOutput("sample"),
            img(src="chess.png", width="50%"),
            p(tags$small(
                "© ", 
                tags$a("University of Graz", href="https://www.uni-graz.at/en/"),
                ", 2022")
            ),
            width=3
        ),
        mainPanel(
            conditionalPanel(
                condition="output.show",
                p(htmlOutput("disclaimer")),
                plotOutput("plot"),
                p(htmlOutput("description")),
                htmlOutput("score")
            ),
        )
    )
)

server <- function(input, output, session) {
    observe({
        query <- parseQueryString(session$clientData$url_search)
        if (!is.null(query[["id"]])) {
            updateTextInput(session, "id", value=query[["id"]])
        }
    })
    output$show <- reactive(input$id %in% rownames(df))
    outputOptions(output, "show", suspendWhenHidden=FALSE)
    
    measure <- reactive(as.numeric(input$measure))  # column index
    data <- reactive(df[ , measure()])  # selected measure
    value <- reactive(round(df[input$id, measure()], 2))  # measure value of selected ID
    percentile <- reactive(round(ecdf(data())(value()) * 100))
    
    output$sample <- renderText({
        paste0(
            "Sample: ", n, " participants<br/>",
            "Age range: ", age_min, "&ndash;", age_max, " years<br/>",
            "Mean age: ", age_mean, " years"
        )
    })
    
    output$title <- renderText(colnames(df)[measure()])
    
    output$disclaimer <- renderText({
        paste0(
            "This website shows your individual results relative to all ", n, " chess ",
            "players participating in the ",
            r"(<a href="https://chess-study.uni-graz.at/en/">Development of Chess )",
            "Expertise</a> study so far. Please keep in mind that the results are only a ",
            "momentary assessment at that specific time and that the feedback does not ",
            "represent a formal psychological assessment of any kind."
        )
    })
    
    output$plot <- renderPlot({
        d <- density(data(), na.rm=TRUE)
        hist(
            x=data(),
            freq=FALSE,
            xlab=NULL,
            ylab=NULL,
            main=colnames(df)[measure()],
            yaxt="n",
            border="white",
            ylim=c(0, max(d$y))
        )
        lines(d$x, d$y, type="l", lwd=3)
        abline(v=value(), lwd=3, col="red")
        abline(v=median(data(), na.rm=TRUE), lwd=2, lty=3)
        text(value(), max(d$y), value(), pos=4, col="red")
    }, res=96)
    
    output$description <- renderText(description[measure()])
    
    output$score <- renderText({
        paste0(
            "<p>In the visualization, you can see the distribution of scores from all ",
            "participants. The x-axis shows the scores, and the y-axis shows the relative ",
            "frequency of a particular score.</p>",
            r"(<p>Your score is <b style="color: red">)", value(), "</b>, indicated by the",
            " red line. The median is shown as a black dotted line; it represents the ",
            "central (typical) score across all participants. Your score is higher than ",
            percentile(), "% (and lower than ", 100 - percentile(), "%) of the scores from",
            " all ", n, " participants."
        )
    })
}

shinyApp(ui, server)
