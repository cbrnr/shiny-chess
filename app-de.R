library(shiny)
library(shinyjs)
library(bslib)
library(tibble)
library(thematic)
library(ragg)

options(shiny.useragg=TRUE)
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
    "Fähigkeiten"=c(
        "Numerische Fähigkeiten"=2,
        "Figural-räumliche Fähigkeiten"=3,
        "Arithmetische Kompetenz"=4
    ),
    "Emotionale Intelligenz"=c(
        "Kognitive Neubewertung"=5,
        "Emotionsunterdrückung"=6,
        "Emotionsmaskierung"=7
    ),
    "Persönlichkeit"=c(
        "Extraversion"=8,
        "Verträglichkeit"=9,
        "Gewissenhaftigkeit"=10,
        "Negative Emotionalität"=11,
        "Offenheit für Erfahrungen"=12
    ),
    "Übungstätigkeiten"=c(
        "Offizielle Spiele"=13,
        "Inoffizielle Spiele "=14,
        "Training"=15,
        "Indirekte Beteiligung"=16
    )
)

ui <- fluidPage(
    theme=bs_theme(bootswatch="flatly"),
    titlePanel("Entwicklung von Schachexpertise"),
    sidebarLayout(
        sidebarPanel(
            useShinyjs(),
            disabled(textInput("id", "ID", value="fugeoo")),
            selectInput("measure", "Merkmal", choices=measures),
            htmlOutput("sample"),
            img(src="chess.png", width="50%"),
            p(tags$small(
                "© ", 
                tags$a("Karl-Franzens-Universität Graz", href="https://www.uni-graz.at/"),
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
            "Stichprobe: ", n, " Teilnehmer/innen<br/>",
            "Altersbereich: ", age_min, "&ndash;", age_max, " Jahre<br/>",
            "Mittelwert Alter: ", age_mean, " Jahre"
        )
    })
    
    output$title <- renderText(colnames(df)[measure()])
    
    output$disclaimer <- renderText({
        paste0(
            "Diese Webseite zeigt Ihr persönliches Ergebnis im Vergleich zu den ",
            "Ergebnissen aller ", n, " Schachspieler/innen, die bisher an der Studie zur ",
            r"(<a href="https://chess-study.uni-graz.at/">Entwicklung von )",
            "Schachexpertise</a> teilgenommen haben. Bitte behalten Sie jedoch im ",
            "Hinterkopf, dass die Ergebnisse nur eine Momentaufnahme sind und diese ",
            "Informationen keine psychologische Diagnostik darstellen."
        )
    })
    
    output$plot <- renderPlot({
        main <- unlist(strsplit(names(unlist(measures))[measure()], ".", fixed=TRUE))[2]
        d <- density(data(), na.rm=TRUE)
        hist(
            x=data(),
            freq=FALSE,
            xlab=NULL,
            ylab=NULL,
            main=main,
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
            "<p>Im Diagramm ist die Verteilung der Ergebnisse aller Teilnehmer/innen ",
            "dargestellt. Die x-Achse zeigt die Ergebnisse und die y-Achse die relative ",
            "Häufigkeit des jeweiligen Ergebnisses.</p>",
            r"(<p>Ihr Ergebnis ist <b style="color: red">)", value(), "</b> und als rote ",
            "Linie im Diagramm ersichtlich. Der statistische Median (das mittlere Ergebnis",
            " aller Teilnehmer/innen) ist als schwarze strichlierte Linie eingezeichnet. ",
            "Ihr Ergebnis ist höher als ", percentile(), "% (und niedriger als ",
            100 - percentile(), "%) der Ergebnisse aller ", n, " Teilnehmer/innen."
        )
    })
}

shinyApp(ui, server)
