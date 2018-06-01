# Shiny app for NLP workflow
# Sourced from Github to run

if (!require(shiny)) {install.packages('shiny')}
if (!require(udpipe)){install.packages("udpipe")}
if (!require(textrank)){install.packages("textrank")}
if (!require(lattice)){install.packages("lattice")}
if (!require(igraph)){install.packages("igraph")}
if (!require(ggraph)){install.packages("ggraph")}
if (!require(wordcloud)){install.packages("wordcloud")}

library(udpipe)
library(textrank)
library(lattice)
library(igraph)
library(ggraph)
library(ggplot2)
library(wordcloud)
library(stringr)
library("shiny")

rm(list = ls()) # Clean the environment and transient objects


ud_model_english <- udpipe_download_model(language = "english")
english_model = udpipe_load_model("./english-ud-2.0-170801.udpipe")



require(stringr)


linkhref = "https://raw.githubusercontent.com/sarathsairam/text-analytics/master/GlobalWarming.txt"

# Define ui function
ui <- shinyUI(
  fluidPage(
    
    titlePanel("NLP Workflow"),
    
    sidebarLayout( 
      
      sidebarPanel(  
        
        fileInput("file", "Upload data (any text file)"),
        checkboxGroupInput("checkGroup", 
                           label = strong("Universal Part of Speech (upos) Selection"), 
                           choices = list("adjective (ADJ)" = "ADJ", 
                                          "noun (NOUN)" = "NOUN", 
                                          "proper noun (PROPN)" = "PROPN",
                                          "adverb (ADV)" = "ADV",
                                          "verb (VERB)" = "VERB"),
                           selected = c("ADJ", "NOUN", "PROPN")
        )
      ),   # end of sidebar panel
      
      mainPanel(
        
        tabsetPanel(type = "tabs",
                    
                    tabPanel("Description / Overview",
                             h4(p("About the App")),
                             p("This app is designed in Shiny with udpipe.
                               The features include annotation in english,
                               natural language processing methodologies and their  
                               implementations",align="justify"),
                             p("This app is able to read through any text file"),
                             a(href=linkhref, "Sample data input file"),
                             br(),
                             h4('How to use this App'),
                             p('To use this app, click on', 
                               span(strong("Upload data (Any Text file)")),
                               'to upload the data file.')),
                    
                    
                    tabPanel("Annotated Document", dataTableOutput('AnnotatDocument')),
                    
                    tabPanel("WordClouds", plotOutput('wc_nouns'), plotOutput('wc_verbs')),
                    
                    tabPanel("Co-Occurances", plotOutput('co_occur')),
                    
                    tabPanel("Skipgram based plot", plotOutput('skps_graph'))
                    
                    ) # end of tabsetPanel
        )# end of main panel
      ) # end of sidebarLayout
  )  # end if fluidPage
) # end of UI

# Define Server function
server <- shinyServer(function(input, output) {
  
  Dataset <- reactive({
    if (is.null(input$file)) { return(NULL) }
    else{
      Data <- readLines(input$file$datapath, n=10)
      readtext  =  str_replace_all(Data, "<.*?>", "")
      readtext = gsub("<.*?>", " ", readtext)
      readtext <- stringr::str_replace_all(readtext,"[^a-zA-Z\\s]", " ")
      readtext <- stringr::str_replace_all(readtext,"[\\s]+", " ")
      Annotation = udpipe_annotate(english_model, x=readtext)
      AnnotatDF <- as.data.frame(Annotation)
      return(AnnotatDF)
    }
  })
  
  model = reactive({
    summary = summary(Dataset())
    return(summmary)
  })  # reactive stmt ends
  
  
  output$AnnotatDocument = renderDataTable({
    print(input$checkGroup)
    AnnotatDocument = Dataset()
    # Annotation = udpipe_annotate(english_model, x=readtext)
    # AnnotatDocument <- as.data.frame(Annotation)
    AnnotatDocument$sentence <- NULL
    AnnotatDocument <- AnnotatDocument %>%
      subset(., upos %in% c(input$checkGroup))
    
  })
  
  output$wc_nouns = renderPlot({
    AnnotatDocument = Dataset()
    AnnotatDocument$sentence <- NULL
    noun_words = AnnotatDocument %>%
      subset(., upos %in% "NOUN")
    group_nouns = txt_freq(noun_words$lemma)
    wordcloud(group_nouns$key[-1], group_nouns$freq[-1], min.freq = 1, colors = 1:10, max.words=200, scale=c(4,0.5))
    
  })
  
  output$wc_verbs = renderPlot({
    AnnotatDocument = Dataset()
    AnnotatDocument$sentence <- NULL
    verbs_words = AnnotatDocument %>%
      subset(., upos %in% "VERB")
    group_verbs = txt_freq(verbs_words$lemma)
    wordcloud(group_verbs$key[-1], group_verbs$freq[-1], min.freq = 1, colors = 1:10, max.words=200, scale=c(4,0.5))
  })
  
  
  output$co_occur = renderPlot({
    AnnotatDocument = Dataset()
    AnnotatDocument$sentence <- NULL
    str(AnnotatDocument)
    # Sentence Co-occurrences for nouns or adjectives
    sen_cooc <- cooccurrence(
      subset(AnnotatDocument, upos %in% c(input$checkGroup)),
      term = "lemma",
      group = c("doc_id", "paragraph_id", "sentence_id")
    )
    # general (non-sentence based) Co-occurrences
    non_sen_cooc_gen <- cooccurrence(
      AnnotatDocument$lemma,
      relevant = AnnotatDocument$upos %in% c(input$checkGroup)
    )
    
    wordnetwrk <- head(sen_cooc, 50)
    wordnetwrk <- igraph::graph_from_data_frame(wordnetwrk) # needs edgelist in first 2 colms.
    
    ggraph(wordnetwrk, layout = "fr") +
      
      geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "orange") +
      geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
      
      theme_graph(base_family = "Arial Narrow") +
      theme(legend.position = "none") +
      
      labs(title = "Cooccurrences within 3 words distance", subtitle = c(input$checkGroup))
    
  })
  
  
  output$skps_graph = renderPlot({
    AnnotatDocument = Dataset()
    AnnotatDocument$sentence <- NULL
    str(AnnotatDocument)
    
    # Skipgram based Co-occurrences: How frequent do words follow one another within skipgram number of words
    str_cooc_skipgm <- cooccurrence(
      AnnotatDocument$lemma,
      relevant = AnnotatDocument$upos %in% c(input$checkGroup),
      skipgram = 4
    )
    
    
    wordnetwrk <- head(str_cooc_skipgm, 50)
    wordnetwrk <- igraph::graph_from_data_frame(wordnetwrk) # needs edgelist in first 2 colms.
    
    ggraph(wordnetwrk, layout = "fr") +
      
      geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "orange") +
      geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
      
      theme_graph(base_family = "Arial Narrow") +
      theme(legend.position = "none") +
      
      labs(title = "Cooccurrences of freq words follow one another within skipgram", subtitle = c(input$checkGroup))
    
  })
  
  
})

# Now call shinyApp function
shinyApp(ui = ui, server = server)