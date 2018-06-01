library(udpipe)
library(textrank)
library(lattice)
library(igraph)
library(ggraph)
library(ggplot2)
library(wordcloud)
library(stringr)


shinyServer(function(input, output) {
  
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