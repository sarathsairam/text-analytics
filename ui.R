#---------------------------------------------------------------------#
#               NLP Workflow App                                      #
#---------------------------------------------------------------------#



library(udpipe)
library(textrank)
library(lattice)
library(igraph)
library(ggraph)
library(ggplot2)
library(wordcloud)
library(stringr)


library("shiny")

linkhref = "https://raw.githubusercontent.com/sarathsairam/text-analytics/master/GlobalWarming.txt"
# Define ui function
shinyUI(
  fluidPage(
    
    titlePanel("NLP Workflow (Rashmi, Sarath , Ramakrishna)"),
    
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
