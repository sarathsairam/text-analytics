create_dtm <- function(plain_text, documents){
  text = tolower(plain_text)
  text = gsub("<.*?>", " ", text)
  text <- stringr::str_replace_all(text,"[^a-zA-Z\\s]", " ")
  text <- stringr::str_replace_all(text,"[\\s]+", " ")
  newdf_text = data_frame(text = text);  newdf_text
  
  #Get the unnested data frame here
  newtext_df <- newdf_text %>% 
    unnest_tokens(word, text) 
  
  getbigrams <- newdf_text %>% unnest_tokens(ngram, text, token = "ngrams", n = 2)
  
  #input or initial source is character vector like newdf_text
  wordsdf = newdf_text %>%   
    mutate(documents = row_number()) %>% # to convert tibble to tidy format
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    group_by(documents) %>%
    count(word, sort = TRUE)
  
  newdtm = wordsdf %>%
    cast_dtm(documents, word, n)
  
  return (newdtm)
}

#Takes factors as input
newly_added_words <- function(curr_var, prev_var){
  new_words <- setdiff(curr_var, prev_var)# checks the difference and gives only new words
  # new_words <- anti_join(curr_var, prev_var)# checks the difference and gives only new words
  return (new_words)
}

build_wordcloud <- function(dtm, 
                            max_words=100,     # max no. of words to accommodate
                            min.freq=1,       # min.freq of words to consider
                            plot.title="wordcloud",
                            filename){
  require(wordcloud)
  if (ncol(dtm) > 20000){   # if dtm is very large, divide into smaller chunks
    
    tst = round(ncol(dtm)/10)  # divide DTM's cols into 10 manageble parts
    a = rep(tst,99)
    b = cumsum(a);rm(a) #Calculates the cumulative sum
    b = c(0,b,ncol(dtm))
    ss.col = c(NULL)
    for (i in 1:(length(b)-1)) {
      tempdtm = dtm[,(b[i]+1):(b[i+1])]
      s = colSums(as.matrix(tempdtm))
      ss.col = c(ss.col,s)
      print(i)      } # i loop ends
    tsum = ss.col
  } else { tsum = apply(dtm, 2, sum) }
  tsum = tsum[order(tsum, decreasing = T)]    # terms in the decreasing order of frequency
  head(tsum);    tail(tsum)
  
  createWordcloud(names(tsum), # words
                  tsum, # their freqs
                  title = "Wordcloud",
                  scale = c(4, 0.5), # range of word sizes
                  minFreq = min.freq, # min.freq of words to consider
                  maxWords = max_words, # max #words
                  # filename,
                  # format = c("png"),
                  width = 700, height = 700, units = "px",
                  palette = brewer.pal(8, "Dark2"), titleFactor = 1) # Plot results in a word cloud
} # func ends
