#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#
#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#
#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#

library(shiny)


#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#
#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#
#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#

shinyServer(
  
  function(input, output){
    
    #\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#
    
    #\# loaging of my vocabularies with 1-, 2- and 3-grams
    
    my_1_word_vocabulary <- reactive({
      
      data <- read.csv("my_1_word_vocabulary.txt",
                       header = TRUE,
                       sep = " "
                       )
      
      for(i in 1:3){data[, i] <- as.character(data[, i])}
      data[, 3] <- as.numeric(data[, 3])
      
      return(data)
      
    })
    
    
    my_2_word_vocabulary <- reactive({
      
      data <- read.csv("my_2_word_vocabulary.txt",
                       header = TRUE,
                       sep = " "
      )
      
      for(i in 1:3){data[, i] <- as.character(data[, i])}
      data[, 3] <- as.numeric(data[, 3])
      
      return(data)
      
    })
    
    
    my_3_word_vocabulary <- reactive({
      
      data <- read.csv("my_3_word_vocabulary.txt",
                       header = TRUE,
                       sep = " "
                       
      )
      
      for(i in 1:3){data[, i] <- as.character(data[, i])}
      data[, 3] <- as.numeric(data[, 3])
      
      return(data)
      
    })
    
    
    #\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#
    
    #\# outputting of inserted phrase
    
    output$inputted_phrase <- renderText({
      
      if(is.null(input$my_inputted_phrase)){
        
        return(" ")
        
      }else{
        
        return(input$my_inputted_phrase)
        
      }
      
      
    })
    
    
    #\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#
    
    #\# I am getting a reactive event according to inputted phrase and
    #\# clearing the phrase as well
    
    my_clear_phrase <- eventReactive(input$my_button, {
      
      inputted_phrase <- tolower(input$my_inputted_phrase)
      
      while(grepl("  ", inputted_phrase)){
        
        inputted_phrase <- gsub("  ", " ", inputted_phrase)
        
      }
      
      if(substr(inputted_phrase, 1, 1) == " "){
        inputted_phrase <- substr(inputted_phrase,
                                  2,
                                  nchar(inputted_phrase)
                                  )
      }
      
      if(substr(inputted_phrase,
                nchar(inputted_phrase),
                nchar(inputted_phrase)) == " "){
        inputted_phrase <- substr(inputted_phrase,
                                  1,
                                  nchar(inputted_phrase)
                                  )
      }
      
      return(inputted_phrase)
      
    })
    
    
    #\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#
    
    #\# helper function for getting of last k words of the phrase
    
    getEndOfMyPhrase <- function(my_phrase, k){
      
      n_of_words <- length(strsplit(my_phrase, split = " ")[[1]])
      
      if(k >= n_of_words){
        
        return(my_phrase)
        
      }else{
        
        return(
          paste(
            strsplit(my_phrase, split = " ")[[1]][
              (n_of_words - k + 1) : n_of_words
              ],
            collapse = " "
          )
        )
        
      }
      
    }
    
    
    #\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#
    
    #\# helper function for prediction of the next word
    
    getMyLikelyNextWord <- function(my_phrase){
      
      my_phrase <- getEndOfMyPhrase(my_phrase, 3)
      
      if(length(strsplit(my_phrase, split = " ")[[1]]) == 3){
        if(my_phrase %in% my_3_word_vocabulary()$phrase){
          
          which_to_choose <- which(
            my_3_word_vocabulary()$phrase == my_phrase
            )
          my_index <- which.max(
            my_3_word_vocabulary()$frequency[which_to_choose]
            )[1]
          return(
            my_3_word_vocabulary()$next_word[which_to_choose][my_index]
            )
          
        }else{
          
          my_phrase <- getEndOfMyPhrase(my_phrase, 2)
          
        }
      }
      
      if(length(strsplit(my_phrase, split = " ")[[1]]) == 2){
        if(my_phrase %in% my_2_word_vocabulary()$phrase){
          
          which_to_choose <- which(
            my_2_word_vocabulary()$phrase == my_phrase
            )
          my_index <- which.max(
            my_2_word_vocabulary()$frequency[which_to_choose]
            )[1]
          return(
            my_2_word_vocabulary()$next_word[which_to_choose][my_index]
            )
          
        }else{
          
          my_phrase <- getEndOfMyPhrase(my_phrase, 1)
          
        }
      }
        
      if(length(strsplit(my_phrase, split = " ")[[1]]) == 1){
        if(my_phrase %in% my_1_word_vocabulary()$phrase){
            
            which_to_choose <- which(
              my_1_word_vocabulary()$phrase == my_phrase
              )
            my_index <- which.max(
              my_1_word_vocabulary()$frequency[which_to_choose]
              )[1]
            return(
              my_1_word_vocabulary()$next_word[which_to_choose][my_index]
              )
            
          }else{
            
            return("the")
            
          }
          
        }
        
    }
    
    
    #\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#
    
    #\# helper function for my conditional probability
    
    getMyConditionalProbability <- function(my_phrase){
      
      my_phrase <- getEndOfMyPhrase(my_phrase, 3)
      
      if(length(strsplit(my_phrase, split = " ")[[1]]) == 3){
        if(my_phrase %in% my_3_word_vocabulary()$phrase){
          
          which_to_choose <- which(
            my_3_word_vocabulary()$phrase == my_phrase
          )
          my_numerator <- my_3_word_vocabulary()$frequency[which.max(
            my_3_word_vocabulary()$frequency[which_to_choose]
          )[1]]
          return(min(1, 
            my_numerator / sum(
              my_3_word_vocabulary()$frequency[which_to_choose]
              ))
          )
          
        }else{
          
          my_phrase <- getEndOfMyPhrase(my_phrase, 2)
          
        }
      }
      
      if(length(strsplit(my_phrase, split = " ")[[1]]) == 2){
        if(my_phrase %in% my_2_word_vocabulary()$phrase){
          
          which_to_choose <- which(
            my_2_word_vocabulary()$phrase == my_phrase
          )
          my_numerator <- my_2_word_vocabulary()$frequency[which.max(
            my_2_word_vocabulary()$frequency[which_to_choose]
          )[1]]
          return(min(1,
            my_numerator / sum(
              my_2_word_vocabulary()$frequency[which_to_choose]
              ))
          )
          
        }else{
          
          my_phrase <- getEndOfMyPhrase(my_phrase, 1)
          
        }
      }
      
      if(length(strsplit(my_phrase, split = " ")[[1]]) == 1){
        if(my_phrase %in% my_1_word_vocabulary()$phrase){
          
          which_to_choose <- which(
            my_1_word_vocabulary()$phrase == my_phrase
          )
          my_numerator <- my_1_word_vocabulary()$frequency[which.max(
            my_1_word_vocabulary()$frequency[which_to_choose]
          )[1]]
          return(min(1,
            my_numerator / sum(
              my_1_word_vocabulary()$frequency[which_to_choose]
              ))
          )
          
        }else{
          
          return("> 0")
          
        }
        
      }
      
    }
    
    
    #\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#
    
    output$predicted_word <- renderText({
      
      getMyLikelyNextWord(my_clear_phrase())
      
    })
    
    
    #\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#
    
    output$my_conditional_probability <- renderText({
      
      getMyConditionalProbability(my_clear_phrase())
      
    })
    
    
    #\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#
    
    
  }
  
)


#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#
#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#
#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#





