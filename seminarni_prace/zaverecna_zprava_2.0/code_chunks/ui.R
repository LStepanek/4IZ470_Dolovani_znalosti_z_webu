#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#
#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#
#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#

library(shiny)


#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#
#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#
#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#

shinyUI(fluidPage(
  
  #titlePanel("The Next Word Prediction"),
  
  #\# zavádím graficky hezky vypadající header ---------------------------------
  
  tagList(
    
    tags$head(
      
      tags$link(rel = "stylesheet",
                type = "text/css",
                href = "style.css"),
      
      tags$script(type = "text/javascript",
                  src = "busy.js")
      
    )
    
  ),
  
  div(id = "header",
      div(id = "title", "The Next Word Prediction"),
      div(id = "subsubtitle",
          
          HTML("Prediction of the <i>n</i>-th word based on given
               previous <i>n</i>-1 words and "),
          
          tags$a(
            href = "http://en.wikipedia.org/wiki/N-gram",
            HTML("<i>n</i>-gramming"),
            target = "_blank"
          ),
          
          HTML("&bull;"),
          
          "Implemented by",
          
          tags$a(
            href = "http://www.fbmi.cvut.cz/user/stepalu2",
            "Lubomír Štěpánek",
            target = "_blank"
          )
          
      )
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      
      #\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#
      
      #\# první záložka
      
      conditionalPanel(
        
        condition = "input.conditionedPanels == 1",
        
        textInput(inputId = "my_inputted_phrase",
                  label = "Input your phrase here:"
                  ),
        
        tags$hr(),

        actionButton(inputId = "my_button",
                     label = "Predict the next word!"),
        
        tags$hr()
        
      ),
        
        
        #\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#
        
        #\# druhá záložka
        
      conditionalPanel(
        
        condition = "input.conditionedPanels == 2",
        
        strong(paste("Here you can find some pieces of information",
                     "about the application.",
                     sep=" "
                     )
        ),
        
        tags$hr()
        
      
      #\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#
      
      
    )
    
    
  ),
  
  #\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#
  #\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#
  #\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#
  
  mainPanel(
    
    tabsetPanel(
      
      #\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#
      
      #\# první záložka
      
      tabPanel(
        title = "Results",
        
        br(),
        
        p(strong("You have entered the following phrase:")),
        
        textOutput("inputted_phrase"),
        
        br(),
        tags$hr(),
        
        p(strong("The most likely next word is:")),
        
        textOutput("predicted_word"),
        
        br(),
        tags$hr(),
        
        p(strong(
          em("P"),
          "(",
          em("the predicted n-th"),
          em("word"),
          "|",
          em("the given (n - 1)-words phrase"),
          HTML("&\#x2227;"),
          em("n-grams got by given corpora"),
          ") ="
        )),
        
        textOutput("my_conditional_probability"),
        
        value = 1
      ),
      
      
      #\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#
      
      #\# druhá záložka
      
      tabPanel(
        title = "About",
        
        br(),
        
        h4("Introduction"),

        p("- purpose of this application is to offer a tool for prediction of", em("n"), "-th",
          "word most likely following the previous (", em("n - 1"), ") words words inputted by user",
          "Last, but not least piece of purpose is to pass 4IZ470 Web Data Mining subject practised",
          "at University of Economics, Prague"),
        
        br(),
        
        p(h4("Analysis, preprocessing before algorithm deployment")),
        
        p(paste("- data originate from well-known HC corpora ('Helsinki Corpora'),",
                "blog, twitter and news corpus datasets were used", sep = " ")),
        
        p(paste("- before analysis, all vulgarisms, swear and stop words were",
              "removed from the datasets",sep = " ")),
        p("-", em("n"), "-grams for", em("n"), HTML("&\#x2208;"), "{2, 3, 4}",
          "were worked out"),
        p(paste("- 2-grams, 3-grams and 4-grams contain at all 45 354,",
                "23 989, and 5 570 phrases, respectively", sep = " ")),
        p("- a naive frequentist probability model based on (", em("n"), "+ 1)-gram",
          "for predicting of (", em("n"), "+ 1)-th word according to",
          "previous", em("n"), "inputted words was conducted"),
        
        br(),
        
        p(h4("Pseudocode of the algorithm")),
        
        code("i-gram <- database of pairs [i-words phrase, its (i + 1)-th likely following word]"),
        br(),
        code("n <- min(3, number of words of the phrase inputted by user)"),
        br(),
        code("while n > 0 do"),
        br(),
        code("...phrase <- last n words of the phrase inputted by user"),
        br(),
        code("...{M} <- {0}"),
        br(),
        code("...for all pairs in n-gram do"),
        br(),
        code("......if phrase in pair then"),
        br(),
        code(".........{M} <- pair"),
        br(),
        code("...if {M} <> {0} then"),
        br(),
        code("......return (n + 1)-th following word of the most frequent pair"),
        br(),
        code("...else"),
        br(),
        code("......n <- n - 1"),
        
        
        br(),
        
        br(),
        
        p(h4("In progress")),
        
        p("- Kneser-Ney smoothing with back-off model will be",
          "eventually applied to predict the smooth probability of",
          "a next word"),
        
        br(),
        
        br(),
        
        p(h4("Usage of the application, conclusions")),
        
        p("- the application s available at http://shiny.statest.cz:3838/the_next_word_prediction/"),
        p("- a user can intuitively input her phrase which contains at least one word"),
        p("- after pushing the 'Predict the next word!' button the most likely following word is predicted
             and the conditional probability of correctness of the prediction, given the inputted phrase
             and given 'static' n-grams based on provided datasets, is returned"),
        
        br(),
        
        p(h4("About the author")),
        
        p("Lubomir Stepanek, M. D."),
        p("- Department of Biomedical Informatics"),
        p("- Faculty of Biomedical Engineering"),
        p("- Czech Technical University in Prague"),
        HTML("<a href='mailto:lubomir.stepanek[AT]fbmi.cvut.cz'>
             lubomir.stepanek[AT]fbmi[DOT]cvut[DOT]cz</a>"),
        br(),
        
        value = 2
      ),
      
      
      #\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#
      
      id = "conditionedPanels"
      
      
      #\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#
      
    )
    
  )
  
  )
  
))




#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#
#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#
#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#





