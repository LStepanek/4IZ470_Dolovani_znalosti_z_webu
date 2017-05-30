###############################################################################
###############################################################################
###############################################################################

## definuji pomocné funkce ----------------------------------------------------

###############################################################################

#### funkce na dělení textu do vět --------------------------------------------

splitTextIntoSentences <- function(
    
    my_text
    
){
    
    # '''
    # Textový řetězec "my_text" o jedné či více větách rozdělí
    # s určitou mírou spolehlivosti na samostatné věty.
    # '''
    
    split_indices <- NULL
    my_sentences <- NULL
    
    for(stop_mark in c(
        "\\.\\s*[A-Z]+",    ## tečka, mezera (>= 0), velké písmeno
        "\\?\\s*[A-Z]+",    ## otazník, mezera (>= 0), velké písmeno
        "\\!\\s*[A-Z]+",    ## vykřičník, mezera (>= 0), velké písmeno
        "\\:\\s*"           ## dvojtečka, mezera (>= 0)
    )){
        
        split_indices <- c(
        
            split_indices,
            gregexpr(
                pattern = stop_mark,
                text = my_text
            )[[1]] + 1
            
        )
        
    }
    
    ordered_split_indices <- split_indices[split_indices > 0][
        order(split_indices[split_indices > 0])
    ]

    if(length(ordered_split_indices) > 0){
        
        ordered_split_indices <- c(
            1,
            ordered_split_indices,
            nchar(my_text)
        )
        
        for(i in 1:(length(ordered_split_indices) - 1)){
            
            my_sentences <- c(
            
                my_sentences,
                substr(
                    my_text,
                    ordered_split_indices[i],
                    ordered_split_indices[i + 1]
                )
                
            )
            
        }        
        
    }else{
        
        my_sentences <- my_text
        
    }
    
    for(j in 1:length(my_sentences)){
        
        while(substr(my_sentences[j], 1, 1) == " "){
            
            my_sentences[j] <- substr(
                my_sentences[j], 2, nchar(my_sentences[j])
            )
            
        }
        
        while(substr(
            my_sentences[j],
            nchar(my_sentences[j]),
            nchar(my_sentences[j])
        ) == " "){
            
            my_sentences[j] <- substr(
                my_sentences[j],
                1,
                (nchar(my_sentences[j]) - 1)
            )
            
        }
        
    }
    
    return(my_sentences)
    
}


#### --------------------------------------------------------------------------

###############################################################################

#### funkce na rozdělení věty na slova ----------------------------------------

splitSentenceIntoWords <- function(
    
    my_sentence
    
){
    
    # '''
    # Rozděluje větu "my_sentence" na jednotlivá slova.
    # '''
    
    return(
        strsplit(
            x = my_sentence,
            split = " "
        )[[1]]
    )
    
}


#### --------------------------------------------------------------------------

###############################################################################

#### funkce pro tvorbu n-gramů ------------------------------------------------

getNGrams <- function(
    
    my_splitted_sentences,
    n = 2
    
){
    
    # '''
    # Nad větou rozdělenou na slova "my_splitted_sentences" vytvoří
	# všechny n-gramy pro zadané "n".
    # '''
    
    output <- NULL
    
    if(length(my_splitted_sentences) >= n){
        
        for(i in 1:(length(my_splitted_sentences) - n + 1)){
            
            output <- c(
                
                output,
                paste(
                    my_splitted_sentences[i:(i + n - 1)],
                    collapse = " "
                )
                
            )
            
        }
        
    }
    
    return(output)
    
}


#### --------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





