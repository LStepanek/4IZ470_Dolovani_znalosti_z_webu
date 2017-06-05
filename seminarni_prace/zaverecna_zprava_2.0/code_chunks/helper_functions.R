#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#
#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#
#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#

#\# definuji pomocné funkce ----------------------------------------------------

#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#

#\#\#\# funkce na dělení textu do vět --------------------------------------------

splitTextIntoSentences <- function(
    
    my_text
    
){
    
    # '''
    # Textový řetězec "my\_text" o jedné či více větách rozdělí
    # s určitou mírou spolehlivosti na samostatné věty.
    # '''
    
    split_indices <- NULL
    my_sentences <- NULL
    
    for(stop_mark in c(
        "\\.\\s*[A-Z]+",    #\# tečka, mezera (>= 0), velké písmeno
        "\\?\\s*[A-Z]+",    #\# otazník, mezera (>= 0), velké písmeno
        "\\!\\s*[A-Z]+",    #\# vykřičník, mezera (>= 0), velké písmeno
        "\\:\\s*"           #\# dvojtečka, mezera (>= 0)
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


#\#\#\# --------------------------------------------------------------------------

#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#

#\#\#\# funkce na rozdělení věty na slova ----------------------------------------

splitSentenceIntoWords <- function(
    
    my_sentence
    
){
    
    # '''
    # Rozděluje větu "my\_sentence" na jednotlivá slova.
    # '''
    
    return(
        strsplit(
            x = my_sentence,
            split = " "
        )[[1]]
    )
    
}


#\#\#\# --------------------------------------------------------------------------

#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#

#\#\#\# funkce pro tvorbu n-gramů ------------------------------------------------

getNGrams <- function(
    
    my_splitted_sentences,
    n = 2
    
){
    
    # '''
    # Nad větou rozdělenou na slova "my\_splitted\_sentences" vytvoří
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


#\#\#\# --------------------------------------------------------------------------

#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#

#\#\#\# funkce pro webscraping jedné stránky Wikipedie (typu článek)
#\#\#\# a pro následnou úpravu formátu do podoby volného textu -------------------

webscrapeMyWikipediaPage <- function(
    
    page_url
    
){
    
    # '''
    # Funkce stáhne statický HTML obsah jedné stránky z (anglické)
    # Wikipedie, která je pod odkazem "page\_url". Poté extrahuje jen
    # odstavcové statě ohraničené HTML tagy <p>...</p>.
    # Z nich pak odstraní veškeré další HTML tagy, HTML entity či
    # wikipedické tagy.
    # Nakonec vrací textový řetezec odpovídající jen přirozenému
    # textu v odstavcích dané stránky Wikipedie.
    # Kromě toho ještě z textu stránky extrahuje interní webové odkazy
    # na další stránky Wikipedie, které je poté možné scrapovat.
    # '''
    
    
    #\# stahuji statický HTML obsah --------------------------------------------
    
    my_html <- readLines(
        con = page_url,
        encoding = "UTF-8"
    )
    
    
    \#\# extrahuji jen odstavcové statě ohraničené HTML tagy <p>...</p> ---------
    
    my_raw_paragraphs <- my_html[
        grepl("<p>", my_html) & grepl("</p>", my_html)
    ]
    
    
    #\# očišťuji text paragrafů o HTML tagy, HTML entity a wikipedické tagy ----
    
    my_paragraphs <- gsub("<.*?>", "", my_raw_paragraphs)
    my_paragraphs <- gsub("&.*?;", "", my_paragraphs)
    my_paragraphs <- gsub("\\[.*?\\]", "", my_paragraphs)
    my_paragraphs <- gsub("\t", "", my_paragraphs)  #\# zbavuji se tabulátorů
    
    
    #\# vytvářím jeden dlouhý řetězec (odstavec) -------------------------------
    
    my_text_output <- paste(my_paragraphs, collapse = " ")
    
    
    #\# extrahuji z textu všechny webové interní linky na další stránky
    #\# Wikipedie --------------------------------------------------------------
    
    my_links <- paste(
        "http://en.wikipedia.org",
        gsub(
            '\\"',
            "",
            gsub(
                '(.*)(href=)(\\"/wiki/.*?\\")(.*)',
                "\\3",
                my_raw_paragraphs[
                    grepl("href=", my_raw_paragraphs)
                ]
            )
        ),
        sep = ""
    )    
    
    
    #\# odstraňuji nesmyslné outlinky -- ty, co obsahují mezeru, eventuálně
    #\# ty, co odkazují na celý portál nebo kategorii (jsou o obvykle jen
    #\# seznamy hesel, tedy nevhodné pro sestavené korpusu) --------------------
    
    my_links <- my_links[!grepl(" ", my_links)]
    my_links <- my_links[!grepl("Portal:", my_links)]
    my_links <- my_links[!grepl("Category:", my_links)]
    
    
    #\# vracím výstup ----------------------------------------------------------
    
    return(
        list(
            "text_stranky" = my_text_output,
            "outlinky_stranky" = my_links
        )
    )    
    
    
}


#\#\#\# --------------------------------------------------------------------------

#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#
#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#
#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#





