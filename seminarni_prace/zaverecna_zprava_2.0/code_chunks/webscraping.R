#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#
#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#
#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#

#\# vytvářím malý korpus z nějakého počtu stránek anglicky psané
#\# Wikipedie ------------------------------------------------------------------

#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#

#\#\#\# vytvářím zatím prázdný vektor linků na wikipedické anglické stránky
#\#\#\# a vektor korpus pro jejich texty,
#\#\#\# oba budu postupně populovat ----------------------------------------------

my_links <- NULL
my_wikipedia_corpus <- NULL
my_initial_page_url <- "https://en.wikipedia.org/wiki/World_War_II"
number_of_scraped_pages <- 0           #\# počet aktuálně scrapovaných
                                       #\# wikipedických stránek
how_many_pages_i_would_like_to_scrape <- 900
                                       #\# počet wikipedických stránek,
                                       #\# který chci scrapovat


my_links <- c(my_links, my_initial_page_url)

while(number_of_scraped_pages < how_many_pages_i_would_like_to_scrape){
    
    my_webscrape <- webscrapeMyWikipediaPage(
            my_links[number_of_scraped_pages + 1]
    )
    
    my_wikipedia_corpus <- c(
        
        my_wikipedia_corpus,
        my_webscrape[["text_stranky"]]
        
    )
    
    my_links <- unique(c(
    
        my_links,
        my_webscrape[["outlinky_stranky"]]
        
    ))
    
    number_of_scraped_pages <- number_of_scraped_pages + 1
    
    flush.console()
    print(
        paste(
            "Právě vyscrapováno ",
            format(
                round(
                    number_of_scraped_pages /
                    how_many_pages_i_would_like_to_scrape * 100,
                    digits = 2                    
                ),
                nsmall = 2
            ),
            " % z požadovaného počtu stránek.",
            sep = ""
        )
    )
    
    
}

my_wikipedia_corpus <- iconv(
    my_wikipedia_corpus,
    to = "UTF-8"
)


#\#\#\# --------------------------------------------------------------------------

#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#

#\#\#\# ukládám wikipedický korpus -----------------------------------------------

setwd(paste(mother_working_directory, "vstupy", sep = "/"))

writeLines(
    text = my_wikipedia_corpus,
    con = "en_US.wikipedia.txt"
)


setwd(mother_working_directory)


#\#\#\# --------------------------------------------------------------------------

#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#
#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#
#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#





