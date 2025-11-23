################################################################################
#                      Scrape Brazilian User Feeds around Ban                  #
#                                                                              #
# Author:  Christopher Schwarz                                                 #
# Date:    2025/08/04                                                          #
# Purpose: Collect feeds for users                                             #
################################################################################

user_names <- read.csv("/Users/christopherschwarz/Documents/GitHub/nitter/Brazil_Deactivation_Collection/user_names_in_ideology_models.csv")
user_names <- user_names[which(user_names$shares > 100),]
collected <- gsub(".csv","",list.files("/Users/christopherschwarz/Documents/GitHub/nitter/Brazil_Deactivation_Collection/2025_08_04"))
user_names <- user_names[!user_names$names %in% collected,]

# Domains to be used
nitter_domains <- c("https://nitter.tiekoetter.com",
                    "https://nitter.kareem.one")

################################################################################
#                               Function Definitions                           #
################################################################################

library(reticulate)
library(rvest)
library(purrr)
library(tidyverse)

# Required Libraries
seleniumbase <- import("seleniumbase")
SB <- seleniumbase$SB

extract_tweet <- function(html_tweets) {
  map_dfr(html_tweets, function(tweet) {
    stats <- html_elements(tweet, ".tweet-stat")
    spans <- html_elements(stats, "span")
    span_classes <- html_attr(spans, "class")
    
    extract_stat <- function(icon) {
      match_index <- which(str_detect(span_classes, icon))
      if (length(match_index)) {
        html_text(stats[[match_index[1]]]) %>% str_trim()
      } else {
        NA_character_
      }
    }
    
    tibble(
      datetime   = tweet %>% html_element(".tweet-date a") %>% html_attr("title"),
      text       = tweet %>% html_element(".tweet-content") %>% html_text(),
      tweet_url  = tweet %>% html_element(".tweet-link") %>% html_attr("href"),
      quote_text = tweet %>% html_element(".quote-text") %>% html_text(),
      quote_url  = tweet %>% html_element(".quote-link") %>% html_attr("href"),
      reply_to   = tweet %>% html_element(".replying-to") %>% html_text(),
      replies    = extract_stat("icon-comment"),
      retweets   = extract_stat("icon-retweet"),
      quotes     = extract_stat("icon-quote"),
      likes      = extract_stat("icon-heart")
    )
  }) -> out
  
  out$datetime <- parse_date_time(gsub(" · ", " ", out$datetime), 
                                  orders = "b d, Y I:M p", tz = "UTC")
  out$replies <- as.numeric(gsub(",","",out$replies))
  out$retweets <- as.numeric(gsub(",","",out$retweets))
  out$quotes <- as.numeric(gsub(",","",out$quotes))
  out$likes <- as.numeric(gsub(",","",out$likes))
  
  out
  
}

################################################################################
#                                 Helper Functions                             #
################################################################################

# Check if being rate limited. If so, refresh.
pass_cf_limiting <- function(sb, max_retries = 10){
  
  html <- sb$get_page_source()
  retries <- 0
  
  while(grepl("You are being rate limited",html) & retries <= max_retries){
    
    sb$refresh_page()
    sb$wait_for_ready_state_complete()
    Sys.sleep(2)
    retries <- retries + 1
    html <- sb$get_page_source()
    
  }
  
}

################################################################################
#                                Scraping Function                             #
################################################################################

scrape_nitter <- function(handle,
                          domain = "https://lightbrd.com",
                          since = NULL,
                          until = NULL,
                          max_pages = Inf,
                          max_retries = 10,
                          headless = FALSE){
  
  # domain = "https://lightbrd.com"
  # handle = "elonmusk"
  # since = "2025-01-01"
  # until = "2025-01-10"
  # max_pages = Inf
  # max_retries = 10
  
  # Modify until since it is not inclusive by default
  until <- as.character(as.Date(until)+1)
  
  #####################################
  # Turn on Infinite Scroll
  # Turn off video playback, etc
  
  ctx <- SB(uc = TRUE, test = FALSE, locale = "en", headless = headless)
  sb <- ctx$`__enter__`()
  sb$activate_cdp_mode(domain)
  sb$wait_for_ready_state_complete()
  sb$uc_gui_click_captcha()
  
  sb$wait_for_ready_state_complete()
  sb$open(paste0(domain,"/settings"))
  
  sb$wait_for_ready_state_complete()
  sb$select_if_unselected('input[name="infiniteScroll"]')
  sb$unselect_if_selected('input[name="mp4Playback"]')
  sb$unselect_if_selected('input[name="proxyVideos"]')
  sb$unselect_if_selected('input[name="autoplayGifs"]')
  
  sb$click('button.pref-submit')
  
  Sys.sleep(1)
  
  #####################################
  # Initialize
  
  # No search restrictions (i.e. scrape full timeline)
  if(is.null(since) & is.null(until)){
    
    url <- paste0(domain, 
                  handle)
    
  }
  
  # Only recent Tweets
  if(!is.null(since) & is.null(until)){
    
    url <- paste0(domain,"/", 
                  handle,"/",
                  "search?f=tweets&q=",
                  "&since=",since)
    
  }
  
  # Only older Tweets
  if(is.null(since) & !is.null(until)){
    
    url <- paste0(domain,"/", 
                  handle,"/",
                  "search?f=tweets&q=",
                  "&until=",until)
    
  }
  
  # Only Tweets in a range
  if(!is.null(since) & !is.null(until)){
    
    url <- paste0(domain,"/", 
                  handle,"/",
                  "search?f=tweets&q=",
                  "&since=",since,
                  "&until=",until)
    
  }  
  
  # Navigate
  sb$open(url)
  
  # Wait until ready
  sb$wait_for_ready_state_complete()
  
  # Ready scraping
  pass_cf_limiting(sb)
  html = read_html(sb$get_page_source())
  collected_tweets <- html %>% html_nodes(".timeline-item")
  collected_tweets <- unlist(lapply(collected_tweets,as.character))
  
  start_time <- Sys.time()
  page_count <- 1
  retries <- 0
  start_tweets <- length(collected_tweets)
  running_total <- start_tweets
  
  # Scraping loop
  while(page_count <= max_pages & retries <= max_retries){
    
    pass_cf_limiting(sb)
    
    # Scroll to bottom
    sb$execute_script("var el = document.querySelector('.show-more a');
                       if (el) el.scrollIntoView({behavior: 'smooth', block: 'center'});")
    
    # Wait for load
    Sys.sleep(1)
    
    # Grab html
    html = read_html(sb$get_page_source())
    
    # Process tweets
    available_tweets <- html %>% html_nodes(".timeline-item")
    available_tweets <- unlist(lapply(available_tweets,as.character))
    collected_tweets <- unique(c(collected_tweets, available_tweets))
    
    # Update counts
    running_total <- length(collected_tweets)  
    page_count <- page_count + 1
    
    # Report progress
    time_diff <- round(as.numeric(difftime(Sys.time(), start_time, units = "secs")),2)
    message(paste("Found", 
                  running_total - start_tweets,
                  "new Tweets. Total of",
                  running_total,
                  "Tweets after",
                  page_count,
                  "iterations in",
                  time_diff,
                  "seconds:",
                  round(running_total/time_diff,2),
                  "TPS"))
    
    
    if(start_tweets == running_total){
      
      retries <- retries + 1
      
    }else{
      
      start_tweets <- running_total
      retries <- 0
      
    }
    
  }
  
  # Exit and return
  ctx$`__exit__`(NULL, NULL, NULL)
  message("Extracting Tweets from HTML...")
  
  restored_tweets <- lapply(collected_tweets, read_html)
  processed_tweets <- extract_tweet(restored_tweets)
  processed_tweets
  
}

################################################################################
#                                  Run the Loop                                #
################################################################################

start <- Sys.time()
test <- scrape_nitter("jonathan_nagler",
                      since = "2024-01-01",
                      until = "2025-01-01",
                      domain = nitter_domains[1],
                      max_retries = 5)
end <- Sys.time()
end - start

start <- Sys.time()
test <- scrape_nitter("jonathan_nagler",
                      since = "2024-01-01",
                      until = "2025-01-01",
                      domain = nitter_domains[1],
                      max_retries = 5,
                      headless = TRUE)
end <- Sys.time()
end - start

library(pbapply)

pblapply(user_names$names, function(name){
  
  tryCatch({
    
    domain <- sample(nitter_domains, 1)
    
    scrape_nitter(name,
                  since = "2024-06-01",
                  until = "2024-12-31",
                  domain = domain,
                  max_retries = 5) -> out
    
    write.csv(out, paste0("/Users/christopherschwarz/Documents/GitHub/nitter/Brazil_Deactivation_Collection/",
                          "2025_08_04/",
                          name,".csv"), row.names = FALSE)
    
  }, error = function(e){e})

  
}) -> all


