################################################################################
#                      Scrape Posts by URL from Nitter                         #
#                                                                              #
# Purpose: Collect individual posts by their tweet IDs/URLs                    #
################################################################################

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
library(lubridate) # Added explicit library call for parse_date_time

# Required Libraries
seleniumbase <- import("seleniumbase")
SB <- seleniumbase$SB

################################################################################
#                              Read URLs from File                             #
################################################################################

read_urls_from_file <- function(file_path) {
  lines <- readLines(file_path)
  all_urls <- unlist(lapply(lines, function(line) {
    urls <- strsplit(line, "[,\\s]+")[[1]]
    trimws(urls)
  }))
  all_urls <- all_urls[all_urls != ""]
  return(all_urls)
}

################################################################################
#                           Extract Single Post Data                           #
################################################################################

extract_single_post <- function(html) {
  tweet <- html %>% html_element(".main-tweet .timeline-item, .main-tweet")
  if (is.na(tweet)) {
    tweet <- html %>% html_element(".timeline-item")
  }
  if (is.na(tweet)) {
    return(NULL)
  }

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
    content    = tweet %>% html_element(".tweet-content") %>% html_text(),
    likes      = extract_stat("icon-heart"),
    retweets   = extract_stat("icon-retweet"),
    comments   = extract_stat("icon-comment"),
    quotes     = extract_stat("icon-quote"),
    views      = extract_stat("icon-play")
  )
}

################################################################################
#                                 Helper Functions                             #
################################################################################

pass_cf_limiting <- function(sb, max_retries = 10){
  html <- sb$get_page_source()
  retries <- 0
  while(grepl("You are being rate limited", html) & retries <= max_retries){
    sb$refresh_page()
    sb$wait_for_ready_state_complete()
    Sys.sleep(2)
    retries <- retries + 1
    html <- sb$get_page_source()
  }
}

################################################################################
#                           Scrape Single Post Function                        #
################################################################################

scrape_single_post <- function(sb, tweet_url, domain) {
  if (!grepl("^http", tweet_url)) {
    full_url <- paste0(domain, "/i/status/", tweet_url)
  } else {
    full_url <- tweet_url
  }

  sb$open(full_url)
  sb$wait_for_ready_state_complete()
  pass_cf_limiting(sb)
  Sys.sleep(1)

  html <- read_html(sb$get_page_source())
  post_data <- extract_single_post(html)
  if (!is.null(post_data)) {
    post_data$url <- tweet_url
  }
  return(post_data)
}

################################################################################
#                              Main Scraping Function                          #
################################################################################

scrape_posts_by_url <- function(urls_file,
                                output_file = "scraped_posts.csv",
                                domain = "https://nitter.tiekoetter.com",
                                headless = FALSE) {

  urls <- read_urls_from_file(urls_file)
  message(paste("Found", length(urls), "URLs to scrape"))

  # Initialize CSV
  header_df <- tibble(
    url = character(), datetime = character(), content = character(),
    likes = numeric(), retweets = numeric(), comments = numeric(),
    quotes = numeric(), views = numeric()
  )
  write.csv(header_df, output_file, row.names = FALSE)

  # Initialize browser
  # NOTE: We activate CDP mode immediately
  ctx <- SB(uc = TRUE, test = FALSE, locale = "en", headless = headless)
  sb <- ctx$`__enter__`()
  sb$activate_cdp_mode(domain)
  
  # --- IMPROVED CLOUDFLARE BYPASS LOGIC ---
  message("Opening settings page...")
  sb$open(paste0(domain, "/settings"))
  sb$wait_for_ready_state_complete()
  
  message("Attempting to solve Cloudflare...")
  Sys.sleep(5)
  
  # DEBUG: Take screenshot of the challenge
  sb$save_screenshot("debug_01_challenge_screen.png")
  
  # Method 1: Try generic GUI click
  tryCatch({
    sb$uc_gui_click_captcha() 
    message("Clicked captcha (GUI method)...")
  }, error = function(e) message("GUI click error: ", e$message))
  
  Sys.sleep(8)
  sb$save_screenshot("debug_02_after_click.png")

  # CHECK: Are we past Cloudflare?
  current_url <- sb$get_current_url()
  
  if (!grepl("settings", current_url)) {
      message("WARNING: URL is ", current_url, " - We are NOT on settings yet.")
      message("Attempting fallback iframe click...")
      
      # Method 2: Direct iframe interaction (Fallback)
      tryCatch({
          if (sb$is_element_visible('iframe[src*="challenge"]')) {
              sb$switch_to_frame('iframe[src*="challenge"]')
              sb$click("span.mark", timeout=3)
              sb$switch_to_default_content()
              Sys.sleep(5)
          }
      }, error = function(e) message("Fallback iframe click failed."))
  }

  # --- SAFETY CHECK FOR SETTINGS ELEMENTS ---
  # Only try to interact with settings if we are actually ON the page
  if (grepl("settings", sb$get_current_url())) {
      message("Successfully loaded settings page. Configuring...")
      
      tryCatch({
        sb$select_if_unselected('input[name="infiniteScroll"]')
        sb$unselect_if_selected('input[name="mp4Playback"]')
        sb$unselect_if_selected('input[name="proxyVideos"]')
        sb$unselect_if_selected('input[name="autoplayGifs"]')
        sb$click('button.pref-submit')
        Sys.sleep(2)
      }, error = function(e) {
        message("Could not click settings toggles: ", e$message)
      })
      
  } else {
      message("CRITICAL ERROR: Failed to bypass Cloudflare. Script may fail on specific posts.")
      sb$save_screenshot("debug_03_failed_bypass.png")
  }
  
  #####################################
  # Scrape each URL

  success_count <- 0

  for (i in seq_along(urls)) {
    tryCatch({
      message(paste("Scraping URL", i, "of", length(urls), ":", urls[i]))
      post_data <- scrape_single_post(sb, urls[i], domain)

      if (!is.null(post_data)) {
        post_data$url <- urls[i]
        post_data$datetime <- as.character(parse_date_time(
          gsub(" · ", " ", post_data$datetime),
          orders = "b d, Y I:M p", tz = "UTC"
        ))
        # Clean numbers
        post_data$likes <- as.numeric(gsub(",", "", post_data$likes))
        post_data$retweets <- as.numeric(gsub(",", "", post_data$retweets))
        post_data$comments <- as.numeric(gsub(",", "", post_data$comments))
        post_data$quotes <- as.numeric(gsub(",", "", post_data$quotes))
        post_data$views <- as.numeric(gsub(",", "", post_data$views))

        post_data <- post_data %>%
          select(url, datetime, content, likes, retweets, comments, quotes, views)

        write.table(post_data, output_file, sep = ",", row.names = FALSE,
                    col.names = FALSE, append = TRUE, quote = TRUE)
        success_count <- success_count + 1
        message(paste("  -> Saved. Total:", success_count))
      }
      Sys.sleep(runif(1, 1, 2))
    }, error = function(e) {
      message(paste("Error scraping URL", urls[i], ":", e$message))
    })
  }

  ctx$`__exit__`(NULL, NULL, NULL)
  message(paste("Completed! Total saved:", success_count))
  
  if (file.exists(output_file)) return(read.csv(output_file)) else return(NULL)
}

################################################################################
#                                  Execution                                   #
################################################################################

result <- scrape_posts_by_url(
   urls_file = "urls_extracted_part1.txt",
   output_file = "scraped_posts_part1.csv",
   domain = "https://nitter.tiekoetter.com",
   headless = TRUE
)
