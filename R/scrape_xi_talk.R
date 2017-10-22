library(tidyverse)
library(rvest)
library(XML)
library(httr)
library(data.table)

setwd("~/xi-talk")
URL = "http://jhsjk.people.cn/article"

# Get all types of talks listed on http://jhsjk.people.cn/article
xi_talk_type = data.frame(
  type = read_html(URL) %>%
    html_nodes("body > div.w1014.btn.clearfix > a > img") %>% 
    html_attr("title"),
  link = paste0("http://jhsjk.people.cn/", 
    read_html(URL) %>%
      html_nodes("body > div.w1014.btn.clearfix > a") %>% 
      html_attr("href")
  ),
  stringsAsFactors = FALSE
)

# Add English type names
xi_talk_type  = merge(
  xi_talk_type,
  data.frame(
    type = c("经济", "政治", "文化", "社会", "生态", "党建", "国防", "外交"),
    type_en = c("economics", "politics", "culture", "society", "ecology", 
      "party", "defense", "diplomacy"),
    stringsAsFactors = FALSE),
  by = "type")

# Function to extract date between "[" and "]".
ExtractDate = function(text) {
  sub(".*\\[(.*)\\].*", "\\1", text)
}

# Function to extract list of talks from a given url into a data frame, 
# example url: http://jhsjk.people.cn/result?type=102
# This helps us to get the url of each talk so that we can later extract 
# the content of each talk under a for loop.
ExtractTalkList = function(url) {
  data.frame(
    date = sapply(
      read_html(url) %>%
        html_nodes("body > div.w1000.d2list.clearfix > div.fr > ul > li") %>%
        html_text(), 
      ExtractDate,
      USE.NAMES = FALSE
    ) %>% as.Date,
    type = type,
    type_en = type_en,
    title = read_html(url) %>%
      html_nodes("body > div.w1000.d2list.clearfix > div.fr > ul > li > a") %>%
      html_text(),
    link = paste0("http://jhsjk.people.cn/",
      read_html(url) %>%
        html_nodes("body > div.w1000.d2list.clearfix > div.fr > ul > li > a") %>%
        html_attr("href")
    ),
    stringsAsFactors = FALSE
  )
}

# Function to extract the last pagination on the page that lists talks.
# This helps us to determine whether to continue extracting list of talks
# on the next page.
ExtractLastPagination = function(url) {
  read_html(url) %>%
    html_nodes("body > div.w1000.d2list.clearfix > div.fr > div > ul > li:last-child > a") %>%
    html_attr("rel")
}

# First, we create an empty list. Every member of the list is a dataframe,
# using type of the talks as its name. 
# This is convenient for slicing and we can also do `data.table::rbindlist` 
# afterwards if we want. 
# After extracting a list of talks, we will `rbind` it onto that type
# of talk.
# In the loop, we will continue to extract the list of talks if there is 
# still a "next" button on the pagination.
# From there we can also get the url of "next page" so that we will know
# where to move on.

xi_talk_list = list()

for (i in 1:nrow(xi_talk_type)) {
  type = xi_talk_type[i, "type"]
  type_en = xi_talk_type[i, "type_en"]
  url = xi_talk_type[i, "link"]
  xi_talk_list[[type]] = ExtractTalkList(url)
  pagination_last = ExtractLastPagination(url)
  while (isTRUE(pagination_last=="next")) {
    url = read_html(url) %>%
      html_nodes("body > div.w1000.d2list.clearfix > div.fr > div > ul > li:last-child > a") %>%
      html_attr("href")
    xi_talk_list[[type]] = base::rbind(
      xi_talk_list[[type]], 
      ExtractTalkList(url))
    pagination_last = ExtractLastPagination(url)
  }
}

# Function to extract content for each talk.
ExtractContent = function(url) {
  read_html(url) %>%
    html_nodes("body > div.d2txt.clearfix > div.d2txt_con.clearfix") %>%
    html_text()
}

# Function to wait for connection error, possibly caused by firewall or proxy 
# if one visits too frequently. 
# If error happens, it will wait for 30 seconds and retry to extract content.
ExtractContentWaitConnectError = function(url) {
  test = try(ExtractContent(url), silent = TRUE)
  while (class(test) == "try-error") {
    Sys.sleep(30)
    test = try(ExtractContent(url), silent = TRUE)
  }
  return(test)
}

# After retrieving a list of talks, we bind this list into a single dataframe
# `xi_talk_df`.
# Then for each row in this dataframe, we feed the url of a single talk
# to function to extract the content of the talk, and save it to the `content`
# column.

xi_talk_df = rbindlist(xi_talk_list)
xi_talk_df$content = ""

for (i in 1:nrow(xi_talk_df)) {
  url = xi_talk_df[[i, "link"]]
  xi_talk_df$content[i] = ExtractContentWaitConnectError(url)
}

write_csv(xi_talk_df, "xi_talk.csv")

# Save talks by type

for (i in 1:nrow(xi_talk_type)) {
  type_select = xi_talk_type[[i, "type"]]
  type_en_select = xi_talk_type[[i, "type_en"]]
  xi_talk_df %>%
    dplyr::filter(type==type_select) %>%
    write_csv(paste0("xi_talk_by_type/xi_talk_", type_en_select, ".csv"))
}

