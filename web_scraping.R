library(dplyr)
#library(glue)
# Glue example: glue("{episode_name_max}")
library(polite)
library(purrr)
library(readr)
library(rvest)
library(stringr)
library(tidyr)

session <- bow("https://www.st-minutiae.com")
scripts <- session |>
  nod(path="resources/scripts") |>
  scrape(content="text/html; charset=UTF-8") |>
  html_node("#main")

# Use descendant combination to select elements and their children
# E.g., all <a> elements of <li> of <ul> w/ class "episode-list"
episodes <- html_elements(scripts, "ul.episode-list li a")

# Example episode
clues_episode <- episodes[97]
clues_url <- html_attr(clues_episode, "href")
clues_name <- html_text2(clues_episode)

clues_html <- session |>
  nod(path=clues_url) |>
  scrape(content="text/html; charset=UTF-8")

# Use html_text to preserve newlines
clues_df_raw <- tibble(text = html_text(clues_html))
clues_df <- clues_df_raw |>
  separate_longer_delim(cols = text, delim = "\n") |>
  mutate(text = trimws(text))

clues_df |>
  slice_head(n = 150) |>
  print(n = 150)

pattern_episode_name = "STAR TREK: \\\"[^\"]*\\\" -"

clues_df |>
  filter(str_detect(text,
                    regex(pattern_episode_name, ignore_case = TRUE))) |>
  pull(text)




# Use first and last TNG episode names to determine range of TNG episodes
# E.g., can't be more specific with "#thenextgeneration" bc they're not children
episode_name_index <- c("Encounter at Farpoint", r"(All Good Things...)")
episode_range      <- map_int(episode_name_index, function(x) which(str_detect(html_text2(episodes), x)))
#map(episode_name_index, function(x) episodes %>% html_text2() %>% str_detect(x) %>% which())



episode_index <- list()
episode_min_n    <- str_detect(html_text2(episodes), episode_min_name)
episode_max_n    <- str_detect(html_text2(episodes), episode_max_name)
html_text2(episodes) %>% str_detect(episode_min_name)


episodes %>% html_text2() %>% str_detect("Encounter at Farpoint") %>% which()




