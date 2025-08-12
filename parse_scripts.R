library(dplyr)
library(glue)
library(purrr)
library(readr)
library(stringr)
library(tidyr)
library(tidytext)

# ------------------------------ Functions 2 ------------------------------
# Function parse_script
parse_script <- function(file) {
  script <- readr::read_file(file) |> 
    stringr::str_split("\n")
  script[[1]] |> 
    stringr::str_squish()
}

# Function parse_sections
parse_sections <- function(script) {
  df <- dplyr::tibble(line = script) |> 
    mutate(
      act = dplyr::case_match(
        line,
        "META" ~ -1L,
        "TEASER" ~ 0L,
        "ACT ONE" ~ 1L,
        "ACT TWO" ~ 2L,
        "ACT THREE" ~ 3L,
        "ACT FOUR" ~ 4L,
        "ACT FIVE" ~ 5L,
        .default = NA_integer_
      )
    ) |> 
    tidyr::fill(act, .direction = "down") |> 
    dplyr::mutate(
      act = ifelse(is.na(act), 0L, act))
  df
}


# ------------------------------ Main Logic 2------------------------------
# 1. Parse script
# 2. Categorize into sections and a dataframe
#script <- parse_script("data/scripts_ds9/453.txt")
#df <- parse_sections(script)

# Alternatively, a purrr map
# Anonymous function for brevity i.e., \(x) x + 1
#purrr::map_chr(seq(402, 432), \(x) glue("data/scripts_ds9/{x}.txt"))
files <- purrr::map_chr(seq(402, 472), \(x) glue("data/scripts_ds9/{x}.txt"))
scripts <- purrr::map(files, \(file) parse_script(file))
dfs <- purrr::map(scripts, \(script) parse_sections(script))
all_df <- dplyr::bind_rows(dfs)


regex_pattern2 <- '(STAR TREK: DS9|STAR TREK|DEEP SPACE|DEEP SPACE NINE):\\s*)?"([^"]+)"\\s*(?:-\\s*(REV\\.\\s*)?(\\d{2}\\/\\d{2}\\/\\d{2})\\s*)'
regex_pattern <- '(STAR TREK: DS9|STAR TREK|DEEP SPACE|DEEP SPACE NINE):\\s*)'


all_df |> 
  mutate(
    title = dplyr::case_match(
      line,
      stringr::str_match_all(line, regex_pattern) ~ line,
      .default = NA_character_
    )
  )






stringr::str_match(script, '"([^"]+)"')
stringr::str_pad(df$line, width = 80, side = "both")






# ------------------------------ Functions ------------------------------
# Function read_script
read_script <- function(file) {
  readr::read_file(file)
}

# Function parse_metadata
parse_metadata <- function(script_lines) {
  metadata <- list()
  
  # Episode Title
  episode_name_regex <- '"([^"]+)"'
  episode_title <- stringr::str_match(script_lines, episode_name_regex)
  # Captured group is the second column
  if (!is.na(episode_title[1, 2])) {
    metadata$episode_title <- stringr::str_trim(episode_title[1, 2])
  }
  
  metadata
}

# Function parse_lines_raw
parse_lines_raw <- function(script) {
  lines <- stringr::str_split(script, "\n")[[1]]
  lines
}

# Function parse_sections
parse_sections <- function(lines) {
  sections <- lines[stringr::str_starts(lines, " ")] |> 
    stringr::str_squish() #|> 
    #stringr::str_pad(60, "both")
  sections
}

# Function parse_lines_clean
parse_lines_clean <- function(raw_lines) {
  lines <- raw_lines |> 
    stringr::str_squish() |> 
    stringr::str_pad(80, "both")
}

# Function parse_acts
parse_acts <- function(lines, sections) {
  acts <- c("TEASER", "ACT ONE","ACT TWO", "ACT THREE", "ACT FOUR","ACT FIVE")
  sections %in% acts
  dplyr::mutate(
    act
  )
}



# ------------------------------ Main Logic ------------------------------
script <- read_script("data/scripts_ds9/453.txt")
lines_raw <- parse_lines_raw(script)
sections <- parse_sections(lines_raw)
sections
lines <- parse_lines_clean(lines_raw)
#lines[1:20]

sections |> stringr::str_squish()

stringr::str_extract(sections, ".*(ACT).*\\w*")












'^(?:\\s*)(?:(STAR TREK: DS9|STAR TREK|DEEP SPACE):\\s*)?"([^"]+)"\\s*(?:-\\s*(REV\\.\\s*)?(\\d{2}\\/\\d{2}\\/\\d{2})\\s*)?-\\s*(CAST|PRONUNCIATION|ACT ONE|ACT TWO|ACT THREE|ACT FOUR|ACT FIVE)(?:\\s*([0-9.]+A?)?\\.)?$'

'^(?:\\s*)(?:(STAR TREK: DS9|STAR TREK|DEEP SPACE):\\s*)?"([^"]+)"\\s*(?:-\\s*(REV\\.\\s*)?(\\d{2}\\/\\d{2}\\/\\d{2})\\s*)?-\\s*(CAST|PRONUNCIATION|ACT ONE|ACT TWO|ACT THREE|ACT FOUR|ACT FIVE)(?:\\s*([0-9.]+A?)?\\.)?$'



# Function to split script into acts
split_acts <- function(script_string) {
  act_pattern <- "(TEASER|ACT ONE|ACT TWO|ACT THREE|ACT FOUR|ACT FIVE)"
  acts_raw <- str_split(script_string, paste0("(?=", act_pattern, ")"))[[1]] %>%
    str_trim() %>%
    keep(~ str_detect(.x, act_pattern)) # Keep only strings that start with an act marker
  
  # Create a named list of acts
  act_list <- acts_raw %>%
    set_names(map_chr(., ~ str_extract(.x, act_pattern))) %>%
    map(~ str_remove(.x, act_pattern)) %>%
    map(~ str_trim(.x))
  
  # Convert to a tidy dataframe
  tibble(
    act_name = names(act_list),
    act_content = unlist(act_list, use.names = FALSE)
  )
}
