#####
# Helper functions for rules
#####

get_for_all_names <- function(index, first_matches, middle_matches,
                              last_matches, get_func) {
  first_name <- get_func(first_matches[index], first_matches) &
    (middle_matches[index] == middle_matches) &
    (last_matches[index] == last_matches)
  middle_name <- (first_matches[index] == first_matches) &
    get_func(middle_matches[index], middle_matches) &
    (last_matches[index] == last_matches)
  last_name <- (first_matches[index] == first_matches) &
    (middle_matches[index] == middle_matches) &
    get_func(last_matches[index], last_matches)
  
  first_name | middle_name | last_name
}

get_one_letters <- function(to_match, name_matches) {
  (str_length(to_match) == 1) &
    (to_match == substr(name_matches, 1, 1)) &
    (to_match != name_matches)
}

get_hyphens <- function(to_match, name_matches) {
  str_detect(to_match, "-") &
    (str_remove_all(to_match, "-") == name_matches)
}

get_caps <- function(to_match, name_matches) {
  (str_to_lower(to_match) == str_to_lower(name_matches)) &
    (to_match != name_matches)
}

#####
# Rule functions
#####

get_one_letter_names <- function(index, first_matches, middle_matches,
                                 last_matches) {
  matched_rows <- get_for_all_names(index, first_matches, middle_matches,
                                    last_matches, get_one_letters)
  
  return(paste(which(matched_rows) + 1, collapse = ', '))
}

get_reversed_first_middle <- function(index, first_matches, middle_matches,
                                      last_matches) {
  matched_rows <- (first_matches[index] == middle_matches) &
    (middle_matches[index] == first_matches) &
    (last_matches[index] == last_matches)
  
  return(paste(which(matched_rows) + 1, collapse = ', '))
}

get_hyphen_caps <- function(index, first_matches, middle_matches,
                            last_matches) {
  matched_rows <- get_for_all_names(index, first_matches, middle_matches,
                                    last_matches, get_hyphens) |
    get_for_all_names(index, first_matches, middle_matches,
                      last_matches, get_caps)
  
  return(paste(which(matched_rows) + 1, collapse = ', '))
}

get_rule_rows <- function(bus_file, rule_func) {
  lapply(1:nrow(bus_file), function(i) {
    rule_func(i, bus_file$`first name`,
              bus_file$`middle name`,
              bus_file$`last name`)
  }) %>%
    unlist()
}

get_fuzzy_matches <- function(index, full_names, addresses, error_margin) {
  all_matches <- (adist(full_names[index], full_names)[1, ] <= error_margin) &
    (addresses[index] == addresses)
  all_matches[index] <- FALSE
  paste(which(all_matches) + 1, collapse = ', ')
}

#####
# Table formatting functions
#####

get_error_table <- function(error_col_name, bus_table) {
  error_col <- bus_table[[error_col_name]]
  which_rows <- which(error_col != "")
  list_matched_rows <- str_split(error_col[which_rows], ", ")
  error_table <- lapply(seq_len(length(which_rows)), function(i) {
    matched_rows <- as.numeric(list_matched_rows[[i]]) - 1
    lapply(matched_rows, function(j) {
      matched_pair <- c(which_rows[i], j)
      first_names <- bus_table$`first name`
      middle_names <- bus_table$`middle name`
      last_names <- bus_table$`last name`
      addresses <- bus_table$`street address`
      tibble("Row number" = c(matched_pair, ""),
             "First name" = c(first_names[matched_pair], ""),
             "Middle name" = c(middle_names[matched_pair], ""),
             "Last name" = c(last_names[matched_pair], ""),
             "Addresses" = c(addresses[matched_pair], ""))
    }) %>%
      bind_rows()
  }) %>%
    bind_rows()
  
  table_cols <- c("Row number", "First name", "Middle name", "Last name",
                  "Addresses")
  name_row <- c(error_col_name, rep("", 4))
  names(name_row) <- table_cols
  blank_row <- rep("", 5)
  names(blank_row) <- table_cols
  bind_rows(name_row, error_table, blank_row, blank_row)
}

#####
# Server
#####

function(input, output) {
  
  bus_data <- reactiveVal("")
  bus_matches <- reactiveVal("")
  
  output$one_letter_name <- renderText("No data loaded")
  output$reversed_first_middle <- renderText("No data loaded")
  output$hyphen_caps <- renderText("No data loaded")
  
  observeEvent(input$load_file, {
    bus_file <- read_xlsx(input$bus_file$datapath, input$sheet_num, col_types = "text")
    bus_file[is.na(bus_file)] <- ""
    
    one_letter_name <- get_rule_rows(bus_file, get_one_letter_names)
    bus_file$`Rows with same name except for a one-letter abbreviation` <- one_letter_name
    reversed_first_middle <- get_rule_rows(bus_file, get_reversed_first_middle)
    bus_file$`Rows with identical last names but reversed first and middle names` <- reversed_first_middle
    hyphen_caps <- get_rule_rows(bus_file, get_hyphen_caps)
    bus_file$`Rows with identical names except for hyphens or capitalization` <- hyphen_caps
    
    full_names <- paste(bus_file$`first name`,
                        bus_file$`middle name`,
                        bus_file$`last name`)
    fuzzy_matches <- lapply(1:nrow(bus_file), function(i) {
      get_fuzzy_matches(i, full_names, bus_file$`street address`,
                        input$margin)
    }) %>%
      unlist()
    bus_file$`Rows with identical addresses and fuzzy matched names` <- fuzzy_matches
    
    output$one_letter_name <- renderText(length(which(one_letter_name != "")))
    output$reversed_first_middle <- renderText(length(which(reversed_first_middle != "")))
    output$hyphen_caps <- renderText(length(which(hyphen_caps != "")))
    
    bus_data(bus_file)
    
    full_matches_table <- lapply(colnames(bus_file)[11:ncol(bus_file)],
                                 get_error_table, bus_table = bus_file) %>%
      bind_rows()
    bus_matches(full_matches_table)
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      "fuzzylogic_data.xlsx"
    },
    content = function(file) {
      new_workbook <- createWorkbook()
      
      addWorksheet(new_workbook, "data")
      writeData(new_workbook, "data", bus_data())
      addStyle(new_workbook, "data",
               cols = 1:ncol(bus_data()), rows = 1,
               style = createStyle(textDecoration = "bold"))
      addStyle(new_workbook, "data",
               cols = 11:ncol(bus_data()),
               rows = seq_len(nrow(bus_data())) + 1, gridExpand = TRUE,
               style = createStyle(border = "TopBottomLeftRight",
                                   fgFill = "yellow"))
      
      addWorksheet(new_workbook, "matches")
      writeData(new_workbook, "matches", bus_matches())
      addStyle(new_workbook, "matches",
               cols = 1:ncol(bus_data()), rows = 1,
               style = createStyle(textDecoration = "bold"))
      
      saveWorkbook(new_workbook, file, overwrite = TRUE)
    }
  )
  
}
