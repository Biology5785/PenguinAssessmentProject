
# A function to make sure the column names are cleaned up, 
clean_column_names <- function(penguins_data) {
  penguins_data %>%
    clean_names()
}
  
  
# A function to remove columns based on a vector of column names
remove_columns <- function(penguins_data, column_names) {
    penguins_data %>%
      select(-one_of(column_names))
  }
  
# A function to make sure the species names are shortened
shorten_species <- function(penguins_data) {
  penguins_data %>%
    mutate(species = case_when(
      species == "Adelie Penguin (Pygoscelis adeliae)" ~ "Adelie",
      species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap",
      species == "Gentoo penguin (Pygoscelis papua)" ~ "Gentoo"
    ))
}

# A function to remove any empty columns or rows
remove_empty_columns_rows <- function(penguins_data) {
  penguins_data %>%
    remove_empty(c("rows", "cols"))
}


# A function to remove rows which contain NA values
remove_NA <- function(penguins_data) {
  penguins_data %>%
    na.omit()
}

cleaning_penguins <- function(raw_data){
  raw_data %>%
    clean_column_names() %>%
    remove_columns(c("comments")) %>%
    shorten_species() %>%
    remove_empty_columns_rows() %>%
    remove_NA()                  
}

