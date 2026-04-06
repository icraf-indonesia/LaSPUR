# List of functions

# Identifikasi Area Tumpang Tindih ----------------------------------------

# 1. identify_overlaps()
# 2. process_overlaps()
# 3. validate_zone_class()

# Perhitungan Indeks PADU-KE ----------------------------------------------

# 4. generate_matrix_serasi()

#' Generate a Compatibility Matrix (SERASI)
#'
#' This function takes two spatial (sf) objects, extracts the unique classes 
#' from their first attribute columns, and organizes them into a tibble 
#' matrix. The first object's classes form the vertical rows, while the 
#' second object's classes form the horizontal headers.
#'
#' @param sf_1 A simple feature (sf) object. The first column is used for the row classes.
#' @param sf_2 A simple feature (sf) object. The first column is used for the column headers.
#' @param fill_value The initial value for the matrix cells (e.g., 0 or NA). Defaults to NA.
#'
#' @return A tibble where the first column is "RTRW_RZWP3K" followed by 
#' columns named after the unique classes in sf_2.
#' @export
#'
#' @examples
#' # result <- generate_matrix_serasi(rtrw_vect, rzwp3k_vect, fill_value = 0)
generate_matrix_serasi <- function(sf_1, sf_2, fill_value = NA) {
  
  require(dplyr)
  require(tibble)
  require(sf)
  
  # Extract unique classes from the first column of each sf object
  # st_drop_geometry is used to treat them as data frames
  rows <- sf_1 %>% 
    st_drop_geometry() %>% 
    distinct(across(1)) %>% 
    pull(1)
  
  cols <- sf_2 %>% 
    st_drop_geometry() %>% 
    distinct(across(1)) %>% 
    pull(1)
  
  # Create a matrix with the specified fill value
  mat <- matrix(fill_value, 
                nrow = length(rows), 
                ncol = length(cols))
  
  # Assign the horizontal headers
  colnames(mat) <- cols
  
  # Convert to tibble and insert the vertical labels in the first column
  matrix_tibble <- as_tibble(mat) %>%
    add_column(RTRW_RZWP3K = rows, .before = 1)
  
  return(matrix_tibble)
}

# 5. merge_attributes_to_map()
# 6. generate_matrix_padu_ke()
# 7. calculate_lulc_adjacency()
# 8. calculate_padu_ke()

# Perhitungan Indeks PADAN ------------------------------------------------

# 9. calculate_padan()
# 10. calculate_recommendation()