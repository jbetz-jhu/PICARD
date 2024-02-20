eval_with_args <- function(FUN, ...) FUN(...)




paste_and <- function(x){
  if(length(x) == 1) {
    return(x)
  } else {
    return(
      paste0(paste0(head(x, n = -1), collapse = ", "), " and ", tail(x, n = 1))
    )
  }
}




parse_delimited_text <-
  function(x, delimiter = text_delimiter){
    if(!is.null(x)){
      parsed_text <-
        do.call(what = c, args = strsplit(x = x, split = delimiter))
      return(stringr::str_trim(parsed_text, side = "both"))
    } else {
      return(NULL)
    }
  }




parse_delimited_text_to_numeric <-
  function(x, delimiter = text_delimiter){
    return(
      suppressWarnings(
        expr = as.numeric(parse_delimited_text(x = x, delimiter = delimiter))
      )
    )
  }




delimited_text_is_numeric <-
  function(x, delimiter = text_delimiter) {
    parsed_x <-
      parse_delimited_text_to_numeric(x, delimiter)
    
    if(sum(!is.finite(parsed_x)) > 0){
      parsed_x <-
        parse_delimited_text(x = x, delimiter = delimiter)
      numeric_x <-
        parse_delimited_text_to_numeric(x = x, delimiter = delimiter)
      invalid_indices <- which(!is.finite(numeric_x))
      
      error_msg <- 
        paste(
          paste0("`", parsed_x[invalid_indices], "`"), collapse = ", ")
      
      return(paste0("Non-numeric values: ", error_msg))
    } else {
      return(NULL)
    }
  }




is_unique <-
  function(x){
    if(any(duplicated(x))){
      table_x <- table(x)
      duplicates_x <- names(table_x)[which(table_x > 1)]
      return(
        paste0(
          "Values are duplicated: ",
          paste(paste0("`", duplicates_x, "`"), collapse = ", ")
        )
      )
    } else {
      return(NULL)
    }
  }




in_range <-
  function(x, range_min, range_max, include_lower = TRUE, include_upper = TRUE){
    
    invalid_indices <-
      which(
        (if(include_lower){
          x < range_min
        } else {
          x <= range_min
        }) |
          (if(include_upper){
            x > range_max
          } else {
            x >= range_max
          })
      )
    
    if(length(invalid_indices) == 0){
      return(NULL)
    } else {
      error_msg <-
        paste(paste0("`", x[invalid_indices], "`"), collapse = ", ")
      return(
        paste0("Values outside ",
               ifelse(test = include_lower, yes = "[", no = "("),
               range_min, ", ", range_max, 
               ifelse(test = include_upper, yes = "]", no = ")"), ": ", 
               error_msg)
      )
    }
  }




sums_to <- function(x, target_sum){
  sum_x <- sum(x)
  if(sum(x) == target_sum){
    return(NULL)
  } else {
    return(paste0("Values must sum to ", target_sum,": Sum = ", sum_x))
  }
}




length_range <-
  function(
    x, length_min, length_max
  ){
    length_x <- length(x)
    if(!is.null(length_max)){
      if(length_x > length_max){
        return(paste0("Length must be ", length_max, " or less."))
      }
    }
    
    if(!is.null(length_min)){
      if(length_x < length_min){
        return(paste0("Length must be ", length_min, " or more."))
      }
    }
    
    return(NULL)
  }




exclude_values <-
  function(x, exclude, message = NULL){
    excluded_values <- which(x %in% exclude)
    if(length(excluded_values) > 0){
      if(is.null(message)){
        return(paste0("Values cannot be in {", 
                      paste0(exclude, collapse = ", "), "}"))
      } else {
        return(message)
      }
    } else {
      return(NULL)
    }
  }




limit_increment <-
  function(
    x, lower_bound = -Inf, upper_bound = Inf,
    sort_first = FALSE,
    sort_decreasing = FALSE,
    take_abs = FALSE
  ) {
    if(sort_first) sort(x, decreasing = sort_decreasing)
    
    diffs <-
      if(take_abs){
        abs(diff(x))
      } else {
        diff(x)
      }
    
    below_lower <- which(diffs < lower_bound)
    above_upper <- which(diffs > upper_bound)
    
    below_values_msg <- above_values_msg <- NULL
    
    bound_l_char <- ifelse(test = take_abs, yes = "|", no = "(")
    bound_u_char <- ifelse(test = take_abs, yes = "|", no = "(")
    increment_string <-
      ifelse(test = take_abs, yes = "Absolute increments", no = "Increments")
    
    if(length(below_lower) > 0 ){
      below_values_msg <- 
        paste0(increment_string, " below ", lower_bound, ": ",
               paste(
                 paste0(bound_l_char, x[below_lower], "-", x[below_lower + 1],
                        bound_u_char), collapse = "; "
               )
        )
    }
    
    if(length(above_upper) > 0 ){
      above_values_msg <- 
        paste0(increment_string, " above ", upper_bound, ": ",
               paste(
                 paste0(bound_l_char, x[above_upper], "-", x[above_upper + 1],
                        bound_u_char), collapse = "; ")
        )
    }
    
    if(is.null(below_values_msg) & is.null(above_values_msg)){
      return(NULL)
    } else {
      return(
        paste0(below_values_msg, "; ", above_values_msg)
      )
    }
  }




check_delimited_text_input <-
  function(x = x, delimiter = text_delimiter,
           check_functions){
    numeric_check <- delimited_text_is_numeric(x = x, delimiter = text_delimiter)
    if(is.null(numeric_check)){
      numeric_x <-
        parse_delimited_text_to_numeric(x = x, delimiter = text_delimiter)
      
      if(length(numeric_x) > 0){
        check_result <- 
          lapply(X = check_functions, FUN = eval_with_args, numeric_x)
        failed_checks <- !sapply(X = check_result, FUN = is.null)
        if(sum(failed_checks) > 0){
          return(check_result[[min(which(failed_checks))]])
        } else {
          return(NULL)
        }
      } else {
        return(NULL)
      }
    } else {
      return(numeric_check)
    }
  }