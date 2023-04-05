# list_union_status
list_union_status <- function(core_data, is_in_union){

  if(is_in_union %in% c("Y", "N")) {
    U <- 1
    get_u_i <- ifelse(core_data$observations$is_in_union %in% c("Y", "N"), 1, NULL)
  }
  
  else {
    U <- 2
    get_u_i <- ifelse(core_data$observations$is_in_union == "Y", 1, 2)
  }
  
  union_status = list(
    U = U,
    get_u_i = get_u_i
  )
  
  return(union_status)
}