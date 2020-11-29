

# FUNCTION TO GET FREDDIE MAC HPI DATA 
# Seasonally Adjusted - State HPI
# Source: http://www.freddiemac.com/research/indices/house-price-index.page
#------------------------------------------------------------

if(!require(pacman)){
  install.packages("pacman")
  library(pacman)
}

p_load(tidyverse, lubridate, readr, readxl)

get_fre_state_hpi <- function(fre_url = NULL, from_yr = NULL){
  if(missing(fre_url)){
    fre_url <- "http://www.freddiemac.com/fmac-resources/research/docs/State_and_US_SA.xls"
  }else{
    fre_url <- fre_url
  }
  
  tf <- tempfile()
  fname <- basename(fre_url)
  download.file(fre_url, destfile = paste0(tf, fname), mode = "wb")
  
  d_fre_load <- read_excel(paste0(tf, fname), skip = 5)
  
  drop_rows <- seq(min(as.numeric(which(rowSums(is.na(d_fre_load)) >10, 
                                        arr.ind = T))),nrow(d_fre_load))
  
  d_fre_load <- d_fre_load%>%
    filter(!row.names(d_fre_load) %in% drop_rows)
  
  dt_replace_mo <- str_sub(max(d_fre_load$Month, rm.na = T), start = -2, -1)
  dt_replace_yr <- str_sub(max(d_fre_load$Month, na.rm = T), start = 0, end = 4)
  new_date <- seq(as.Date("1975-01-01"), 
                  as.Date(sprintf("%s-%s-01",dt_replace_yr, dt_replace_mo)), by = "month")
  
  d_fre <- d_fre_load%>%
    dplyr::select(-Month)%>%
    mutate(date = new_date)%>%
    mutate_if(is.character, as.numeric)%>%
    pivot_longer(-date, names_to= "state_abb", values_to = "value")%>%
    mutate(dt_mo = month(date), 
           dt_yr = year(date), 
           value = as.numeric(value))%>%
    group_by(state_abb)%>%
    mutate(hpa_yoy = value/lag(value, 12)-1, 
           hpa_mom1 = value/lag(value, 1)-1)%>%
    ungroup()%>%
    filter(!state_abb == "United States seasonally adjusted")
  
  unlink(tf)
  
  if(missing(from_yr)){
    return(d_fre)
  }else if (from_yr%>%as.numeric() > max(year(d_fre$date), na.rm = T) || 
            from_yr%>%as.numeric() < min(year(d_fre$date), na.rm = T)) {
    paste0("Variable dt_yr must be within ", min(year(d_fre$date), na.rm = T), 
           " and ", max(year(dt_fre$date), na.rm = T))
    
  }else{
    from_yr <- (from_yr%>%as.numeric()-1)%>%as.character()
    from_dt <- ymd(sprintf("%s-12-01", from_yr))
    
    d_fre_sub <- d_fre%>%
      filter(date > from_dt)
    
    return(d_fre_sub)
    
    # print(paste0("Data set from: ", 
    #       range(d_fre_sub$date)[1], 
    #       " to ", range(d_fre_sub$date)[2]))
  }
  
}





