library(tidyverse)
library(httr)
library(rvest)

getBillWebpage <- \(billno, year) {
  print(str_c('Capturing data for ', billno, ' of ', year, '...'))
  
  Sys.sleep(1)
  str_c(
    "https://nyassembly.gov/leg/?default_fld=&leg_video=&bn=",
    billno,
    "&term=",
    year,
    "&Summary=Y&Committee%26nbspVotes=Y&Floor%26nbspVotes=Y&Memo=Y&Actions=Y"
  )  %>% 
    httr::GET(config = httr::config(ssl_verifypeer = FALSE)) %>%
    httr::content(as='text') %>%
    return
}

getBillInfo <- \(billno, year) {
  # grab assembly bill page
  bill.pg <- getBillWebpage(billno, year)
    
  # basic bill info
  billinfo <- 
    bill.pg %>% 
    read_html() %>%
    html_table() %>%
    .[[1]] %>%
    rename(name = X1, value = X2) %>%
    mutate(value = str_replace(value, 'SAME AS ',''),
           value = str_replace(value, '^UNI. ','')
    ) %>%
    filter(value != "") %>%
    drop_na()
  
  # if assembly bill page not found then try one year earlier
  if(billinfo[billinfo['name'] == 'BILL NO',]$value == '') 
    bill.pg <- getBillWebpage(billno, year-1)
  
  if (billinfo[billinfo['name'] == 'SAME AS',]$value != '')
    sameas.pg <- getBillWebpage(billinfo[billinfo['name'] == 'SAME AS',]$value, year)
  
  # committee votes, using map to repeat proccess over
  # various pages
  commVotes <- 
    cbind(bill.pg, sameas.pg) %>%
    map(
      ~read_html(.) %>%
        html_elements('table') %>%
        map(
          \(x) {
            label = html_element(x, 'caption label') %>% html_text
            if(!is.na(label) & label == 'Committee:') {
              votes <-  html_table(x) %>%
                pivot_longer(everything()) %>%
                mutate(colname = ifelse(row_number() %% 2, 'Member', 'Vote'),
                       name = ceiling(row_number()/2)
                ) %>%
                pivot_wider( names_from = colname, values_from = value) %>%
                select(-name) %>%
                filter(Member != Vote) %>%
                arrange(Member)
              
              commName <- html_elements(x, 'caption b')[2] %>%
                html_text %>%
                str_match('^(.*?)\\W{3,}') %>%
                str_trim %>% .[1] %>%
                str_to_title
              
              voteTotal <- html_elements(x, 'caption span')[7] %>%
                html_text %>%
                str_match('^(.*?)\\W{2,}') %>%
                str_trim %>% .[1]
              
              act <- html_elements(x, 'caption span')[7] %>%
                html_text %>%
                str_match('Action: (.*?)$') %>%
                .[1,2] %>%
                str_trim
              
              date <- html_elements(x,'caption span')[4] %>% html_text
              
              return(list( date = date, name = commName, voteTotal = voteTotal, action = act, votes = votes))
            }
          }
        )
    )
  
  commVotes <- commVotes %>% unlist(recursive = F) 
  commVotes <- commVotes[!sapply(commVotes, is.null)] 
  
  floorVotes <- cbind(bill.pg, sameas.pg) %>%
    map(
      ~read_html(.) %>%
        html_elements('table') %>%
        map(
          \(x) {
            label = html_element(x, 'caption') %>% html_text
            
            if(!is.na(label) & grepl('Assembly Vote',label)) {
              votes <- html_table(x) %>%
                pivot_longer(everything()) %>%
                mutate(colname = ifelse(row_number() %% 2, 'Member', 'Vote'),
                       name = ceiling(row_number()/2)
                ) %>%
                pivot_wider( names_from = colname, values_from = value) %>%
                select(-name) %>%
                filter(Member != Vote) %>%
                arrange(Member)
              date <- html_elements(x,'caption span')[2] %>% html_text
              act <- html_elements(x,'caption span')[3] %>% html_text
              
              voteTotal <- 
                html_elements(x,'caption span')[6] %>% html_text %>%
                str_match('YEA/NAY: (.*?)$') %>% .[,2] %>% str_trim
              
              return(list(date = date, total = voteTotal, votes = votes, action = act))
            }
          }
        )
    )
  
  floorVotes <- floorVotes %>% unlist(recursive = F) 
  floorVotes <- floorVotes[!sapply(floorVotes, is.null)] 
  
  return(
    list(billno = billno, billinfo = billinfo, commitee = commVotes, floor = floorVotes)
  )
}  


