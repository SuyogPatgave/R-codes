rome_2_int<- function(rome){
  rome<- toupper(rome)
  rome_map<- c(I = 1, V = 5, X = 10, L = 50, C = 100, D = 500, M = 1000)
  
  rome_length<- nchar(rome)
  total_sum<- 0
  
  for(i in 1:(rome_length-1)){ 
    first_char<- substr(rome, i, i)
    sec_char<- substr(rome, i+1, i+1)
    
    if(rome_map[first_char]<rome_map[sec_char]){
      total_sum<- total_sum-rome_map[first_char]
    }else{total_sum<- total_sum+rome_map[first_char]
    }
  }
  
  total_sum<- total_sum+rome_map[substr(rome, rome_length, rome_length)]
  return(total_sum)
}

rome_2_int("IV")