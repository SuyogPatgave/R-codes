#library)
# Fizz-Buzz ---------------------------------------------------------------

# Write a function fizz_buzz_sum to find the sum of all 
# multiples of 3 or 5 below a target value.
# For example, if the target value was 10, 
# the multiples of 3 or 5 below 10 are 3, 5, 6, and 9.
# Because 3+5+6+9=23, the function would return 23.
# >> Passed all test cases
fizz_buzz<- function(target){
  
  num_1<- 3
  num_2<- 5
  
  range_num_1<- seq(num_1, target-1, by=num_1)
  range_num_2<- seq(num_2, target-1, by=num_2)
  total_range<- unique(c(range_num_1, range_num_2))
  total_sum<- sum(total_range)
  return(total_sum)
}

fizz_buzz(10)



# Roman to Numerical ------------------------------------------------------



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