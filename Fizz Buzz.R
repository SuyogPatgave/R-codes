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
