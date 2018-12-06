# -*-*-*-*-*-*-*-*-*-*-*-*
# SOLUTION LIBRARY
# -*-*-*-*-*-*-*-*-*-*-*-*

# -*-*-*-*-*-*-*-*-*-*-*-*

chronal_calibration<-function(iv_current, iv_change){
  
  change <- data.frame("payload" = c(iv_change))
  change_length = nrow(change)
  
  curr        = 0
  new_current = vector()
  cha         = 0 
  new_change  = vector()
  res         = iv_current 
  new_result  = vector()
  
  for( count in 1:change_length ){
    curr <- res
    cha  <- change[count, 1]
    res  <- cha + curr
  
    new_result = c(new_result, res)
    new_change = c(new_change, cha)
    new_current = c(new_current, curr)

  }
  result <- data.frame(new_current, new_change, new_result )
  
}

first_double_frequency<-function(change_list){

  change <- change_list
  frequencies <- c(0)
  duplicates <- vector()

  repeat{
    
    first_value <- frequencies[NROW(frequencies)]
    matrix_a <- chronal_calibration(first_value, change)
    
    frequencies <- c(frequencies, matrix_a[,3])
    duplicates <- duplicated(frequencies)
    
    result <- frequencies[duplicates]
    if (length(result) > 0){
      break  
    }
  }
  result[1]
}

# # -*-*-*-*-*-*-*-*-*-*-*-*
# # UNIT TESTING
# # -*-*-*-*-*-*-*-*-*-*-*-*
# # ASSERTS

source('app/library/Assert.R')

# -*-*-*-*-*-*-*-*-*-*-*-*
# VARIANTS
# -*-*-*-*-*-*-*-*-*-*-*-*

# write.csv(payload, file = "app//payload//Day1_chronal_calibration2.csv",row.names=FALSE, na="")

variant <- c(+1, -2, +3, +1)
expected <- 2
actual <- first_double_frequency(variant)
message  = '0'
myAssert.integer.equals(message, expected, actual)

variant <- c(+1, -1)
expected <- 0
actual <- first_double_frequency(variant)
message  = '1'
myAssert.integer.equals(message, expected, actual)

variant <- c(+3, +3, +4, -2, -4)
expected <- 10
actual <- first_double_frequency(variant)
message  = '2'
myAssert.integer.equals(message, expected, actual)

variant <- c(-6, +3, +8, +5, -6)
expected <- 5
actual <- first_double_frequency(variant)
message  = '3'
myAssert.integer.equals(message, expected, actual)

variant <- c(+7, +7, -2, -7, -4)
expected <- 14
actual <- first_double_frequency(variant)
message  = '4'
myAssert.integer.equals(message, expected, actual)

# -------- GET THE RESULT AFTER TESTING 

payload <- read.csv(file = "app//payload//Day1_chronal_calibration2.csv",header=FALSE)
result <- first_double_frequency(payload)

print("--- Result ---")
print(result)
