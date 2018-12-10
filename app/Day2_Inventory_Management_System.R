# -*-*-*-*-*-*-*-*-*-*-*-*
# SOLUTION LIBRARY
# -*-*-*-*-*-*-*-*-*-*-*-*

# -*-*-*-*-*-*-*-*-*-*-*-*

count_duplicates<-function(id_list){
  
  list_size <- length(id_list)
  result <- 1
  count_one   = 0
  count_two   = 0
  count_three = 0
  
  for(i in 1:list_size){
    id_row <- id_list[i]
    id_size <- nchar(id_row)
    id_array <- vector()
    result <- vector()

    for (j in 1:id_size){
      id <- substr(id_row, j, j)
      id_array <- c(id_array, id)
    
    }
    row_duplicates <- table(id_array)
    row_duplicates <- sort(row_duplicates)
    dup_size <- length(row_duplicates)
    previous_value = 0
    current_value = 0
    for ( k in 1:dup_size){
      current_value = row_duplicates[k]
      if (current_value > previous_value){
        
        if ( current_value == 2){
          count_two <- count_two + 1
        }
        
        if ( current_value == 3){
          count_three <- count_three + 1
        } 
        
      }
      previous_value = current_value
    }
  }
  result = count_two * count_three
  
}

# # -*-*-*-*-*-*-*-*-*-*-*-*
# # UNIT TESTING
# # -*-*-*-*-*-*-*-*-*-*-*-*
# # ASSERTS

source('app/library/Assert.R')

# -*-*-*-*-*-*-*-*-*-*-*-*
# VARIANTS
# -*-*-*-*-*-*-*-*-*-*-*-*

# write.csv(payload, file = "app//payload//Day2_Inventory_Management_System.csv",row.names=FALSE, na="")

# variant <- c("abcdef","bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab")
# expected <- 12
# actual <- count_duplicates(variant)
# message  = '1'
# myAssert.integer.equals(message, expected, actual)

# -------- GET THE RESULT AFTER TESTING 

payload <- read.csv(file = "app//payload//Day2_Inventory_Management_System.csv",header=FALSE)
result <- count_duplicates(payload)

print("--- Result ---")
print(result)
