# -*-*-*-*-*-*-*-*-*-*-*-*
# SOLUTION LIBRARY
# -*-*-*-*-*-*-*-*-*-*-*-*

# -*-*-*-*-*-*-*-*-*-*-*-*

count_duplicates<-function(id_list){
  
  list_size <- length(id_list)
  
  for(i in 1:list_size){
    id_row <- id_list[i]
    
    id_size <- nchar(id_row)
    id_array <- vector()
    for (j in 1:id_size){
      id <- substr(id_row, j, j)
      id_array <- c(id_array, id)
    }
    row_duplicates <- table(id_array)
    row_duplicates
  }
  
}

get_checksum<-function(id_list){
  
  result
}

# # -*-*-*-*-*-*-*-*-*-*-*-*
# # UNIT TESTING
# # -*-*-*-*-*-*-*-*-*-*-*-*
# # ASSERTS

source('app/library/Assert.R')

# -*-*-*-*-*-*-*-*-*-*-*-*
# VARIANTS
# -*-*-*-*-*-*-*-*-*-*-*-*

# write.csv(payload, file = "app//payload//Day2_Inventory_Management_System.cvs",row.names=FALSE, na="")

variant <- c("abcdef","bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab")
expected <- 12
actual <- count_duplicates(variant)
message  = '1'
myAssert.integer.equals(message, expected, actual)

# -------- GET THE RESULT AFTER TESTING 

# payload <- read.csv(file = "app//payload//Day2_Inventory_Management_System.cvs",header=FALSE)
# result <- first_double_frequency(payload)
# 
# print("--- Result ---")
# print(result)
