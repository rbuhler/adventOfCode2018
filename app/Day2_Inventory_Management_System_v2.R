# -*-*-*-*-*-*-*-*-*-*-*-*
# SOLUTION LIBRARY
# -*-*-*-*-*-*-*-*-*-*-*-*

# -*-*-*-*-*-*-*-*-*-*-*-*

count_duplicates<-function(id_list){
  
  list_size <- length(id_list)
  
  for(i in 1:list_size){
    id_rowA <- as.character(id_list[i])
    id_size <- nchar(id_rowA)

    for (j in 1:list_size){
      id_rowB <- as.character(id_list[j])
      
      id_both <- paste0(id_rowA, id_rowB)
      comparison <- table(sort(id_both))
      comparison <- sort(comparison)
      
    }
  }
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

variant <- c("abcde","fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz")
expected <- "fgij"
actual <- count_duplicates(variant)
message  = '1'
myAssert.integer.equals(message, expected, actual)

# -------- GET THE RESULT AFTER TESTING 

# payload <- read.csv(file = "app//payload//Day2_Inventory_Management_System2.csv",header=FALSE)$V1
# result <- count_duplicates(payload)
# 
# print("--- Result ---")
# print(result)
