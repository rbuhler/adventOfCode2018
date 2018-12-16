# -*-*-*-*-*-*-*-*-*-*-*-*
# SOLUTION LIBRARY
# -*-*-*-*-*-*-*-*-*-*-*-*

# -*-*-*-*-*-*-*-*-*-*-*-*

read_claim<-function(claim){
  
  seq  <- 0
  left <- 0
  top  <- 0
  wide <- 0
  tall <- 0
  step <- ""
  char_order <- ""
  char_left  <- ""
  char_top   <- ""
  char_wide  <- ""
  char_tall  <-""
  coma <- FALSE
  x <- FALSE
  
  numbers <- "1234567890"
  result <- vector()
  
  claim_size <- nchar(claim)
  
  for(j in 1:claim_size){
    char <- substr(claim,j,j)

    if (step == "sequence"){
      if (grepl(char, numbers, fixed=TRUE)){
        char_order <- paste0(char_order, char)
      }
    }
    
    if (step == "position"){
      if (grepl(char, numbers, fixed=TRUE)){
       
        if (!coma){
          char_left <- paste0(char_left, char)          
        }else{
          char_top <- paste0(char_top, char)  
        }          
      }else{
        if (char == ','){
          coma <- TRUE
        }
      }
    }

    if (step == "dimention"){
      if (grepl(char, numbers, fixed=TRUE)){
        
        if(!x){
          char_wide <- paste0(char_wide, char)  
        }else{
          char_tall <- paste0(char_tall, char)  
        }
      }else{
        if( char == 'x'){
          x <- TRUE
        }
      }
    }
    
    
    # --------------------------------    
    if (char == "#"){
      step <- "sequence"
    }
    
    if(char == "@"){
      step <- "position"
    }
    
    if(char == ":"){
      step <- "dimention"
    }    
     
  }
  
  order <- as.numeric(char_order)
  left <- as.numeric(char_left)
  top <- as.numeric(char_top)
  wide <- as.numeric(char_wide)
  tall <- as.numeric(char_tall)
  
  result <- c(order, left, top, wide, tall)
  
}

count_overlap<-function(claim_list){
  
  return <- 0
  # read claim list
  list_size <- length(claim_list)
  for (i in 1:list_size){
    read <- read_claim(claim_list[i])
    read
    
  }

  # in an array of 1000 identify a claim
  
  # store each claim in a vector
  
  # return the vector of claims
  
  return
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

claim_one   = "#1 @ 1,3: 4x4"
claim_two   = "#2 @ 3,1: 4x4"
claim_three = "#3 @ 5,5: 2x2"

claim_list = c(claim_one, claim_two, claim_three)

variant <- claim_list
expected <- 4
actual <- count_overlap(variant)
message  = 'Variant 1'
myAssert.integer.equals(message, expected, actual[1])

# -------- GET THE RESULT AFTER TESTING 

# payload <- read.csv(file = "app//payload//Day2_Inventory_Management_System2.csv",header=FALSE)$V1
# result <- count_overlap(payload)
# 
# print("--- Result ---")
# print(result)
