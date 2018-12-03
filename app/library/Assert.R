# -*-*-*-*-*-*-*-*-*-*-*-*
# UNIT TESTING
# -*-*-*-*-*-*-*-*-*-*-*-*
# ASSERTS
myAssert.integer.equals<-function(message, expected, actual){
  if (expected != actual){
    
    print("Assertion ERROR : Not equals")
    print(paste("Expected[ ", expected, " ]" )) 
    print(paste("Actual  [ ", actual, " ]"))
    print(paste("Message : ", message))
    
  }
}