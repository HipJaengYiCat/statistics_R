readMatrix <- function()
{ #---------------------------------------------#
  # function to read Matrix data file
  #---------------------------------------------#
  cat("\n ----   TYPE the DATA File Name : ")
  name <- readline()
  data.m <- scan(name)

        cat("\n -------------------------------")
        cat("\n   (1) Upper Triangular Matrix  ")
        cat("\n   (2) Lower Triangular Matrix  ")
        cat("\n   (3) Full  Matrix             ")
        cat("\n -------------------------------")
        cat("\n  Type the number(Default=1) : ")
        upperValue <- readline()
        cat("\n --- Number of rows : ")
        n <- readline()
        n <- as.integer(n)
        if( upperValue == "2")
        {  DistanceArray <- array(0, n*(n-1)/2 )
           for(i in 1:(n-1) )
             for(j in (i+1):n )
                 { kk1 <- (j-1)*(j-2)/2 + i
                   kk2 <- n*(i-1)- i*(i-1)/2 + j-i
                   DistanceArray[kk2] <- data.m[kk1] }
        }
        else if( upperValue == "3")
             DistanceArray <- MDistanceVectorFtn(data.m)
        else
             DistanceArray <- data.m
 

     #cat("\n Variable Name file (If not want, only RETURN) : ")

     #lab.file <- readline()

     #if( lab.file != "")        
     #     labName <- scan(lab.file, what="")  
     #else 
     #     labName <- seq(n)

     
     DistanceArray = 9 - DistanceArray


     MD <- matrix(0, nrow=n, ncol=n)

     for(j in 1:(n-1) )
        for(k in (j+1):n)
        {    kk <- n*(j-1) - j*(j-1)/2 + k-j
             cat("\n kk= ", kk,  " j = ", j, " k = ", k) 
             MD[j,k] <- MD[k,j] <- DistanceArray[kk]
         }


     print(MD)

     #colnames(MD) <- labName
     #rownames(MD) <- labName

     return(MD)
}


MDistanceVectorFtn <- function(DistanceMatrix)
{ #---------------------------------------------------#
  # function of making distance vector(upper) from 
  # full distance matrix
  #---------------------------------------------------#
    n <- ncol(DistanceMatrix)
    mv <- array(0, n*(n-1)/2 )
    for(i in seq(n-1))
       for(j in seq(from = i+1, to = n))
       {   arr.n <- n*(i-1) - i*(i-1)/2 + j-i
           mv[arr.n] <- DistanceMatrix[i,j]
       }
    return(mv)
}




