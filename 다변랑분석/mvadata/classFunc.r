#----------------------------------------------------------------------#
# Function to calculate Fisher's Classification coefficients           #
# This code follows the formulas in Legendre and Legendre's Numerical Ecology (1998)   #
#----------------------------------------------------------------------#
classfunc.lda <- function(x, groups){
  x.lda <- lda(groups ~ ., as.data.frame(x))

  gr <- length(unique(groups))   ## groups might be factors or numeric
  v <- ncol(x)                   ## variables
  m <- x.lda$means               ## group means

  w <- array(NA, dim = c(v, v, gr))

  tot.len <- nrow(x)
  subgr.len <- array(0, gr)

  for(i in 1:gr){
    tmp <- scale(subset(x, groups == unique(groups)[i]), scale = FALSE)
    w[,,i] <- t(tmp) %*% tmp
    subgr.len[i] <- nrow(tmp)
  }

  W <- w[,,1]
  for(i in 2:gr)
    W <- W + w[,,i]

  V <- W/(nrow(x) - gr)
  iV <- solve(V)

  class.func <- matrix(NA, nrow = v + 1, ncol = gr)

  colnames(class.func) <- rownames(m)
  rownames(class.func) <- c("constant", colnames(m) )

  for(i in 1:gr) {
    ni <- subgr.len[i]
    class.func[1, i] <- -0.5 * t(m[i,]) %*% iV %*% (m[i,]) + log(ni / tot.len)
    class.func[2:(v+1) ,i] <- iV %*% (m[i,])
  }

  x.lda$class.func <- class.func

  return(x.lda)
}  
