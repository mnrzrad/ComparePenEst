library('glmnet')

set.seed(888)
x1 <- rnorm(50)
x2 <- x1 + rnorm(50,mean = 0.1)
beta1 = 0.6
beta2 = 0.4
epsilon <- rnorm(50)
y = x1*beta1 + x2*beta2 + epsilon

standardize <- function(x) {
  (x-mean(x))/sd(x)
}

z0 <- standardize(y)
z1 <- standardize(x1)
z2 <- standardize(x2)
lstsq <- lm(z0~z1+z2-1)
lstsq_beta <- coef(lstsq)


n_lstsq <- 50
s <- seq(-1, 1, length=n_lstsq)
rss_lstsq <- matrix(NA, nrow=n_lstsq, ncol=n_lstsq)
for (i in 1:n_lstsq) {
  for (j in 1:n_lstsq) {
    rss_lstsq[i, j] <- sum((z0-s[i]*z1-s[j]*z2)^2)
  }
}
# persp(s, s, rss_lstsq, xlab="beta1", ylab="beta2", zlab="rss_lstsq")


draw_axes <- function(main = "Ridge") {
  k2 <- seq(-1, 1, length=5)
  par(mar=c(2.6, 2.6, 1.6, 1.6), xaxs="i", yaxs="i")
  plot(1.02*range(s), 1.02*range(s), type="n", axes=FALSE ,xlab = " ", ylab = " " , main = main )
  axis(side=1, pos=0, col="black", at=k2, labels=rep(" ", length(k2)))
  axis(side=2, pos=0, col="black", at=k2, labels=rep(" ", length(k2)))
  text(0.05,1,TeX("$\\beta_2$"))
  text(0.98,0.08,TeX("$\\beta_1$"))
  text(k2[-3], -0.05, k2[-3], cex=0.9, col="black")
  text(-0.05, k2[-3], k2[-3], cex=0.9, col="black")
}

find_closest <- function(x, target) {
  d <- abs(x-target)
  return(which(d==min(d))[1])
}

draw_circle <- function(r,lwd = 1, col = "red") {
  radians <- seq(0, 2*pi, length=100)
  lines(r*sin(radians), r*cos(radians),col = col ,lwd = lwd)
}

draw_diamond<- function(d, lwd, col){
  segments( d, 0, 0, d,col ,lwd, lty = 1)
  segments( 0, d,-d, 0,col ,lwd, lty = 1)
  segments(-d, 0, 0,-d,col ,lwd, lty = 1)
  segments( 0,-d, d, 0,col ,lwd, lty = 1)
}

ridge <- glmnet(cbind(z1, z2), z0, 
                alpha=0, intercept=FALSE, nlambda=1000)

m_ridge <- dim(ridge$beta)[2]
rss_ridge <- rep(NA,m_ridge)
for (i in 1:m_ridge) {
  rss_ridge[i] <- sum((z0 - ridge$beta[1, i]*z1 -ridge$beta[2, i]*z2)^2)
}


lasso <- glmnet(cbind(z1, z2), z0, alpha=1, intercept=FALSE, nlambda=1000)
m_lasso <- dim(lasso$beta)[2]
rss_lasso <- rep(NA,m_lasso)
for (i in 1:m_lasso) {
  rss_lasso[i] <- sum((z0 - lasso$beta[1, i]*z1 -lasso$beta[2, i]*z2)^2)
}

k1 <- c(0, 1, 1.1, 1.2, 1.5, 2, 2.5, 3:9)
k1 <- c(0.1 * k1, k1, 10 * k1, 100 * k1, 1000 * k1)

# 
plotFunction <- function(value, type) {
  # Define contour levels with k1
  k1 <- min(rss_lstsq) + c(0.1, 0.5, seq(1, 20, 2), seq(30, 1000, 10))
  k1_filtered <- k1[k1 <= (min(rss_lstsq) + value)]
  
  # Draw axes
  draw_axes(main = " ")
  
  contour(s, s,
          matrix(rss_lstsq, nrow=n_lstsq),
          levels = k1,
          add = TRUE,
          col = "black",
          drawlabels = FALSE)

  k1_filtered <- k1[k1 <= (min(rss_lstsq) + value)]
  for (i in seq_along(k1_filtered)) {
    contour(s, s,
            matrix(rss_lstsq, nrow=n_lstsq),
            levels = k1_filtered[i],
            add = TRUE,
            col = "black",
            lwd = 2,
            drawlabels = FALSE)
  }
  

  # Plot true beta values with "OLS" label
  points(lstsq_beta[1], lstsq_beta[2], pch = 19, col = "black", cex = 1)
  text(lstsq_beta[1], lstsq_beta[2] + 0.01, "OLS", col = "black", cex = 1, pos = 3)
  
  # # Find the closest value for contouring
  # r1 <- find_closest(rss_ridge, k1[1])
  
  if(type == "Ridge"){
    for (i in seq_along(k1_filtered)) {
      if(i == 1){
        points(lstsq_beta[1], lstsq_beta[2], pch = 19, col = "red", cex = 1)
        draw_circle(sqrt(lstsq_beta[1]^2 + lstsq_beta[2]^2), lwd = 3, col = "blue")
      }else{
        r <- find_closest(rss_ridge, k1_filtered[i])
        draw_circle(sqrt(ridge$beta[1, r]^2 + ridge$beta[2, r]^2), lwd = 3, col = "blue")
        if(i==2){
          arrows(lstsq_beta[1], lstsq_beta[2], ridge$beta[1, r], ridge$beta[2, r], len=0.05,lwd = 4, col = 'green')
        }else{
          r0 <- find_closest(rss_ridge, k1_filtered[i-1])
          arrows(ridge$beta[1, r0], ridge$beta[2, r0], ridge$beta[1, r], ridge$beta[2, r], len=0.05, lwd = 4, col = 'green')
          
        }
      }
    }
  }
  
  if(type == "LASSO"){
    for (i in seq_along(k1_filtered)) {
      if(i == 1){
        points(lstsq_beta[1], lstsq_beta[2], pch = 19, col = "red", cex = 1)
        d0 <-  abs(lstsq_beta[1])+abs(lstsq_beta[2])
        draw_diamond(d0, lwd = 3, col = "blue")
      }else{
        r <- find_closest(rss_lasso, k1_filtered[i])
        d <- abs(lasso$beta[1, r])+abs(lasso$beta[2, r])
        draw_diamond(d, lwd = 3, col = "blue")
        if(i==2){
          arrows(lstsq_beta[1], lstsq_beta[2], lasso$beta[1, r], lasso$beta[2, r], len=0.05,lwd = 4, col = 'green')
        }else{
          r0 <- find_closest(rss_lasso, k1_filtered[i-1])
          arrows(lasso$beta[1, r0], lasso$beta[2, r0], lasso$beta[1, r], lasso$beta[2, r], len=0.05, lwd = 4, col = 'green')
          
        }
      }
    }
  }
}
