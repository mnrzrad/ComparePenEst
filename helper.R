library('glmnet')


# x1 <- rnorm(50, sd = 1)
# x2 <- rnorm(50,mean = 0.1,sd=2)

x1 <- rnorm(50, sd = 0.5)
x2 <- x1 + rnorm(50, mean = 0.5, sd = 2)
  
beta1 = 0.6
beta2 = 0.4
epsilon <- rnorm(50)
y = x1*beta1 + x2*beta2 + epsilon

standardize <- function(x) {
  (x-mean(x))#/sd(x)
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

s2 = sum(lstsq$residuals**2)/(n_lstsq)
Z <- cbind(z1,z2) #cbind(x1,x2)
mg <- s2 / diag(solve(t(Z)%*%Z))

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

draw_diamond<- function(d, w, lwd, col){
  segments( d, 0, 0, d,col ,lwd, lty = 1)
  segments( 0, d,-d, 0,col ,lwd, lty = 1)
  segments(-d, 0, 0,-d,col ,lwd, lty = 1)
  segments( 0,-d, d, 0,col ,lwd, lty = 1)
}

draw_combined_penalty <- function(r, d, alpha, lwd = 1) {
  # Define the number of points to draw
  n <- 100
  t <- seq(0, 2*pi, length.out = n)
  
  # Draw circle
  circle_x <- r * cos(t)
  circle_y <- r * sin(t)
  lines(circle_x, circle_y, col = "red", lwd = lwd)
  
  # Draw diamond
  diamond_x <- c(d, 0, -d, 0, d)
  diamond_y <- c(0, d, 0, -d, 0)
  lines(diamond_x, diamond_y, col = "blue", lwd = lwd)
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

alasso <- glmnet(x = cbind(z1, z2), y = z0, alpha = 1,intercept=FALSE, nlambda=1000,
                 penalty.factor = 1 / abs(lstsq_beta))
m_alasso <- dim(alasso$beta)[2]
rss_alasso <- rep(NA,m_alasso)
for (i in 1:m_alasso) {
  rss_alasso[i] <- sum((z0 - alasso$beta[1, i]*z1 -alasso$beta[2, i]*z2)^2)
}

mglasso <- glmnet(x = cbind(z1, z2), y = z0, alpha = 1,intercept=FALSE, nlambda=1000, penalty.factor = 1 / mg)
m_mglasso <- dim(mglasso$beta)[2]
rss_mglasso <- rep(NA,m_mglasso)
for (i in 1:m_mglasso) {
  rss_mglasso[i] <- sum((z0 - mglasso$beta[1, i]*z1 -mglasso$beta[2, i]*z2)^2)
}

enet <- glmnet(x = cbind(z1, z2), y = z0, alpha = 0.5,intercept=FALSE, nlambda=1000, penalty.factor = 1 / mg)
m_enet <- dim(enet$beta)[2]
rss_enet <- rep(NA,m_enet)
for (i in 1:m_enet) {
  rss_enet[i] <- sum((z0 - enet$beta[1, i]*z1 -enet$beta[2, i]*z2)^2)
}


# k1 <- c(0, 1, 1.1, 1.2, 1.5, 2, 2.5, 3:9)
# k1 <- c(0.1 * k1, k1, 10 * k1, 100 * k1, 1000 * k1)
# Define contour levels with k1
k1 <- min(rss_lstsq) + c(0.1, 0.5, seq(1, 20, 2), seq(30, 1000, 10))
 
plotFunction <- function(value, type, data) {
  
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
        draw_diamond(d0, 1, lwd = 3, col = "blue")
      }else{
        r <- find_closest(rss_lasso, k1_filtered[i])
        d <- abs(lasso$beta[1, r])+abs(lasso$beta[2, r])
        draw_diamond(d, 1, lwd = 3, col = "blue")
        if(i==2){
          arrows(lstsq_beta[1], lstsq_beta[2], lasso$beta[1, r], lasso$beta[2, r], len=0.05,lwd = 4, col = 'green')
        }else{
          r0 <- find_closest(rss_lasso, k1_filtered[i-1])
          arrows(lasso$beta[1, r0], lasso$beta[2, r0], lasso$beta[1, r], lasso$beta[2, r], len=0.05, lwd = 4, col = 'green')
          
        }
      }
    }
  }
  
  # if(type == "ElasticNet"){
  #   for (i in seq_along(k1_filtered)) {
  #     if(i == 1){
  #       points(lstsq_beta[1], lstsq_beta[2], pch = 19, col = "red", cex = 1)
  #       d0 <-  abs(lstsq_beta[1])+abs(lstsq_beta[2])
  #       draw_combined_penalty(d0, d0, alpha= 0.5, lwd = 2)
  #     }else{
  #       r <- find_closest(rss_enet, k1_filtered[i])
  #       d <- abs(lasso$beta[1, r])+abs(lasso$beta[2, r])
  #       R <- sqrt(ridge$beta[1, r]^2 + ridge$beta[2, r]^2)
  #       draw_combined_penalty(R, d, alpha= 0.5, lwd = 2)
  #       if(i==2){
  #         arrows(lstsq_beta[1], lstsq_beta[2], enet$beta[1, r], enet$beta[2, r], len=0.05,lwd = 4, col = 'green')
  #       }else{
  #         r0 <- find_closest(rss_enet, k1_filtered[i-1])
  #         arrows(enet$beta[1, r0], enet$beta[2, r0], enet$beta[1, r], enet$beta[2, r], len=0.05, lwd = 4, col = 'green')
  #         
  #       }
  #     }
  #   }
  # }
  
  
  if(type == "adaptiveLASSO"){
    for (i in seq_along(k1_filtered)) {
      if(i == 1){
        points(lstsq_beta[1], lstsq_beta[2], pch = 19, col = "red", cex = 1)
        d0 <-  abs(lstsq_beta[1])+abs(lstsq_beta[2])
        w <- abs(lstsq_beta[1])/abs(lstsq_beta[2])
        draw_diamond(d0, w, lwd = 3, col = "blue")
      }else{
        r <- find_closest(rss_alasso, k1_filtered[i])
        d <- abs(alasso$beta[1, r])+abs(alasso$beta[2, r])
        w <- abs(lstsq_beta[1])/abs(lstsq_beta[2])
        draw_diamond(d, w, lwd = 3, col = "blue")
        if(i==2){
          arrows(lstsq_beta[1], lstsq_beta[2], alasso$beta[1, r], alasso$beta[2, r], len=0.05,lwd = 4, col = 'green')
        }else{
          r0 <- find_closest(rss_alasso, k1_filtered[i-1])
          arrows(alasso$beta[1, r0], alasso$beta[2, r0],alasso$beta[1, r], alasso$beta[2, r], len=0.05, lwd = 4, col = 'green')
        }
      }
    }
  }
  
  if(type == "marginalizedLASSO"){
    for (i in seq_along(k1_filtered)) {
      if(i == 1){
        points(lstsq_beta[1], lstsq_beta[2], pch = 19, col = "red", cex = 1)
        d0 <-  abs(lstsq_beta[1])+abs(lstsq_beta[2])
        w <- mg[1]/mg[2]
        draw_diamond(d0, w, lwd = 3, col = "blue")
      }else{
        r <- find_closest(rss_mglasso, k1_filtered[i])
        d <- abs(mglasso$beta[1, r])+abs(mglasso$beta[2, r])
        w <- mg[1]/mg[2]
        draw_diamond(d, w,  lwd = 3, col = "blue")
        if(i==2){
          arrows(lstsq_beta[1], lstsq_beta[2], mglasso$beta[1, r], mglasso$beta[2, r], len=0.05,lwd = 4, col = 'green')
        }else{
          r0 <- find_closest(rss_mglasso, k1_filtered[i-1])
          arrows(mglasso$beta[1, r0], mglasso$beta[2, r0],mglasso$beta[1, r], mglasso$beta[2, r], len=0.05, lwd = 4, col = 'green')
        }
      }
    }
  }
}




getExplanation <- function(estimator) {
  if (estimator == "Ridge") {
    list(
      author = "Hoerl, Arthur E., and Kennard, Robert W.",
      photos = c(
        "https://media.licdn.com/dms/image/v2/C4E22AQGlRxiu0U_d_Q/feedshare-shrink_800/feedshare-shrink_800/0/1604091944335?e=1727308800&v=beta&t=ccm43ntOgBF5xmzB1LBsS1mO1xspVRCraS7O-inIj0M",
        "https://k2-storage-prod.s3.amazonaws.com/storage/userimg/original/20240520045812-6062379b39cf44248fa8548ba7c6a9e5-original.jpg"
      ),
      formula = "$$\\hat{\\beta}^{\\textrm{ridge}} = \\arg\\min_{\\beta} \\left\\{ \\|y - X\\beta\\|_2^2 + \\lambda \\|\\beta\\|_2^2\\right\\}$$",
      reference = "Hoerl, A. E., & Kennard, R. W. (1970). Ridge Regression: Biased Estimation for Nonorthogonal Problems. Technometrics, 12(1), 55–67. https://doi.org/10.2307/1267351"
    )
  } else if (estimator == "LASSO") {
    list(
      author = "Tibshirani, Robert",
      photo = "https://statistics.stanford.edu/sites/statistics/files/styles/hs_medium_square_360x360/public/media/people/tibshirani_new.jpg?h=dc1a5897&itok=G95tWrMG",
      formula = "$$\\hat{\\beta}^{\\textrm{lasso}} = \\arg\\min_{\\beta} \\left\\{\\|y - X\\beta\\|_2^2 + \\lambda \\|\\beta\\|_1\\right\\}$$",
      reference = "Tibshirani, R. (1996). Regression Shrinkage and Selection via the Lasso. Journal of the Royal Statistical Society. Series B (Methodological), 58(1), 267–288. http://www.jstor.org/stable/2346178"
    )
  } else if (estimator == "adaptiveLASSO") {
    list(
      author = "Zou, Hui",
      photo = "http://users.stat.umn.edu/~zouxx019/myphotos/zou-department-photo.jpg",
      formula = "$$\\hat{\\beta}^{\\textrm{adaptiveLASSO}} = \\arg\\min_{\\beta} \\left\\{\\|y - X\\beta\\|_2^2 + \\lambda \\sum_{j} w_j |\\beta_j|\\right\\} \\quad \\text{where } w_j = (|\\beta^{\\textrm{OLS}}_j|)^{-1}$$",
      reference = "Zou, H. (2006). The Adaptive Lasso and Its Oracle Properties. Journal of the American Statistical Association, 101(476), 1418–1429. http://www.jstor.org/stable/27639762"
    )
  } else if (estimator == "marginalizedLASSO") {
    list(
      author = "A. K. Md. Ehsanes Saleh",
      photo = "https://carleton.ca/math/wp-content/uploads/a_k_saleh-240x240.jpg",  # Replace with an actual URL
      formula = "$$
                \\begin{align*}
                \\hat{\\beta}^{\\textrm{marginalizedLASSO}} &= \\arg\\min_{\\beta} \\left\\{\\|y - X\\beta\\|_2^2 + \\lambda \\sum_{j} C^{jj} |\\beta_j|\\right\\} \\
                & \\text{where } C^{jj} \\text{ denotes the } j\\text{th diagonal of } \\sigma^2(X^\\top X)^{-1}
                \\end{align*}
                $$",  # Update with the correct formula
      reference = "Saleh, A. K. Md. E. S., Arashi, M., Saleh, R., (2022). Rank-Based Methods for Shrinkage and Selection: With Application to Machine Learning, Wiley"
    )
  }else if (estimator == "ElasticNet") {
    list(
      author = "Zou, Hui and Hastie, Trevor",
      photos = c(
        "http://users.stat.umn.edu/~zouxx019/myphotos/zou-department-photo.jpg",
        "https://statistics.stanford.edu/sites/statistics/files/styles/hs_medium_square_360x360/public/media/people/hastie_new.jpg?h=07d0026a&itok=6KSgB3EY"
      ),
      formula = "$$\\hat{\\beta}^{\\textrm{elastic net}} = \\arg\\min_{\\beta} \\left\\{\\|y - X\\beta\\|^2 + \\lambda \\left( \\alpha \\|\\beta\\|_1 + (1 - \\alpha) \\|\\beta\\|_2^2\\right)\\right\\}$$",
      reference = "Zou, H., & Hastie, T. (2005). Regularization and Variable Selection via the Elastic Net. Journal of the Royal Statistical Society. Series B (Statistical Methodology), 67(2), 301–320. http://www.jstor.org/stable/3647580"
    )
  }
}