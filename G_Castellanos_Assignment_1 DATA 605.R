
  
  ## Quarto
  



install.packages("animation")
library(animation)
x=c(seq(-2,-1,length.out=500), rep(-2,1000), seq(-2,-1,length.out=500), seq(-1,-1,length.out=500), seq(-1.5,-1,length.out=500), rep(1, 1000), seq(1, 2, length.out = 1000), seq(1, 2, length.out = 1000))
y=c(rep(-1,500), seq(-1,1,length.out=1000), rep(1,500), seq(0,-1,length=500), seq(0,0,length.out=500), seq(-1,1, length.out=1000), rep(1,1000), rep(-1,1000))
z=rbind(x,y)
plot(y~x, xlim=c(-3,3), ylim=c(-3,3), col='blue')




y <- diag(nrow = nrow(z))

ident <- diag(2)
ident[1,2] <- .5

z <- ident %*% z


a <- diag(2)


for (i in seq(-2,2, length.out = 100)){
  a[2,1] = i
  newmat=apply(z, 2, function(x) a%*%x)
  plot(newmat[2,] ~ newmat[1,], xlim=c(-3,3), ylim=c(-3,3), col='blue')
  ani.record()  
  
  Sys.sleep(.02)  
}

for (i in seq(-3,3, length.out = 200)){
  a[1,2] = i
  newmat=apply(z, 2, function(x) a%*%x)
  plot(newmat[2,] ~ newmat[1,], xlim=c(-3,3), ylim=c(-3,3), col='blue')
  ani.record()  
  

}




for (i in seq(-2,2, length.out = 200)){
  a[1,1] = i
  newmat=apply(z, 2, function(x) a%*%x)
  plot(newmat[2,] ~ newmat[1,], xlim=c(-3,3), ylim=c(-3,3), col='blue')
  ani.record()  
  
  Sys.sleep(.02)
  
}








for (i in seq(-2,2, length.out = 200)){
  a[2,2] = i
  newmat=apply(z, 2, function(x) a%*%x)
  plot(newmat[2,] ~ newmat[1,], xlim=c(-3,3), ylim=c(-3,3), col='blue')
  ani.record()  
  
  Sys.sleep(.02)
  
}





myf <- function(x) {matrix(c(cos(x), -sin(x), sin(x), cos(x)), byrow = TRUE, nrow = 2)}
for (i in seq(0,20, length.out = 100)){
  a = myf(i)
  newmat=apply(z, 2, function(x) a%*%x)
  plot(newmat[2,] ~ newmat[1,], xlim=c(-3,3), ylim=c(-3,3), col='blue')
  ani.record()  
  Sys.sleep(.02)
  
}

saveGIF(ani.replay(), img.name = 'pp', convert = 'magick', clean = TRUE)


