library(dplyr)
library(animation)

duom <- read.table(file="data.txt", header=F)[, 1] %>% as.numeric()

raw.bin.tree <- function(data){
### m <- the size of the array. If we want a all of our data to be stored in the tree, 
###      m should usually be a "big" number. The maximum number should not exceed 2^n, where 
###      n is the size of the data set 
  n  <- length(data)
  m  <- 2^n 
  id <- seq(from=1, by=1, length.out=m)
  array <- matrix(ncol=2, nrow=m) %>% as.data.frame()
  array[, 1] <- id
  intervals  <- 1 
  
  for(j in 1:n){
    tmp.nr <- data[j]
    i <- 1
    while(i < m){
      if(is.na(array[i, 2])){
        array[i ,2] <- tmp.nr
        i <- m
      }else{
          if((!is.na(array[i, 2])) && array[i, 2] < tmp.nr)   i <- 2*i + 1 
          if((!is.na(array[i, 2])) && array[i, 2] >= tmp.nr)  i <- 2*i
          if(max(intervals) <= 2*i + 1) intervals <- c(2*i, 2*i + 1)
      }
  }
  }
  
  x <- max(intervals) %>% log(base=2) %>% round(0)
  x <- x - 1
  index <- 2^x - 1
  # cat(array[, 2])
  array <- array[-((index+1):m), ]
  return(array[, 2])
}

raw.tree <- raw.bin.tree(duom)

get.depth <- function(raw.tree){
  m     <- length(raw.tree)
  i     <- 1
  left  <- 1
  right <- 1
  while(i < m){ 
    if((!is.na(raw.tree[2^i]) || is.na(raw.tree[2^i])) && 2^i <= m ) left  <- left + 1
    if((!is.na(raw.tree[2^(i+1) - 1]) || is.na(raw.tree[2^(i+1) - 1])) && (2^(i+1) - 1) <= m) right <- right + 1 
    i <- i + 1 
  }
  depth <- max(left, right) 
  return(depth)
}

get.depth(raw.tree)

draw.tree <- function(raw.tree){
  scale <- get.depth(raw.tree)
  data  <- raw.tree[!is.na(raw.tree)]
  plot(NULL, xlim=c(0, 2^scale), ylim=c(1, scale + 1), ylab="y lable", xlab="x lable")
  abline(h=1:scale)
  x.param <- 0:2^scale
  y.param <- 0:(scale +1)
  x.start <- median(x.param)
  points(x = x.start, y = scale + 0.5, col = "blue", pch = 1, cex = 4)
  text(x.start, scale + 0.5, raw.tree[1], cex=1.2)
  
  arrow.pointer <- matrix(ncol=3, nrow=length(data)) %>% as.data.frame()
  colnames(arrow.pointer) <- c("Number", "Left", "Right")
  arrow.pointer[, 1] <- data
  
  for(k in 1:length(data)){
    index <- which(raw.tree==data[k])
    if(!is.na(raw.tree[2*index])) arrow.pointer[arrow.pointer[, "Number"]==data[k], "Left"] <- 1
    if(!is.na(raw.tree[2*index + 1])) arrow.pointer[arrow.pointer[, "Number"]==data[k], "Right"] <- 1
    }
   
  tree.values <- raw.tree[2:3]   
  ind.left  <- arrow.pointer[arrow.pointer[, "Number"]==raw.tree[1], "Left"]
  ind.right <- arrow.pointer[arrow.pointer[, "Number"]==raw.tree[1], "Right"]
  
  if(!is.na(ind.left)){
    segments(x0 = median(x.param), y0 = scale + 0.5, x1 = median(x.param)/2, y1 = scale - 0.5, lty=2)
  }
  
  if(!is.na(ind.right)){
    segments(x0 = median(x.param), y0 = scale + 0.5, x1 = 1.5*median(x.param), y1 = scale - 0.5, lty=2)
  }
  
  
  for(h in 1:(scale-1)){
    tree.values  <- raw.tree[(2^h):(2^(h+1) - 1)]
    if(h==1) x.end   <- x.start  + x.start/2
    if(h!=1) x.end   <- x.end    + x.start/2
    x.start <- x.start - x.start/2
    x.coord <- seq(from=x.start, to=x.end, length.out=2^h)
    
    for(j in 1:length(tree.values)){
      if(!is.na(tree.values[j])){ 
        ind.left  <- arrow.pointer[arrow.pointer[, "Number"]==tree.values[j], "Left"]
        ind.right <- arrow.pointer[arrow.pointer[, "Number"]==tree.values[j], "Right"]
        points(x = x.coord[j], scale - h + 0.5, col = "blue", pch = 1, cex = 4)
        
        if(!is.na(ind.left)){
          x.coord1 <- seq(from=x.start - x.start/2, to=x.end + x.start/2, length.out=2^(h+1))
          segments(x0 = x.coord[j], y0 = scale - h + 0.5, x1 = x.coord1[2*j - 1], y1 = scale - h -0.5, lty=2)
        }
      
        if(!is.na(ind.right)){
          x.coord1 <- seq(from=x.start - x.start/2, to=x.end + x.start/2, length.out=2^(h+1))
          segments(x0 = x.coord[j], y0 = scale - h + 0.5, x1 = x.coord1[2*j], y1 = scale - h -0.5, lty=2)
        }
        
          text(x.coord[j] , scale - h + 0.5, tree.values[j], cex = 1.2)
      }
    }
  }
  
}

draw.tree(raw.tree)



#path.to.convert <- paste0(shortPathName(
 # "C:\\Program Files (x86)\\ImageMagick-6.9.0-Q16\\"), "convert.exe")
#ani.options(convert=path.to.convert)

#saveGIF({
 # ani.options(nmax = 40)
  #draw.tree(raw.tree)
  #}, interval = 0.20, movie.name = "tree.gif", ani.width = 700, ani.height = 700)
  
  
