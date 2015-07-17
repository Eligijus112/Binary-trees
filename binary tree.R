library(dplyr)
library(animation)

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
    while(i <= m){
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

search.tree <- function(raw.tree, key){
  compare  <- raw.tree[1]
  i     <- 1
  depth <- get.depth(raw.tree)
  
  while(i <= 2^depth){
    if(is.na(compare)){ 
      cat(key, "is not in the tree\n")
      break
    }
    
    if(key==compare){
      node <- i
      break
    }else{ 
       if(key < compare && !is.na(compare)){
         compare <- raw.tree[2*i]
       }else{
         compare <- raw.tree[2*i + 1]
       }
       if(!is.na(compare)) i <- which(raw.tree==compare)
  }
  
 }
 
 if(!is.na(compare)) return(node)
}


insert.element <- function(raw.tree, new.element){
  compare  <- raw.tree[1]
  i     <- 1
  depth <- get.depth(raw.tree)
  left.value  <- 0
  right.value <- 0
  
  while(i <= 2^depth){
      if((new.element < compare) && !is.na(compare)){ 
        compare     <- raw.tree[2*i]
        left.value  <- 1
        right.value <- 0
      }else{
        compare     <- raw.tree[2*i + 1]
        left.value  <- 0
        right.value <- 1
      }
      if(is.na(compare)){
        if(left.value==1)  raw.tree[2*i]     <- new.element
        if(right.value==1) raw.tree[2*i + 1] <- new.element
        break
      }
      i <- which(raw.tree==compare)
  }
  draw.tree(raw.tree)
  return(raw.tree)
}

animate.tree <- function(elements){
  n    <- length(elements)
  new.tree <- raw.bin.tree(elements[1:2])
  for(i in 4:n){
     new.tree <- insert.element(new.tree, elements[i])
    }
}

delete.element <- function(raw.tree, element){
  
  old.tree <- raw.tree
  n <- get.depth(raw.tree)
  data <- raw.tree[!is.na(raw.tree)]
  arrow.pointer <- matrix(ncol=3, nrow=length(data)) %>% as.data.frame()
  colnames(arrow.pointer) <- c("Number", "Left", "Right")
  arrow.pointer[, 1] <- data
  
  for(k in 1:length(data)){
    index <- which(raw.tree==data[k])
    if(!is.na(raw.tree[2*index])) arrow.pointer[arrow.pointer[, "Number"]==data[k], "Left"] <- 1
    if(!is.na(raw.tree[2*index + 1])) arrow.pointer[arrow.pointer[, "Number"]==data[k], "Right"] <- 1
  }
  
  tmp <- raw.tree[1]
  i   <- 1
  
  while(i <= 2^n){
    if(element==tmp){
      
      if(is.na(arrow.pointer[arrow.pointer[, "Number"]==element, "Left"]) && 
           is.na(arrow.pointer[arrow.pointer[, "Number"]==element, "Right"])){
        raw.tree[j] <- NA
        break  
      }
      
      j <- i
      k <- i
      right.element <- 8888
      left.element  <- 8888
      while(j <= 2^n && k <=2^n){
        
        
        if(!is.na(raw.tree[2*(k)])){
          raw.tree[k] <- raw.tree[2*(k+1)]
          left.element  <- raw.tree[2*k]
        }
        
        if(!is.na(raw.tree[2*(j) + 1])){
          raw.tree[j] <- raw.tree[2*(j) + 1]
          right.element <- raw.tree[2*j + 1]
        }
        
        j <- which(old.tree==right.element)
        k <- which(old.tree==left.element)
      }
    }
    
    if(element < tmp){ 
      tmp <- raw.tree[2*i]
      i   <- which(raw.tree==tmp)
    }
    if(element > tmp){ 
      tmp <- raw.tree[2*i + 1]
      i   <- which(raw.tree==tmp)
    }
  }   
}
###############
# Realization # 
###############

duom <- read.table(file="data.txt", header=F)[, 1] %>% as.numeric()
raw.tree <- raw.bin.tree(duom)
draw.tree(raw.tree)
get.depth(raw.tree)
insert.element(raw.tree, 999)
search.tree(raw.tree, 21)

path.to.convert <- paste0(shortPathName(
  "C:\\Program Files (x86)\\ImageMagick-6.9.0-Q16\\"), "convert.exe")
ani.options(convert=path.to.convert)

numbers <- sample(1:20)
saveGIF({
  ani.options(nmax = 40)
  animate.tree(numbers)
  }, interval = 2, movie.name = "bin tree.gif", ani.width = 500, ani.height = 500)
  
  
