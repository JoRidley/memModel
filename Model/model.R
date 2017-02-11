
n.images <- 100
n.chosen.images <- 5
chosen.images <- 1:n.chosen.images

int.act.chosen <- 1
int.act.seen <- 0.7
decay.rate.A <- 0.03
decay.rate.B <- 0.05

# list of memory times by hr
memory.time <- c(3, 12, 24, 168)


run.trial.variable.decay <- function(mem.length, decay.rate.A, decay.rate.B, int.act.chosen, int.act.seen){
  image.list <- data.frame(n = 1:n.images, 
                           int.act =  c(rep(int.act.chosen,n.chosen.images), 
                                        rep(int.act.seen,n.images-n.chosen.images)), 
                           chosen = c(rep(T,n.chosen.images), 
                                      rep(F,n.images-n.chosen.images)))
  
  # step through each image and give it an int.act val
  int <- function(image.list) {
    i <- 1
    #chosen.images <- sort(chosen.images)
    for(i in 1:n.images){
      
      if(is.element(image.list[i,]$n, chosen.images)){
        image.list[i,]$int.act <- int.act.chosen
      } 
      else {
        image.list[i,]$int.act <- int.act.seen
      }
    }
    return(image.list)
  }
  
  
  # Decay A all images that were choosen
  image.list <- int(image.list)
  image.list$int.act <- mapply(function(image.act, image.num) {
    if(is.element(image.num, chosen.images)){
      return(image.act * (1-decay.rate.A) ^ mem.length)
      # Decay B all images not choosen
    } else {
      return(image.act * (1-decay.rate.B) ^ mem.length)
    }
  }, image.list$int.act, image.list$n)
  
  # images remembered 

  return(image.list)
}

test.parameters <- function(decay.rate.A, decay.rate.B, int.act.chosen, int.act.seen){
  fit.data <- expand.grid(mem.length=memory.time, chosen=c(T,F))
  fit.data$act <- mapply(function(mem.length, ch){
    output <- run.trial.variable.decay(mem.length, decay.rate.A, decay.rate.B, int.act.chosen, int.act.seen)
    mean.activation <- mean(subset(output, chosen==ch)$int.act)
    return(mean.activation)
  }, fit.data$mem.length, fit.data$chosen)
  return(fit.data)
}

fit.data <- test.parameters(decay.rate.A, decay.rate.B, int.act.chosen, int.act.seen)
library(ggplot2)
ggplot(fit.data, aes(x=mem.length, y=act, colour=chosen)) + geom_line()


