library("ggplot2")

f3 <- function(x) {
  return(sin(x))
}
f4 <- function(x) {
  return(cos(x+pi/2))
}
x=seq(0 , 9, 0.1)
ysen=c()
for(i in 1:length(x)){
  ysen[i]=sin(x[i])
}


seg <- seq.int(0, 9, length.out = 9)
segcos <- seq.int(0, 9, length.out = 9)

fx <- vector(length = length(seg))
fxcos <- vector(length = length(segcos))

for (i in 1:length(seg)) {
  fx[i] <- f3(seg[i])
}
for (i in 1:length(segcos)) {
  fxcos[i] <- f4(segcos[i])
}

df <- data.frame(xend = seg, 
                 y = rep(0, 9), 
                 yend = fx, 
                 yend1 = c(fx[2:9], fx[9]), 
                 xend1 = c(seg[2:9], seg[9]),
                 xendcos = segcos, 
                 ycos = rep(0, 9), 
                 yendcos = fxcos, 
                 yend1cos = c(fxcos[2:9], fxcos[9]), 
                 xend1cos = c(segcos[2:9], segcos[9]))

ggplot(data = df) + 
  stat_function(fun = f3, size = 1.05, alpha = 0.75, color='blue') + 
  stat_function(fun = f4, size = 1.05, alpha = 0.75, color='red') + 
  geom_segment(aes(x=xend, y=y, xend=xend, yend=yend)) + 
  geom_ribbon(aes(x=xend, ymin=y, ymax=yend), fill = 'blue', alpha = 0.3) +
  
  geom_segment(aes(x=xendcos, y=ycos, xend=xendcos, yend=yendcos)) + 
  geom_ribbon(aes(x=xendcos, ymin=ycos, ymax=yendcos), fill = 'blue', alpha = 0.3) +
  xlim(c(0, 9))