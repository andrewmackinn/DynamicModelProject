#pt. 2 of final project
rm(list = ls())
library(ggplot2)
library(tidyr)

ddSim=function(t,y,p){
  H = y[1]
  P = y[2]
  b=p[1]
  a=p[2]
  w=p[3]
  d=p[4]
  e=p[5]
  s=p[6]
  
  dHdt=(b*H)*(1-a*H)-(w*P)*(H/(d+H))
  dPdt=(e*w*P)*(H/(d+H))-s*P
  
  return(list(c(dHdt,dPdt)))
}
# pt.2a Specific given initial parameters
params = c(.8,.001,5,400,.07,.2)
NO = c(500,120)
times = seq(0,200, by = .1)
modelSim = ode(y=NO, times = times, func = ddSim, parms = params)
out= data.frame(data=modelSim, time = modelSim[,1], prey=modelSim[,2], pred=modelSim[,3])
out %>%
  gather(key,value, prey, pred) %>%
  ggplot(aes(x=time, y=value, color=key)) + geom_line()
# modeled with changing parameters
parameters2 = data.frame("b"=c(.8,1.6,.4,.8,.8,.8,.8,.8,.8,.8,.8,.8,.8), 
                         "a"=c(.001,.001,.001,.002,.0005,.001,.001,.001,.001,.001,.001,.001,.001),
                         "w"=c(5,5,5,5,5,10,2.5,5,5,5,5,5,5),
                         "d"=c(400,400,400,400,400,400,400,800,200,400,400,400,400),
                         "e"=c(.07,.07,.07,.07,.07,.07,.07,.07,.07,.14,.035,.07,.07),
                         "s"=c(.2,.2,.2,.2,.2,.2,.2,.2,.2,.2,.2,.4,.1))
modelSimList=list()
for(i in 1:nrow(parameters2)){
  params = unlist(parameters2[i,])
  modelSim2 = ode(y=NO, times = times, func = ddSim, parms = params)
  modelSimList[[i]] = data.frame(time = modelSim2[,1], prey=modelSim2[,2], pred=modelSim2[,3])
}
for(j in 1:length(modelSimList)){
  plot <- modelSimList[[j]] %>%
    gather(key,value, prey, pred) %>%
    ggplot(aes(x=time, y=value, color=key)) + geom_line()
  print(plot)
}
plot <- modelSimList[[1]] %>%
  gather(key,value, prey, pred) %>%
  ggplot(aes(x=time, y=value, color=key)) + geom_line() +ggtitle("Base")
print(plot)

plot <- modelSimList[[2]] %>%
  gather(key,value, prey, pred) %>%
  ggplot(aes(x=time, y=value, color=key)) + geom_line() +ggtitle("HighB")
print(plot)

plot <- modelSimList[[3]] %>%
  gather(key,value, prey, pred) %>%
  ggplot(aes(x=time, y=value, color=key)) + geom_line() +ggtitle("LowB")
print(plot)

plot <- modelSimList[[4]] %>%
  gather(key,value, prey, pred) %>%
  ggplot(aes(x=time, y=value, color=key)) + geom_line() +ggtitle("HighA")
print(plot)

plot <- modelSimList[[5]] %>%
  gather(key,value, prey, pred) %>%
  ggplot(aes(x=time, y=value, color=key)) + geom_line() +ggtitle("LowA")
print(plot)

plot <- modelSimList[[6]] %>%
  gather(key,value, prey, pred) %>%
  ggplot(aes(x=time, y=value, color=key)) + geom_line() +ggtitle("HighW")
print(plot)

plot <- modelSimList[[7]] %>%
  gather(key,value, prey, pred) %>%
  ggplot(aes(x=time, y=value, color=key)) + geom_line() +ggtitle("LowW")
print(plot)

plot <- modelSimList[[8]] %>%
  gather(key,value, prey, pred) %>%
  ggplot(aes(x=time, y=value, color=key)) + geom_line() +ggtitle("HighD")
print(plot)

plot <- modelSimList[[9]] %>%
  gather(key,value, prey, pred) %>%
  ggplot(aes(x=time, y=value, color=key)) + geom_line() +ggtitle("LowD")
print(plot)

plot <- modelSimList[[10]] %>%
  gather(key,value, prey, pred) %>%
  ggplot(aes(x=time, y=value, color=key)) + geom_line() +ggtitle("HighE")
print(plot)

plot <- modelSimList[[11]] %>%
  gather(key,value, prey, pred) %>%
  ggplot(aes(x=time, y=value, color=key)) + geom_line() +ggtitle("LowE")
print(plot)

plot <- modelSimList[[12]] %>%
  gather(key,value, prey, pred) %>%
  ggplot(aes(x=time, y=value, color=key)) + geom_line() +ggtitle("HighS")
print(plot)

plot <- modelSimList[[13]] %>%
  gather(key,value, prey, pred) %>%
  ggplot(aes(x=time, y=value, color=key)) + geom_line() +ggtitle("LowS")
print(plot)