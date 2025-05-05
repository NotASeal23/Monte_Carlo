#pi estimation using monte carlo method
library(tidyverse)
library(ggplot2)
library(gridExtra)

xk = seq(0,1,0.00001) #circle
yk =sqrt(1 - (xk)^2)
kolo = data.frame(x=xk,y=yk)



#function calculate pi
calc_pi =  function(n){
x = runif(n)
y = runif(n)
punkty = data.frame(x=x,y=y)
punkty = punkty |> 
  mutate(colored = (y <= sqrt(1-(x)^2)))
#g=ggplot(punkty,aes(x,y,color = as.factor(colored)))+geom_point(show.legend = FALSE)+scale_colour_manual(values = c("orange","green"))+xlim(0,1)+ylim(0,1)+geom_step(data= kolo,color='red',linewidth=1.2)

#counting points
N = 0
p = 0
for (i in 1:n){
  N = N+1 
  if(y[i] <= sqrt(1-(x[i])^2))
    p = p+1
}

Pi = 4 * p/N
return(Pi)

}

#plots

#1 - convergence towards 3.14

a = 1:1000
b = 1:1000

for (i in a)
{
  b[i] = calc_pi(100*i) # multiplying by 100 to get higher numbers, 100:100 000 with step 100 instead of 1:1000
}
df = data.frame(no_gen_points=a,pi_est=b)


ggplot(df,aes(no_gen_points,pi_est,color="red"))+
  geom_point(alpha=0.7)+geom_abline(slope = 0,intercept = 3.14)+ 
  scale_y_continuous(breaks=c(3.08,3.14,3.2))+ 
  scale_x_continuous(breaks=c(0,100,200,300,400,500,600,700,800,900,1000))+
  xlab("no_gen_points (*10^2)") #number of generated points


#2 - boxplot
#grouping 0-10 000, 10 000-20 000..
df$grupa <- cut(df$no_gen_points, 
                  breaks = c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000), 
                  labels = c("<1", "1-2", "2-3", "3-4", 
                             "4-5", "5-6", "6-7", 
                             "7-8", "8-9", "9-10"),
                  include.lowest = TRUE)

ggplot(df,aes(grupa,pi_est))+
  geom_boxplot()+
  scale_x_discrete()+
  xlab("no_gen_points (*10^4)") #number of generated points

#3 generated points

n = 100000
x = runif(n)
y = runif(n)
punkty = data.frame(x=x,y=y)
punkty = punkty |> 
  mutate(colored = (y <= sqrt(1-(x)^2)))
g1 = ggplot(punkty[0:100,],aes(x,y,color = as.factor(colored)))+geom_point(show.legend = FALSE)+scale_colour_manual(values = c("orange","green"))+xlim(0,1)+ylim(0,1)+geom_step(data= kolo,color='red',linewidth=1.2)+ggtitle(paste("n=10^2   ", calc_pi(100)))
g2 = ggplot(punkty[0:1000,],aes(x,y,color = as.factor(colored)))+geom_point(show.legend = FALSE)+scale_colour_manual(values = c("orange","green"))+xlim(0,1)+ylim(0,1)+geom_step(data= kolo,color='red',linewidth=1.2)+ggtitle(paste("n=10^3   ", calc_pi(1000)))
g3 = ggplot(punkty[0:10000,],aes(x,y,color = as.factor(colored)))+geom_point(show.legend = FALSE)+scale_colour_manual(values = c("orange","green"))+xlim(0,1)+ylim(0,1)+geom_step(data= kolo,color='red',linewidth=1.2)+ggtitle(paste("n=10^4   ", calc_pi(10000)))
g4 = ggplot(punkty,aes(x,y,color = as.factor(colored)))+geom_point(show.legend = FALSE)+scale_colour_manual(values = c("orange","green"))+xlim(0,1)+ylim(0,1)+geom_step(data= kolo,color='red',linewidth=1.2)+ggtitle(paste("n=10^5   ", calc_pi(100000)))

grid.arrange(g1, g2, g3, g4, nrow = 2, ncol = 2) # plotting multiple plots

