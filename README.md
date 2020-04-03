 
## Primcipal component analysis



```{r}
data("iris")
# summary(iris)

X <- as.matrix(iris[,c(1,3)])
  
m <- apply(X,2,mean)

Xc <- scale(X, center = TRUE, scale=FALSE)
 
S <- cov(X)
res <- eigen(S)
lambda <- drop(res$values)
U <- res$vectors

# U%*%diag(lambda)%*%t(U) - S
 
theta <- acos(U[1,1])  

N <- 360
theta_vec <- c(rep(0,N),seq(0,theta,,N),rep(theta,N))

dat1 <- c()
dat2 <- c()

for (i in 1:(3*N)) {
  theta_i <- theta_vec[i]
  
  R <- matrix(c(cos(theta_i),sin(theta_i),-sin(theta_i),cos(theta_i)),2,2)

  Z <- X%*%R

  dat_i <- cbind(Z,theta_i,i,iris$Species)


  dat1 <- rbind(dat1,dat_i)  
  dat2 <- rbind(dat2,c(lambda,theta,frame))
}

colnames(dat1) <- c("pcomp1","pcomp2","theta","frame","class")
colnames(dat2) <- c("lambda1","lambda2","theta","frame")
```


```{r}
library(tidyverse)
library(gganimate)
library(ggforce)

library(lattice)
theme_set(theme_bw())

tib1 <- as.tibble(dat1)
tib2 <- as.tibble(dat2)

recode <- c(setosa = 1, versicolor = 2, virginica=3)

tib1$class <- factor(tib1$class,
                     levels=recode,
                     labels=names(recode))

g1 <- tib1 %>%
  filter(frame==1) %>%
  ggplot(aes(x=pcomp1,y=pcomp2, color=class)) +
  geom_point(size=3) +
  stat_ellipse(aes(x=pcomp1,y=pcomp2),inherit.aes = FALSE) +
  ggtitle("Original Space") +
  xlab("Sepal.Length") +
  ylab("Petal.Length") 

g1
ggsave("orig_plot.png", width = 6, height = 4, dpi=100)

g3 <- tib1 %>%
  filter(frame==(3*N)) %>%
  ggplot(aes(x=pcomp1,y=pcomp2, color=class)) +
  geom_point(size=3) +
  stat_ellipse(aes(x=pcomp1,y=pcomp2),inherit.aes = FALSE) + 
  ggtitle("Principal component space") +
  xlab("PC1") +
  ylab("PC2")

g3
ggsave("pc_plot.png", width = 6, height = 4, dpi=100)
```


<center> 
<img src="./orig_plot.png" height="300"/> 

<img src="./pc_plot.png" height="300"/>

</center>

```{r}
g2 <- tib1 %>%
  ggplot(aes(x=pcomp1,y=pcomp2, group=frame, color=class)) +
  geom_point(size=1) +
  stat_ellipse() + 
  ggtitle("Principal component analysis as a rotation in space") +
  xlab("") +
  ylab("") +
#  geom_ellipse(data=tib2, 
#               aes(x0 = 0, 
#                   y0 = 0, 
#                   a = lambda1, 
#                   b = lambda2, 
#                   angle = theta)) +
  transition_manual(frame)
  
g2 
anim_save("anim1.gif", width = 600, height = 400, end_pause=200)
```


<center> 
<img src="./anim1.gif" height="600" width="800"/>
</center>