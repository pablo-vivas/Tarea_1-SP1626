# Preguntas 8-13 ----------------------------------------------------------

library(tidyverse)
library(invgamma)

#Funciones
funciones <- c("dgamma", "dinvgamma","dbeta","dnorm","dbinom","dpois") 

#Dominios
d_g <- seq(1,16,length.out = 100)
d_gi <- seq(0,3,length.out = length(d_g))
d_be <- seq(0,1,length.out = length(d_g))
d_n <- seq(-6,6,length.out = length(d_g))
d_b = d_p =  seq (0, 25, 1)

#Escenarios
ec <-data.frame(
  g1 = c(1,1), g2 = c(1,3), g3 = c(2,4), g4 = c(5,2), g5 = c(7,1),
  gi1 = c(1,1), gi2 = c(1,3), gi3 = c(2,0.5), gi4 = c(2,1), gi5 = c(3,0.5),
  be1 = c(0.5,0.5), be2 = c(1,3), be3 = c(2,2), be4 = c(2,5), be5 = c(5,1),
  n1 = c(0,1), n2 = c(0,3), n3 = c(1,4), n4 = c(-2,2), n5 = c(1,1),
  b1 = c(15, 0.5), b2 = c(15, 0.8), b3 = c(25, 0.5), b4 = c(25, 0.2), b5 = c(25, 0.7),
  p1 = c(5), p2 = c(10), p3 = c(15), p4 = c(20), p5 = c(25)
)
  
#Parámetros
parametros <- list(
  ##Gamma
  list(d_g,ec[1,1],1/ec[2,1]),list(d_g,ec[1,2],1/ec[2,2]),
  list(d_g,ec[1,3],1/ec[2,3]),list(d_g,ec[1,4],1/ec[2,4]),
  list(d_g,ec[1,5],1/ec[2,5]),
  ##Gamma Inversa
  list(d_gi,ec[1,6],1/ec[2,6]),list(d_gi,ec[1,7],1/ec[2,7]),
  list(d_gi,ec[1,8],1/ec[2,8]),list(d_gi,ec[1,9],1/ec[2,9]),
  list(d_gi,ec[1,10],1/ec[2,10]),
  ##Beta
  list(d_be,ec[1,11],ec[2,11]),list(d_be,ec[1,12],ec[2,12]),
  list(d_be,ec[1,13],ec[2,13]),list(d_be,ec[1,14],ec[2,14]),
  list(d_be,ec[1,15],ec[2,15]),
  ##Normal
  list(d_n,ec[1,16],ec[2,16]),list(d_n,ec[1,17],ec[2,17]),
  list(d_n,ec[1,18],ec[2,18]),list(d_n,ec[1,19],ec[2,19]),
  list(d_n,ec[1,20],ec[2,20]),
  ##Binomial
  list(d_b,ec[1,21],ec[2,21]),list(d_b,ec[1,22],ec[2,22]),
  list(d_b,ec[1,23],ec[2,23]),list(d_b,ec[1,24],ec[2,24]),
  list(d_b,ec[1,25],ec[2,25]),
  ##Poisson
  list(d_p,ec[1,26]),list(d_p,ec[1,27]),list(d_p,ec[1,28]),
  list(d_p,ec[1,29]),list(d_p,ec[1,30])
)

resultado <- invoke_map(rep(funciones, each = 5), parametros)

##tabla
t_res <- tibble(
  y = unlist(resultado),
  x = c(rep(d_g,5),rep(d_gi,5),rep(d_be,5),rep(d_n,5),rep(d_b,5),rep(d_p,5)),
  f = factor(c(rep(c("gamma","gamma_inv","beta","normal"),each=length(d_g)*5),
        rep(c("binomial","poisson"),each = length(d_p)*5))),
  p = factor(c(rep(c(1:20),each=length(d_g)),rep(c(21:30),each = length(d_p))))
)

#rm(funciones, parametros, d_g,d_gi,d_n,d_b,d_p, resultado)

##Gamma
t_res %>% filter(f =="gamma") %>% 
  select(x,y,p) %>% 
  ggplot() +
  geom_line(aes(x = x, y = y , group = p , color = p),size = 0.9) +
  labs(x = "" , y = "") +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0),limits = c(0,0.2)) +
  scale_color_manual(name="Parámetros",
                     values = c("#e8615d","#f49436","#2d9de5","#3bbdbd","#634792"),
                     labels = str_c("\u03B1","=", ec[1,1:5], " , ","\u03b2","=", ec[2,1:5])) +
  ggsave("p_8.png")

##Gamma_Inv
t_res %>% filter(f =="gamma_inv") %>% 
  select(x,y,p) %>% 
  ggplot() +
  geom_line(aes(x = x, y = y , group = p , color = p),size = 0.9) +
  labs(x = "" , y = "") +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0),limits = c(0,2)) + 
  scale_y_continuous(expand = c(0, 0),limits = c(0,1.65)) +
  scale_color_manual(name="Parámetros",
                     values = c("#e8615d","#f49436","#2d9de5","#3bbdbd","#634792"),
                     labels = str_c("\u03B1","=", ec[1,6:10], " , ","\u03b2","=", ec[2,6:10])) +
  ggsave("p_9.png")

##Beta
t_res %>% filter(f =="beta") %>% 
  select(x,y,p) %>% 
  ggplot() +
  geom_line(aes(x = x, y = y , group = p , color = p),size = 0.9) +
  labs(x = "" , y = "") +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0),limits = c(0,4)) +
  scale_color_manual(name="Parámetros",
                     values = c("#e8615d","#f49436","#2d9de5","#3bbdbd","#634792"),
                     labels = str_c("\u03B1","=", ec[1,11:15], " , ","\u03b2","=", ec[2,11:5])) +
  ggsave("p_10.png")

##Normal
t_res %>% filter(f =="normal") %>% 
  select(x,y,p) %>% 
  ggplot() +
  geom_line(aes(x = x, y = y , group = p , color = p),size = 0.9) +
  labs(x = "" , y = "") +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0),limits = c(0,0.41)) +
  scale_color_manual(name="Parámetros",
                     values = c("#e8615d","#f49436","#2d9de5","#3bbdbd","#634792"),
                     labels = str_c("\u03bc","=", ec[1,16:20], " , ","\u03c3","=", ec[2,16:20])) +
  ggsave("p_13.png")

##Binomial
t_res %>% filter(f =="binomial") %>% 
  select(x,y,p) %>% 
  ggplot(aes(x,y)) +
  geom_point(aes(colour = p)) +
  geom_line(aes(colour = p),size = 0.9) +
  labs(x = "" , y = "") +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0),limits = c(0,0.255)) +
  scale_color_manual(name="Parámetros",
                     values = c("#e8615d","#f49436","#2d9de5","#3bbdbd","#634792"),
                     labels = str_c("n","=", ec[1,21:25], " , ","p","=", ec[2,21:25])) +
  ggsave("p_11.png")

##Poisson
t_res %>% filter(f =="poisson") %>% 
  select(x,y,p) %>% 
  ggplot(aes(x,y)) +
  geom_point(aes(colour = p)) +
  geom_line(aes(colour = p),size = 0.9) +
  labs(x = "" , y = "") +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0),) +
  scale_color_manual(name="Parámetros",
                     values = c("#e8615d","#f49436","#2d9de5","#3bbdbd","#634792"),
                     labels = str_c("\u03bb","=", ec[1,26:30])) +
  ggsave("p_12.png")



# Pregunta 14 -------------------------------------------------------------
set.seed(6827)

x1 <- rbinom(300,1,0.37)
sum(x1)

# Pregunta 15 -------------------------------------------------------------

x2 <- rpois(39,14)
sum(x2)

# Pregunta 16 -------------------------------------------------------------

gen_nor <- function(mu){
  sig <- sample(c(1:5),1)
  x <- rnorm(1,mu,sqrt(sig))
  return(x)
}


x_a <- NULL
for(i in 1:112) x_a[i] <- gen_nor(0.8) 

x3 <- tibble(
  x = 1:112,
  y = x_a
)

ggplot(x3) + 
  geom_line(aes(x,y),size = 0.9) +
  geom_point(aes(x,y)) +
  labs(x = "" , y = "") +
  theme_bw() +
  ggsave("p_16.png")
