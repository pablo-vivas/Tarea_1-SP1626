# 1 -----------------------------------------------------------------------

#2
dom <- seq(0.005,1,length.out = 100) 
p2 <- tibble(
  x = rep(dom,3),
  y = c(dbeta(dom,1,4),dbeta(dom,5,25),dbeta(dom,6,29)),
  t = factor(rep(c("previa","verosimilitud","posterior"),each=100))
)

p <- p2 %>%
      ggplot() +
      geom_line(aes(x = x, y = y,group = t,colour = t),size = 0.9) +
      theme_bw() +
      labs(x = "" , y = "") +
      scale_x_continuous(limits = c(0,0.75)) +
      scale_color_manual(name=" ",
                         values = c("#e8615d","#f49436","#2d9de5"),
                         labels = c("Posterior","Previa","Verosimilitud"))

p + theme(legend.position="bottom", legend.box = "horizontal") +
  ggsave("p_2.png")
# 7 -----------------------------------------------------------------------

#c)

pbeta(0.6,1.5,1.5)

#f)

p7 <- tibble(
  x = rep(dom,3),
  y = c(dbeta(dom,1.5,1.5),dbeta(dom,37,13),dbeta(dom,38.5,14.5)),
  t = factor(rep(c("previa","verosimilitud","posterior"),each=100))
)

p <- p7 %>%
  ggplot() +
  geom_line(aes(x = x, y = y,group = t,colour = t),size = 0.9) +
  theme_bw() +
  labs(x = "" , y = "") +
  scale_color_manual(name=" ",
                     values = c("#e8615d","#f49436","#2d9de5"),
                     labels = c("Posterior","Previa","Verosimilitud"))

p + theme(legend.position="bottom", legend.box = "horizontal") +
  ggsave("p_7.png")

#g)

pbeta(0.6,38.5,15.5)


# 8 -----------------------------------------------------------------------

#c) 
set.seed(5142)
#Lambda = 10
x <- rpois(20,10)
sum(x)
mean(x)
#f)
dom2 <- seq(0,50,length.out = 100) 

p8 <- tibble(
  x = rep(dom2,3),
  y = c(dgamma(dom2,4,0.25),dgamma(dom2,203,20),dgamma(dom2,207,20.25)),
  t = factor(rep(c("previa","verosimilitud","posterior"),each=100))
)

p <- p8 %>%
  ggplot() +
  geom_line(aes(x = x, y = y,group = t,colour = t),size = 0.9) +
  theme_bw() +
  labs(x = "" , y = "") +
  scale_color_manual(name=" ",
                     values = c("#e8615d","#f49436","#2d9de5"),
                     labels = c("Posterior","Previa","Verosimilitud"))

p + theme(legend.position="bottom", legend.box = "horizontal") +
  ggsave("p_8.png")

