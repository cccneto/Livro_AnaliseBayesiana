# Capitulo 1 - Introdução

# 1.2 Estudo da maturidade sexual do peixe-galo

# base <- data.frame(x, n, y, yn, xc, ny)
x <- c(12.5, 22.5, 32.5, 62.5)  # ponto médio das classes
n <- c(3, 5, 4, 5)  # numero de fêmeas
y <- c(0, 1, 3, 5)  # numero de fêmeas maturas
yn <- x / n
xc <- mean(x)

# vamos ver a relacao entre a proporção de maturos e o comprimento. Ficará evidente que
# a proporção de femeas maturas tem una relação positiva com os comprimentos.

fig1 <-
  plot(
    x,
    y / n,
    xlab = "Comprimento dos peixes (cm)",
    ylab = "Proporção de Maturos",
    xlim = c(10, 70),
    ylim = c(0, 1),
    pch = 16
  )

library(ggplot2)
# figura 1
fig1 <- ggplot() + aes(x = x, y = y / n) +
  geom_point() + theme_classic() +
  labs(x = "Comprimento dos peixes (cm)", y = "Proporção de Maturos")
coord_cartesian(xlim = c(10, 70))

# Centralizando os dados de comprimento - criando a variavel media de x
xc <- x-mean(x)

# construindo a tabela com os dados de interesse que armazenamos em objeto denominado "galo"
galo <- cbind(xc, n, y)

# Criar objeto contendo o numero de femeas maturas (y)e femeas nao maturas (n-y)
resposta <- cbind(y, n-y)

# Obtendo os estimadores de maxima verossimilhança dos betas 0 e 1 via regressão logistica
galo.glm <- glm(resposta ~ xc, family = binomial(link = "logit"))

# visualizando regressao
summary(galo.glm) # as estimativas de maxima verossimilhança foram beta0= 1.163 e beta1= 0.269
                  # Nenhum desses valores é considerado estatisticamente distinto de zero em um teste de 
                  # de hipotese convencional (ex. alpha = 0.05)

# Um Beta1 = 0 indica desvinculação entre maturidade sexual e comprimento
# Um Beta0 = 0 indicaria que p = 0.05 quando x = 32.5 cm. Logo LT-50 = 32.5

# Calculando o *intervalo de confiança*  de acordo estatistica convencional - hipotese de distribuição normal
# assintotica 

beta0 <- coef(galo.glm)[1]
beta1 <- coef(galo.glm)[2]  # coeficientes apenas do parametro beta1
se.beta1 <- sqrt(vcov(galo.glm)[2,2]) # calculando o desvio padrão de beta1
ci <- c(beta1 + qnorm(0.025)*se.beta1, beta1 + qnorm(0.975)*se.beta1) # vemos que o CI inclui o zero.

# como LT-50 depende dos dois parametros desconhecidos, o seu valor estimado pode ser obtido, como:
LT50 <- -(beta0/beta1) + mean(x) # 28.14

#reproduzindo exemplo da pagina 7
beta.zero <- c(1.52155, 1.49447, 3.10884)
beta.um <- c(0.447795, 0.382833, 0.392359)

# agrupando dois vetores
ambeta <- cbind(beta.zero, beta.um)

plot(density(ambeta[,2]), xlab = expression(beta[1]), ylab = expression(p(paste(beta[1], "|", dados))), main = "")

hist(amostra, main = "")



