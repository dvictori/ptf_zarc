# Validação cruzada M1 e M2
# Nesse script é feita a validação cruzada dos modelos M1 e M2
# Daniel de Castro Victoria
# daniel.victoria@embrapa.br
# 1/set/2020

library(MASS)

#### Carregando dados ####

# Lendo os dados no formato csv, separado por ponto e vírgula (;)
# Usar o mesmo formato quando for salvar
dados <- read.csv2("dados/bd_zarc.csv")

#### Modelo 1: AD (sem transformação) usando frações AT, SIL, ARG) ####
# Modelo poderia ser escrito como AD ~ -1 + AT*SIL*ARG
# Escrevendo de forma explícita para facilitar interpretação

## Validação cruzada com 20 subconjuntos

grupo <- rep(1:20, length.out = nrow(dados))

ajuste <- data.frame(rodada = numeric(),
                     r2 = numeric(),
                     r2_ad = numeric(),
                     rmse = numeric())

AD_valid_m1 <- rep(-99, nrow(dados))

for (i in 1:20) {
  valid <- dados[grupo == i,]
  calib <- dados[grupo != i,]
  
  m1 <- lm(AD ~ -1 + AT + SIL + ARG +
             I(AT*SIL) + I(AT*ARG) + I(SIL*ARG) + I(AT*SIL*ARG), data=calib)
  
  # armazena apenas dados preditos p/ o subgrupo
  valid$AD_ptf <- predict(m1, newdata = valid)
  
  valid_lm <- lm(AD ~ AD_ptf, data = valid)
  ajuste <- rbind(ajuste, 
                  data.frame(rodada = i,
                             r2 = summary(valid_lm)$r.squared,
                             r2_ad = summary(valid_lm)$adj.r.squared,
                             rmse = sqrt(mean((valid$AD - valid$AD_ptf)^2))
                  )
  )
  
  # salva dados do subgrupo para juntar com as outras iterações
  AD_valid_m1[grupo == i] <- valid$AD_ptf
  
}

# RMSE médio das 20 iterações da validação cruzada é 0.358 mm cm-1
ajuste
mean(ajuste$rmse)

# RMSE calculado apenas para os dados independentes
# 0.362 mm cm-1 ~ 0.0362 cm3 cm-3
sqrt(mean((dados$AD - AD_valid_m1)^2))

plot(dados$AD * 10, AD_valid_m1 * 10,
     xlab = 'AD observada (mm cm⁻¹)', ylab = 'AD estimada, M1 (mm cm⁻¹)')
abline(a = 0, b = 1, col = 'red')

#### Modelo 2 - ADT transformação Box-Cox (sem intercepto) ####
# Resíduos do M1 apresentam tendência. Usar transformação Box-Cox

# Transformação Box - Cox
# Transformaçao dos dados (normalidade, homocedasticidade e aditividade) - BOX-COX
# Determinação do fator lambda

bc <- boxcox(AD ~ -1 + AT + SIL + ARG +
               I(AT*SIL) + I(AT*ARG) + I(SIL*ARG) +
               I(AT*SIL*ARG), data = dados, lambda=(2000:5000)/10000)
lambda <- bc$x[which(bc$y==max(bc$y))]

# Transformando os dados utilizando o lambda - ADt
ADt <- (dados$AD ^ lambda-1) / lambda

## Validação cruzada para avaliar o modelo
# Usando o mesmo lambda para cada iteração da validação cruzada

grupo <- rep(1:20, length.out = nrow(dados))

ajuste <- data.frame(rodada = numeric(),
                     r2 = numeric(),
                     r2_ad = numeric(),
                     rmse = numeric())

AD_valid_m2 <- rep(-99, nrow(dados))

for (i in 1:20) {
  valid <- dados[grupo == i,]
  ADt_valid <- ADt[grupo == i]
  calib <- dados[grupo != i,]
  ADt_calib <- ADt[grupo != i]
  
  m2 <- lm(ADt_calib ~ -1 + AT + SIL + ARG +
             I(AT*SIL) + I(AT*ARG) + I(SIL*ARG) + I(AT*SIL*ARG), data=calib)
  
  valid$AD_ptf_bc <- predict(m2, newdata = valid)
  
  # destransformando Box-Cox
  valid$AD_ptf <- (lambda * valid$AD_ptf_bc + 1) ^(1 / lambda)
  
  valid_lm <- lm(AD ~ AD_ptf, data = valid)
  ajuste <- rbind(ajuste, 
                  data.frame(rodada = i,
                             r2 = summary(valid_lm)$r.squared,
                             r2_ad = summary(valid_lm)$adj.r.squared,
                             rmse = sqrt(mean((valid$AD - valid$AD_ptf)^2))
                  )
  )
  
  AD_valid_m2[grupo == i] <- valid$AD_ptf
  
}

# RMSE médio da validação é 0.362
ajuste
mean(ajuste$rmse)

# RMSE calculado apenas para os dados independentes
# 0.366 mm cm-1 ~ 0.0366 cm3 cm3
sqrt(mean((dados$AD - AD_valid_m2)^2))

plot(dados$AD * 10, AD_valid_m2 * 10,
     xlab = 'AD observada (mm cm⁻¹)', ylab = 'AD estimada, M2 (mm cm⁻¹)')
abline(a = 0, b = 1, col = 'red')
