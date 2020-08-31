######## SCRIPT PARA ANALISES DE AJUSTE DE MODELOS MIXTURE - SIMPLEX
# AJUSTE DE MODELOS PREDITIVOS DE AGUA DISPONÍVEL EM FUNÇÃO DE TEORES DE AREIA, SILTE, ARGILA
# VERSAO 12.1 -
# Autoria: Julio Bueno (UFLA), Wenceslau Teixeira (Embrapa - CNPS),
# Daniel Victora (Embrapa CNPTIA), Alexandre Hugo (Embrapa Solos - UEP)
#
# Atualização em 28/07/2020
# Refatoração em 20 ago 2020 - Daniel de Castro Victoria

# Carregando pacotes que serão utilizados na analise

library(MASS)
library(Ternary)

# Arquivo CSV com os dados estão na pasta dados
# Recomenda-se rodar esse script carregando o projeto do RStudio,
# assim não é necessário arrumar o working directory (setwd)

#### Carregando dados e vendo distribuição ####

# Lendo os dados no formato csv, separado por ponto e vírgula (;)
# Usar o mesmo formato quando for salvar
dados <- read.csv2("dados/bd_zarc.csv")

# convertendo AD de cm3/cm3 para mm/cm
dados$AD <- dados$AD * 10

# Grafico da Função de Densidade de Probabilidade - FDP (Kernel)
# dos dados de AD e demais preditores.
# Apenas a título de exemplo. Graficos usados na publicação
# estão no script estatistica_descritiva_bd_zarc.R

plot(density(dados$AD),
     main = '', xlab = 'Água disponível - AD - [mm cm⁻¹]', ylab = 'FDP Kernel')
abline(h = 0)

# Plontando um grafico de densidade dos dados de AT - verificando a distribuição de dados de AT
plot(density(dados$AT * 100),
     main = '', xlab = 'Areia total - AT - [%] ', ylab = 'FDP Kernel')
abline(h = 0)

# Plontando um grafico de densidade dos dados de SIL - verificando a distribuição de dados de SIL
plot(density(dados$SIL * 100),
     main = '', xlab = 'Silte - SIL - [%]', ylab = 'FDP kernel')
abline(h = 0)

# Plontando um grafico de densidade dos dados de ARG -verificando a distribuição de dados de ARG
plot(density(dados$ARG * 100),
     main = '', xlab = 'Argila - ARG - [%]', ylab = 'Distribuição')
abline(h = 0)

#### Plotando a base de dados no grafico ternario ####

# Criando uma paleta de cores aos valores de AD em 5 classes

paleta <- colorRampPalette(c('red',  'green', 'blue'))
quebras_amostra <- cut(dados$AD,
          breaks = seq(min(dados$AD),
                       max(dados$AD),
                       len = 6),
          include.lowest = TRUE)

cores <- paleta(6)[quebras_amostra]

# Plota o triangulo e adiciona os pontos, de acordo com a paleta

# salvando configurações padrão do gráfico
# para voltar ao normal depois
orig.par <- par(no.readonly = TRUE)

# reduzindo as margens do plot
par(mar = c(1,1,2,1))

TernaryPlot(main = 'Gráfico ternário dos dados de AD, base ZARC',
            alab = 'Argila - [%]', blab = 'Silte - [%]', clab = 'Areia Total - [%]')
AddToTernary(points, dados[c('ARG', 'SIL', 'AT')],
             cex = 0.5, pch = 19, col = cores)
legend('topright',
       title = 'mm cm⁻¹',
       legend = levels(quebras_amostra),
       cex=0.8, bty='n', pch=21, pt.cex=1.8,
       pt.bg = paleta(6))

# Agora plotando no triang. com as cores e novas classes de AD propostas ZARC 2021

## Paleta de cores para as classes de AD do ZARC
cores_zarc <- c('#bdbdbd',
                '#f03b20', '#feb24c', '#ffeda0',
                '#31a354', '#a6bddb', '#2b8cbe',
                '#f0f0f0')

# Função para dividir AD nas classes do ZARC
# espera valores de AD em mm/cm
# Trabalhando com 6 classes de AD
# 6 classe é aberta, ou seja, inclui tudo acima de 1.84
classe_ad <- function(ad) {
  
  # Quebras
  #quebras <- c(0, 0.34, 0.48, 0.67, 0.94, 1.32, 1.84, 2.57, 4)
  quebras <- c(0, 0.34, 0.48, 0.67, 0.94, 1.32, 1.84, 4)
  classes <-c('AD 0',
              'AD 1 (0,40)', 'AD 2 (0,56)', 'AD 3 (0,78)',
              'AD 4 (1,10)', 'AD 5 (1,53)', 'AD 6 (2,14)')
  
  cut(ad, quebras, labels = classes)
  
}

# indice para depois extrair a cor do vetor de cores
classe_dados <- classe_ad(dados$AD)

TernaryPlot(main = 'Gráfico ternário dos dados de AD, base ZARC',
            alab = 'Argila - [%]', blab = 'Silte - [%]', clab = 'Areia Total - [%]')

AddToTernary(points, dados[c('ARG', 'SIL', 'AT')],
             cex = 0.5, pch = 19, col = cores_zarc[classe_dados])

legend('topright',
       title  = 'mm cm⁻¹',
       legend = levels(classe_dados),
       cex=0.8, bty='n', pch=21, pt.cex=1.8,
       pt.bg = cores_zarc,
       ncol = 1)

# voltando configurações da área do gráfico
par(orig.par)

#### Modelo 1: AD (sem transformação) usando frações AT, SIL, ARG) ####
# Modelo poderia ser escrito como AD ~ -1 + AT*SIL*ARG
# Escrevendo de forma explícita para facilitar interpretação

## Validação cruzada com 20 subconjuntos
# para avaliar o modelo
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
# 0.362 mm cm-1
sqrt(mean((dados$AD - AD_valid_m1)^2))

plot(dados$AD, AD_valid_m1,
     xlab = 'AD observada (mm cm⁻¹)', ylab = 'AD estimada, M1 (mm cm⁻¹)')
abline(a = 0, b = 1, col = 'red')

# agora modelo com todos os dados

m1 <- lm(AD ~ -1 + AT + SIL + ARG +
           I(AT*SIL) + I(AT*ARG) + I(SIL*ARG) + I(AT*SIL*ARG), data=dados)

# Apresentando a analíse de variância do modelo - Anova
# O comando summary apresenta os coeficientes, residuos e estatisticas do ajuste
# RSE, R2, R2a e significancia dos coeficientes.

anova(m1)
summary(m1)
coefficients(m1)

# MODELO SIMPLEX-MISTURE E COEFICIENTES
# Note que os termos com Silte sao pouco importantes
# estão mantidos no modelo porque se pode utilizar a equação com bases maiores

# RMSE - 0.0359 - Próximo ao valor obtido na validação cruzada
RMSEm1 <- sqrt(mean(residuals(m1)^2))
RMSEm1

######## Qualidade do Ajuste:
###      Preditos vs Residuos, Preditos vs Residuos normalizados e analise de outliers
par(mfrow=c(2,2), mar = c(2,2,2,2))
plot(m1)
par(mfrow=c(1,1), mar = c(5,4,4,2))

# Vemos que os resíduos apresentam um desvio (afastamento) da distribuição (Normal Q-Q plot)
# e a variancia certa heterogeneidade (Scale-location plot)


#### Modelo 2 - ADT transformação Box-Cox  (sem intercepto) ####
# Resíduos do M1 apresentam tendência. Usar transformação Box-Cox

#### Transformação Box - Cox ####
# Transformaçao dos dados (normalidade, homocedasticidade e aditividade) - BOX-COX
# Determinação do fator lambda
bc <- boxcox(AD ~ -1 + AT + SIL + ARG +
                  I(AT*SIL) + I(AT*ARG) + I(SIL*ARG) +
                  I(AT*SIL*ARG), data = dados, lambda=(2000:5000)/10000)
(lambda <- bc$x[which(bc$y==max(bc$y))])

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
# 0.366 mm cm-1
sqrt(mean((dados$AD - AD_valid_m2)^2))

plot(dados$AD, AD_valid_m2,
     xlab = 'AD observada (mm cm⁻¹)', ylab = 'AD estimada, M2 (mm cm⁻¹)')
abline(a = 0, b = 1, col = 'red')

# Aplicando modelo a todos os dados

m2 <- lm(ADt ~ -1 + AT + SIL + ARG +
           I(AT*SIL) + I(AT*ARG) + I(SIL*ARG) + I(AT*SIL*ARG), data=dados)

# APRESENTANDO A ANALISE DE VARIANCIA DO MODELO DADOS - anova
# O comando summary apresenta os coeficientes, residuos e estatisticas do ajuste
# RSE, R2, R2a e significancia dos coeficientes.

anova(m2)

# MODELO SIMPLEX-MISTURE E COEFICIENTES
# Termos com Silte tem Pr(>F) um pouco menos sifnificativo
# Mas aparentemente melhor que m1

summary(m2)
coefficients(m2)

# RMSE dos dados transformados
# Praticamente igual ao obtido em M1
# E resultado do M2 é igual ao resultado obtido na validação cruzada
fit.m2_destrans <- (lambda * fitted(m2) + 1) ^(1 / lambda)
RMSEm2 <- sqrt(mean((dados$AD - fit.m2_destrans)^2))

RMSEm2
RMSEm1

######## Qualidade do Ajuste:
###      Preditos vs Residuos, Preditos vs Residuos normalizados e analise de outliers
par(mfrow=c(2,2),  mar = c(2,2,2,2))
plot(m2)
par(mfrow=c(1,1),  mar = c(5,4,4,2))

# Análise gráfica dos resíduso indica melhora da distribuição e da variância dos resíduos

#### Comparando resulados dos modelos ####
AD <- dados$AD
fit.m1 <- fitted.values(m1)
fit.m2 <- fitted.values(m2)

# Medidos vs Preditos m1
plot(AD,fit.m1,
     xlab = 'AD amostra', ylab = 'AD modelo',
     main = 'AD a partir das frações granulométricas',
     sub = 'Modelo m1 - sem transformação')
abline(a = 0, b = 1, col = 'black')

# Medidos vs preditos m2
plot(AD, fit.m2_destrans,
     xlab = 'AD amostra (mm cm⁻¹)', ylab = 'AD modelo (mm cm⁻¹)',
     main = 'AD a partir das frações granulométricas',
     sub = 'com transformação Box-Cox')
abline(a = 0, b = 1, col = 'black')

# Preditos m1 vs Preditos m2
plot(fit.m1,fit.m2_destrans,
     xlab = 'Sem transformação', ylab = 'com transformação')
abline(a = 0, b = 1, col = 'red')

# Preditos m1 e m2 vs AD avaliada
plot(AD,fit.m1,
     xlab = 'AD amostras', ylab = 'AD modelo')
points(AD, fit.m2_destrans, col = 'red')
legend('topleft', legend = c('sem transformação', 'com transformação'),
       col = c('black', 'red'), pch = 1)

# ANALISE GRAFICA DAS RELACOES DA GRANULOMETRIA COM DADOS AVALIADOS E PREDITOS

# Plot preditos versus granulometria modelo 1 - Dados originais

par(mfrow=c(1,2))
plot(dados$ARG, fit.m1,
     xlab = 'Argila', ylab = 'AD predita (m1)') # ARG vs preditos
plot(dados$ARG, AD,
     xlab = 'Argila', ylab = 'AD amostra') # ARG vs ADorg

plot(dados$AT, fit.m1,
     xlab = 'Areia', ylab = 'AD predita (m1)') # Preditos vs AT
plot(dados$AT, AD,
     xlab = 'Areia', ylab = 'AD amostra') # ARG vs ADorg

plot(dados$SIL, fit.m1,
     xlab = 'Silte', ylab = 'AD predita (m1)') # Preditos vs SIL
plot(dados$SIL, AD,
     xlab = 'Silte', ylab = 'AD amostra') # SIL vs ADorg

# Plot preditos versus granulometria modelo 2 - Dados detransoformados

plot(dados$ARG, fit.m2_destrans,
     xlab = 'Argila', ylab = 'AD predita (m2)') # ARG vs preditos
plot(dados$ARG, AD,
     xlab = 'Argila', ylab = 'AD amostra (m2)') # ARG vs ADorg

plot(dados$AT, fit.m2_destrans,
     xlab = 'Areia', ylab = 'AD predita (m2)') # Preditos vs AT
plot(dados$AT, AD,
     xlab = 'Areia', ylab = 'AD amostra') # ARG vs ADorg

plot(dados$SIL, fit.m2_destrans,
     xlab = 'Silte', ylab = 'AD predita (m2)') # Preditos vs SIL
plot(dados$SIL, AD,
     xlab = 'Silte', ylab = 'AD amostra') # SIL vs ADorg

# retornando configuração de plot para
# o original
par(mfrow = c(1,1))

#Analise de correlação
cor(fit.m1,dados$AD)
cor(fit.m2,dados$AD)
cor(fit.m2_destrans,dados$AD)

#Plot dos residuos m1 e m2
res.m1 <- residuals(m1)
res.m2 <- residuals(m2)

# Analise grafica dos residuos
par(mfcol=c(3,2))
plot(res.m1)
boxplot(res.m1)
hist(res.m1)

plot(res.m2)
boxplot(res.m2)
hist(res.m2)

par(mfrow=c(1,1))
plot(res.m1,res.m2)

# Aqui se ve alguns resiudos outliers

# Plot preditos vs residuos
par(mfrow=c(1,2))

# Preditos m1 vs resi m1
plot(m1$fitted.values, res.m1,
     xlab = 'Preditos (m1)', ylab = 'Resíduos (m1)')
abline(h = 0)

# Preditos m2 vs resi m2 (atenção para usar dados destransformados)
par(mfrow=c(2,1), mar = c(2,2,2,2))
plot(fit.m2_destrans, dados$AD - fit.m2_destrans,
     main = 'Dispersão dos resíduos vs AD predito',
     xlab = 'Preditos (mm cm⁻¹)', ylab = 'Resíduos (mm cm⁻¹)')
abline(h = 0)

hist(dados$AD - fit.m2_destrans,
     main = 'Histograma dos resíduos',
     xlab = 'Resíduo (mm cm⁻¹)',
     ylab = 'Frequência')

par(mfrow=c(1,1), mar = c(5,4,4,2))

#### Salvando dados em uma planilha única ####

ZARC12 <- data.frame(ID = dados$ID, AT = dados$AT, ARG = dados$ARG,
                     SIL = dados$SIL, AD = dados$AD,
                     fit.m1, fit.m2 = fit.m2_destrans,
                     res.m1, res.m2 = dados$AD - fit.m2_destrans)

# Salvando dados originais, preditos (M1 e M2, destransformado) e resíduos
# se encontra na pasta resultados.
write.csv2(ZARC12, file="resultados/PTF_ZARC_m1_m2.csv")

######## Intervalos de confianca para as estimativas
###     presentes no delineamento
#p <- predict(m,intervals="confidence")
#write.csv2(p,file="Dados AD Preditos.csv")
# FINAL DO AJUSTE DO MODELO

#### Aplicando modelos ao triângulo textural ####
# PREDICAO DA MATRIZ DE TODAS AS COMBINACOES DE RESULTADOS
# DE AREIA TOTAL, SILTE, ARGILA (EM % CUJA SOMA E 100% SEM DECIMAIS)
# ISTO GERA 5151 RESULTADOS POSSIVEIS, NA MATRIZ (AT, SIL, ARG, AD)

######## Gerando uma matriz para todas combinações - Lookup Table
grade <- expand.grid(0:100, 0:100, 0:100)
dim(grade)
soma  <- grade[,1] + grade[,2] + grade[,3]
index <- which(soma == 100)
length(index)

AT  <- grade[index,1]
SIL <- grade[index,2]
ARG <- grade[index,3]

triang_m1 <- data.frame(AT, SIL, ARG)
ad_triang_m1 <- predict(m1, triang_m1, interval="confidence")

triang_m1$ADmedia <- ad_triang_m1[,1]
triang_m1$LI_IC   <- ad_triang_m1[,2]
triang_m1$LS_IC   <- ad_triang_m1[,3]

# salvando valores preditos pelo M1, juntamente com o intervalo de confiânça
# arquivos estão no `working directory`
write.csv2(triang_m1, file = 'resultados/Preditos_triangulo_AD_m1.csv', row.names = FALSE)

# Fazendo as predicoes usando o modelo m2
triang_m2 <- data.frame(AT, SIL, ARG)
ad_triang_m2 <- predict(m2, triang_m2, interval="confidence")

# destransformando Box-Cox
ad_triang_m2 <- (lambda * ad_triang_m2 + 1)^(1 / lambda)

triang_m2$ADm2 <- ad_triang_m2[,1]
triang_m2$LI_ICm2 <- ad_triang_m2[,2]
triang_m2$LS_ICm2 <- ad_triang_m2[,3]

# Salvando resultados preditos pelo modelo 2 (já transformados)
write.csv2(triang_m2, file = 'resultados/Preditos_triangulo_AD_m2.csv', row.names = FALSE)

#### Plotando resultado de m1 no triângulo ####
par(mar = c(1,1,2,1))
TernaryPlot(main = 'Modelo m1',
            alab = 'Argila [%]', blab = 'Silte [%]', clab = 'Areia [%]')

# Utiliza o modelo ajustado (m1) anteriormente para colocar as cores
# funcao recebe valores ternários na escala de 0 a 1.
# Por isso multiplica por 100
func_to_contour <- function (ARG, SIL, AT) {
  predict(m1, newdata = data.frame(AT, SIL, ARG) * 100) 
}

# Valores em cada ponto
valores <- TernaryPointValues(func_to_contour)

# Adiciona superfície colorida - escala de cores padrão é VIRIDIS.
# e adiciona linhas de contorno
ColorTernary(valores)
TernaryContour(func_to_contour, resolution = 30)

# Legenda com min, mean, max
legend('topright',
       title = 'mm cm⁻¹',
       legend=c(round(min(dados$AD), 2),
                round(mean(dados$AD), 2),
                round(max(dados$AD), 2)),
       cex=0.8, bty='n', pch=21, pt.cex=1.8,
       pt.bg=c(viridisLite::viridis(256, alpha = 0.6)[1],
               viridisLite::viridis(256, alpha = 0.6)[128],
               viridisLite::viridis(256, alpha = 0.6)[256])
)

# Modelo m1 com as cores do ZARC
TernaryPlot(main = 'Modelo m1',
            alab = 'Argila [%]', blab = 'Silte [%]', clab = 'Areia [%]')

valores <- TernaryPointValues(func_to_contour, resolution = 150)

classe_triang <- classe_ad(valores[3,])
cor_triang <- cores_zarc[classe_triang]

valores[3,] <- cor_triang

ColorTernary(valores, spectrum = NULL)
TernaryContour(func_to_contour, resolution = 30)

legend('topright',
       title = 'mm cm⁻¹',
       legend = levels(classe_dados),
       cex=0.8, bty='n', pch=21, pt.cex=1.8,
       pt.bg = cores_zarc,
       ncol = 1)

#### Plotando resultado de m2 no triângulo ####
# Utiliza o modelo M2, ajustado anteriormente para colocar as cores
# precisa desfazer transformação Box-Cox

# salvando configurações padrão do gráfico
# para voltar ao normal depois
orig.par <- par(no.readonly = TRUE)

# reduzindo as margens do plot
par(mar = c(1,1,2,1))

TernaryPlot(main = 'AD estimada',
            alab = 'Argila', blab = 'Silte', clab = 'Areia')

# funcao recebe valores ternários na escala de 0 a 1.
# Por isso multiplica por 100
func_to_contour <- function (ARG, SIL, AT) {
  adt <- predict(m2, newdata = data.frame(AT, SIL, ARG) * 100)
  (lambda*adt+1)^(1/lambda)
}

# Valores em cada ponto
valores <- TernaryPointValues(func_to_contour)

# Adiciona superfície colorida - escala de cores padrão é VIRIDIS.
# e adiciona linhas de contorno
ColorTernary(valores)
TernaryContour(func_to_contour, resolution = 30)

# Legenda com min, mean, max
legend('topright',
       title = 'mm cm⁻¹',
       legend=c(round(min(dados$AD), 2),
                round(mean(dados$AD), 2),
                round(max(dados$AD), 2)),
       cex=0.8, bty='n', pch=21, pt.cex=1.8,
       pt.bg=c(viridisLite::viridis(256, alpha = 0.6)[1],
               viridisLite::viridis(256, alpha = 0.6)[128],
               viridisLite::viridis(256, alpha = 0.6)[256])
)

## Modelo 2 com as cores do ZARC
# é preciso substituir 3 coluna da matriz de valores pelo código das cores

TernaryPlot(main = 'Modelo m2',
            alab = 'Argila [%]', blab = 'Silte [%]', clab = 'Areia [%]')

valores <- TernaryPointValues(func_to_contour, resolution = 150)

classe_triang <- classe_ad(valores[3,])
cor_triang <- cores_zarc[classe_triang]

# adicionando transparência
# precisa adicionar codigo hexadecimal de transparência (alpha)
# __ao final__ do código da cor
# códigos HEX estão aqui: https://gist.github.com/lopspower/03fb1cc0ac9f32ef38f4
cor_triang <- paste0(cor_triang, 'BF')

valores[3,] <- cor_triang

ColorTernary(valores, spectrum = NULL)
TernaryContour(func_to_contour, resolution = 30)

legend('topright',
       title = 'mm cm⁻¹',
       legend = levels(classe_dados),
       cex=0.8, bty='n', pch=21, pt.cex=1.8,
       pt.bg = cores_zarc,
       ncol = 2)

dev.copy(png, 'figs/triangulo_ad_modelo_m2.png')
dev.off()

# voltando configurações da área do gráfico
par(orig.par)


#### Plotando intervalo de confiânça do modelo M2 no triângulo ####
# Utiliza o modelo M2, ajustado anteriormente para colocar as cores
# precisa desfazer transformação Box-Cox

# salvando configurações padrão do gráfico
# para voltar ao normal depois
orig.par <- par(no.readonly = TRUE)

# reduzindo as margens do plot
par(mar = c(1,1,2,1))

TernaryPlot(main = 'Intervalo de confiânça da AD estimada (M2)',
            alab = 'Argila', blab = 'Silte', clab = 'Areia')

# funcao recebe valores ternários na escala de 0 a 1.
# Por isso multiplica por 100
contour_intervalo_confianca <- function (ARG, SIL, AT) {
  adt <- predict(m2, newdata = data.frame(AT, SIL, ARG) * 100,
                 interval = 'confidence')
  ad <- (lambda*adt+1)^(1/lambda)
  intervalo <- ad[,3] - ad[,2]
  return(intervalo)
}

# Valores em cada ponto
valores <- TernaryPointValues(contour_intervalo_confianca)

# Adiciona superfície colorida - escala de cores padrão é VIRIDIS.
# e adiciona linhas de contorno
ColorTernary(valores)
TernaryContour(contour_intervalo_confianca, resolution = 30)

# Legenda com min, mean, max
legend('topright',
       title = 'mm cm⁻¹',
       legend=c(round(min(triang_m2$LS_ICm2 - triang_m2$LI_ICm2), 2),
                round(mean(triang_m2$LS_ICm2 - triang_m2$LI_ICm2), 2),
                round(max(triang_m2$LS_ICm2 - triang_m2$LI_ICm2), 2)),
       cex=0.8, bty='n', pch=21, pt.cex=1.8,
       pt.bg=c(viridisLite::viridis(256, alpha = 0.6)[1],
               viridisLite::viridis(256, alpha = 0.6)[128],
               viridisLite::viridis(256, alpha = 0.6)[256])
)

dev.copy(png, 'figs/triangulo_intervalo_confianca_m2.png')
dev.off()

# voltando configurações da área do gráfico
par(orig.par)

# Fim