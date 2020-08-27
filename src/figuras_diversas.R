# Script com algumas figuras usadas no trabalho
# que não se enquadram em uma análise específica

library(Ternary)

#### triangulo AD ZARC 3 solos ####

textura <- expand_grid(areia = 0:100,
                       silte = 0:100,
                       argila = 0:100)

soma <- apply(textura, 1, sum)
textura <- textura[soma == 100,]

ad_zarc_orig <- function(argila, silte, areia) {
  
  argila <- argila * 100
  areia <- areia * 100
  silte <- silte * 100
  
  niveis = factor(c('Fora', 'Solo 1 (0.7)', 'Solo 2 (1.1)', 'Solo 3 (1.5)'),
                  levels = c('Fora', 'Solo 1 (0.7)', 'Solo 2 (1.1)', 'Solo 3 (1.5)'))
  
  grupo = ifelse(argila >= 35, 4,
                 ifelse(argila >= 15 & (areia - argila) < 50, 3,
                        ifelse(argila >= 10 |
                                 (argila >= 15 & (areia - argila) >= 50),
                               2, 1)
                 )
  )
  return(niveis[grupo])
}

textura$grupo <- ad_zarc_orig(textura$argila / 100,
                              textura$silte / 100,
                              textura$areia / 100)

orig.par <- par(no.readonly = TRUE)
par(mar = c(1,1,2,1))

cores_zarc_velho <- c('#bdbdbdBF', '#ffeda0BF',
                      '#31a354BF', '#a6bddbBF')

TernaryPlot(main = 'Classes de AD, ZARC solos 1, 2 e 3',
            alab = 'Argila - [%]', blab = 'Silte - [%]', clab = 'Areia Total - [%]')

valores <- TernaryPointValues(ad_zarc_orig, resolution = 150)

cor_triang <- cores_zarc_velho[valores[3,]]

valores[3,] <- cor_triang

ColorTernary(valores)

legend('topright',
       title = 'AD [mm cm⁻¹]',
       legend = levels(textura$grupo),
       cex=0.8, bty='n', pch=21, pt.cex=1.8,
       pt.bg = cores_zarc_velho)

dev.copy(png, 'figs/triangulo_zarc_3solos.png')
dev.off()

# voltando configurações da área do gráfico
par(orig.par)
