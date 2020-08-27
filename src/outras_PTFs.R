# PTFs diversas para avaliar
# Daniel de Castro Victoria
# 20 ago 2020

### PTFs para avaliar ####

# ASSAD, M. L. L.; SANS, L. M. A.; ASSAD, E. D. 
# Relação entre água retida e conteúdo de areia total em solos brasileiros.
# Revista Brasileira de Agrometeorologia, v. 9, n. 3, p. 588–596, 2001.
# areia: areia total (%)
# ad: conteúdo volumétrico de Água Disponível
ptf_assad <- function(areia) {
  ad <- 12.76278562 - 9.8726e-6 * areia^3
  return(ad)
}

# GIAROLA, N. F. B.; SILVA, A. P.; IMHOFF, S.
# Relações entre propriedades físicas e características de solos da região sul do Brasil.
# Revista Brasileira de Ciência do Solo, v. 26, n. 4, p. 885–893, dez. 2002. 
# Tem uma PTF para pmp do horizonte A e outra para o B
# usando valor intermediário do intercepto da regressão
# silte e argila: em porcentagem
# ad: conteúdo columétrico (cm3/cm3)
ptf_giarola <-function(silte, argila){
  pmp <- 0.005 * silte + 0.003 * argila - 0.0035
  cc <- 0.081 + 0.005 * silte + 0.004 * argila
  ad <- cc - pmp
  return(ad)
}

# VAN DEN BERG, M. et al.
# Pedotransfer Functions for the Estimation of Moisture Retention Characteristics of Ferralsols and Related Soils.
# Geoderma, v. 78, n. 3–4, p. 161–180, ago. 1997. 
# usando eq. 13, AD em função da Argila
# argila em kg/kg * 100
# ad: conteúdo volumétrico (m3 / m3) * 100
ptf_van_den_berg <- function(argila) {
  ad <- 7.676 + 0.093 * argila
  return(ad)
}

# regra ZARC original (3 solos)
ptf_ZARC_orig <- function(areia, argila) {
  ad <- rep(0, length(argila))
  ad[argila >= 10] <- 0.7
  ad[argila >= 15 & (areia - argila) >= 50] <- 0.7
  ad[argila >= 15 & (areia - argila) < 50] <- 1.1
  ad[argila >= 35] <- 1.5
  return(ad)
}
