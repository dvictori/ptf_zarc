# Sensibilidade do risco climático à CAD
# Rodadas do modelo do ZARC, para análise foi realizado 'por fora'
# e dados foram organizados em planilha por Eduardo Monteiro
# Script R é usado para converter dados de CAD para AD
# e gerar as figuras usadas no trabalho

library(tidyverse)

prof_raiz <- 40

dados <- read_csv('dados/sensibilidade_risco_cad.csv') %>%
  mutate(risco = as.numeric(risco) * 100,
         ad = cad / prof_raiz,
         regiao = factor(regiao, c('Seca', 'Seca moderada',
                                   'Úmida moderada', 'Úmida')))

cores <- colorRampPalette(c('red', 'blue'))

dados_plot <- dados %>%
  filter(faixa_isna == '< 0.5')

#### Gráfico 1. Risco vs. AD ####

ggplot(dados_plot) +
  geom_point(aes(x = ad, y = risco, col = regiao)) +
  scale_color_brewer(type = 'div', palette = 5) +
  geom_smooth(data = dados_plot,
              aes(x = ad, y = risco, col = regiao),
              method = 'lm',
              formula = y ~ log(x),
              se = FALSE,
              fullrange = TRUE,
              lty = 'dashed', lwd = 0.5) +
  xlim(0.0001,3) + ylim (0 ,100) +
  labs(x = 'AD (mm/cm)', y = 'Risco hídrico ISNA < 0,50 (%)',
       col = 'Região')
ggsave('figs/sensibilidade_risco_ad.png')

ggplot(dados_plot) +
  geom_point(aes(x = ad, y = risco, col = regiao)) +
  scale_color_brewer(type = 'div', palette = 5) +
  geom_smooth(data = dados_plot,
              aes(x = ad, y = risco, col = regiao),
              method = 'lm',
              formula = y ~ log(x),
              se = FALSE,
              fullrange = TRUE,
              lty = 'dashed', lwd = 0.5) +
  xlim(0.0001, 3) + ylim (0 ,100) +
  coord_cartesian(xlim = c(0.2, 0.7), ylim = c(10, 90)) +
  scale_y_continuous(breaks = seq(0, 100, 10), minor_breaks = seq(0, 100, 5)) +
  labs(x = 'AD (mm/cm)', y = 'Risco hídrico ISNA < 0,50 (%)',
       col = 'Região')

#### Gráfico 2. Diferença de risco entre classes de AD ####

m_sm <- lm(risco ~ log(ad),
             data = dados_plot %>%
               filter(regiao == 'Seca moderada'))
m_s <- lm(risco ~ log(ad),
           data = dados_plot %>%
             filter(regiao == 'Seca'))
m_u <- lm(risco ~ log(ad),
           data = dados_plot %>%
             filter(regiao == 'Úmida'))
m_um <- lm(risco ~ log(ad),
           data = dados_plot %>%
             filter(regiao == 'Úmida moderada'))

sensibilidade <- tibble(ad = seq(0.1, 3, 0.1)) %>%
  mutate(cad = ad * 40)
sensibilidade$risco_sm <- predict(m_sm, newdata = sensibilidade)
sensibilidade$risco_s <- predict(m_s, newdata = sensibilidade)
sensibilidade$risco_u <- predict(m_u, newdata = sensibilidade)
sensibilidade$risco_um <- predict(m_um, newdata = sensibilidade)

diff_risco <- sensibilidade %>%
  filter(near(ad, 0.3) |
         near(ad, 0.5) |
         near(ad, 0.9) |
           near(ad, 1.3) |
           near(ad, 1.5)) %>%
  as.matrix() %>% diff()

diff_risco <- as_tibble(diff_risco) %>%
  mutate(ad = c('AD 0,3 - AD 0.5', 'AD 0,5 - AD 0,9',
                'AD 0,9 - AD 1,3', 'AD 1,3 - AD 1,5')) %>%
  pivot_longer(c(-ad, -cad), 
               names_to = 'regiao', names_prefix = 'risco_',
               values_to = 'risco') %>%
  mutate(risco = abs(risco))

diff_risco <- diff_risco %>%
  mutate(
    regiao = case_when(
      regiao == 'sm' ~ 'Seca moderada',
      regiao == 's'  ~ 'Seca',
      regiao == 'u' ~ 'Úmida',
      regiao == 'um' ~ 'Úmida moderada'
    )) %>%
  mutate(
    regiao = factor(regiao, c('Seca', 'Seca moderada',
                              'Úmida moderada', 'Úmida'))
  )

ggplot(diff_risco %>%
         filter(ad != 'AD 0,9 - AD 1,3')) +
  geom_col(aes(x = ad, y = risco, fill = regiao), position = 'dodge') +
  scale_fill_brewer(type = 'div', palette = 5) +
  labs(x = '', y = 'Diferença de risco (%) entre ADs', fill = 'Região')
ggsave('figs/diferenca_risco_ads.png')

#### Gráfico 3. Variação do risco nas classes originais do ZARC ####
# considerando risco para clima Seco moderado

risco_cad_orig <- tibble(solo = rep(1:3,3),
                         faixa =  rep(c('medio', 'mínimo', 'máximo'), each = 3),
                         ad = c(c(0.7, 1.1, 1.5),
                                   c(0.7, 1.1, 1.5) - 0.2,
                                   c(0.7, 1.1, 1.5) + 0.2)
                         )
risco_cad_orig$risco <- predict(m_sm, newdata = risco_cad_orig)

risco_orig_pivot <- risco_cad_orig %>%
  pivot_wider(c(solo), names_from = faixa, values_from = risco)

ggplot(risco_orig_pivot) +
  geom_rect(aes(x = as.character(solo), xmin = solo - 0.3, xmax = solo + 0.3,
                ymin = mínimo, ymax = máximo)) +
  scale_x_discrete(labels = paste('Solo', 1:3, '\nAD média', c(0.7, 1.1, 1.5))) +
  geom_text(aes(x = as.character(solo), y = máximo - 0.5,
                label = paste(round(máximo, 1), '%'))) +
  geom_text(aes(x = as.character(solo), y = mínimo + 0.5,
                label = paste(round(mínimo, 1), '%'))) +
  geom_text(aes(x = as.character(solo), y = medio,
                label = paste(round(mínimo - máximo, 1), '%'))) +
  labs(y = 'Risco (%)', x = '')

ggsave('figs/variacao_risco_original.png')

#### Gráfico 4. Variação do risco para 7 classes de AD ####
# considerando risco para clima Seco moderado

risco_novas_classes <- tibble(
  id = 1:7,
  classes = c(
    'AD 1 (0.40)',
    'AD 2 (0.50)',
    'AD 3 (0.65)',
    'AD 4 (0.85)',
    'AD 5 (1.10)',
    'AD 6 (1.40)',
    'AD 7 (1.80)'
  ),
  ad_minimo = c(0.34, 0.45, 0.58, 0.75, 0.98, 1.25, 1.60),
  ad_maximo = c(0.45, 0.58, 0.75, 0.98, 1.25, 1.60, 2)
) 
risco_novas_classes$ad_medio <-
  (risco_novas_classes$ad_minimo + risco_novas_classes$ad_maximo) / 2
risco_novas_classes$risco_min <- predict(m_sm,
                                         newdata = list(ad = risco_novas_classes$ad_minimo))
risco_novas_classes$risco_med <- predict(m_sm,
                                         newdata = list(ad = risco_novas_classes$ad_medio))
risco_novas_classes$risco_max <- predict(m_sm,
                                         newdata = list(ad = risco_novas_classes$ad_maximo))

ggplot(risco_novas_classes) +
  geom_rect(aes(x = classes, xmin = id - 0.3, xmax = id + 0.3,
                ymin = risco_min, ymax = risco_max)) +
  scale_x_discrete(labels = gsub('\\(', '\n\\(', risco_novas_classes$classes)) +
  geom_text(aes(x = id, y = risco_max - 0.5,
                label = paste(round(risco_max, 1), '%'))) +
  geom_text(aes(x = id, y = risco_min + 0.5,
                label = paste(round(risco_min, 1), '%'))) +
  geom_text(aes(x = id, y = risco_med,
                label = paste(round(risco_min - risco_max, 1), '%'))) +
  labs(y = 'Risco (%)', x = '')

ggsave('figs/variavao_risco_novas_classes.png')

#### Gráfico 5. Variação do risco com 6 classes de AD ####
# Por sugestão do Lumbreras - variação de 40% entre classes de AD

teste_ad_40 <- tibble(
  id = 1:6,
  classes = as.character(1:6),
  ad = c(0.4, 0.56, 0.78, 1.1, 1.53, 2.14),
  ad_min = c(.34, .48, .67, .94, 1.32, 1.84),
  ad_max = c(.48, .67, .94, 1.32, 1.84, 2.57))

teste_ad_40$risco_min <- predict(m_sm,
                                 newdata = list(ad = teste_ad_40$ad_min))
teste_ad_40$risco_med <- predict(m_sm,
                                 newdata = list(ad = teste_ad_40$ad))
teste_ad_40$risco_max <- predict(m_sm,
                                 newdata = list(ad = teste_ad_40$ad_max))

ggplot(teste_ad_40) +
  geom_rect(aes(x = classes, xmin = id - 0.3, xmax = id + 0.3,
                ymin = risco_min, ymax = risco_max)) +
  scale_x_discrete(labels = paste0('AD ', teste_ad_40$classes, 
                                  '\n(', teste_ad_40$ad, ')')) +
  geom_text(aes(x = id, y = risco_max - 1.2,
                label = paste('AD:', ad_max, '\nRisco:', round(risco_max, 1), '%'))) +
  geom_text(aes(x = id, y = risco_min + 1.2,
                label = paste('AD:', ad_min, '\nRisco:', round(risco_min, 1), '%'))) +
  geom_text(aes(x = id, y = risco_med,
                label = paste(round(risco_min - risco_max, 1), '%'))) +
  labs(y = 'Risco (%)', x = '')

ggplot(teste_ad_40) +
  geom_rect(aes(x = classes, xmin = id - 0.3, xmax = id + 0.3,
                ymin = risco_min, ymax = risco_max)) +
  scale_x_discrete(labels = paste0('AD ', teste_ad_40$classes, 
                                   '\n(', teste_ad_40$ad, ')')) +
  geom_text(aes(x = id, y = risco_max - 0.5,
                label = paste(round(risco_max, 1), '%'))) +
  geom_text(aes(x = id, y = risco_min + 0.5,
                label = paste(round(risco_min, 1), '%'))) +
  geom_text(aes(x = id, y = risco_med,
                label = paste(round(risco_min - risco_max, 1), '%'))) +
  labs(y = 'Risco (%)', x = '')
ggsave('figs/variacao_risco_6_classes.png')

#### Gráfico 6. Variação do risco com 5 faixas de AD ####

teste_ad_5classes <- tibble(
  id = 1:5,
  classes = paste('AD', 1:5),
  ad = c(0.4, 0.58, 0.83, 1.19, 1.72),
  ad_min = c(0.34, 0.49, 0.7, 1.01, 1.46),
  ad_max = c(0.49, 0.7, 1.01, 1.46, 2.1)
)

teste_ad_5classes$risco_min <- predict(m_sm,
                                 newdata = list(ad = teste_ad_5classes$ad_min))
teste_ad_5classes$risco_med <- predict(m_sm,
                                 newdata = list(ad = teste_ad_5classes$ad))
teste_ad_5classes$risco_max <- predict(m_sm,
                                 newdata = list(ad = teste_ad_5classes$ad_max))

ggplot(teste_ad_5classes) +
  geom_rect(aes(x = classes, xmin = id - 0.3, xmax = id + 0.3,
                ymin = risco_min, ymax = risco_max)) +
  scale_x_discrete(labels = gsub('\\(', '\n\\(', teste_ad_5classes$classes)) +
  geom_text(aes(x = id, y = risco_max - 1.2,
                label = paste('AD:', ad_max, '\nRisco:', round(risco_max, 1), '%'))) +
  geom_text(aes(x = id, y = risco_min + 1.2,
                label = paste('AD:', ad_min, '\nRisco:', round(risco_min, 1), '%'))) +
  geom_text(aes(x = id, y = risco_med,
                label = paste(round(risco_min - risco_max, 1), '%'))) +
  labs(y = 'Risco (%)', x = '')


#### Gráfico 7. Compara as 6 classes AD ####

ggplot(teste_ad_40) +
  geom_rect(aes(xmin = id - 0.3, xmax = id + 0.3,
                ymin = ad_min, ymax = ad_max,
                fill = ad)) +
  coord_flip() +
  scale_x_continuous(breaks = teste_ad_40$id,
                     labels = paste('AD', teste_ad_40$classes)) +
  geom_label(aes(x = id , y = ad, label = ad),
             label.padding = unit(0.15, "lines")) +
  labs(x = 'Classes de Água Disponível (AD)', y = 'AD (mm cm⁻¹)', fill = '') +
  theme(legend.position = 'NONE')
ggsave('figs/ads_6_classes.png')
