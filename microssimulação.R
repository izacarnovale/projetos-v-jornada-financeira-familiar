# ============================================================
# PARTE 6 — MICROSSIMULAÇÃO
# Bancos, pais e jovens
# ============================================================


# ------------------------------------------------------------
# 25. PARÂMETROS DA SIMULAÇÃO
# ------------------------------------------------------------

set.seed(777)

# Número de jovens simulados
N <- 1000


# Gamma estimado anteriormente com a pesquisa
# Não precisa colocar o número manualmente:
# o objeto "gamma" já foi criado nas etapas anteriores.

gamma


# ------------------------------------------------------------
# PARÂMETROS COMPORTAMENTAIS
# ------------------------------------------------------------
#
# beta_j = peso dado pelo jovem ao bem-estar futuro
# beta_p = peso dado pelos pais ao bem-estar futuro
#
# Hipótese do modelo:
#
# beta_p > beta_j
#
# Ou seja, os pais se preocupam relativamente mais
# com as consequências futuras do endividamento.
# ------------------------------------------------------------

beta_j <- 0.5
beta_p <- 0.9

# Peso do bem-estar do filho na utilidade dos pais.
# Como aqui estamos interessados principalmente em comparar
# U_p(0) com U_p(1), podemos manter lambda = 1.

lambda <- 1

# ------------------------------------------------------------
# 26. CRIAR A POPULAÇÃO SIMULADA
# ------------------------------------------------------------
#
# Estratégias do banco:
#
# s = 0 -> crédito agressivo
# s = 1 -> crédito responsável
#
# O modelo estabelece:
#
# C(0) > C(1)
# F(1) > F(0)
#
# Crédito agressivo:
# maior consumo hoje, pior situação futura.
#
# Crédito responsável:
# menor consumo imediato, melhor situação futura.
# ------------------------------------------------------------


simulacao <- tibble(
  
  id = 1:N,
  
  # ----------------------------------------------------------
  # CONSUMO PRESENTE
  # ----------------------------------------------------------
  
  # Consumo sob crédito responsável
  C1 = runif(
    N,
    min = 3,
    max = 7
  ),
  
  # Quanto o crédito agressivo permite consumir a mais
  ganho_consumo_agressivo = runif(
    N,
    min = 1,
    max = 3
  ),
  
  # ----------------------------------------------------------
  # BEM-ESTAR FINANCEIRO FUTURO
  # ----------------------------------------------------------
  
  # Situação futura sob crédito agressivo
  F0 = runif(
    N,
    min = 2,
    max = 6
  ),
  
  # Quanto o crédito responsável melhora
  # a situação financeira futura
  ganho_futuro_responsavel = runif(
    N,
    min = 1.5,
    max = 4
  )
  
) %>%
  
  mutate(
    
    # Crédito agressivo permite maior consumo presente
    
    C0 = pmin(
      C1 + ganho_consumo_agressivo,
      10
    ),
    
    
    # Crédito responsável melhora o bem-estar futuro
    
    F1 = pmin(
      F0 + ganho_futuro_responsavel,
      10
    )
  )
 
# ------------------------------------------------------------
# 27. UTILIDADE DOS JOVENS
# ------------------------------------------------------------

simulacao <- simulacao %>%
  mutate(
    
    UJ0 =
      C0 +
      beta_j * F0,
    
    UJ1 =
      C1 +
      beta_j * F1
    
  )

# ------------------------------------------------------------
# 28. UTILIDADE DOS PAIS
# ------------------------------------------------------------

simulacao <- simulacao %>%
  mutate(
    
    UP0 =
      lambda * (
        C0 +
          beta_p * F0
      ),
    
    UP1 =
      lambda * (
        C1 +
          beta_p * F1
      )
    
  )

# ------------------------------------------------------------
# 29. RECOMENDAÇÃO DOS PAIS
# ------------------------------------------------------------

simulacao <- simulacao %>%
  mutate(
    
    R = if_else(
      UP1 > UP0,
      1,
      0
    )
    
  )

# ------------------------------------------------------------
# 30. ESCOLHA SEM INFLUÊNCIA PARENTAL
# ------------------------------------------------------------

simulacao <- simulacao %>%
  mutate(
    
    escolhe_responsavel_sem_pais =
      if_else(
        UJ1 > UJ0,
        1,
        0
      )
    
  )

# ------------------------------------------------------------
# 31. UTILIDADE COM INFLUÊNCIA DOS PAIS
# ------------------------------------------------------------

simulacao <- simulacao %>%
  mutate(
    
    UJ1_com_pais =
      UJ1 +
      gamma * R
    
  )

# ------------------------------------------------------------
# 32. ESCOLHA COM INFLUÊNCIA PARENTAL
# ------------------------------------------------------------

simulacao <- simulacao %>%
  mutate(
    
    escolhe_responsavel_com_pais =
      if_else(
        UJ1_com_pais > UJ0,
        1,
        0
      )
    
  )

# ------------------------------------------------------------
# 33. RESULTADOS DA MICROSSIMULAÇÃO
# ------------------------------------------------------------

resultado_simulacao <- simulacao %>%
  summarise(
    
    N = n(),
    
    # Jovens atraídos sem influência dos pais
    responsavel_sem_pais =
      sum(escolhe_responsavel_sem_pais),
    
    # Jovens atraídos com influência dos pais
    responsavel_com_pais =
      sum(escolhe_responsavel_com_pais),
    
    # Percentuais
    percentual_sem_pais =
      mean(escolhe_responsavel_sem_pais) * 100,
    
    percentual_com_pais =
      mean(escolhe_responsavel_com_pais) * 100
  ) %>%
  
  mutate(
    
    # Jovens adicionais conquistados
    delta_N =
      responsavel_com_pais -
      responsavel_sem_pais,
    
    # Diferença em pontos percentuais
    delta_percentual =
      percentual_com_pais -
      percentual_sem_pais
  )


resultado_simulacao

# ------------------------------------------------------------
# 34. TABELA PARA APRESENTAÇÃO
# ------------------------------------------------------------

tabela_simulacao <- tibble(
  
  cenario = c(
    "Sem influência parental",
    "Com influência parental"
  ),
  
  jovens_responsavel = c(
    
    sum(
      simulacao$escolhe_responsavel_sem_pais
    ),
    
    sum(
      simulacao$escolhe_responsavel_com_pais
    )
  ),
  
  percentual = c(
    
    mean(
      simulacao$escolhe_responsavel_sem_pais
    ) * 100,
    
    mean(
      simulacao$escolhe_responsavel_com_pais
    ) * 100
  )
)


tabela_simulacao

# ------------------------------------------------------------
# 35. GRÁFICO — EFEITO DA INFLUÊNCIA PARENTAL
# ------------------------------------------------------------

grafico_simulacao <- ggplot(
  tabela_simulacao,
  aes(
    x = cenario,
    y = percentual
  )
) +
  
  geom_col(
    width = 0.6
  ) +
  
  geom_text(
    aes(
      label = paste0(
        round(percentual, 1),
        "%"
      )
    ),
    vjust = -0.5,
    size = 5
  ) +
  
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20)
  ) +
  
  labs(
    title = "Impacto da influência parental na escolha do banco",
    subtitle = paste0(
      "Microssimulação com γ = ",
      round(gamma, 2)
    ),
    x = NULL,
    y = "Jovens que escolheriam o banco responsável (%)"
  ) +
  
  theme_minimal(base_size = 13)


grafico_simulacao

# ============================================================
# PARTE 7 — ANÁLISE DE SENSIBILIDADE DO GAMMA
# ============================================================


# ------------------------------------------------------------
# 36. TESTAR DIFERENTES VALORES DE GAMMA
# ------------------------------------------------------------

valores_gamma <- seq(
  from = 0,
  to = 10,
  by = 0.5
)


sensibilidade <- tibble(
  gamma_teste = valores_gamma
)


for (g in valores_gamma) {
  
  escolha <- if_else(
    
    simulacao$UJ1 +
      g * simulacao$R >
      simulacao$UJ0,
    
    1,
    0
  )
  
  sensibilidade[
    sensibilidade$gamma_teste == g,
    "percentual_responsavel"
  ] <- mean(escolha) * 100
}

# ------------------------------------------------------------
# 37. GRÁFICO DE SENSIBILIDADE
# ------------------------------------------------------------

grafico_sensibilidade <- ggplot(
  sensibilidade,
  aes(
    x = gamma_teste,
    y = percentual_responsavel
  )
) +
  
  geom_line(
    linewidth = 1
  ) +
  
  geom_point(
    size = 2
  ) +
  
  geom_vline(
    xintercept = gamma,
    linetype = "dashed"
  ) +
  
  annotate(
    "text",
    x = gamma,
    y = 5,
    label = paste0(
      "γ estimado = ",
      round(gamma, 2)
    ),
    angle = 90,
    vjust = -0.5
  ) +
  
  scale_x_continuous(
    breaks = seq(0, 10, 1)
  ) +
  
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20)
  ) +
  
  labs(
    title = "Sensibilidade da estratégia à influência parental",
    subtitle = "Percentual de jovens atraídos para diferentes valores de γ",
    x = "Influência parental (γ)",
    y = "Jovens que escolheriam o banco responsável (%)"
  ) +
  
  theme_minimal(base_size = 13)


grafico_sensibilidade