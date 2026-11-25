# ============================================================
# PROJETO V
# Influência parental na escolha bancária
# ============================================================


# ------------------------------------------------------------
# 1. PACOTES
# ------------------------------------------------------------

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)


# ------------------------------------------------------------
# 2. IMPORTAR A BASE
# ------------------------------------------------------------

dados <- read_excel(
  "Influência dos pais na escolha do banco (respostas).xlsx"
)


# ------------------------------------------------------------
# 3. RENOMEAR AS VARIÁVEIS QUE VAMOS USAR
# ------------------------------------------------------------

dados <- dados %>%
  rename(
    
    # Influência dos pais na escolha da primeira conta (0 a 10)
    influencia_inicial =
      `Pensando na escolha da sua primeira conta, quanto seus pais ou responsáveis influenciaram sua decisão?`,
    
    # Indica se a pessoa permaneceu ou mudou de banco
    mesmo_banco =
      `Seu principal banco atual é o mesmo em que você abriu sua primeira conta?`,
    
    # Influência dos pais na decisão de mudança
    influencia_troca =
      `Seus pais ou responsáveis tiveram alguma influência na decisão de mudar de banco?`,
    
    # Forma da influência na mudança
    forma_influencia_troca =
      `De que forma seus pais ou responsáveis influenciaram essa mudança?`,
    
    # Pais já eram clientes do primeiro banco
    pais_mesmo_banco =
      `Na época em que você abriu sua primeira conta, algum dos seus pais ou responsáveis já era cliente desse mesmo banco?`
  )


# ------------------------------------------------------------
# 4. LIMPEZA DA BASE
# ------------------------------------------------------------

dados <- dados %>%
  
  # Remove linhas vazias
  filter(
    !is.na(mesmo_banco),
    !is.na(influencia_inicial)
  ) %>%
  
  mutate(
    
    # Garantir que a influência inicial seja numérica
    influencia_inicial = as.numeric(influencia_inicial),
    
    # Criar uma variável de trajetória bancária
    trajetoria = case_when(
      mesmo_banco == "Sim" ~ "Permaneceu no primeiro banco",
      mesmo_banco == "Não" ~ "Mudou de banco",
      TRUE ~ NA_character_
    ),
    
    # Dummy de permanência
    permaneceu = case_when(
      mesmo_banco == "Sim" ~ 1,
      mesmo_banco == "Não" ~ 0,
      TRUE ~ NA_real_
    )
  )


# ------------------------------------------------------------
# 5. RECODIFICAR A INFLUÊNCIA NA TROCA PARA ESCALA 0–10
# ------------------------------------------------------------
#
# Não estamos normalizando os dados.
# Estamos apenas colocando a pergunta categórica de mudança
# na mesma escala de 0 a 10 usada na primeira pergunta.
#
# Nenhuma influência = 0
# Pouca influência = 2,5
# Alguma influência = 5
# Muita influência = 7,5
# Foram decisivos = 10
# ------------------------------------------------------------

dados <- dados %>%
  mutate(
    
    influencia_troca_num = case_when(
      
      influencia_troca == "Nenhuma influência" ~ 0,
      
      influencia_troca == "Pouca influência" ~ 2.5,
      
      influencia_troca == "Alguma influência" ~ 5,
      
      influencia_troca == "Muita influência" ~ 7.5,
      
      influencia_troca == "Foram decisivos para a mudança" ~ 10,
      
      TRUE ~ NA_real_
    )
  )


# ============================================================
# PARTE 1 — CÁLCULO DO GAMMA
# ============================================================


# ------------------------------------------------------------
# 6. CRIAR A MEDIDA INDIVIDUAL UTILIZADA PARA O GAMMA
# ------------------------------------------------------------
#
# Para todos os respondentes (quem permaneceu e quem mudou)
# usamos a influência dos pais na escolha do PRIMEIRO banco.
# Assim o gamma mede sempre o mesmo momento da decisão:
# a abertura da primeira conta. A influência na troca de banco
# não entra no gamma (ela aparece só na Parte 3, como comparação).
#
# ------------------------------------------------------------

dados <- dados %>%
  mutate(
    
    influencia_gamma = case_when(
      
      mesmo_banco == "Sim" ~ influencia_inicial,
      
      mesmo_banco == "Não" ~ influencia_inicial,
      
      TRUE ~ NA_real_
    )
  )


# ------------------------------------------------------------
# 7. TABELA DOS DOIS GRUPOS
# ------------------------------------------------------------

tabela_gamma <- dados %>%
  filter(!is.na(influencia_gamma)) %>%
  group_by(trajetoria) %>%
  summarise(
    
    n = n(),
    
    influencia_media =
      mean(influencia_gamma, na.rm = TRUE),
    
    .groups = "drop"
    
  ) %>%
  mutate(
    
    # Peso de cada grupo na amostra
    peso = n / sum(n),
    
    # Parcela da contribuição de cada grupo para o gamma
    contribuicao_gamma =
      influencia_media * peso
  )


tabela_gamma


# ------------------------------------------------------------
# 8. GAMMA FINAL
# ------------------------------------------------------------

gamma <- sum(tabela_gamma$contribuicao_gamma)

gamma

# ============================================================
# PARTE 2 — INFLUÊNCIA INICIAL E PERMANÊNCIA
# ============================================================


# ------------------------------------------------------------
# 9. TABELA: INFLUÊNCIA INICIAL POR TRAJETÓRIA
# ------------------------------------------------------------

tabela_inicial <- dados %>%
  group_by(trajetoria) %>%
  summarise(
    
    n = n(),
    
    media =
      mean(influencia_inicial, na.rm = TRUE),
    
    mediana =
      median(influencia_inicial, na.rm = TRUE),
    
    desvio_padrao =
      sd(influencia_inicial, na.rm = TRUE),
    
    .groups = "drop"
  )


tabela_inicial

# ------------------------------------------------------------
# 10. GRÁFICO DE BARRAS
# ------------------------------------------------------------

grafico_media <- ggplot(
  tabela_inicial,
  aes(
    x = trajetoria,
    y = media
  )
) +
  
  geom_col(
    width = 0.6
  ) +
  
  geom_text(
    aes(
      label = round(media, 2)
    ),
    vjust = -0.5,
    size = 5
  ) +
  
  scale_y_continuous(
    limits = c(0, 10),
    breaks = seq(0, 10, 2)
  ) +
  
  labs(
    title = "Influência dos pais na escolha do primeiro banco",
    subtitle = "Comparação entre jovens que permaneceram e que mudaram de instituição",
    x = NULL,
    y = "Influência parental média (0–10)"
  ) +
  
  theme_minimal(base_size = 13)


grafico_media

# ------------------------------------------------------------
# 11. BOXPLOT + RESPOSTAS INDIVIDUAIS
# ------------------------------------------------------------

grafico_distribuicao <- ggplot(
  dados,
  aes(
    x = trajetoria,
    y = influencia_inicial
  )
) +
  
  geom_boxplot(
    width = 0.45,
    outlier.shape = NA
  ) +
  
  geom_jitter(
    width = 0.10,
    height = 0,
    alpha = 0.7,
    size = 2.5
  ) +
  
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 18,
    size = 4
  ) +
  
  scale_y_continuous(
    limits = c(0, 10),
    breaks = seq(0, 10, 2)
  ) +
  
  labs(
    title = "Distribuição da influência parental na escolha inicial",
    subtitle = "Cada ponto representa um respondente",
    x = NULL,
    y = "Influência dos pais (0–10)"
  ) +
  
  theme_minimal(base_size = 13)


grafico_distribuicao

# ============================================================
# PARTE 3 — ENTRE QUEM MUDOU DE BANCO
# PRIMEIRA ESCOLHA X ESCOLHA POSTERIOR
# ============================================================


# ------------------------------------------------------------
# 12. SELECIONAR APENAS QUEM MUDOU
# ------------------------------------------------------------

mudou_banco <- dados %>%
  filter(
    mesmo_banco == "Não",
    !is.na(influencia_troca_num)
  )


# Número de pessoas nesse grupo

nrow(mudou_banco)

# ------------------------------------------------------------
# 13. MÉDIAS ENTRE QUEM MUDOU
# ------------------------------------------------------------

comparacao_mudanca <- mudou_banco %>%
  summarise(
    
    n = n(),
    
    influencia_primeira_escolha =
      mean(influencia_inicial, na.rm = TRUE),
    
    influencia_na_troca =
      mean(influencia_troca_num, na.rm = TRUE)
  )


comparacao_mudanca

# ------------------------------------------------------------
# 14. PREPARAR OS DADOS
# ------------------------------------------------------------

mudanca_long <- mudou_banco %>%
  select(
    influencia_inicial,
    influencia_troca_num
  ) %>%
  pivot_longer(
    
    cols = everything(),
    
    names_to = "momento",
    
    values_to = "influencia"
  ) %>%
  
  mutate(
    
    momento = case_when(
      
      momento == "influencia_inicial" ~
        "Escolha do primeiro banco",
      
      momento == "influencia_troca_num" ~
        "Mudança de banco"
    )
  )

# ------------------------------------------------------------
# 15. GRÁFICO: PRIMEIRA ESCOLHA X TROCA
# ------------------------------------------------------------

grafico_momentos <- ggplot(
  mudanca_long,
  aes(
    x = momento,
    y = influencia
  )
) +
  
  geom_boxplot(
    width = 0.45,
    outlier.shape = NA
  ) +
  
  geom_jitter(
    width = 0.08,
    alpha = 0.7,
    size = 2.5
  ) +
  
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 18,
    size = 4
  ) +
  
  scale_y_continuous(
    limits = c(0, 10),
    breaks = seq(0, 10, 2)
  ) +
  
  labs(
    title = "Influência parental ao longo da trajetória bancária",
    subtitle = "Comparação apenas entre respondentes que mudaram de banco",
    x = NULL,
    y = "Influência parental (0–10)"
  ) +
  
  theme_minimal(base_size = 13)


grafico_momentos

# ============================================================
# PARTE 4 — INFLUÊNCIA INICIAL E TAXA DE PERMANÊNCIA
# ============================================================


dados <- dados %>%
  mutate(
    
    nivel_influencia = case_when(
      
      influencia_inicial <= 3 ~
        "Baixa (0–3)",
      
      influencia_inicial <= 7 ~
        "Média (4–7)",
      
      influencia_inicial >= 8 ~
        "Alta (8–10)"
    ),
    
    nivel_influencia = factor(
      nivel_influencia,
      levels = c(
        "Baixa (0–3)",
        "Média (4–7)",
        "Alta (8–10)"
      )
    )
  )

# ------------------------------------------------------------
# 16. TAXA DE PERMANÊNCIA POR NÍVEL DE INFLUÊNCIA
# ------------------------------------------------------------

tabela_permanencia <- dados %>%
  group_by(nivel_influencia) %>%
  summarise(
    
    n = n(),
    
    permaneceram =
      sum(permaneceu, na.rm = TRUE),
    
    taxa_permanencia =
      mean(permaneceu, na.rm = TRUE),
    
    .groups = "drop"
  )


tabela_permanencia

# ------------------------------------------------------------
# 17. GRÁFICO: INFLUÊNCIA INICIAL E PERMANÊNCIA
# ------------------------------------------------------------

grafico_permanencia <- ggplot(
  tabela_permanencia,
  aes(
    x = nivel_influencia,
    y = taxa_permanencia
  )
) +
  
  geom_col(
    width = 0.6
  ) +
  
  geom_text(
    aes(
      label = paste0(
        round(taxa_permanencia * 100, 1),
        "%"
      )
    ),
    vjust = -0.5,
    size = 5
  ) +
  
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    labels = function(x) paste0(x * 100, "%")
  ) +
  
  labs(
    title = "Influência parental inicial e permanência no banco",
    subtitle = "Percentual que ainda utiliza a instituição em que abriu a primeira conta",
    x = "Influência dos pais na primeira escolha",
    y = "Percentual que permaneceu"
  ) +
  
  theme_minimal(base_size = 13)


grafico_permanencia

# ============================================================
# PARTE 5 — AVALIAÇÃO DA PROPOSTA BANCÁRIA
# ============================================================


# ------------------------------------------------------------
# 18. RENOMEAR VARIÁVEIS DA PROPOSTA
# ------------------------------------------------------------

dados <- dados %>%
  rename(
    
    chance_proposta =
      `Se seus pais já fossem clientes de um banco que oferecesse essa jornada, qual seria a chance de você escolher esse mesmo banco para abrir sua primeira conta?`,
    
    proposta_permanencia =
      `Considerando o contexto [Uma proposta como essa aumentaria sua disposição de permanecer nesse banco ao longo dos anos?]`,
    
    proposta_confianca =
      `Considerando o contexto [Uma proposta como essa aumentaria sua confiança no banco?]`,
    
    credito_responsavel =
      `Considerando o contexto [Saber que um banco busca evitar a oferta de limites de crédito incompatíveis com a renda e o perfil financeiro de jovens diminuiria ou aumentaria sua confiança nesse banco?]`,
    
    caracteristicas_proposta =
      `Quais características de uma proposta familiar mais aumentariam sua chance de escolher o mesmo banco dos seus pais? Selecione até 3 opções.`
  )

# ------------------------------------------------------------
# 19. RESUMO DA CHANCE DE ADESÃO À PROPOSTA
# ------------------------------------------------------------

resumo_proposta <- dados %>%
  summarise(
    n = sum(!is.na(chance_proposta)),
    media = mean(chance_proposta, na.rm = TRUE),
    mediana = median(chance_proposta, na.rm = TRUE),
    
    n_8a10 = sum(chance_proposta >= 8, na.rm = TRUE),
    
    percentual_8a10 =
      mean(chance_proposta >= 8, na.rm = TRUE) * 100
  )

resumo_proposta

# ------------------------------------------------------------
# 20. GRÁFICO — CHANCE DE ESCOLHER O BANCO COM A PROPOSTA
# ------------------------------------------------------------

grafico_proposta <- ggplot(
  dados,
  aes(x = chance_proposta)
) +
  
  geom_bar(width = 0.7) +
  
  scale_x_continuous(
    breaks = 0:10,
    limits = c(-0.5, 10.5)
  ) +
  
  labs(
    title = "Potencial de adesão à jornada financeira familiar",
    subtitle = "Chance de escolher o mesmo banco dos pais caso ele oferecesse a proposta",
    x = "Chance de escolher o banco (0–10)",
    y = "Número de respondentes"
  ) +
  
  theme_minimal(base_size = 13)

grafico_proposta

# ------------------------------------------------------------
# 21. DISTRIBUIÇÃO DAS RESPOSTAS À PROPOSTA
# ------------------------------------------------------------

tabela_avaliacao <- dados %>%
  select(
    proposta_permanencia,
    proposta_confianca,
    credito_responsavel
  ) %>%
  
  pivot_longer(
    cols = everything(),
    names_to = "dimensao",
    values_to = "resposta"
  ) %>%
  
  mutate(
    dimensao = case_when(
      
      dimensao == "proposta_permanencia" ~
        "Permanência no banco",
      
      dimensao == "proposta_confianca" ~
        "Confiança no banco",
      
      dimensao == "credito_responsavel" ~
        "Confiança com crédito responsável"
    ),
    
    resposta = factor(
      resposta,
      levels = c(
        "Diminuiria muito",
        "Diminuiria",
        "Não faria diferença",
        "Aumentaria",
        "Aumentaria muito"
      )
    )
  )

tabela_avaliacao_resumo <- tabela_avaliacao %>%
  count(dimensao, resposta) %>%
  group_by(dimensao) %>%
  mutate(
    percentual = n / sum(n) * 100
  )

tabela_avaliacao_resumo

# ------------------------------------------------------------
# 22. GRÁFICO — AVALIAÇÃO DAS DIMENSÕES DA PROPOSTA
# ------------------------------------------------------------

grafico_avaliacao <- ggplot(
  tabela_avaliacao_resumo,
  aes(
    x = dimensao,
    y = percentual,
    fill = resposta
  )
) +
  
  geom_col() +
  
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%")
  ) +
  
  labs(
    title = "Avaliação da proposta de jornada financeira familiar",
    subtitle = "Impacto percebido sobre permanência e confiança",
    x = NULL,
    y = "Percentual dos respondentes",
    fill = "Resposta"
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    axis.text.x = element_text(
      angle = 15,
      hjust = 1
    )
  )

grafico_avaliacao

# ------------------------------------------------------------
# 23. CARACTERÍSTICAS MAIS VALORIZADAS
# ------------------------------------------------------------

caracteristicas <- dados %>%
  select(caracteristicas_proposta) %>%
  
  separate_rows(
    caracteristicas_proposta,
    sep = ", "
  ) %>%
  
  filter(
    !is.na(caracteristicas_proposta)
  ) %>%
  
  count(
    caracteristicas_proposta,
    sort = TRUE
  ) %>%
  
  mutate(
    percentual = n / nrow(dados) * 100
  )

caracteristicas

# ------------------------------------------------------------
# 24. GRÁFICO — CARACTERÍSTICAS MAIS VALORIZADAS
# ------------------------------------------------------------

grafico_caracteristicas <- ggplot(
  caracteristicas,
  aes(
    x = reorder(caracteristicas_proposta, n),
    y = n
  )
) +
  
  geom_col() +
  
  geom_text(
    aes(label = n),
    hjust = -0.3
  ) +
  
  coord_flip() +
  
  labs(
    title = "Características mais valorizadas da proposta",
    subtitle = "Cada respondente pôde selecionar até três alternativas",
    x = NULL,
    y = "Número de respondentes"
  ) +
  
  theme_minimal(base_size = 13)

grafico_caracteristicas

