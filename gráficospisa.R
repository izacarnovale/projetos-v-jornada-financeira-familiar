# ============================================================
# PISA 2022 - GRÁFICOS DO PROJETO
# BRASIL | FINANCIAL LITERACY
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(haven)

# ============================================================
# 0. CONFIGURAÇÕES GERAIS
# ============================================================

tema_pisa <- theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

dummy_ge <- function(x, limite) {
  x_num <- as.integer(x)
  ifelse(is.na(x_num), NA_real_, as.numeric(x_num >= limite))
}

dummy_sim <- function(x) {
  x_num <- as.integer(x)
  ifelse(is.na(x_num), NA_real_, as.numeric(x_num == 1))
}

resumir_binarias <- function(base, variaveis, rotulos) {
  base %>%
    select(grupo_conversa_pais, all_of(variaveis)) %>%
    filter(!is.na(grupo_conversa_pais)) %>%
    pivot_longer(
      cols = all_of(variaveis),
      names_to = "indicador",
      values_to = "resultado"
    ) %>%
    group_by(grupo_conversa_pais, indicador) %>%
    summarise(
      percentual = mean(resultado, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      indicador = factor(
        indicador,
        levels = variaveis,
        labels = unname(rotulos[variaveis])
      )
    )
}

plot_comparacao <- function(dados, titulo, subtitulo) {
  ggplot(
    dados,
    aes(
      x = indicador,
      y = percentual,
      fill = grupo_conversa_pais
    )
  ) +
    geom_col(
      position = position_dodge(width = 0.8),
      width = 0.7
    ) +
    geom_text(
      aes(label = percent(percentual, accuracy = 0.1)),
      position = position_dodge(width = 0.8),
      vjust = -0.3,
      size = 3.4
    ) +
    scale_y_continuous(
      labels = percent,
      expand = expansion(mult = c(0, 0.12))
    ) +
    labs(
      title = titulo,
      subtitle = subtitulo,
      x = NULL,
      y = "Percentual dos estudantes",
      fill = "Frequência de conversa\ncom pais ou responsáveis",
      caption = "Fonte: PISA 2022 – Financial Literacy, Brasil"
    ) +
    tema_pisa
}


# ============================================================
# 1. BASE DE ANÁLISE COMUM AOS GRÁFICOS 3 A 7
# ============================================================

vars_conversa_pais <- c(
  "conversa_pais_sobre_gastos",
  "conversa_pais_sobre_poupanca",
  "conversa_pais_sobre_orcamento_familiar",
  "conversa_pais_sobre_economia_financas",
  "conversa_pais_sobre_compras_online"
)

pisa_analise <- pisa_brasil %>%
  mutate(
    indice_conversa_pais = rowMeans(
      across(all_of(vars_conversa_pais), ~ as.integer(.x)),
      na.rm = TRUE
    ),

    grupo_conversa_pais = case_when(
      !is.finite(indice_conversa_pais) ~ NA_character_,
      indice_conversa_pais >= 3 ~ "Conversa mais frequentemente",
      TRUE ~ "Conversa menos frequentemente"
    ),

    # Comportamentos financeiros
    guarda_banco_frequente = dummy_ge(guarda_dinheiro_banco, 4),
    verifica_dinheiro_frequente = dummy_ge(verifica_dinheiro_disponivel, 4),
    gastou_mais_frequente = dummy_ge(gastou_mais_que_planejado, 4),
    influencia_amigos_frequente = dummy_ge(comprou_por_influencia_amigos, 4),

    # Inclusão bancária
    conta_dummy = dummy_sim(tem_conta_bancaria),
    cartao_dummy = dummy_sim(tem_cartao_debito),
    app_dummy = dummy_sim(tem_app_bancario),

    # Confiança financeira
    transferencia_seguro = dummy_ge(confianca_transferencia_bancaria, 3),
    extrato_seguro = dummy_ge(confianca_entender_extrato, 3),
    saldo_seguro = dummy_ge(confianca_verificar_saldo, 3),
    planejamento_seguro = dummy_ge(confianca_planejar_despesas, 3),
    contrato_seguro = dummy_ge(confianca_entender_contrato, 3),

    # Planejamento e poupança
    metas_positivo = dummy_ge(define_metas_poupanca, 3),
    longo_prazo_positivo = dummy_ge(consegue_objetivos_longo_prazo, 3),

    # Risco / impulsividade
    compra_emocao = dummy_ge(compra_por_emocao, 3),
    poupa_so_sobra = dummy_ge(poupa_so_se_sobrar, 3),
    influencia_amigos = dummy_ge(amigos_influenciam_gastos, 3),
    gasta_mais_amigos = dummy_ge(gasta_mais_com_amigos, 3),
    excede_planejado = dummy_ge(gastou_mais_que_planejado, 4)
  )


# ============================================================
# GRÁFICO 1
# INCLUSÃO BANCÁRIA ENTRE JOVENS BRASILEIROS
# ============================================================

dados_g1_inclusao <- pisa_brasil %>%
  select(
    tem_conta_bancaria,
    tem_cartao_debito,
    tem_app_bancario
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "indicador",
    values_to = "resposta"
  ) %>%
  filter(!is.na(resposta)) %>%
  count(indicador, resposta) %>%
  group_by(indicador) %>%
  mutate(percentual = n / sum(n)) %>%
  ungroup() %>%
  mutate(
    indicador = recode(
      indicador,
      tem_conta_bancaria = "Conta bancária",
      tem_cartao_debito = "Cartão de débito",
      tem_app_bancario = "Aplicativo bancário"
    )
  )

g1_inclusao <- ggplot(
  dados_g1_inclusao,
  aes(
    x = indicador,
    y = percentual,
    fill = resposta
  )
) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = percent(percentual, accuracy = 0.1)),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 3.5
  ) +
  scale_y_continuous(
    labels = percent,
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    title = "Inclusão bancária entre jovens brasileiros",
    subtitle = "PISA 2022 – Financial Literacy",
    x = NULL,
    y = "Percentual dos estudantes",
    fill = "Resposta"
  ) +
  tema_pisa

print(g1_inclusao)


# ============================================================
# GRÁFICO 2
# FREQUÊNCIA DE CONVERSAS SOBRE DINHEIRO COM OS PAIS
# ============================================================

dados_g2_conversas <- pisa_brasil %>%
  select(
    conversa_pais_sobre_gastos,
    conversa_pais_sobre_poupanca,
    conversa_pais_sobre_orcamento_familiar,
    conversa_pais_sobre_economia_financas,
    conversa_pais_sobre_compras_online
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "tema",
    values_to = "frequencia"
  ) %>%
  filter(!is.na(frequencia)) %>%
  count(tema, frequencia) %>%
  group_by(tema) %>%
  mutate(percentual = n / sum(n)) %>%
  ungroup() %>%
  mutate(
    tema = recode(
      tema,
      conversa_pais_sobre_gastos = "Decisões de gastos",
      conversa_pais_sobre_poupanca = "Poupança",
      conversa_pais_sobre_orcamento_familiar = "Orçamento familiar",
      conversa_pais_sobre_economia_financas = "Economia e finanças",
      conversa_pais_sobre_compras_online = "Compras online"
    )
  )

g2_conversas <- ggplot(
  dados_g2_conversas,
  aes(
    x = tema,
    y = percentual,
    fill = frequencia
  )
) +
  geom_col() +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Frequência de conversas sobre dinheiro com pais ou responsáveis",
    subtitle = "PISA 2022 – Brasil",
    x = NULL,
    y = "Percentual dos estudantes",
    fill = "Frequência"
  ) +
  tema_pisa +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

print(g2_conversas)


# ============================================================
# GRÁFICO 3
# CONVERSAS COM OS PAIS X COMPORTAMENTO FINANCEIRO
# ============================================================

vars_g3 <- c(
  "guarda_banco_frequente",
  "verifica_dinheiro_frequente",
  "gastou_mais_frequente",
  "influencia_amigos_frequente"
)

rotulos_g3 <- c(
  guarda_banco_frequente = "Guarda dinheiro\nno banco",
  verifica_dinheiro_frequente = "Verifica quanto\ndinheiro possui",
  gastou_mais_frequente = "Gasta mais que\no planejado",
  influencia_amigos_frequente = "Compra por influência\ndos amigos"
)

dados_g3_comportamento <- resumir_binarias(
  pisa_analise,
  vars_g3,
  rotulos_g3
)

g3_comportamento <- plot_comparacao(
  dados_g3_comportamento,
  titulo = "Conversas financeiras com os pais e comportamento dos jovens",
  subtitulo = "Percentual que realiza cada comportamento ao menos uma ou duas vezes por semana"
)

print(g3_comportamento)


# ============================================================
# GRÁFICO 4
# CONVERSAS COM OS PAIS X INCLUSÃO BANCÁRIA
# ============================================================

vars_g4 <- c(
  "conta_dummy",
  "cartao_dummy",
  "app_dummy"
)

rotulos_g4 <- c(
  conta_dummy = "Conta bancária",
  cartao_dummy = "Cartão de débito",
  app_dummy = "Aplicativo bancário"
)

dados_g4_inclusao_pais <- resumir_binarias(
  pisa_analise,
  vars_g4,
  rotulos_g4
)

g4_inclusao_pais <- plot_comparacao(
  dados_g4_inclusao_pais,
  titulo = "Conversas financeiras com os pais e inclusão bancária",
  subtitulo = "Posse de conta bancária, cartão de débito e aplicativo bancário"
)

print(g4_inclusao_pais)


# ============================================================
# GRÁFICO 5
# CONVERSAS COM OS PAIS X CONFIANÇA FINANCEIRA
# ============================================================

vars_g5 <- c(
  "transferencia_seguro",
  "extrato_seguro",
  "saldo_seguro",
  "planejamento_seguro",
  "contrato_seguro"
)

rotulos_g5 <- c(
  transferencia_seguro = "Fazer\ntransferência",
  extrato_seguro = "Entender\nextrato",
  saldo_seguro = "Verificar\nsaldo",
  planejamento_seguro = "Planejar\ndespesas",
  contrato_seguro = "Entender\ncontrato"
)

dados_g5_confianca <- resumir_binarias(
  pisa_analise,
  vars_g5,
  rotulos_g5
)

g5_confianca <- plot_comparacao(
  dados_g5_confianca,
  titulo = "Conversas financeiras com os pais e confiança financeira",
  subtitulo = "Percentual de jovens que se sentem seguros ou muito seguros"
)

print(g5_confianca)


# ============================================================
# GRÁFICO 6
# CONVERSAS COM OS PAIS X PLANEJAMENTO E POUPANÇA
# ============================================================

vars_g6 <- c(
  "metas_positivo",
  "longo_prazo_positivo",
  "verifica_dinheiro_frequente",
  "guarda_banco_frequente"
)

rotulos_g6 <- c(
  metas_positivo = "Define metas\nde poupança",
  longo_prazo_positivo = "Busca objetivos\nde longo prazo",
  verifica_dinheiro_frequente = "Verifica quanto\ndinheiro possui",
  guarda_banco_frequente = "Guarda dinheiro\nno banco"
)

dados_g6_planejamento <- resumir_binarias(
  pisa_analise,
  vars_g6,
  rotulos_g6
)

g6_planejamento <- plot_comparacao(
  dados_g6_planejamento,
  titulo = "Conversas financeiras com os pais e planejamento financeiro",
  subtitulo = "Indicadores de poupança, monitoramento e orientação de longo prazo"
)

print(g6_planejamento)


# ============================================================
# GRÁFICO 7
# CONVERSAS COM OS PAIS X IMPULSIVIDADE / RISCO FINANCEIRO
# ============================================================

vars_g7 <- c(
  "compra_emocao",
  "poupa_so_sobra",
  "influencia_amigos",
  "gasta_mais_amigos",
  "excede_planejado"
)

rotulos_g7 <- c(
  compra_emocao = "Compra por\nemoção",
  poupa_so_sobra = "Poupa apenas\nse sobrar",
  influencia_amigos = "Amigos influenciam\nos gastos",
  gasta_mais_amigos = "Gasta mais\ncom amigos",
  excede_planejado = "Gasta mais que\no planejado"
)

dados_g7_risco <- resumir_binarias(
  pisa_analise,
  vars_g7,
  rotulos_g7
)

g7_risco <- plot_comparacao(
  dados_g7_risco,
  titulo = "Conversas financeiras com os pais e comportamentos de risco",
  subtitulo = "Indicadores de impulsividade e influência social sobre os gastos"
)

print(g7_risco)


# ============================================================
# GRÁFICO 8
# CONVERSAS COM OS PAIS X LETRAMENTO FINANCEIRO
# SOMENTE BRASIL
# ============================================================

# A base bruta QQQ precisa estar na pasta do projeto.
pisa_qqq <- read_sas("FLT_SAS/CY08MSP_FLT_QQQ.SAS7BDAT")

vars_fl <- paste0("PV", 1:10, "FLIT")

vars_conversa_raw <- c(
  "FL167Q01HA",
  "FL167Q02HA",
  "FL167Q03HA",
  "FL167Q04HA",
  "FL167Q05HA",
  "FL167Q06JA",
  "FL167Q07JA"
)

dados_g8_literacia <- pisa_qqq %>%
  filter(CNT == "BRA") %>%
  mutate(
    across(
      all_of(c(vars_conversa_raw, vars_fl)),
      ~ as.numeric(zap_labels(.x))
    ),

    indice_conversa_pais = rowMeans(
      across(all_of(vars_conversa_raw)),
      na.rm = TRUE
    ),

    financial_literacy = rowMeans(
      across(all_of(vars_fl)),
      na.rm = TRUE
    ),

    grupo_conversa = case_when(
      !is.finite(indice_conversa_pais) ~ NA_character_,
      indice_conversa_pais < 1.5 ~ "Nunca ou quase nunca",
      indice_conversa_pais < 2.5 ~ "Uma ou duas vezes por mês",
      indice_conversa_pais < 3.5 ~ "Uma ou duas vezes por semana",
      TRUE ~ "Quase todos os dias"
    ),

    grupo_conversa = factor(
      grupo_conversa,
      levels = c(
        "Nunca ou quase nunca",
        "Uma ou duas vezes por mês",
        "Uma ou duas vezes por semana",
        "Quase todos os dias"
      )
    )
  ) %>%
  filter(
    !is.na(grupo_conversa),
    is.finite(financial_literacy)
  ) %>%
  group_by(grupo_conversa) %>%
  summarise(
    nota_media = mean(financial_literacy, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

g8_literacia <- ggplot(
  dados_g8_literacia,
  aes(
    x = grupo_conversa,
    y = nota_media,
    group = 1
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_text(
    aes(label = round(nota_media, 1)),
    vjust = -0.8,
    size = 4
  ) +
  labs(
    title = "Conversas financeiras com os pais e letramento financeiro",
    subtitle = "Pontuação média no PISA 2022 segundo a frequência média de conversas financeiras em família",
    x = "Frequência média de conversas financeiras com pais ou responsáveis",
    y = "Pontuação média em letramento financeiro",
    caption = "Fonte: PISA 2022 – Financial Literacy, Brasil"
  ) +
  tema_pisa +
  theme(
    axis.text.x = element_text(angle = 20, hjust = 1)
  )

print(g8_literacia)

# Conferência dos valores do gráfico 8
dados_g8_literacia
'''

path = Path("/mnt/data/graficos_pisa_limpo.R")
path.write_text(codigo, encoding="utf-8")

print(f"Arquivo criado: {path}")
