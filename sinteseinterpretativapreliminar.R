# ============================================================
# PARTE 8 — SÍNTESE INTERPRETATIVA PRELIMINAR
# ============================================================


# ------------------------------------------------------------
# 38. EXTRAIR OS PRINCIPAIS RESULTADOS
# ------------------------------------------------------------

# Número total de respondentes
n_total <- nrow(dados)


# Influência inicial entre quem permaneceu
n_permaneceu <- tabela_inicial %>%
  filter(trajetoria == "Permaneceu no primeiro banco") %>%
  pull(n)

media_permaneceu <- tabela_inicial %>%
  filter(trajetoria == "Permaneceu no primeiro banco") %>%
  pull(media)


# Influência inicial entre quem mudou
n_mudou <- tabela_inicial %>%
  filter(trajetoria == "Mudou de banco") %>%
  pull(n)

media_mudou <- tabela_inicial %>%
  filter(trajetoria == "Mudou de banco") %>%
  pull(media)


# Entre quem mudou:
# influência dos pais na primeira escolha
# versus influência na mudança posterior

media_inicial_mudou <- comparacao_mudanca %>%
  pull(influencia_primeira_escolha)

media_troca <- comparacao_mudanca %>%
  pull(influencia_na_troca)


# Avaliação da proposta

media_proposta <- resumo_proposta %>%
  pull(media)

pct_proposta_8a10 <- resumo_proposta %>%
  pull(percentual_8a10)


# Percentual que disse que a proposta aumentaria
# ou aumentaria muito a permanência

pct_aumenta_permanencia <- tabela_avaliacao_resumo %>%
  filter(
    dimensao == "Permanência no banco",
    resposta %in% c("Aumentaria", "Aumentaria muito")
  ) %>%
  summarise(
    valor = sum(percentual)
  ) %>%
  pull(valor)


# Percentual que disse que a proposta aumentaria
# ou aumentaria muito a confiança

pct_aumenta_confianca <- tabela_avaliacao_resumo %>%
  filter(
    dimensao == "Confiança no banco",
    resposta %in% c("Aumentaria", "Aumentaria muito")
  ) %>%
  summarise(
    valor = sum(percentual)
  ) %>%
  pull(valor)


# ------------------------------------------------------------
# RESULTADOS DA MICROSSIMULAÇÃO
# ------------------------------------------------------------

pct_sem_pais <- resultado_simulacao %>%
  pull(percentual_sem_pais)

pct_com_pais <- resultado_simulacao %>%
  pull(percentual_com_pais)

delta_n <- resultado_simulacao %>%
  pull(delta_N)

delta_pp <- resultado_simulacao %>%
  pull(delta_percentual)

N_simulado <- resultado_simulacao %>%
  pull(N)

# ------------------------------------------------------------
# 39. FUNÇÃO PARA FORMATAR NÚMEROS
# ------------------------------------------------------------

fmt <- function(x, casas = 2) {
  formatC(
    x,
    format = "f",
    digits = casas,
    decimal.mark = ","
  )
}

# ------------------------------------------------------------
# 40. SÍNTESE INTERPRETATIVA PRELIMINAR
# ------------------------------------------------------------

cat(
  "\n\n",
  "SÍNTESE INTERPRETATIVA PRELIMINAR\n\n",
  
  "A pesquisa de campo conta, até o momento, com ",
  n_total,
  " respondentes e fornece evidências preliminares sobre o papel da influência ",
  "familiar na formação da trajetória bancária dos jovens. O parâmetro de influência ",
  "parental utilizado na microssimulação foi calibrado em γ = ",
  fmt(gamma, 2),
  ", em uma escala de 0 a 10. Esse valor corresponde à influência média declarada dos pais ",
  "na escolha do primeiro banco, considerando todos os respondentes, e deve ser interpretado como uma ",
  "calibração exploratória do canal de influência familiar, e não como uma estimativa causal.\n\n",
  
  "Os resultados sugerem que a influência parental é particularmente relevante no ",
  "momento de entrada do jovem no sistema bancário. Entre os ",
  n_permaneceu,
  " respondentes que ainda permanecem na instituição em que abriram sua primeira conta, ",
  "a influência média dos pais na escolha inicial foi de ",
  fmt(media_permaneceu, 2),
  " em 10. Entre os ",
  n_mudou,
  " jovens que posteriormente mudaram de banco, essa média foi de ",
  fmt(media_mudou, 2),
  ". A diferença indica uma associação entre maior influência familiar na escolha inicial ",
  "e maior permanência posterior na instituição. Como a pesquisa é observacional e possui ",
  "amostra reduzida e não probabilística, esse resultado não permite afirmar que a ",
  "influência dos pais seja a causa da permanência, mas aponta para um mecanismo relevante ",
  "para a estratégia geracional proposta.\n\n",
  
  "A comparação entre diferentes momentos da trajetória bancária reforça essa interpretação. ",
  "Considerando apenas os respondentes que efetivamente mudaram de banco, a influência média ",
  "dos pais na escolha da primeira instituição foi de ",
  fmt(media_inicial_mudou, 2),
  ", enquanto a influência parental na decisão posterior de mudança foi de ",
  fmt(media_troca, 2),
  ". Esse padrão sugere que a influência familiar tende a estar mais concentrada no início ",
  "da trajetória financeira, quando o jovem possui menor autonomia e frequentemente depende ",
  "dos responsáveis para acessar produtos bancários. À medida que ganha independência, outros ",
  "atributos da instituição podem assumir maior importância nas decisões de troca.\n\n",
  
  "Essa evidência é especialmente relevante para a proposta do projeto. Em vez de depender ",
  "da influência dos pais durante toda a vida financeira do jovem, a estratégia pode utilizar ",
  "o relacionamento já existente com a família como um canal de aquisição no momento em que ",
  "essa influência parece ser mais elevada. A instituição teria, então, o desafio de transformar ",
  "essa entrada inicial em um relacionamento de longo prazo por meio de uma jornada adequada à ",
  "transição para a autonomia financeira.\n\n",
  
  "A avaliação direta da proposta pelos respondentes também apresenta resultados favoráveis. ",
  "Quando questionados sobre a possibilidade de escolher o mesmo banco dos pais caso a instituição ",
  "oferecesse uma jornada financeira familiar com entrada facilitada, orientação e crédito ",
  "compatível com renda e perfil, a chance média declarada de escolha foi de ",
  fmt(media_proposta, 2),
  " em 10. Além disso, ",
  fmt(pct_proposta_8a10, 1),
  "% dos respondentes atribuíram notas entre 8 e 10 à possibilidade de escolher essa instituição. ",
  "Em relação à retenção, ",
  fmt(pct_aumenta_permanencia, 1),
  "% afirmaram que a proposta aumentaria ou aumentaria muito sua disposição de permanecer no banco, ",
  "enquanto ",
  fmt(pct_aumenta_confianca, 1),
  "% indicaram aumento de confiança na instituição.\n\n",
  
  "Por fim, a microssimulação utiliza γ = ",
  fmt(gamma, 2),
  " para avaliar de que maneira esse canal de influência parental pode alterar a escolha entre ",
  "uma política de crédito agressiva e uma política de crédito responsável. No cenário sem ",
  "influência parental, ",
  fmt(pct_sem_pais, 1),
  "% dos jovens simulados escolheram o banco responsável. Com a incorporação do canal de ",
  "influência familiar, esse percentual passou para ",
  fmt(pct_com_pais, 1),
  "%, uma diferença de ",
  fmt(delta_pp, 1),
  " pontos percentuais, correspondente a ",
  delta_n,
  " jovens adicionais em uma população simulada de ",
  N_simulado,
  " indivíduos. Esse resultado deve ser interpretado como uma análise de cenário condicionada ",
  "às hipóteses adotadas para os demais parâmetros da microssimulação, e não como uma previsão ",
  "do impacto real da política.\n\n",
  
  "Em conjunto, os resultados preliminares apontam para uma lógica estratégica coerente: ",
  "a família pode funcionar como um canal relevante de aquisição no início da trajetória ",
  "financeira; uma maior influência parental na escolha inicial aparece associada a maior ",
  "permanência posterior; e os próprios jovens demonstram elevada receptividade a uma proposta ",
  "que combine relacionamento familiar, autonomia progressiva e crédito responsável. Assim, ",
  "a estratégia geracional pode permitir ao banco utilizar o relacionamento já construído com ",
  "os pais para antecipar a aquisição do jovem e, posteriormente, sustentar sua permanência por ",
  "meio de uma proposta de valor adequada à sua crescente autonomia financeira.",
  
  "\n\n",
  sep = ""
)