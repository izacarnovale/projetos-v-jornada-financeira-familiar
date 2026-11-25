# ============================================================
# PISA 2022 - FINANCIAL LITERACY - BRASIL
# BASE ORGANIZADA PARA EXCEL
# ============================================================

library(haven)
library(dplyr)
library(writexl)

# ------------------------------------------------------------
# 1. IMPORTAR BASE
# ------------------------------------------------------------

pisa_fl <- read_sas("FLT_SAS/CY08MSP_FLT_QQQ.SAS7BDAT")


# ------------------------------------------------------------
# 2. FILTRAR BRASIL E QUESTIONÁRIO FINANCEIRO
# ------------------------------------------------------------

pisa_brasil <- pisa_fl %>%
  filter(CNT == "BRA") %>%
  select(
    CNT,
    CNTSTUID,
    CNTSCHID,
    starts_with("FL150"),
    starts_with("FL159"),
    starts_with("FL160"),
    starts_with("FL161"),
    starts_with("FL162"),
    starts_with("FL163"),
    starts_with("FL164"),
    starts_with("FL166"),
    starts_with("FL167"),
    starts_with("FL169"),
    starts_with("FL170"),
    starts_with("FL171"),
    starts_with("FL172"),
    starts_with("FL174")
  )


# ------------------------------------------------------------
# 3. TRANSFORMAR LABELS SAS EM RESPOSTAS LEGÍVEIS
# ------------------------------------------------------------

pisa_brasil <- pisa_brasil %>%
  mutate(
    across(
      starts_with("FL"),
      ~ haven::as_factor(.)
    )
  )


# ------------------------------------------------------------
# 4. RENOMEAR VARIÁVEIS
# ------------------------------------------------------------

pisa_brasil <- pisa_brasil %>%
  
  rename(
    
    # --------------------------------------------------------
    # IDENTIFICAÇÃO
    # --------------------------------------------------------
    
    pais = CNT,
    id_estudante = CNTSTUID,
    id_escola = CNTSCHID,
    
    
    # --------------------------------------------------------
    # FL150 - ONDE APRENDEU A ADMINISTRAR DINHEIRO
    # --------------------------------------------------------
    
    educ_fin_disciplina_especifica = FL150Q01TA,
    educ_fin_outra_disciplina = FL150Q02TA,
    educ_fin_fora_escola = FL150Q03TA,
    
    
    # --------------------------------------------------------
    # FL159 - AUTONOMIA FINANCEIRA
    # --------------------------------------------------------
    
    decide_sozinho_como_gastar = FL159Q01HA,
    permissao_pais_gastos_maiores = FL159Q02HA,
    permissao_pais_qualquer_gasto = FL159Q03HA,
    responsavel_pelo_proprio_dinheiro = FL159Q04HA,
    
    
    # --------------------------------------------------------
    # FL160 - COMPORTAMENTO DE COMPRA
    # --------------------------------------------------------
    
    compara_precos_lojas = FL160Q01HA,
    compara_loja_e_online = FL160Q02HA,
    compra_sem_comparar_precos = FL160Q03HA,
    espera_preco_diminuir = FL160Q04HA,
    
    
    # --------------------------------------------------------
    # FL161 - INCLUSÃO BANCÁRIA
    # --------------------------------------------------------
    
    tem_conta_bancaria = FL161Q01HA,
    tem_cartao_debito = FL161Q02HA,
    tem_app_bancario = FL161Q03HA,
    
    
    # --------------------------------------------------------
    # FL162 - CONFIANÇA EM ATIVIDADES FINANCEIRAS
    # --------------------------------------------------------
    
    confianca_transferencia_bancaria = FL162Q01HA,
    confianca_formularios_banco = FL162Q02HA,
    confianca_entender_extrato = FL162Q03HA,
    confianca_entender_contrato = FL162Q04HA,
    confianca_verificar_saldo = FL162Q05HA,
    confianca_planejar_despesas = FL162Q06HA,
    
    
    # --------------------------------------------------------
    # FL163 - SERVIÇOS FINANCEIROS DIGITAIS
    # --------------------------------------------------------
    
    confianca_transferencia_digital = FL163Q01HA,
    confianca_saldo_digital = FL163Q02HA,
    confianca_pagamento_debito = FL163Q03HA,
    confianca_pagamento_celular = FL163Q04HA,
    confianca_seguranca_online = FL163Q05HA,
    
    
    # --------------------------------------------------------
    # FL164 - CONCEITOS FINANCEIROS
    # --------------------------------------------------------
    
    conhece_pagamento_juros = FL164Q01HA,
    conhece_juros_compostos = FL164Q02HA,
    conhece_taxa_cambio = FL164Q03HA,
    conhece_depreciacao = FL164Q04HA,
    conhece_acoes = FL164Q05HA,
    conhece_retorno_investimentos = FL164Q06HA,
    conhece_dividendos = FL164Q07HA,
    conhece_diversificacao = FL164Q08HA,
    conhece_cartao_debito = FL164Q09HA,
    conhece_emprestimo_bancario = FL164Q10HA,
    conhece_aposentadoria = FL164Q11HA,
    conhece_orcamento = FL164Q12HA,
    conhece_salario = FL164Q13HA,
    conhece_empresario = FL164Q14HA,
    conhece_banco_central = FL164Q15HA,
    conhece_imposto_renda = FL164Q16HA,
    
    
    # --------------------------------------------------------
    # FL166 - ATIVIDADES FINANCEIRAS NA ESCOLA
    # --------------------------------------------------------
    
    escola_objetivo_dinheiro = FL166Q01HA,
    escola_necessidade_vs_desejo = FL166Q02HA,
    escola_planejamento_despesa = FL166Q03HA,
    escola_direitos_consumidor = FL166Q05HA,
    escola_mercado_acoes = FL166Q06HA,
    escola_analise_publicidade = FL166Q07HA,
    
    
    # --------------------------------------------------------
    # FL167 - CONVERSAS COM PAIS / FAMÍLIA
    # --------------------------------------------------------
    
    conversa_pais_sobre_gastos = FL167Q01HA,
    conversa_pais_sobre_poupanca = FL167Q02HA,
    conversa_pais_sobre_mesada = FL167Q06JA,
    conversa_pais_sobre_orcamento_familiar = FL167Q03HA,
    conversa_pais_sobre_compras = FL167Q04HA,
    conversa_pais_sobre_economia_financas = FL167Q05HA,
    conversa_pais_sobre_compras_online = FL167Q07JA,
    
    
    # --------------------------------------------------------
    # FL169 - ATITUDES FINANCEIRAS
    # --------------------------------------------------------
    
    gosta_falar_financas = FL169Q01HA,
    sabe_gerenciar_dinheiro = FL169Q05JA,
    jovens_devem_decidir_gastos = FL169Q02HA,
    compra_por_emocao = FL169Q06JA,
    poupa_so_se_sobrar = FL169Q07JA,
    financas_nao_relevantes = FL169Q03HA,
    quer_ter_negocio = FL169Q04HA,
    consegue_objetivos_longo_prazo = FL169Q08JA,
    define_metas_poupanca = FL169Q10JA,
    dinheiro_facilita_controle_gastos = FL169Q11JA,
    
    
    # --------------------------------------------------------
    # FL170 - FONTES DE DINHEIRO
    # --------------------------------------------------------
    
    renda_mesada_com_tarefas = FL170Q01JA,
    renda_mesada_sem_tarefas = FL170Q02JA,
    renda_trabalho_fora_escola = FL170Q03JA,
    renda_negocio_familia = FL170Q04JA,
    renda_trabalho_informal = FL170Q05JA,
    renda_presentes = FL170Q06JA,
    renda_venda_produtos = FL170Q07JA,
    
    
    # --------------------------------------------------------
    # FL171 - COMPORTAMENTOS FINANCEIROS
    # --------------------------------------------------------
    
    confere_troco = FL171Q01JA,
    dinheiro_insuficiente = FL171Q02JA,
    compra_online = FL171Q03JA,
    pagamento_smartphone = FL171Q04JA,
    pagamento_cartao = FL171Q05JA,
    gastou_mais_que_planejado = FL171Q07JA,
    verifica_dinheiro_disponivel = FL171Q08JA,
    comprou_por_influencia_amigos = FL171Q09JA,
    envia_dinheiro_smartphone = FL171Q10JA,
    guarda_dinheiro_banco = FL171Q11JA,
    guarda_dinheiro_casa = FL171Q12JA,
    
    
    # --------------------------------------------------------
    # FL172 - INFLUÊNCIA DOS AMIGOS
    # --------------------------------------------------------
    
    amigos_influenciam_gastos = FL172Q01JA,
    quer_acompanhar_estilo_amigos = FL172Q03JA,
    gasta_mais_com_amigos = FL172Q05JA,
    compra_recomendacao_amigos = FL172Q06JA,
    
    
    # --------------------------------------------------------
    # FL174 - ONDE RECEBEU EDUCAÇÃO FINANCEIRA
    # --------------------------------------------------------
    
    educ_fin_matematica = FL174Q01JA,
    educ_fin_ciencias_humanas = FL174Q02JA,
    educ_fin_cidadania = FL174Q03JA,
    educ_fin_economia_negocios = FL174Q04JA,
    educ_fin_em_outra_aula = FL174Q05JA,
    educ_fin_visitante_escola = FL174Q06JA,
    educ_fin_extracurricular = FL174Q07JA
  )


# ------------------------------------------------------------
# 5. CRIAR DICIONÁRIO DE VARIÁVEIS
# ------------------------------------------------------------

dicionario <- tibble(
  
  variavel = c(
    
    "tem_conta_bancaria",
    "tem_cartao_debito",
    "tem_app_bancario",
    
    "conversa_pais_sobre_gastos",
    "conversa_pais_sobre_poupanca",
    "conversa_pais_sobre_mesada",
    "conversa_pais_sobre_orcamento_familiar",
    "conversa_pais_sobre_compras",
    "conversa_pais_sobre_economia_financas",
    "conversa_pais_sobre_compras_online",
    
    "sabe_gerenciar_dinheiro",
    "compra_por_emocao",
    "poupa_so_se_sobrar",
    "consegue_objetivos_longo_prazo",
    "define_metas_poupanca",
    
    "gastou_mais_que_planejado",
    "verifica_dinheiro_disponivel",
    "guarda_dinheiro_banco",
    "guarda_dinheiro_casa",
    
    "amigos_influenciam_gastos",
    "gasta_mais_com_amigos"
  ),
  
  codigo_pisa = c(
    
    "FL161Q01HA",
    "FL161Q02HA",
    "FL161Q03HA",
    
    "FL167Q01HA",
    "FL167Q02HA",
    "FL167Q06JA",
    "FL167Q03HA",
    "FL167Q04HA",
    "FL167Q05HA",
    "FL167Q07JA",
    
    "FL169Q05JA",
    "FL169Q06JA",
    "FL169Q07JA",
    "FL169Q08JA",
    "FL169Q10JA",
    
    "FL171Q07JA",
    "FL171Q08JA",
    "FL171Q11JA",
    "FL171Q12JA",
    
    "FL172Q01JA",
    "FL172Q05JA"
  ),
  
  descricao = c(
    
    "Possui conta em banco, instituição de crédito imobiliário ou cooperativa de crédito.",
    "Possui cartão de pagamento ou cartão de débito.",
    "Possui aplicativo de celular para acessar sua conta.",
    
    "Frequência com que discute com os pais suas decisões sobre como gastar dinheiro.",
    "Frequência com que discute com os pais suas decisões relacionadas à poupança.",
    "Frequência com que discute com os pais como usar auxílio ou mesada.",
    "Frequência com que discute com os pais o orçamento familiar.",
    "Frequência com que discute com os pais dinheiro para coisas que deseja comprar.",
    "Frequência com que discute com os pais notícias relacionadas à economia ou finanças.",
    "Frequência com que discute com os pais compras pela internet.",
    
    "Concordância com a afirmação: Eu sei como gerenciar meu dinheiro.",
    "Concordância com a afirmação: Eu compro coisas de acordo com o que estou sentindo no momento.",
    "Concordância com a afirmação: Guardar dinheiro é algo que eu só faço se tiver dinheiro sobrando.",
    "Concordância com a afirmação sobre conseguir atingir objetivos de longo prazo.",
    "Concordância com a afirmação sobre estabelecer metas de economia.",
    
    "Frequência com que comprou algo que custou mais do que pretendia gastar.",
    "Frequência com que verificou quanto dinheiro tinha.",
    "Frequência com que guardou dinheiro em uma conta bancária.",
    "Frequência com que guardou dinheiro em casa.",
    
    "Concordância com a afirmação de que amigos têm forte influência sobre decisões de gastos.",
    "Concordância com a afirmação de que às vezes gasta mais do que gostaria quando está com amigos."
  )
)


# ------------------------------------------------------------
# 6. EXPORTAR PARA EXCEL
# ------------------------------------------------------------

write_xlsx(
  list(
    "Dados" = pisa_brasil,
    "Dicionario" = dicionario
  ),
  "PISA_2022_Financial_Literacy_Brasil.xlsx"
)