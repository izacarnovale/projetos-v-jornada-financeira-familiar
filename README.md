# Jornada Financeira Familiar — Projetos V (FGV EESP, 2026.2)

Scripts em R e dados do projeto **"Jornada Financeira Familiar: crédito responsável e autonomia progressiva para jovens de 18 a 25 anos no Banco Itaú"**, desenvolvido na disciplina Projetos V (Graduação em Economia, FGV EESP, ênfase em Economia Política), com o Banco Itaú como parceiro externo.

**Grupo 1:** Beatriz Guimarães Ferez, Guilherme de Carvalho e Silva, Izabela Carnovale Nunes e Pedro Augusto da Silva Nakayama.
**Professor tutor:** Caio de Souza Castro.

## Conteúdo

| Arquivo | O que faz |
|---|---|
| `estimaçãogamma.R` | Lê o survey, calcula o parâmetro de influência parental γ (média da influência dos pais na escolha do **primeiro banco**, escala 0–10; γ = 6,0) e gera os gráficos de influência, permanência e avaliação da proposta. |
| `microssimulação.R` | Microssimulação comportamental (N = 1.000; β<sub>J</sub> = 0,5; β<sub>P</sub> = 0,9; λ = 1) comparando a escolha do banco responsável com e sem influência parental, e análise de sensibilidade de γ entre 0 e 10. |
| `sinteseinterpretativapreliminar.R` | Extrai os principais números do survey e da simulação para o texto. |
| `PISA2022.R` | Lê o arquivo de letramento financeiro do PISA 2022, filtra o Brasil, recodifica as variáveis e exporta a base tratada. |
| `gráficospisa.R` | Índice de conversas financeiras com os pais e gráficos de confiança, planejamento, inclusão bancária e letramento. |
| `Influência dos pais na escolha do banco (respostas).xlsx` | Respostas do survey próprio (n = 30, coleta de 24 a 31 ago. 2026). Anônimo; a coluna de data/hora de envio foi removida. |
| `figuras/` | Gráficos usados no relatório final. |

## Como rodar

1. Instale os pacotes: `install.packages(c("readxl", "dplyr", "ggplot2", "tidyr", "haven", "writexl"))`.
2. Abra o R com esta pasta como diretório de trabalho.
3. **Survey e microssimulação** (nesta ordem, na mesma sessão, pois cada script usa objetos do anterior):
   ```r
   source("estimaçãogamma.R", encoding = "UTF-8")
   source("microssimulação.R", encoding = "UTF-8")
   source("sinteseinterpretativapreliminar.R", encoding = "UTF-8")
   ```
4. **PISA 2022**: baixe o arquivo *Financial Literacy* (formato SAS) em <https://www.oecd.org/en/data/datasets/pisa-2022-database.html> e salve `CY08MSP_FLT_QQQ.SAS7BDAT` em uma pasta `FLT_SAS/`. Os microdados da OCDE não estão no repositório por causa do tamanho. Depois rode `PISA2022.R` e `gráficospisa.R`.

## Fontes de dados

- **Survey próprio** — "Influência dos pais na escolha do banco", Grupo 1, 2026. Amostra de conveniência, não probabilística: os resultados são exploratórios.
- **PISA 2022 — Financial Literacy** — OCDE (2024). Extração em 7 out. 2026.
- **SCR.data** — Banco Central do Brasil, dados abertos (<https://dadosabertos.bcb.gov.br/dataset/scr_data>). Extração em 4 out. 2026. O script de tratamento do SCR (`scr_pf_anual.R`) não está neste repositório.

## Observação sobre a microssimulação

O resultado (71,7% de escolha do banco responsável com influência parental, contra 19,8% sem ela) é um **cenário contrafactual**, não um efeito causal. As preferências β<sub>J</sub> e β<sub>P</sub> são hipóteses do grupo, e só γ vem dos dados. A análise de sensibilidade mostra que o resultado se estabiliza a partir de γ = 1,5.
