# ==============================================================================
# 1. CARREGAR PACOTES
# ==============================================================================
# Instalar se necessário: install.packages(c("dplyr", "survey", "broom", "stringr","tableone"))
library(dplyr)
library(survey)
library(stringr) # Essencial para procurar texto nas variáveis de medicação
library(tableone)

# ==============================================================================
# 2. GERAR DUMMY DATASET (Simulação de 1000 observações do NHANES)
# ==============================================================================
set.seed(123) 

n_obs <- 1000

dummy_data <- data.frame(
        SEQN = 1:n_obs,
        
        # Variáveis de Desenho Amostral
        WTMEC2YR = runif(n_obs, min = 5000, max = 150000), 
        SDMVSTRA = sample(1:15, n_obs, replace = TRUE),    
        SDMVPSU  = sample(1:5, n_obs, replace = TRUE),     
        
        # Variável Dependente (Outcome)
        MCQ160B = sample(c(1, 2, 7, 9), n_obs, replace = TRUE, prob = c(0.1, 0.85, 0.025, 0.025)), 
        
        # Variáveis Independentes Nutricionais
        DR1TSODI = rnorm(n_obs, mean = 3400, sd = 1000), 
        DR1TPROT = rnorm(n_obs, mean = 80, sd = 20),     
        DR1TKCAL = rnorm(n_obs, mean = 2000, sd = 500),  
        DR1IFDCD = sample(c(1000, 2000, 3000), n_obs, replace = TRUE),
        
        # Fatores Demográficos e Socioeconómicos
        RIDAGEYR = sample(18:80, n_obs, replace = TRUE), 
        RIAGENDR = sample(1:2, n_obs, replace = TRUE),   
        INDFMPIR = runif(n_obs, min = 0, max = 5),       
        DMDEDUC2 = sample(1:5, n_obs, replace = TRUE),   
        
        # Variáveis Clínicas / Comorbilidades
        BMXBMI = rnorm(n_obs, mean = 28, sd = 6),        
        DIQ010 = sample(1:3, n_obs, replace = TRUE, prob = c(0.15, 0.8, 0.05)), 
        BPXSY1 = rnorm(n_obs, mean = 120, sd = 15),
        
        # Medicação (Simulada em formato texto como vem no NHANES)
        RXDDRUG = sample(c("FUROSEMIDE", "LISINOPRIL", "IBUPROFEN", "METFORMIN", "NONE"), n_obs, replace = TRUE)
)

# ==============================================================================
# 3. TRANSFORMAÇÕES DO MEMBRO 2 E MÉTODO DE WILLETT
# ==============================================================================
dados_analise <- dummy_data %>%
        mutate(
                # 3.1. OUTCOME E PESOS (Mantidos como numéricos estritos)
                IC_Binaria = case_when(
                        MCQ160B == 1 ~ 1,
                        MCQ160B == 2 ~ 0,
                        TRUE ~ NA_real_
                ),
                WTMEC8YR = WTMEC2YR / 4,
                
                # 3.2. VARIÁVEIS DE AJUSTE (Convertidas para Fatores de Texto Limpo)
                
                Genero = factor(RIAGENDR, 
                                levels = c(1, 2), 
                                labels = c("Masculino", "Feminino")),
                
                Educacao = factor(DMDEDUC2, 
                                  levels = c(1, 2, 3, 4, 5), 
                                  labels = c("< 9º Ano", "9º a 12º Ano", "Secundário Completo", 
                                             "Frequência Universitária", "Licenciatura ou +")),
                
                Diabetes = factor(ifelse(DIQ010 == 1, 1, 0),
                                  levels = c(0, 1),
                                  labels = c("Não", "Sim")),
                
                Toma_Medicacao_Cardio = factor(ifelse(str_detect(toupper(RXDDRUG), "FUROSEMIDE|LISINOPRIL|HCTZ"), 1, 0),
                                               levels = c(0, 1),
                                               labels = c("Não", "Sim")),
                
                # Origem Proteica: "Mista" colocada em 1º lugar para servir de referência na regressão
                Origem_Proteina = factor(case_when(
                        DR1IFDCD == 1000 ~ "Animal",
                        DR1IFDCD == 2000 ~ "Vegetal",
                        TRUE ~ "Mista"
                ), levels = c("Mista", "Animal", "Vegetal"))
                
        ) %>%
        filter(!is.na(IC_Binaria) & !is.na(WTMEC8YR) & !is.na(DR1TKCAL))

# ------------------------------------------------------------------------------
# 3.1. APLICAÇÃO DO MÉTODO DOS RESÍDUOS DE WILLETT
# ------------------------------------------------------------------------------

# Regressão Linear para obter os resíduos ajustados para calorias
modelo_sodio_cal = lm(DR1TSODI ~ DR1TKCAL, data = dados_analise)
modelo_prot_cal  = lm(DR1TPROT ~ DR1TKCAL, data = dados_analise)
# Este modelo calcula qual é a quantidade esperada de sódio e de proteína para
# qualquer nível de calorias. Por exemplo, o modelo pode determinar que, para 
# alguém que come 2000 calorias, o esperado é ingerir 3000 mg de sódio.

media_sodio = mean(dados_analise$DR1TSODI, na.rm = TRUE)
media_prot  = mean(dados_analise$DR1TPROT, na.rm = TRUE)
# Aqui estamos a descobrir qual é o consumo médio de sódio e de proteína de toda
# a população da tua amostra.

dados_analise <- dados_analise %>%
        mutate(
                Sodio_Willett = resid(modelo_sodio_cal) + media_sodio,
                Prot_Willett  = resid(modelo_prot_cal) + media_prot
        )
# A Função resid() extrai os "resíduos" matemáticos do modelo linear.
# O resíduo é a diferença entre o que a pessoa realmente comeu e o que era
# esperado que comesse (calculado no Passo 1).

# Exemplo: Se a pessoa comeu 3500 mg de sódio, mas pelo seu nível de calorias o 
# modelo só esperava 3000 mg, o resíduo é +500 mg. Significa que esta pessoa tem
# uma dieta desproporcionalmente rica em sódio, independentemente do quanto come.

# A Soma da Média (+ media_sodio): Se ficassemos apenas pelos resíduos, teriamos
# pessoas com valores negativos (ex: -500 mg de sódio), o que é biologicamente
# impossível e muito confuso de explicar num dashboard. Ao somar a média
# calculada no Passo 2, "empurramos" os resíduos de volta para uma escala real.

# ==============================================================================
# 4. CONFIGURAÇÃO DO DESENHO AMOSTRAL (svydesign)
# ==============================================================================
# Agora que temos o nosso dataset de análise completo, precisamos configurar o 
# desenho amostral para a modelagem logística complexa. O R normal assume que
# cada linha do Excel tem a mesma importância. A função svydesign anula essa
# regra. Cria um "objeto" (nhanes_design) que guarda os dados em conjunto com
# o "mapa" de como esses dados foram recolhidos no mundo real. Daqui para a
# frente, nenhuma função do pacote survey funciona sem usar este mapa.

nhanes_design <- svydesign(
        id      = ~SDMVPSU, # Primary Sampling Unit (Unidade Primária de Amostragem)
        strata  = ~SDMVSTRA, # Fatia demográfica ou geográfica gigante
        weights = ~WTMEC8YR, # "Multiplicador" ou o "mega-fone" de cada pessoa
        nest    = TRUE, # Significa que os bairros (PSUs) estão "dentro" dos estratos
        data    = dados_analise
)

# ==============================================================================
# 4.1. ANÁLISE DESCRITIVA PONDERADA (TABELA 1)
# ==============================================================================
# 1. Definir todas as variáveis que queremos que apareçam na Tabela 1
vars_tabela <- c("Sodio_Willett", "Prot_Willett", "Origem_Proteina", "RIDAGEYR", 
                 "Genero", "INDFMPIR", "Educacao", "BMXBMI", 
                 "Diabetes", "BPXSY1", "Toma_Medicacao_Cardio")

# 2. Informar o R sobre quais destas variáveis são categorias (fatores/binárias)
# para que ele calcule percentagens (%) em vez de médias.
vars_cat <- c("Origem_Proteina", "Genero", "Educacao", "Diabetes", "Toma_Medicacao_Cardio")

# 3. Gerar a Tabela 1 Ponderada
# O argumento 'strata' divide a tabela em duas colunas: Com IC (1) e Sem IC (0)
# A função svyCreateTableOne garante que os pesos WTMEC8YR são aplicados.
tabela_1_ponderada <- svyCreateTableOne(
        vars = vars_tabela,
        strata = "IC_Binaria", 
        data = nhanes_design,
        factorVars = vars_cat
)

# 4. Imprimir a tabela pronta para exportação
print("--- TABELA 1: CARACTERÍSTICAS DA POPULAÇÃO DOS EUA (PONDERADA) ---")
print(tabela_1_ponderada, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

# level 0: Representa a população SEM Insuficiência Cardíaca (IC_Binaria = 0).
# level 1: Representa a população COM Insuficiência Cardíaca (IC_Binaria = 1).
# p (p-value): É o teste estatístico que compara a coluna 0 com a coluna 1.
# A regra de ouro é: se o valor for inferior a 0.05, existe uma diferença
# estatisticamente significativa entre os doentes e os saudáveis em relação a essa variável.

# n: tamanho da população
# Apesar de o dummy dataset ter apenas 1000 linhas, a tabela mostra 1.6e+07 
# (cerca de 16 milhões de pessoas) sem a doença e 2e+06 (cerca de 2 milhões) com a doença.
# O que significa: O pacote tableone leu os pesos amostrais (WTMEC8YR). Percebeu
# que os 1000 participantes representam, na verdade, cerca de 18 milhões de adultos americanos.
# É isto que garante a representatividade nacional.


# ==============================================================================
# 5. MODELAÇÃO LOGÍSTICA COMPLEXA (Múltipla)
# ==============================================================================
# Agora SIM, com todas as variáveis da nossa tabela mestre integradas!~
# glm() para fazer regressões logísticas. A função svyglm() é a versão dessa
# ferramenta transformada pelo pacote survey. Ao contrário de um modelo normal,
# esta função sabe que tem de ler o "mapa" geográfico e os pesos demográficos
# que configurasmos no passo anterior para não tratar todas as pessoas por igual.
modelo_final <- svyglm(
        IC_Binaria ~ Sodio_Willett + Prot_Willett + Origem_Proteina + RIDAGEYR +
                Genero + INDFMPIR + Educacao + BMXBMI + Diabetes + 
                BPXSY1 + Toma_Medicacao_Cardio, 
        design = nhanes_design, # regras de cálculo do NHANES (pesos, estratos e PSUs)
        family = quasibinomial()
)

summary(modelo_final)
exp(confint(modelo_final))

# ==============================================================================
# 6. EXTRAÇÃO DE RESULTADOS (PARA O DASHBOARD DO MEMBRO 3)
# ==============================================================================
# Extrair os Odds Ratios convertendo os coeficientes de log(odds) usando exp()
tabela_OR <- data.frame(
        Variavel = names(coef(modelo_final)),
        OR = exp(coef(modelo_final)),
        IC_2.5 = exp(confint(modelo_final))[,1],
        IC_97.5 = exp(confint(modelo_final))[,2],
        row.names = NULL
)

# Arredondar APENAS as colunas que são numéricas (Odds_Ratio, LI_95, LS_95) a 3 casas decimais
tabela_OR <- tabela_OR %>%
        mutate(across(where(is.numeric), ~ round(.x, 3)))

print("--- RESULTADOS DO MODELO (ODDS RATIO) ---")
print(tabela_OR, 3)
