# ==============================================================================
# 1. CARREGAR PACOTES
# ==============================================================================
# Instalar se necessário: install.packages(c("dplyr", "survey"))
library(dplyr)
library(survey)

# ==============================================================================
# 2. GERAR DUMMY DATASET (Simulação de 1000 observações do NHANES)
# ==============================================================================
set.seed(123) # Garante que os valores aleatórios são reprodutíveis

n_obs <- 1000

dummy_data <- data.frame(
        SEQN = 1:n_obs, # ID do participante
        
        # Variáveis de Desenho Amostral (Cruciais para o pacote survey)
        WTMEC2YR = runif(n_obs, min = 5000, max = 150000), # Pesos estatísticos do ciclo de 2 anos
        SDMVSTRA = sample(1:15, n_obs, replace = TRUE),    # Estratos (Pseudo-stratum)
        SDMVPSU  = sample(1:2, n_obs, replace = TRUE),     # Unidades Primárias (Pseudo-PSU)
        
        # Variável Dependente (Outcome)
        MCQ160B = sample(c(1, 2, 7, 9), n_obs, replace = TRUE, prob = c(0.1, 0.85, 0.025, 0.025)), # 1=Sim, 2=Não, 7/9=Missing
        
        # Variáveis Independentes Nutricionais
        DR1TSODI = rnorm(n_obs, mean = 3400, sd = 1000), # Sódio em mg
        DR1TPROT = rnorm(n_obs, mean = 80, sd = 20),     # Proteína total em g
        
        # Fatores Demográficos e Socioeconómicos
        RIDAGEYR = sample(18:80, n_obs, replace = TRUE), # Idade (Apenas adultos, como planeado)
        RIAGENDR = sample(1:2, n_obs, replace = TRUE),   # Género: 1=Masc, 2=Fem
        INDFMPIR = runif(n_obs, min = 0, max = 5),       # Rácio de Pobreza (PIR)
        DMDEDUC2 = sample(1:5, n_obs, replace = TRUE),   # Nível de escolaridade
        
        # Variáveis Clínicas / Comorbilidades
        BMXBMI = rnorm(n_obs, mean = 28, sd = 6),        # IMC
        DIQ010 = sample(1:3, n_obs, replace = TRUE, prob = c(0.15, 0.8, 0.05)), # Diabetes: 1=Sim, 2=Não, 3=Borderline
        BPXSY1 = rnorm(n_obs, mean = 120, sd = 15)       # Pressão arterial sistólica
)

# ==============================================================================
# 3. TRANSFORMAÇÕES DO MEMBRO 2 (Preparação Estatística)
# ==============================================================================
dados_analise <- dummy_data %>%
        # Recodificar a Insuficiência Cardíaca para formato binário (1 = Sim, 0 = Não)
        # Excluir as respostas 7 (Recusou) e 9 (Não sabe) transformando-as em NA
        mutate(
                IC_Binaria = case_when(
                        MCQ160B == 1 ~ 1,
                        MCQ160B == 2 ~ 0,
                        TRUE ~ NA_real_
                ),
                
                # Recodificar Diabetes para binário para facilitar o modelo
                Diabetes_Binaria = ifelse(DIQ010 == 1, 1, 0),
                
                # TAREFA CRÍTICA DO PLANO: Dividir os pesos por 4 porque vão usar 4 ciclos (2011-2018)
                WTMEC8YR = WTMEC2YR / 4
        ) %>%
        # O pacote survey não lida bem com NA's nas variáveis de design, 
        # portanto filtramos logo os indivíduos sem informação sobre a IC.
        filter(!is.na(IC_Binaria) & !is.na(WTMEC8YR))

# ==============================================================================
# 4. CONFIGURAÇÃO DO DESENHO AMOSTRAL (svydesign)
# ==============================================================================
# A opção nest = TRUE é obrigatória no NHANES porque os PSUs estão "aninhados" nos estratos
nhanes_design <- svydesign(
        id      = ~SDMVPSU, 
        strata  = ~SDMVSTRA, 
        weights = ~WTMEC8YR, 
        nest    = TRUE, 
        data    = dados_analise
)

# Teste Rápido: Verificar a prevalência ponderada de Insuficiência Cardíaca
prevalencia_ic <- svymean(~IC_Binaria, nhanes_design, na.rm = TRUE)
print("Prevalência Nacional Estimada de Insuficiência Cardíaca:")
print(prevalencia_ic)

# Teste Rápido: Modelo Logístico Preliminar
modelo_teste <- svyglm(
        IC_Binaria ~ DR1TSODI + RIDAGEYR + BMXBMI, 
        design = nhanes_design, 
        family = quasibinomial() # quasibinomial é exigido pelo svyglm para regressão logística
)
summary(modelo_teste)