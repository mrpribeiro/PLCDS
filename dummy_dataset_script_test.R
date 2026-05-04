# ==============================================================================
# 1. CARREGAR PACOTES
# ==============================================================================
# Instalar se necessário: install.packages(c("dplyr", "survey", "broom"))
library(dplyr)
library(survey)

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
        SDMVPSU  = sample(1:2, n_obs, replace = TRUE),     
        
        # Variável Dependente (Outcome)
        MCQ160B = sample(c(1, 2, 7, 9), n_obs, replace = TRUE, prob = c(0.1, 0.85, 0.025, 0.025)), 
        
        # Variáveis Independentes Nutricionais
        DR1TSODI = rnorm(n_obs, mean = 3400, sd = 1000), # Sódio em mg
        DR1TPROT = rnorm(n_obs, mean = 80, sd = 20),     # Proteína total em g
        DR1TKCAL = rnorm(n_obs, mean = 2000, sd = 500),  # NOVO: Energia total em kcal
        
        # Fatores Demográficos e Socioeconómicos
        RIDAGEYR = sample(18:80, n_obs, replace = TRUE), 
        RIAGENDR = sample(1:2, n_obs, replace = TRUE),   
        INDFMPIR = runif(n_obs, min = 0, max = 5),       
        DMDEDUC2 = sample(1:5, n_obs, replace = TRUE),   
        
        # Variáveis Clínicas / Comorbilidades
        BMXBMI = rnorm(n_obs, mean = 28, sd = 6),        
        DIQ010 = sample(1:3, n_obs, replace = TRUE, prob = c(0.15, 0.8, 0.05)), 
        BPXSY1 = rnorm(n_obs, mean = 120, sd = 15)       
)

# ==============================================================================
# 3. TRANSFORMAÇÕES DO MEMBRO 2 E MÉTODO DE WILLETT
# ==============================================================================
dados_analise <- dummy_data %>%
        mutate(
                IC_Binaria = case_when(
                        MCQ160B == 1 ~ 1,
                        MCQ160B == 2 ~ 0,
                        TRUE ~ NA_real_
                ),
                Diabetes_Binaria = ifelse(DIQ010 == 1, 1, 0),
                WTMEC8YR = WTMEC2YR / 4
        ) %>%
        filter(!is.na(IC_Binaria) & !is.na(WTMEC8YR) & !is.na(DR1TKCAL))

# ------------------------------------------------------------------------------
# 3.1. APLICAÇÃO DO MÉTODO DOS RESÍDUOS DE WILLETT
# ------------------------------------------------------------------------------
# Passo 1: Calcular modelos de regressão linear (Nutriente previsto pelas Calorias)
modelo_sodio_cal = lm(DR1TSODI ~ DR1TKCAL, data = dados_analise)
modelo_prot_cal  = lm(DR1TPROT ~ DR1TKCAL, data = dados_analise)

# Passo 2: Extrair os resíduos (a variação isolada do nutriente) e 
# adicionar a média do nutriente para centrar os dados num formato interpretável.
media_sodio = mean(dados_analise$DR1TSODI, na.rm = TRUE)
media_prot  = mean(dados_analise$DR1TPROT, na.rm = TRUE)

dados_analise <- dados_analise %>%
        mutate(
                # Estas serão as novas variáveis independentes para usar no svyglm
                Sodio_Willett = resid(modelo_sodio_cal) + media_sodio,
                Prot_Willett  = resid(modelo_prot_cal) + media_prot
        )

# ==============================================================================
# 4. CONFIGURAÇÃO DO DESENHO AMOSTRAL (svydesign)
# ==============================================================================
nhanes_design <- svydesign(
        id      = ~SDMVPSU, 
        strata  = ~SDMVSTRA, 
        weights = ~WTMEC8YR, 
        nest    = TRUE, 
        data    = dados_analise
)

# ==============================================================================
# 5. MODELAÇÃO LÓGISTICA COMPLEXA
# ==============================================================================
# Agora utilizamos o 'Sodio_Willett' em vez do sódio bruto.
# Nota: Adicionadas as variáveis de ajuste que constam no plano (Idade, IMC, Diabetes)
modelo_final <- svyglm(
        IC_Binaria ~ Sodio_Willett + Prot_Willett + RIDAGEYR + BMXBMI + Diabetes_Binaria, 
        design = nhanes_design, 
        family = quasibinomial()
)

summary(modelo_final)