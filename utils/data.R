print("[DEBUG][SERVER] Loading Data.R")

data_loaded <- FALSE

gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

regulation_type_enum <- function() {
  list(
    NOT_REG = "Not Regulated", 
    MAYBE_UPREG = "Maybe Upregulated", 
    UPREG = "Upregulated",
    MAYBE_DOWNREG = "Maybe Downregulated",
    DOWNREG = "Downregulated"
  )
}
regulation_type <- regulation_type_enum()

get_regulation_type <- function(fold_change, p_value) {
  case_when(
    p_value <= alpha ~ ifelse(
      fold_change < threshold.reg,
      regulation_type$DOWNREG,
      regulation_type$UPREG
    ),
    p_value <= trend ~ ifelse(
      fold_change < threshold.reg,
      regulation_type$MAYBE_DOWNREG,
      regulation_type$MAYBE_UPREG
    ),
    is.na(p_value) | is.na(fold_change) ~ NA_character_, # Not necessary, but just in case ...
    TRUE ~ regulation_type$NOT_REG
  )
}

get_stars <- function(expr, p.val) {ifelse(expr == regulation_type$NOT_REG, "", gtools::stars.pval(p.val))}

# -------------------------------------------------------------

df <- data.frame()

parse_data <- function(file_path) {
  tryCatch({
    df <<- load_data(file_path)
    return(TRUE)
  }, error = function(e) {
    print("[INFO] Error loading data !")
    print(e)
    return(FALSE)
  })
}

# -------------------------------------------------------------

load_data <- function(file_path) {
  raw.data <- readxl::excel_sheets(file_path) %>% 
    purrr::set_names() %>% 
    purrr::map_df(~ readxl::read_excel(path = file_path, sheet = .x), .id = "couche") %>%
    janitor::clean_names() %>%
    rename(gene = target_name, sample = sample_name, ct = ct_mean, dct = d_ct) %>%
    ## Extracting columns from Sample
    extract(sample, into = c("experiment", "litter", "pup", "condition"), regex = "([A-Z])([A-Z]+)([0-9]+)(H|N)", convert = TRUE, remove = FALSE) %>%
    # select(sample, couche, gene, condition, ct, dct) %>%
    ## Adding Fold change
    group_by(couche, gene, condition) %>%
    mutate(avg.dct = mean(dct)) %>% #gm_mean
    mutate(avg.N = mean(dct)) %>% #gm_mean
    ungroup() %>%
    mutate(avg.N = ifelse(condition == "N", avg.N, NA)) %>%
    group_by(couche, gene) %>%
    mutate(avg.N = ifelse(is.na(avg.N), mean(avg.N, na.rm = TRUE), avg.N)) %>%
    ungroup() %>%
    mutate(ddct = avg.dct - avg.N) %>%
    mutate(two = 2**-ddct) %>%
    group_by(couche, gene, condition) %>%
    mutate(fold = mean(two)) %>%
    ungroup() %>% 
    ## Adding model data (gof, significance, ...)
    #TODO replace with models
    group_by(couche, gene) %>%
    mutate(
      p.val = t.test(dct ~ condition, var.equal = FALSE)$p.value,
      expression = ifelse(is.na(dct) | is.na(fold), NA, get_regulation_type(fold, p.val))
    ) %>%
    ungroup() %>%
    ## Selecting / arranging data
    select(couche, sample, litter, gene, condition, dct, fold, p.val, expression) %>%
    mutate(condition = factor(condition, levels = c("N", "H"))) %>%
    arrange(couche, gene, condition, sample)
  
  raw.data %>%
    ## Adding models
    group_by(couche, gene) %>%
    do(
      lm.mod = lm(dct ~ condition, data = .)
      # lmer.mod = lmerTest::lmer(dct ~ condition + (1 | litter), data = ., control = my.lmer.control.params, REML = TRUE)
    ) %>%
    ## Merging
    inner_join(raw.data, by = c("couche", "gene")) %>%
    ## Adding residuals
    group_by(couche, gene) %>%
    mutate(
      n.H = insight::get_data(lm.mod[[1]]) %>% filter(condition == "H") %>% nrow(),
      n.N = insight::get_data(lm.mod[[1]]) %>% filter(condition == "N") %>% nrow(),
      lm.resid = resid(lm.mod[[1]], type = "response"),
      
      # p.val = tidy(car::Anova(lm.mod[[1]], white.adjust = TRUE))$p.value[1],
      p.val = t.test(insight::get_data(lm.mod[[1]]) %>% filter(condition == "H") %>% pull(dct), insight::get_data(lm.mod[[1]]) %>% filter(condition == "N") %>% pull(dct), var.equal = FALSE)$p.value,
      
      expression = get_regulation_type(fold, p.val)
    ) %>%
    ungroup() %>%
    select(couche, gene, sample, litter, condition, p.val, dct, fold, expression, n.N, n.H, lm.mod, lm.resid) %>%
    mutate(across(where(is.numeric), round, 4)) %>%
    arrange(couche, gene, condition, sample)
}

# -------------------------------------------------------------

compute_summary <- function(data) {
  data %>%
    group_by(couche, gene, condition) %>%
    summarize(
      n = n(),
      mean = mean(dct),
      median = median(dct),
      sd = sd(dct)
    ) %>% 
    ungroup()
}

# -------------------------------------------------------------


compute_fit <- function(data) {
  data %>% 
    group_by(couche, gene) %>%
    mutate(
      levene.p = tidy(car::leveneTest(insight::find_formula(lm.mod[[1]])$conditional, data = insight::get_data(lm.mod[[1]]))) %>% filter(term == "group") %>% pull(p.value),
      # lm.BIC = BIC(lm.mod[[1]]),
      resid.SW.p = shapiro.test(lm.resid)$p.value,
      resid.AD.p = nortest::ad.test(lm.resid)$p.value,
      r2 = max(r2(lm.mod[[1]])$R2_adjusted[[1]], 0),
    ) %>%
    select(couche, gene, p.val, levene.p, resid.SW.p, resid.AD.p, r2) %>%
    summarize(
      n = n(),
      across(where(is.numeric), mean),
    ) %>%
    mutate(across(where(is.numeric), round, 4))
}

# -------------------------------------------------------------

compute_statistics <- function(data) {
  data %>%
    group_by(couche, gene) %>%
    summarize(
      across(where(is.numeric), mean),
      across(where(is.character), first),
      
      levene.p = tidy(car::leveneTest(insight::find_formula(lm.mod[[1]])$conditional, data = insight::get_data(lm.mod[[1]]))) %>% filter(term == "group") %>% pull(p.value),
      
      # lm.es = abs(standardize_parameters(lm.mod[[1]])$Std_Coefficient[[2]]),
      
      Hedge.g = hedges_g(
        insight::find_formula(lm.mod[[1]])$conditional,
        correction = TRUE,
        data = insight::get_data(lm.mod[[1]]),
        pooled_sd = ifelse(levene.p <= alpha, FALSE, TRUE),
      )$Hedges_g %>% abs(),

      power = pwr.t2n.test(
        n1 = n.H,
        n2 = n.H,
        d = Hedge.g,
        sig.level = alpha,
        alternative = "two.sided"
      )$power,
      
      r2 = max(r2(lm.mod[[1]])$R2_adjusted[[1]], 0),
      
      add.n.for.nominal.power = max(round(
        pwr.t.test(
          d = Hedge.g,
          power = .80,
          sig.level = alpha,
          alternative = "two.sided"
        )$n - (n.N + n.H)
      ), 0),
    ) %>% 
    mutate(across(where(is.numeric), round, 4)) %>%
    select(couche, gene, n.N, n.H, p.val, Hedge.g, power, levene.p, expression, r2, add.n.for.nominal.power)
}

# -------------------------------------------------------------

get.nm_link <- function(nm.id) {
  glue('<a href="https://www.ncbi.nlm.nih.gov/nuccore/{nm.id}" target="_blank">{nm.id}</a>')
}

get.gene_link <- function(gene, gene.id) {
  glue('<a href="https://www.ncbi.nlm.nih.gov/gene/{gene.id}" target="_blank">{gene}</a>')
}

fetch_ncbi_info <- function(data) {
  data %>%
    distinct(gene) %>%
    arrange(gene) %>%
    pull(gene) %>%
    parse_ncbi() %>% 
    select(-rna.seq) %>% 
    mutate(nm.id = get.nm_link(nm.id))
}