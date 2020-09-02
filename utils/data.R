data_loaded <- FALSE

data_path <- here("data", "Data_P12_HI.xlsx")

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

# -------------------------------------------------------------

df <- data.frame()

parse_data <- function(file_path) {
  tryCatch({
    df <<- load_data(file_path)
    return(TRUE)
  }, error = function(e) {
    print("[INFO] Error loading data !")
    return(FALSE)
  })
}

# -------------------------------------------------------------

load_data <- function(file_path) {
  readxl::excel_sheets(file_path) %>% 
    purrr::set_names() %>% 
    purrr::map_df(~ readxl::read_excel(path = file_path, sheet = .x), .id = "couche") %>%
    janitor::clean_names() %>%
    rename(gene = target_name, sample = sample_name, ct = ct_mean, dct = d_ct) %>%
    mutate(condition = ifelse(endsWith(sample, "N"), "N", "H"), .after = "sample") %>%
    select(sample, couche, gene, condition, ct, dct) %>% 
    group_by(couche, gene, condition) %>% 
    mutate(avg.dct = gm_mean(dct)) %>% 
    mutate(avg.N = gm_mean(dct)) %>% 
    ungroup() %>%
    mutate(avg.N = ifelse(condition == "N", avg.N, NA)) %>%
    group_by(couche, gene) %>% 
    mutate(avg.N = ifelse(is.na(avg.N), mean(avg.N, na.rm=TRUE), avg.N)) %>% 
    ungroup() %>%
    mutate(ddct = avg.dct - avg.N) %>%
    mutate(two = 2 ** -ddct) %>%
    group_by(couche, gene, condition) %>%
    mutate(fold = mean(two)) %>%
    ungroup() %>% 
    select(sample, couche, gene, condition, dct, fold) %>%
    mutate(condition = factor(condition, levels = c("N", "H"))) %>%
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
    ) %>% ungroup()
}

# -------------------------------------------------------------

compute_fit <- function(data) {
  data %>%
    group_by(couche, gene) %>%
    mutate(
      SW.p.resid = shapiro.test(residuals(lm(dct ~ condition), type = "response"))$p.value
    ) %>%
    ungroup() %>%
    group_by(couche, gene, condition) %>%
    mutate(
      norm.fit = list(fitdist(dct, "norm")$estimate),
      #lognorm.fit = list(fitdist(dct, "lnorm")$estimate),
      #gamma.fit = list(fitdist(dct, "gamma")$estimate)
    ) %>%
    summarize(
      n = n(),
      mean.median = mean(dct) / median(dct),
      skew = skewness(dct),
      kurt = kurtosis(dct),
      SW.p = shapiro.test(dct)$p.value,
      AD.p = ad.test(dct, "norm", norm.fit[[1]][1], norm.fit[[1]][2])$p.value %>% round(4),
      SW.p.resid = mean(SW.p.resid)
      #gof.lnorm = ad.test(dct, "lnorm", lognorm.fit[[1]][1], lognorm.fit[[1]][2])$p.value %>% round(4),
      #gof.gamma = ad.test(dct, "gamma", gamma.fit[[1]][1], gamma.fit[[1]][2])$p.value %>% round(4)
    ) %>% ungroup()
}

# -------------------------------------------------------------

compute_statistics <- function(data) {
  data %>%
    group_by(couche, gene, condition) %>%
    summarise(dct = list(dct), fold = mean(fold)) %>%
    spread(condition, dct) %>% 
    summarise(
      F.p = var.test(unlist(H), unlist(N))$p.value,
      t.p = t.test(unlist(H), unlist(N), var.equa = FALSE)$p.value,
      Hedge.g = abs(
        cohen.d(
          unlist(H),
          unlist(N),
          pooled = TRUE,
          paired = FALSE,
          na.rm = FALSE,
          mu = 0,
          hedges.correction = TRUE,
          conf.level = 1 - alpha
        )$estimate
      ),
      power = pwr.t2n.test(
        n1 = length(unlist(N)),
        n2 = length(unlist(H)),
        d = Hedge.g,
        sig.level = alpha,
        alternative = "two.sided"
      )$power,
      add.n.for.nominal.power = max(round(
        pwr.t.test(
          d = Hedge.g,
          power = .80,
          sig.level = alpha,
          alternative = "two.sided"
        )$n - (length(unlist(N)) + length(unlist(H)))
      ), 0),
      expression = get_regulation_type(fold, t.p)
    ) %>% distinct(couche, gene, .keep_all = TRUE)
}

# %>% mutate(expression = get_regulation_type(fold, t.p)) %>% select(-fold)

# -------------------------------------------------------------

fetch_ncbi_info <- function(data) {
  data %>%
    distinct(gene) %>%
    arrange(gene) %>%
    pull(gene) %>% head(1) %>%
    parse_ncbi() %>% 
    select(-rna.seq)
}