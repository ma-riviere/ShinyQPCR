print("[DEBUG][SERVER] Loading Data.R")

data_loaded <- FALSE

# gm_mean = function(x, na.rm=TRUE){
#   exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
# }

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

add_fold_change <- function(dat) {
  return(dat
         %>% mutate.(
           avg.dct = mean(dct),
           avg.N = mean(dct),
           .by = c(couche, gene, condition)
         )
         %>% mutate.(avg.N = ifelse(condition == "N", avg.N, NA))
         %>% fill.(
           avg.N,
           .direction = "down",
           .by = c(couche, gene)
         )
         %>% mutate.(ddct = avg.dct - avg.N)
         %>% mutate.(two = 2**(-ddct))
         %>% mutate.(
           fold = mean(two),
           .by = c(couche, gene, condition)
         )
  )
}

add_expression <- function(dat) {
  return(dat
         %>% mutate.(
           p.val = t.test(dct ~ condition, var.equal = FALSE)$p.value,
           .by = c(couche, gene)
         )
         %>% mutate.(
           expression = ifelse(condition == "H", get_regulation_type(fold, p.val), NA),
           .by = c(couche, gene)
         )
         %>% fill.(
           expression,
           .direction = "up",
           .by = c(couche, gene)
         )
  )
}

add_models <- function(dat) {
  return(dat %>%
           nest_by(couche, gene, .key = "dta")
         %>% mutate(lm.mod = list(lm(dct ~ condition, data = dta)))
         %>% select.(-dta)
         %>% inner_join.(dat, by = c("couche", "gene"))
         %>% group_by(couche, gene)
         %>% mutate(
           n.H = insight::get_data(lm.mod[[1]]) %>% filter(condition == "H") %>% nrow(),
           n.N = insight::get_data(lm.mod[[1]]) %>% filter(condition == "N") %>% nrow(),
           lm.resid = resid(lm.mod[[1]], type = "response"),
           lm.cooksd = cooks.distance(lm.mod[[1]]),
           
           p.val = tidy(car::Anova(lm.mod[[1]], white.adjust = TRUE))$p.value[1],
           # p.val = t.test(insight::get_data(lm.mod[[1]]) %>% filter(condition == "H") %>% pull(dct), insight::get_data(lm.mod[[1]]) %>% filter(condition == "N") %>% pull(dct), var.equa = FALSE)$p.value,
           expression = get_regulation_type(fold, p.val)
         )
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
    print(e)
    return(FALSE)
  })
}

# -------------------------------------------------------------

load_data <- function(file_path) {
  return(readxl::excel_sheets(file_path)
         %>% purrr::set_names()
         %>% map_df.(~ readxl::read_excel(path = file_path, sheet = .x), .id = "couche")
         %>% select.(couche, sample = matches("sample"), gene = matches("target"), ct = starts_with("ct"), dct = matches("dct"))
         %>% extract(sample, into = c("experiment", "litter", "pup", "condition"), regex = "([A-Z]+)([A-Z]+)([0-9]+)(H|N)", convert = TRUE, remove = FALSE)
         %>% select.(-c(experiment, pup))
         %>% drop_na.(dct)
         %>% add_fold_change()
         %>% add_expression()
         # %>% mutate.(Stade = stade)
         # %>% add_sex()
         # %>% add_pathway()
         ## Selecting / arranging data
         %>% mutate.(condition = factor(condition, levels = c("N", "H")))
         %>% add_models()
         %>% select.(couche, gene, sample, litter, condition, p.val, dct, fold, expression, n.N, n.H, lm.mod, lm.resid)
         %>% mutate_across.(where(is.numeric), round, 4)
         %>% arrange.(couche, gene, condition, sample)
  )
}

# load_stades <- function(liste_stades) {
#   map_df.(liste_stades, ~ load_stade(.x)) %>% 
#     mutate.(
#       Condition = factor(Condition, levels = c("N", "H")),
#       Couche = factor(Couche, levels = layer.order),
#       Stade = factor(Stade, levels = liste_stades)
#     ) %>%
#     arrange.(Stade, Couche, Gene, Condition, Sex)
# }

# -------------------------------------------------------------

compute_summary <- function(data) {
  return(data
    %>% summarize.(
      n = n.(),
      mean = mean(dct),
      median = median(dct),
      sd = sd(dct),
      .by = c(couche, gene, condition)
    )
  )
}

# -------------------------------------------------------------


compute_fit <- function(data) {
  return(data
    %>% group_by(couche, gene)
    %>% mutate(
      levene.p = tidy(car::leveneTest(insight::find_formula(lm.mod[[1]])$conditional, data = insight::get_data(lm.mod[[1]]))) %>% filter(row_number() == 1) %>% pull(p.value),
      # lm.BIC = BIC(lm.mod[[1]]),
      resid.SW.p = shapiro.test(lm.resid)$p.value,
      resid.AD.p = nortest::ad.test(lm.resid)$p.value,
      r2 = max(r2(lm.mod[[1]])$R2_adjusted[[1]], 0),
    )
    %>% select(couche, gene, p.val, levene.p, resid.SW.p, resid.AD.p, r2)
    %>% summarize(
      n = n(),
      across(where(is.numeric), mean),
    ) 
    %>% mutate(across(where(is.numeric), round, 4))
  )
}

# -------------------------------------------------------------

compute_statistics <- function(data) {
  return(data
    %>% group_by(couche, gene)
    %>% summarize(
      across(where(is.numeric), mean),
      across(where(is.character), dplyr::first),
      
      levene.p = tidy(car::leveneTest(insight::find_formula(lm.mod[[1]])$conditional, data = insight::get_data(lm.mod[[1]]))) %>% filter(row_number() == 1) %>% pull(p.value),
      
      # lm.es = abs(standardize_parameters(lm.mod[[1]])$Std_Coefficient[[2]]),
      
      Hedge.g = effectsize::hedges_g(
        insight::find_formula(lm.mod[[1]])$conditional,
        correction = 1,
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
    )
    %>% mutate(across(where(is.numeric), round, 4))
    %>% select(couche, gene, n.N, n.H, p.val, Hedge.g, power, levene.p, expression, r2, add.n.for.nominal.power)
  )
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