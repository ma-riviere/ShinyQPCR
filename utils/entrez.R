print("[DEBUG][ENTREZ] Loading rentrez utils functions")

parse_gene_data <- function(gene.data) {
  coms <- xpathApply(gene.data, "//Gene-commentary_products/Gene-commentary", parse_gene_commentary)
  
  gene.data <- do.call(rbind.data.frame, coms %>% list.filter(!is.na(id)))
  
  if (nrow(gene.data) > 0) {
    gene.data %<>% distinct(id, .keep_all = TRUE)
  }
  
  # When there's only one variant, if it's not name, make it variant one
  if (nrow(gene.data) == 1) {
    gene.data %<>% mutate(label = ifelse(is.na(label) | is.null(label), "None", label))
  }
  
  return(gene.data)
}

parse_gene_commentary <- function(gene.com) {
  label <- as.character(xpathApply(gene.com, "Gene-commentary_label", xmlValue)[1])
  accn <- as.character(xpathApply(gene.com, "Gene-commentary_accession", xmlValue)[1])
  
  res <- data.frame(id = character(), label = character())
  
  if (!is.null(accn) && !is_empty(accn)) {
    if (str_detect(accn, regex("NM_", ignore_case = T))) {
      res[nrow(res) + 1, ] <- list(id = accn, label = ifelse(is_empty(label) || label == "NULL", NA_character_, label))
    }
  }
  return(res)
}

parse_rna_data <- function(rna.data.xml) {
  exons <- xpathApply(rna.data.xml, "//GBSeq_feature-table/GBFeature", parse_exons)
  rna.seq <- xpathApply(rna.data.xml, "//GBSeq_sequence", xmlValue) %>%
    toupper() %>%
    stringr::str_trim()
  
  exon.str <- do.call(rbind.data.frame, exons) %>%
    filter(type == "exon") %>%
    distinct(loc, .keep_all = TRUE) %>%
    pull(loc) %>%
    stringi::stri_paste(., collapse = " / ")
  
  return(cbind(rna.seq, exon.str) %>% as.data.frame())
}

parse_exons <- function(exon) {
  type <- as.character(xpathApply(exon, "GBFeature_key", xmlValue)[1])
  loc <- as.character(xpathApply(exon, "GBFeature_location", xmlValue)[1])
  # TODO: GBFeature_quals
  
  res <- data.frame(type = character(), loc = character())
  
  if (!is.null(type) & !is.null(loc)) {
    res[nrow(res) + 1, ] <- list(type = type, loc = loc)
  }
  
  return(res)
}

#############################################################################################

parse_ncbi <- function(genes) {
  res <- data.frame()
  
  for (gene in genes) {
    gene <- stringr::str_trim(gene)
    
    ## Extract gene identifier for Mus Musculus
    
    gene.ids <- entrez_search(db = "gene", term = glue("({toupper(gene)}[TITL] AND Mus Musculus[ORGN]"))$ids
    
    if (length(gene.ids) == 0) {
      print(glue("[ERROR] No gene ID for gene {gene}, ignoring"))
      next
    }
    
    gene.ids.filtered <- c()
    if (length(gene.ids) > 1) {
      print(glue("[WARNING] More than one gene ID returned for gene {toupper(gene)} : {length(gene.ids)} IDs found >> Filtering"))
      for (gene.id in gene.ids) {
        temp.gene.data <- entrez_fetch(db = "gene", id = gene.id, rettype = "xml", parsed = TRUE)
        temp.gene.loc <- as.character(xpathSApply(temp.gene.data, "//Entrezgene_gene/Gene-ref/Gene-ref_locus", xmlValue)[1])
        temp.gene.status <- as.character(xpathSApply(temp.gene.data, "//Gene-track_status", xmlValue)[1])
        
        if ((!is.null(temp.gene.loc) & toupper(temp.gene.loc) == toupper(gene)) & (!is.null(temp.gene.status) & temp.gene.status == 0)) {
          gene.ids.filtered <- c(gene.ids.filtered, gene.id)
        }
      }
      if (length(gene.ids.filtered) > 1) {
        print(glue("[WARNING] Still {length(gene.ids.filtered)} gene IDs left for gene {toupper(gene)} : {glue_collapse(gene.ids.filtered, sep = ', ')}"))
      } else {
        print(glue("[INFO] Filtering successful, one gene ID left for gene {toupper(gene)} : {gene.ids.filtered[1]}"))
      }
    } else {
      gene.ids.filtered <- gene.ids
    }
    
    if (length(gene.ids.filtered) == 0) {
      print(glue("[ERROR] No gene ID for gene {gene} after filtering, ignoring"))
      next
    }
    
    ## Extract full gene data
    
    gene.data <- entrez_fetch(db = "gene", id = gene.ids.filtered[1], rettype = "xml", parsed = TRUE)
    if (is.null(gene.data)) {
      print(glue("[ERROR] No gene data for gene {gene}, ignoring"))
      next
    }
    
    ## Extract Gene description / function
    
    description <- as.character(xpathSApply(gene.data, "//Entrezgene_gene/Gene-ref/Gene-ref_desc", xmlValue)[1])
    
    if (is.null(description) | is.na(description)) {
      print(glue("[ERROR] No description for gene {gene}, ignoring"))
      next
    }
    
    ## Extract NM_ sequences (multiple)
    
    nm.data <- parse_gene_data(gene.data) %>% mutate(gene.id = gene.ids.filtered[1])
    nm.ids <- nm.data$id
    
    ## Extract Nuccore IDs for each NM_ sequence
    
    nuc.data <- data.frame(nm.id = character(), nuc.id = character())
    for (nm.id in nm.ids) {
      nm.id <- stringr::str_trim(toupper(nm.id))
      
      nuc.id <- entrez_search(db = "nuccore", term = glue("({nm.id}[ACCN])"))$ids
      
      if (length(nuc.id) > 1) {
        print(glue("[WARNING] More than one unique nuccore ID returned for sequence {nm.id} of gene {gene}"))
      }
      
      if (!is.null(nuc.id) & !is.na(nuc.id) & length(nuc.id) == 1) {
        nuc.data[nrow(nuc.data) + 1, ] <- list(nm.id = nm.id, nuc.id = nuc.id[1])
        # nuc.data <- rbind(nuc.data, data.frame(nm.id, nuc.id))
      } else {
        print(glue("[DEBUG] No nuccore ID returned for sequence {nm.id} of gene {gene}")) # ({nuc.id[1]})
      }
    }
    # print(nuc.data)
    
    if (is.null(nuc.data) | is.null(dim(nuc.data)) | dim(nuc.data)[1] == 0) {
      print(glue("[ERROR] No nuccore data for gene {gene}, ignoring"))
      next
    }
    
    ## Extract ARN sequences for each nuccore ID
    
    seq.data <- data.frame(nuc.id = character(), rna.seq = character(), exons = character())
    for (nuc.id in nuc.data$nuc.id) {
      rna.data.xml <- tryCatch(
        {
          entrez_fetch(db = "nuccore", id = nuc.id, rettype = "gb", retmode = "xml", parsed = T)
        },
        error = function(cond) {
          print(glue("[WARNING] No RNA data found for Nuccore ID {nuc.id} of gene {gene}, ignoring"))
          return(NA)
        }
      )

      if (!is.null(rna.data.xml) & !is.na(rna.data.xml)) {
        rna.data.xml %<>% xmlParse()

        rna.data <- parse_rna_data(rna.data.xml)

        if (!is.null(rna.data) & nrow(rna.data) != 0) {
          seq.data[nrow(seq.data) + 1, ] <- list(nuc.id = nuc.id, rna.seq = rna.data$rna.seq, exons = rna.data$exon.str)
          # seq.data <- rbind(seq.data, data.frame(nuc.id, rna.seq))
        }
      }
    }
    # print(seq.data)

    if (is.null(seq.data) | is.null(dim(seq.data)) | dim(seq.data)[1] == 0) {
      print(glue("[ERROR] No sequence data for gene {gene}, ignoring"))
      next
    }
    
    ## Merge results
    temp <- data.frame(gene, description, nm.ids) %>%
      full_join(nuc.data, by = c("nm.ids" = "nm.id")) %>%
      full_join(nm.data, by = c("nm.ids" = "id")) %>% 
      full_join(seq.data, by = "nuc.id")
    
    res <- rbind(res, temp)
    
    print(glue("[SUCCESS] Data obtained for gene {gene}"))
  }
  
  res %<>% rename(nm.id = nm.ids) %>%
    select(-nuc.id) %>%
    rename(variant = label) %>%
    mutate(variant = str_extract(variant, "[^ ]+$")) %>%
    mutate(variant = ifelse(!is.na(as.numeric(variant)), str_pad(variant, 2, pad = "0"), variant)) %>%
    relocate(exons, .after = variant) %>%
    arrange(gene, variant)
  
  print("[INFO][Entrez] Data fetching done !")
  return(res)
}