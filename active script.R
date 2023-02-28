# function for automate MR drawing
library(tidyverse)
library(forestplot)
library(TwoSampleMR)

# function

forest_default <- function(harmonised_data, mr_method, or = NULL){
  res <- mr(harmonised_data, method_list = mr_method)
  
  if(or){res <- generate_odds_ratios(res) %>%
    select(outcome, method, nsnp, or, or_lci95, or_uci95)
  
  names(res)[4:6] <- c("or", "lci", "uci")}
  else{res <- res %>% select(outcome, method, nsnp, b, lo_ci, up_ci)
  
  names(res)[4:6] <- c("or", "lci", "uci")}
  
  out_var <- unique(res$outcome)
  nsnp <- res$nsnp[res$outcome == out_var]
  out_var <- paste0(out_var, " (", nsnp, ")")
  
  res_or <- formatC(res$or, digits = 2, format = "f")
  res_lci <- formatC(res$lci, digits = 2, format = "f")
  res_uci <- formatC(res$uci, digits = 2, format = "f")
  res_tab <- paste0(res_or, " (", res_lci, ", ", res_uci, ")")
  
  out_col <- c()
  for(i in 1:length(out_var)){
    temp <- c(out_var[i], rep(NA, length(mr_method) -1))
    out_col <- c(out_col, temp)
  }
  
  tabtxt <- cbind(out_col, res$method, res_tab)
  
  if(or){
  out <- forestplot(tabtxt, mean = res[,4], lower = res[,5], upper = res[,6],
                    new_page = F, xlog = T)}
  else{out <- forestplot(tabtxt, mean = res[,4], lower = res[,5], upper = res[,6],
                         new_page = F)}

  return(plot(out))
}

# create fake data
exp <- extract_instruments("ieu-b-40")
out <- extract_outcome_data(exp$SNP, 
                            c("ieu-a-7", "ebi-a-GCST009541", "finn-b-I9_IHD"))

har <- harmonise_data(exp, out)

pdf("test.pdf", width = 15)

forest_default(harmonised_data = har, 
                       mr_method = c("mr_ivw", "mr_egger_regression"),
                       or = T)
dev.off()