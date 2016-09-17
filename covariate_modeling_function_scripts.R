###function for comparing covariate models 

regression_modeling <- function(df, outcomes, predictor, covariate){
  setClass(Class = "modeling", representation(r_models="data.frame", f_models="data.frame", partial_f="data.frame"))
  results <- data.frame()
  for(o in outcomes){
    assign(x = paste(predictor,"reduced_regression", o , sep = "_"),
           value = lm(eval(parse(text=o))~eval(parse(text=predictor)), data = df))
    assign(x = paste(predictor, "full_regression", o, "w", covariate, sep = "_"), 
           value = lm(eval(parse(text=o))~eval(parse(text=predictor))+eval(parse(text=covariate)), data = df))
    assign(x = paste(predictor, "partial_f", o, "w", covariate, sep = "_"),
           value = anova(lm(eval(parse(text=o))~eval(parse(text=predictor)), data = df),
                         lm(eval(parse(text=o))~eval(parse(text=predictor))+eval(parse(text=covariate)), data = df)))
  }
  r_models <- data.frame()
  for(b in grep(pattern = paste(predictor, "reduced_regression", sep = "_"), x = ls(), value = T)){
    r_models <- rbind(r_models, as.data.frame(summary(eval(parse(text=b)))$coefficients))
  }
  r_dfs <- data.frame()
  for(df in grep(pattern = paste(predictor, "reduced_regression", sep = "_"), x = ls(), value = T)){
    r_dfs <- rbind(NA, summary(eval(parse(text=df)))$df[2])
  }
  r_confint <- data.frame()
  for(c in grep(pattern = paste(predictor, "reduced_regression", sep = "_"), x = ls(), value = T)){
    r_confint <- rbind(r_confint, confint(eval(parse(text=c))))
  }
  r_models <- cbind(outcomes = rep(sort(outcomes), each = 2, times = 1), 
                    parameters = rep(c("(intercept)","(b1)"), each = 1, times = length(rownames(r_models))/2),
                    round(r_models, digits = 2), 
                    df = r_dfs,
                    round(r_confint, digits = 2))
  f_models <- data.frame()
  for(bb in grep(pattern = paste(predictor, "full_regression", sep = "_"), x = ls(), value = T)){
    f_models <- rbind(f_models, as.data.frame(summary(eval(parse(text=bb)))$coefficients))
  }
  f_dfs <- data.frame()
  for(df2 in grep(pattern = paste(predictor, "full_regression", sep = "_"), x = ls(), value = T)){
    f_dfs <- rbind(NA, NA, summary(eval(parse(text=df2)))$df[2])
  }
  f_confint <- data.frame()
  for(cc in grep(pattern = paste(predictor, "full_regression", sep = "_"), x = ls(), value = T)){
    f_confint <- rbind(f_confint, confint(eval(parse(text=cc))))
  }
  f_models <- cbind(outcomes = rep(sort(outcomes), each = 3, times = 1),
                    parameters = rep(c("(intercept)","(b1)","(b2)"), each = 1, times = length(rownames(f_models))/3), 
                    round(f_models, digits = 2),
                    df2 = f_dfs, 
                    round(f_confint, digits = 2))
  partial_f <- data.frame()
  for(f in grep(pattern = paste(predictor, "partial_f", sep = "_"), x = ls(), value = T)){
    partial_f <- rbind(partial_f, eval(parse(text=f)))
  }
  partial_f <- cbind(outcomes = rep(sort(outcomes), each = 2, times = 1), 
                     model = rep(c("(reduced model)","(full model)"), each = 1, times = length(rownames(partial_f))/2),
                     round(partial_f, digits = 2))
  return(new("modeling", r_models=r_models, f_models=f_models, partial_f=partial_f))
}
