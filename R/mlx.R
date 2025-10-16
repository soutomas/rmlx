#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Filename : mlx.R
# Use      : Convenient Functions for Processing of Monolix Results 
# Author   : Tomas Sou (souto1)
# Created  : 2025-10-16
# Updated  : 2025-10-16
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Notes 
# - na
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Updates
# - na 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Functions 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Functions to compile run results  
ask_mlx = function(ifall=T,path=mlx_path){
  # ifall = readline("Include all Mlx files? [y][0=n]:")
  if(!ifall) {
    # mlxfiles = select.list(dir(),multiple = T)
    mlxruns = select.list(dir(path=path,pattern=".mlxtran$",full.names=T), multiple=T) # mlxtran only 
  } else {
    mlxruns = dir(path=path,pattern=".mlxtran$",full.names=T)
  }
  mlxruns = gsub(".mlxtran","",mlxruns)
  return(mlxruns)
}

get_parafname = function(mlxrun){
  fdirname = gsub(".mlxtran","",mlxrun) 
  out = file.path(fdirname,"populationParameters.txt")
}

get_summfname = function(mlxrun){
  fdirname = gsub(".mlxtran","",mlxrun) 
  out = file.path(fdirname,"summary.txt")
}

get_ofv = function(mlxrun){
  fdirname = gsub(".mlxtran","",mlxrun) 
  fname = get_summfname(mlxrun)
  if(!file.exists(fname)){
    ofvs = tibble(
      RUN = mlxrun,
      OFV = NA,
      AIC = NA,
      BIC = NA,
      BICc = NA,
    ) 
    return(ofvs)
  } 
  ofvs = read_fwf(fname,fwf_widths(c(41,14,9)),show_col_types=F) %>%
    filter(grepl("OFV|AIC|BICc|BIC",X2)) %>% 
    mutate(X3 = as.numeric(X3)) %>% 
    # mutate(X3 = tibble::num(X3,digits=2)) %>% 
    mutate(across(where(is.double), ~round(.x, digits=2))) %>% 
    select(X2,X3) %>% 
    tidyr::pivot_wider(names_from=X2,values_from=X3) %>% 
    rename(OFV=1,AIC=2,BICc=3,BIC=4) %>%
    mutate(RUN = basename(fdirname), .before=1) 
  return(ofvs)
}

get_para = function(mlxrun){
  fname = get_parafname(mlxrun) 
  if(!file.exists(fname)) return(paste0("No results for: ",basename(mlxrun),"\n"))
  para = read.csv(fname,header=T) %>% 
    tibble() %>% 
    select(PARA=parameter, Value=value, CV, RSEsa=any_of("rse_sa"), RSElin=any_of("rse_lin")) %>% 
    # mutate(CV = ifelse(grepl("^omega",PARA),Value*100,NA)) %>% # omega on SD scale
    mutate(across(where(is.double), ~signif(.x, digits=3))) %>% 
    mutate(across(everything(), ~replace(.x, is.nan(.x), "NaN")))    
  # mutate(RUN = mlxrun, .before=PARA)
  return(para) 
} 

# Fn: Get OFVs of all runs 
get_allofv = function(mlxruns, sortAIC=F){
  allofv = purrr::map_df(mlxruns, get_ofv) %>% 
    mutate(dOFV=OFV-lag(OFV), .after=OFV) %>%
    mutate(dAIC=AIC-lag(AIC), .after=AIC) %>%
    mutate(dBICc=BICc-lag(BICc), .after=BICc) %>%
    mutate(dBIC=BIC-lag(BIC), .after=BIC) 
  if(sortAIC) allofv = allofv %>% arrange(AIC) 
  allofv = allofv %>% mutate(ROW = row_number(), .before=1) 
  # print(allofv)
  return(allofv)
}

# Fn: Get parameters of all runs 
get_allpara = function(mlxruns){
  allpara = purrr::map(mlxruns,get_para)
  names(allpara) = basename(mlxruns)
  # print(allpara)
  return(allpara)
}

# Fn: Compare parameters of two runs 
# mlxruns: list of runs to search parameters from 
# run1 and run2 <chr>: starting part of the run names to compare 
# addto: previous compare_para output to add to; NULL = none 
compare_para = function(addto=NULL,run1=NA,run2=NA,rse=T,cv=F,mlxruns){
  run1n = grep(paste0("/",run1),mlxruns,value=T) 
  run2n = grep(paste0("/",run2),mlxruns,value=T) 
  runs = run1n 
  if(!is.na(run2)) runs = c(run1n,run2n) 
  if(!is.null(addto)) runs = c(runs,"+others") 
  # print(runs) # show full run names for debugging 
  valname = paste0(run1,".val")
  rsename = paste0(run1,".rse")
  rsename2 = paste0(run1,".rseLin")
  cvname = paste0(run1,".cv")
  para1 = get_para(run1n) 
  if(!is.data.frame(para1)) {cat(para1); return(addto)}
  if(is.data.frame(para1)) para1 = para1 %>% rename(!!valname:=Value,!!cvname:=CV,!!rsename:=any_of("RSEsa"),!!rsename2:=any_of("RSElin"))
  out = para1 
  if(!is.na(run2)) valname = paste0(run2,".val") 
  if(!is.na(run2)) rsename = paste0(run2,".rse")
  if(!is.na(run2)) rsename2 = paste0(run2,".rseLin")
  if(!is.na(run2)) cvname = paste0(run2,".cv")
  if(!is.na(run2)) para2 = get_para(run2n)
  if(!is.na(run2) && !is.data.frame(para2)) {cat(para2); return(out)}
  if(!is.na(run2) && is.data.frame(para2)) para2 = para2 %>% rename(!!valname:=Value,!!cvname:=CV,!!rsename:=any_of("RSEsa"),!!rsename2:=any_of("RSElin"))
  if(!is.na(run2)) out = full_join(para1,para2,by="PARA") 
  if(!is.null(addto)) out = full_join(addto,out,by="PARA")
  if(!rse) out = out %>% select(-ends_with("rse"))
  if(!cv) out = out %>% select(-ends_with("cv"))
  pop = out %>% filter(grepl("pop$",PARA),!grepl("^beta",PARA)) 
  omg = out %>% filter(grepl("^omega",PARA)) 
  cov = out %>% filter(grepl("^beta",PARA)) 
  corr = out %>% filter(grepl("^corr",PARA)) 
  res = out %>% filter(!grepl("pop$|^omega|^beta|^corr",PARA))
  out = bind_rows(pop,cov,omg,corr,res)
  return(out) 
} 

# Fn: Compare parameters of all runs 
# runs <chr>: starting part of the run names to compare 
compare_allpara = function(mlxruns,rse=T,cv=F){
  # runs = substr(mlxruns,1,3) # extract run number 
  runs = str_extract(basename(mlxruns),"^.*?[_]") %>% 
    str_sort(numeric=T) %>% 
    str_remove("_") # extract run number 
  out = compare_para(run1=runs[1],rse=rse,cv=cv,mlxruns=mlxruns) 
  if(length(runs)==1) return(out = out |> rowid_to_column("ROW"))
  out = compare_para(run1=runs[1],run2=runs[2],rse=rse,cv=cv,mlxruns=mlxruns)
  if(length(runs)==2) return(out = out |> rowid_to_column("ROW"))
  if(length(runs)>2){ 
    for(i in runs[-(1:2)]){
      out = compare_para(addto=out,run1=i,rse=rse,cv=cv,mlxruns=mlxruns) 
    }
    return(out = out |> rowid_to_column("ROW"))
  }
} 

# Fn: Compare OFV and parameters of selected runs 
# runnums <chr>: run numbers, e.g., "^v35|^v53|^v55|^v56" 
fn.compRuns = function(runnums,ifOFV=T,ifParam=T){
  mlxruns = ask_mlx() # all runs in dir 
  runs = grep(runnums,mlxruns,value=T) %>% str_sort(numeric=T) # select runs 
  if(ifOFV) get_allofv(runs,sortAIC=F) %>% kbl2() %>% print() # all OFVs 
  if(ifParam) compare_allpara(runs,rse=T) %>% kbl2(fnote="NB: IIV on SD scale") # all parameters 
} 
