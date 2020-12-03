
createColumnColors = function(featNames,reagent_names,reagent_colors){
  
  lcolors = matrix(nrow=length(featNames),ncol=1,dimnames=list(featNames,c('reagent')))
  
  for(reagent_name in reagent_names){
    
    lcolors[grep(reagent_name,featNames),'reagent'] = reagent_colors[reagent_name]
    
  }
  
 
  
  return(lcolors)
  
}

