
createColumnShapes = function(featNames,antigen_names,antigen_shapes){
  
  lshapes = matrix(nrow=length(featNames),ncol=1,dimnames=list(featNames,c('shapes')))
  
  for(antigen_name in antigen_names){
    
    lshapes[grep(antigen_name,featNames),'shapes'] = antigen_shapes[antigen_name]
    
  }
  
  
  
  return(lshapes)
  
}