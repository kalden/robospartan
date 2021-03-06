library(xml2)
library(shinycssloaders)


#' Recursively search the XML file for the tag attributes that are being changed
#' @param child_nodes Current set of child nodes being examined
#' @param attribute_name Name of the attribute to find
#' @param attribute_value Value to set to that attribute
#' @param found_in_file Whether the attribute has been located
#' @return Boolean stating whether the attribute has been located and value altered
search_child_nodes_and_set_attribute<-function(child_nodes, attribute_name,attribute_value, found_in_file=FALSE)
{
  node<-1
  while(node<=length(child_nodes) && !found_in_file)
  {
    #print(node)
    #print(child_nodes[node])
    if(all(is.na(xml_attr(child_nodes[node],attribute_name))) & length(xml_children(child_nodes[node]))>0)
    {
      found_in_file<-search_child_nodes_and_set_attribute(xml_children(child_nodes[node]), attribute_name, attribute_value, found_in_file)
    }
    else if(!all(is.na(xml_attr(child_nodes[node],attribute_name))))
    {
      xml_set_attr(child_nodes[node],attribute_name,attribute_value)
      found_in_file=TRUE
      return(TRUE)
    }
    node<-node+1
  }
  return(found_in_file)
}

#' For a generated sample, make files to run in ARGoS Simulator
#' @param argos_file_path Full path to an ARGoS file
#' @param output_folder Where the generated files should go, this is just a temporary folder so that the files can be zipped.
#' @param parameters ARGoS parameters of interest
#' @param generated_sample Sample of parameter values to insert into XML files
make_argos_file_from_sample<-function(argos_file_path, output_folder, parameters, generated_sample)
{
  for(s in 1:nrow(generated_sample))
  {
    
    argos_file <- read_xml(argos_file_path)
    child_nodes<<-xml_children(argos_file)
      
    for(param in 1:length(parameters))
    {
      search_child_nodes_and_set_attribute(child_nodes,parameters[param], generated_sample[s,param])
    }

    # Write out the XML file
    write_xml(argos_file,file.path(output_folder,paste("argos_experiment_set_",s,".argos",sep="")),options="format")
  }
  
  #current_wd<-getwd()
  #setwd(output_folder)
  #zip(zipfile = zipLocation, dir(file.path(output_folder), full.names = TRUE), flags="-qjr")
  #showModal(modalDialog(
  #  title = "Zip File Created",
  #  "A Zip file of ARGoS files has been created at:       ", zipLocation))

  #for(s in 1:nrow(generated_sample)) #Remove all XML files once they've been zipped
  #{
  # file.remove(file.path(output_folder, paste0("argos_experiment_set_",s,".argos")))
  #}
  # Remove the generated directory
  #unlink(file.path(getwd(),"argosFiles"),recursive=TRUE, force=TRUE)
  
  # Change the wd back
  #setwd(current_wd)
  
   
  
}
