#xmlfile2<-xmlParse("/home/kja505/Documents/roboSpartan/sample_argos_file.argos")
#gCol=getNodeSet(xmlfile2, "//argos-configuration//controllers//params")


library(xml2)
# Specify the parameters
parameters<-c("epsilon","memory_factor")
# Read in the LHC sample:
r<-read.csv("/home/kja505/Desktop/LHC_Parameters_for_Runs.csv",header=T)
# Change the header to match the parameters we're testing changing here:
colnames(r)<-parameters

# Read in the ARGoS file


#for(i in 1:nrow(r))
for(i in 1:2)
{
  x <- read_xml("/home/kja505/Documents/roboSpartan/sample_argos_file.argos")
  baz <- xml_find_all(x, ".//params")
  children<-xml_children(baz)  
  
  for(param in 1:length(parameters))
  {
    for(child in 1:length(children))
    {
      current_val <- xml_attr(children[child],parameters[param])
      
      if(!is.na(current_val))
        xml_set_attr(children[child],parameters[param],r[i,parameters[param]])
    }
  }
  
  # Write out the XML file
  write_xml(x,paste("/home/kja505/Documents/roboSpartan/lhc_experiment_set_",i,".argos",sep=""),options="format")
}




xml_attr(s[1],"epsilon")
xml_attr(s[2],"epsilon")
xml_set_attr(s[2],"epsilon",0.75)

