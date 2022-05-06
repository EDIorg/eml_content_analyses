
get_fair_for_eml <- function(eml_file) {
  
  # this function take an EML file as input and return a data frame of results
  # it has only been tested with EML files from EDI

  # dataset has ID and ID is resolvable
  
  eml_id <- xml_attr(eml_file, 'packageId')

  eml_alt_id <- xml_text(xml_find_all(eml_file, './dataset/alternateIdentifier'))
  eml_alt_id_syst <- xml_find_all(eml_file, './dataset/alternateIdentifier') %>%
    xml_attr('system')
  eml_alt_id <- str_remove(eml_alt_id, 'doi:')
  
  eml_id_text <- paste(eml_alt_id_syst, eml_alt_id, sep = '/')
  eml_id_resolv <- ifelse(str_detect(eml_id_text, "https://|http://"), 1, 0)
  
  # title length
  
  eml_title <- xml_text(xml_find_first(eml_file, './/title'))
  title_words <- str_split(eml_title, '\\s+')
  title_length <- length(title_words[[1]])
  
  # abstract length
  
  eml_abstract <- xml_text(xml_find_first(eml_file, './/abstract'))
  eml_abstract <- str_replace_all(eml_abstract, '\\\n', ' ')
  eml_abstract <- str_remove_all(eml_abstract, '\\\t')
  abstract_words <- str_split(eml_abstract, '\\s+')
  abstract_length <- length(abstract_words[[1]])
  
  
  # number of keywords, keyword types, keyword thesaurus
  
  eml_keywordsets <- xml_find_all(eml_file,'.//keywordSet')
  eml_keywords <- xml_find_all(eml_keywordsets, './/keyword')
  num_keywords <- length(eml_keywords)
  eml_keyword_attr <- xml_has_attr(eml_keywords, 'keywordType')
  num_keywordtype <- length(which(eml_keyword_attr))
  eml_thesaurus <- xml_find_all(eml_keywordsets, './/keywordThesaurus')
  num_thesaurus <- length(eml_thesaurus)
  
  
  # pub date
  
  eml_pubdate <- xml_text(xml_find_first(eml_file, './/pubDate'))
  
  
  #creator and orcid ID
  
  num_creators <- length(xml_find_all(eml_file, './dataset/creator'))
  num_orcids <- length(xml_find_all(eml_file, './dataset/creator/userId'))

  
  # coverages present
  
  eml_geog_num <- length(xml_find_all(eml_file, './/geographicCoverage'))
  if (eml_geog_num > 0) {eml_geog <- "yes"} else {eml_geog <- "no"}
  eml_geog_descr_num <- length(xml_find_all(eml_file, './/geographicDescription'))
  if (eml_geog_descr_num == eml_geog_num) {eml_geog_descr <- "yes"} else {eml_geog_descr <- "no"}
  eml_time_num <- length(xml_find_all(eml_file, './/temporalCoverage'))
  if (eml_time_num > 0) {eml_time <- "yes"} else {eml_time <- "no"}
  eml_taxon_num <- length(xml_find_all(eml_file, './/taxonomicCoverage'))
  if (eml_taxon_num > 0 ) {eml_taxon <- "yes"} else {eml_taxon <- "no"}
  
  
  # access is public
  
  eml_access <- xml_text(xml_find_all(eml_file, './access/allow/principal'))
  eml_public <- str_detect(eml_access, 'public')
  public_num <- length(which(eml_public))
  if (public_num > 0) {public <- "yes"} else {public <- "no"}
  
  
  # contact and contact ID present
  
  eml_contact <- length(xml_find_all(eml_file, './dataset/contact/electronicMailAddress'))
  
  eml_contact_id <- length(xml_find_all(eml_file, './dataset/contact/userId'))
  
  
  # publisher and publisher ID present
  
  # this is automatically added by EDI
  
  eml_publisher <- length(xml_find_all(eml_file, './dataset/publisher'))
  eml_publisher_id <- length(xml_find_all(eml_file, './dataset/publisher/userID'))
  
  
  # landing page link present
  
  eml_landing <- xml_text(xml_find_all(eml_file, './dataset/distribution/online/url[@function="information"]'))
  eml_landing <- ifelse(length(eml_landing > 0), eml_landing, "")
  eml_landing_resolv <- ifelse(str_detect(eml_landing, "https://|http://"), 1, 0)
  
  
  # quality description present
  
  num_qualitydesc <- length(xml_find_all(eml_file, '//qualityControl'))
  
  
  # methods description present and length
  
  eml_methods <- xml_text(xml_find_all(eml_file, '//methods'))
  if (length(eml_methods) > 0){
    eml_methods <- str_replace_all(eml_methods, '\\\n', ' ')
    eml_methods <- str_remove_all(eml_methods, '\\\t')
    eml_methods_word <- str_split(eml_methods, '\\s+')
    methods_length <- length(eml_methods_word[[1]])
  }
  
  
  # license present
  
  num_license <- length(xml_find_all(eml_file, '//intellectualRights'))
  num_license <- num_license + length(xml_find_all(eml_file, '//licensed'))
  
  
  # provenance data source present
  
  num_provdatasource <- length(xml_find_all(eml_file, '//dataSource'))
  
  
  # processing code present and described
  
  # This checks for certain file extensions
  
  num_software <- length(xml_find_all(eml_file, '//software'))
  eml_script <- xml_text(xml_find_all(eml_file, '//otherEntity/physical/objectName'))
  extensions <- character(0)
  extensions_to_check <- c('R', 'r', 'py', 'sql')
  script <- 'no'
  if (length(eml_script) > 0){
    for (j in 1:length(eml_script)) {
      eml_scriptparts <- str_split(eml_script[j], '\\.')
      p <- length(eml_scriptparts[[1]])
      extensions <- append(extensions, eml_scriptparts[[1]][p])
    }
    for (j in 1:length(extensions_to_check)) {
      if(extensions_to_check[j]  %in% extensions){
        script <- 'yes'
      }
    }
  }
  
  
  # entity information
  
  eml_entities <- xml_find_all(eml_file, './/entityName')
  num_entities <- length(eml_entities)
  eml_entity_info <- xml_siblings(eml_entities)
  num_entity_url <- length(xml_find_all(eml_entity_info, './distribution/online/url'))
  num_checksum <- length(xml_find_all(eml_entity_info, './authentication[1]'))
  num_entitydescr <- length(xml_find_all(eml_entity_info, '//entityDescription'))
  num_enitydescrsufficient <- 0
  if (num_entitydescr > 0){
    eml_entitydescr <- xml_text(xml_find_all(eml_entity_info, '//entityDescription'))
    for (j in 1:num_entitydescr) {
      entitydescr_words <- str_split(eml_entitydescr[j], '\\s+')
      if (length(entitydescr_words[[1]]) > 2){
        num_enitydescrsufficient <- num_enitydescrsufficient + 1
      }
    }
  }
  num_entity_format <- length(xml_find_all(eml_entity_info, '//physical/dataFormat'))
  
  entity_ids_text <- xml_attr(xml_parent(eml_entities), 'id')
  entity_syst_text <- xml_attr(xml_parent(eml_entities), 'system')
  entity_ids <- length(entity_ids_text[!is.na(entity_ids_text)])
  entity_syst <- length(entity_syst_text[!is.na(entity_syst_text)])
  entity_alt_ids <- length(xml_find_all(xml_parent(eml_entity_info), './alternateIdentifier'))
  entity_ids <- entity_ids + entity_alt_ids
  num_entity_id <- ifelse(entity_ids > num_entities, num_entities, entity_ids)
  entity_alt_syst <- length(xml_find_all(xml_parent(eml_entity_info), './alternateIdentifier[@system]'))
  entity_syst <- entity_syst + entity_alt_syst
  num_entity_syst <- ifelse(entity_syst > num_entities, num_entities, entity_syst)
  
  num_otherentity <- length(xml_find_all(eml_file, '//otherEntity'))
  
  #find out if file types are proprietary or open
  
  eml_entity_filename <- xml_text(xml_find_all(eml_file, './/physical/objectName'))
  entity_extensions <- vector()
  
  if(length(eml_entity_filename) > 0) {
    for (j in 1:length(eml_entity_filename)) {
      eml_extension_parts <- str_split(eml_entity_filename[j], '\\.')
      p <- length(eml_extension_parts[[1]])
      if(p>1) {
        entity_extensions[j] <- eml_extension_parts[[1]][p]
      }
    }
  }
  
  extensions <- data.frame(entity_extensions)
  standard <- read.csv('https://github.com/NCEAS/metadig-checks/raw/main/data/DataONEformats.csv')
  
  standard <- standard %>%
    distinct(File.Extension, isProprietary) %>%
    mutate(entity_extensions = str_trim(File.Extension)) %>%
    mutate(isProprietary = str_trim(isProprietary)) %>%
    filter(nchar(File.Extension) > 1)
  
  format <- left_join(extensions, standard, by = "entity_extensions")
  
  open <- format %>%
    group_by(isProprietary) %>%
    summarize(count = n())
  
  num_file_proprietary <- ifelse(any(open$isProprietary == 'Y'), open$count[open$isProprietary == 'Y'], 0)
  num_file_open  <- ifelse(any(open$isProprietary == 'N'), open$count[open$isProprietary == 'N'], 0)
  
  
  
  # table entity specific information
  
  eml_tableentities <- xml_find_all(eml_file, './/dataTable')
  num_tables <- length(eml_tableentities)
  
  
  # attribute information
  
  num_attributes <- 0
  num_attributedefs <- 0
  num_attrdefsufficient <- 0
  num_attributedefdifferent <- 0
  num_attributestoragetype <- 0
  attr_nameunique <- 0
  num_attributeprecision <- 0
  
  if (length(eml_tableentities) > 0){
    for (j in 1:length(eml_tableentities)){
      eml_attributes <- xml_find_all(eml_tableentities[j], './/attribute')
      num_attributes <- num_attributes + length(eml_attributes)
      num_attributedefs <- num_attributedefs + length(xml_find_all(eml_attributes, './attributeDefinition'))
      num_attributeprecision <- num_attributeprecision + length(xml_find_all(eml_attributes, './measurementScale/interval/precision'))
      num_attributeprecision <- num_attributeprecision + length(xml_find_all(eml_attributes, './measurementScale/ratio/precision'))
      num_attributeprecision <- num_attributeprecision + length(xml_find_all(eml_attributes, './measurementScale/dateTime/dateTimePrecision'))
      num_attributestoragetype <- num_attributestoragetype + length(xml_find_all(eml_attributes, './storageType'))
      attr_names <- c('')
      
      if (length(eml_attributes) > 0){
        for (k in 1:length(eml_attributes)) {
          eml_attributename <- xml_text(xml_find_first(eml_attributes[k], './attributeName'))
          eml_attributedef <- xml_text(xml_find_first(eml_attributes[k], './attributeDefinition'))
          if (eml_attributedef != eml_attributename){
            num_attributedefdifferent <- num_attributedefdifferent + 1
          }
          attr_names <- append(attr_names, eml_attributename)
          attrdef_words <- str_split(eml_attributedef, '\\s+')
          attrdef_length <- length(attrdef_words[[1]])
          if (attrdef_length > 2){
            num_attrdefsufficient <- num_attrdefsufficient + 1
          }
        }
      }
      
      if (length(unique(attr_names)) == length(attr_names)){
        attr_nameunique <- 1
      }
    }
    
    
  }
  
  df_result <- data.frame(eml_id = eml_id,
                          eml_id_resolv = eml_id_resolv,
                          title_length = title_length,
                          abstract_length = abstract_length,
                          num_keywords = num_keywords,
                          num_keywordtype = num_keywordtype,
                          num_thesaurus = num_thesaurus,
                          pub_date = eml_pubdate,
                          num_creators = num_creators,
                          num_orcids = num_orcids,
                          geog_coverage = eml_geog,
                          geog_descr = eml_geog_descr,
                          temp_coverage = eml_time,
                          taxon_coverage = eml_taxon,
                          public = public,
                          contact = eml_contact,
                          contact_id = eml_contact_id,
                          eml_publisher = eml_publisher,
                          eml_publisher_id = eml_publisher_id,
                          eml_landing_resolv = eml_landing_resolv,
                          methods_length = methods_length,
                          num_software = num_software,
                          num_license = num_license,
                          num_provdatasource = num_provdatasource,
                          num_tables = num_tables,
                          attr_nameunique = attr_nameunique,
                          num_entities = num_entities,
                          num_entity_url = num_entity_url,
                          num_checksum = num_checksum,
                          num_entitydescr = num_entitydescr,
                          num_enitydescrsufficient = num_enitydescrsufficient,
                          num_entity_format = num_entity_format,
                          num_file_open = num_file_open,
                          num_file_proprietary = num_file_proprietary,
                          num_entity_id = num_entity_id,
                          num_entity_syst = num_entity_syst,
                          num_otherentity = num_otherentity,
                          extensions = script,
                          num_attributes = num_attributes,
                          num_attributedefs = num_attributedefs,
                          num_attrdefsufficient = num_attrdefsufficient,
                          num_attributedefdifferent = num_attributedefdifferent,
                          num_attributestoragetype = num_attributestoragetype,
                          num_qualitydesc = num_qualitydesc,
                          num_attributeprecision = num_attributeprecision)
  
  return(df_result)
}
