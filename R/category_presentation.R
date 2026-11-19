.categoryVocabularyLabels <- function(handle,records,s) {
  style<-s$label_style %||% "name"
  if(style=="source_code") .categoryReject("source_code needs an independently admitted source-concept operand; use standard code.","category_source_forbidden")
  if(style=="name" && (is.null(s$language)||identical(s$language,"en")))return(records)
  bp<-.buildBlueprint(handle);tables<-bp$tables$table_name[bp$tables$present_in_db]
  if(!"concept" %in% tables)return(records)
  identities<-vapply(records,function(r)r$effective_id %||% r$id,"")
  ids<-sub("^concept:","",identities);ids<-ids[grepl("^[0-9]+$",ids)]
  if(!length(ids))return(records)
  table<-.qualifyTable(handle,"concept",.resolveTableSchema(handle,"concept","Vocabulary"))
  fields<-intersect(c("concept_id","concept_code","vocabulary_id"),bp$columns$concept$column_name)
  data<-do.call(rbind,lapply(.sqlIdChunks(ids),function(list)
    .executeQuery(handle,paste0("SELECT ",paste(fields,collapse=", ")," FROM ",table," WHERE concept_id IN (",list,")"))))
  synonyms<-NULL
  if(is.list(s$language) && "concept_synonym" %in% tables) {
    table<-.qualifyTable(handle,"concept_synonym",.resolveTableSchema(handle,"concept_synonym","Vocabulary"))
    synonyms<-do.call(rbind,lapply(.sqlIdChunks(ids),function(list)
      .executeQuery(handle,paste0("SELECT concept_id, concept_synonym_name FROM ",table," WHERE language_concept_id = ",s$language$language_concept_id," AND concept_id IN (",list,")"))))
  }
  for(k in names(records)) {
    r<-records[[k]];if(r$origin %in% c("variable","recipe","recode","producer","sentinel"))next
    identity<-identities[[k]]
    if(!grepl("^concept:",identity))next
    i<-match(sub("^concept:","",identity),as.character(data$concept_id))
    name<-r$name %||% sub("^concept:","",identity)
    candidates<-as.character(synonyms$concept_synonym_name[as.character(synonyms$concept_id)==sub("^concept:","",identity)])
    candidates<-candidates[!is.na(candidates)&nzchar(trimws(candidates))]
    if(length(candidates)){name<-sort(unique(enc2utf8(candidates)),method="radix")[1L];r$label_origin<-"synonym"}
    code<-if(!is.na(i)&&"concept_code" %in% names(data))as.character(data$concept_code[i]) else NA_character_
    vocab<-if(!is.na(i)&&"vocabulary_id" %in% names(data))as.character(data$vocabulary_id[i]) else NA_character_
    if(is.na(code)||!nzchar(code))code<-sub("^concept:","",identity)
    if(is.na(vocab)||!nzchar(vocab))vocab<-sub("^concept:","",identity)
    r$name<-switch(style,name=name,code=code,code_name=paste(code,name,sep=" - "),vocabulary_code=paste(vocab,code,sep=":"))
    r$label<-r$name;records[[k]]<-r
  }
  records
}
#' @export
`[.dsomop_category_factor` <- function(x,...,drop=FALSE) {
  contract<-attr(x,"omop_category_contract");out<-NextMethod("[",drop=FALSE)
  attr(out,"omop_category_contract")<-contract;out
}
