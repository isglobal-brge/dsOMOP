"""Vendor the unchanged mutate-v1 type checker into the closed numeric context."""
from pathlib import Path
import re
root=Path(__file__).resolve().parents[2]
def function(file,name):
 s=(root/file).read_text();m=re.search(r'^'+re.escape(name)+r' <- function.*?(?=^\}\n)',s,re.M|re.S)
 if m:return m.group(0)+'}\n'
 m=re.search(r'^'+re.escape(name)+r' <- function[^\n]*\n',s,re.M)
 if not m:raise ValueError(name)
 return m.group(0)
# .verbScalar is a one-line definition: handle it separately.
names=['.verbColumn','.verbCheckLiteral','.verbCheckPredicate','.mutateUnify','.mutateOpTypes','.mutateVariant','.mutateType']
parts=[function('R/mutate.R',n) for n in names]
body='''# Generated from the existing mutate-v1 checker by tools/numbers/vendor_expression_types.py.
# Only admission adapters differ: all initial columns are declared private aliases.
.numberAstCheck <- local({
.verbReject <- function(caller,message,field=NULL) .numberReject(message,"number_expression_invalid")
.assignmentBlockedColumn <- function(name) name %in% c("person_id","subject_id","row_id","cohort_row_id","event_id")
.validateString <- function(x,short=FALSE) {if(nchar(x)>4096L) .numberReject("Expression string exceeds the existing bound.");x}
.verbScalar <- function(v) if(is.list(v)&&length(v)==1L&&is.null(names(v)))v[[1L]] else v
'''+function('R/extraction.R','.isoDate')+function('R/extraction.R','.extractionCap')+function('R/extraction.R','.filterComplexityVisit')+'\n'.join(parts)+'''
function(exprs,aliases,result) {
  types<-setNames(rep("numeric",length(aliases)),aliases)
  for(a in exprs$assign) types[[a$name]]<-.mutateType(a$expr,types,character(),"numeric expression","/exprs")
  if(!types[[result]] %in% c("numeric","integer")) .numberReject("Expression result must be numeric; use native bins/flags.","number_expression_invalid")
  invisible(types[[result]])
}
})
'''
# Avoid a new client dependency for a literal-domain check; same finite signed64 bound.
body=body.replace('!is.na(suppressWarnings(bit64::as.integer64(v)))','v > -2^63 && v < 2^63')
for pkg in [root,root.parent/'dsOMOPClient']:(pkg/'R/number_expression_types.R').write_text(body)
