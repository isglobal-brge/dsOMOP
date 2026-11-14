"""A-29 closed numeric schema extension. Idempotent; leaves mutate-v1 unchanged."""
import json, pathlib, hashlib, copy
root=pathlib.Path(__file__).resolve().parents[2]
ref=lambda n:{'$ref':'#/$defs/'+n}
arr=lambda t,**kw:dict(type='array',items=t,**kw)
obj=lambda props,req=():dict(type='object',additionalProperties=False,properties=props,required=list(req))
num={'type':'number'};string={'type':'string','minLength':1};enum=lambda *v:{'enum':list(v)}
for filename in ['plan-v2.json','ir-v2.json']:
 p=root/'inst/schema'/filename;s=json.loads(p.read_text());d=s['$defs']
 # Embed unchanged mutation grammar with references relocated into the enclosing schema.
 m=json.loads((root/'inst/schema/mutate-v1.json').read_text())
 def relocate(x):
  if isinstance(x,list):return [relocate(y) for y in x]
  if isinstance(x,dict):return {k:('#/$defs/number_ast_'+v.split('/')[-1] if k=='$ref' else relocate(v)) for k,v in x.items()}
  return x
 for k,v in m['$defs'].items():d['number_ast_'+k]=relocate(v)
 d['number_exprs']=relocate({k:v for k,v in m.items() if k not in ['$defs','$id','$schema']})
 d['number_unit']={'oneOf':[obj({'kind':{'const':'source'}},['kind']),obj({'kind':{'const':'event_unit'},'spec':ref('unit')},['kind','spec']),obj(dict(kind={'const':'fixed_unit'},**{k:string for k in ['from','to']},factor=dict(type='number',exclusiveMinimum=0),offset=num),['kind','from','to','factor'])]}
 d['number_transform']=obj(dict(method=enum('identity','log','log10','log2','log1p','sqrt','abs','exp','expm1','affine','standardize','clip'),**{k:num for k in ['factor','offset','center','scale','lower','upper']}),['method'])
 d['number_transform']={'oneOf':[obj(dict(method={'const':method},**{k:num for k in keys}),['method']+[k for k in keys if not(method=='affine' and k=='offset')]) for method,keys in [(m,[]) for m in ['identity','log','log10','log2','log1p','sqrt','abs','exp','expm1']]+[('affine',['factor','offset']),('standardize',['center','scale']),('clip',['lower','upper'])]]}
 d['number_round']=obj(dict(method=enum('none','decimals','significant','floor','ceiling','trunc'),digits={'type':'integer','minimum':-308,'maximum':308}),['method'])
 fields=dict(unit=ref('number_unit'),transform=arr(ref('number_transform'),minItems=1),rounding=ref('number_round'),representation=enum('number','bins','category','flag','present','missing'),breaks=arr({'oneOf':[num,enum('-Inf','Inf')]},minItems=2),grid=enum('count_1_2_5','proportion_quarters','signed_decades'),closed=enum('left','right'),labels=arr(string,minItems=1),range=enum('reference','thresholds','bins'),thresholds=arr(num,minItems=1,maxItems=2),op=enum('lt','le','gt','ge','eq','ne','inside','outside'),category_map=arr(string,minItems=1),levels=arr(string,minItems=1))
 stages=copy.deepcopy(d['number_unit']['oneOf'][1:])
 for kind,key in [('transform','number_transform'),('round','number_round')]:
  for branch in d[key].get('oneOf',[d[key]]):
   t=copy.deepcopy(branch);t['properties']['kind']={'const':kind};t['required']+=['kind'];stages.append(t)
 stages.append(obj(dict(kind={'const':'expression'},exprs=ref('number_exprs'),result=ref('name'),unit=string),['kind','exprs']))
 for kind,keys in dict(bins=['breaks','grid','closed','labels'],category=['range','thresholds','labels','category_map','levels'],flag=['op','thresholds','closed'],present=[],missing=[]).items():stages.append(obj(dict(kind={'const':kind},**{k:fields[k] for k in keys}),['kind']))
 d['number_stage']={'oneOf':stages};fields['pipeline']=arr(ref('number_stage'),minItems=1);d['number_spec']={'oneOf':[obj({k:v for k,v in fields.items() if k!='pipeline'}),obj({'pipeline':fields['pipeline']},['pipeline'])]}
 s['properties']['options']['properties']['number_spec']=ref('number_spec')
 for branch in d['variable']['oneOf']:branch['properties']['number_spec']=ref('number_spec')
 variants=[]
 for kind,props,req in [
  ('number_rows',dict(source=ref('source_ref'),value=ref('column_name')),['source','value']),
  ('number_count',dict(source=ref('source_ref'),value=ref('column_name'),window=ref('window'),count_kind=enum('nonmissing','missing')),['source','value','count_kind']),
  ('number_change',dict(source=ref('source_ref'),value=ref('column_name'),window=ref('window'),method=enum('difference','percent_change','ratio','slope','time_to_threshold'),missing=enum('skip','keep'),reference={'oneOf':[ref('date_source'),obj({'kind':{'const':'first_observation'}},['kind'])]},time_unit=enum('days','weeks'),threshold=num,op=enum('ge','gt','le','lt')),['source','value','method','missing']),
  ('number_derive',dict(inputs=arr(obj(dict(alias=ref('name'),variable=ref('variable'),stage=enum('canonical','presented')),['alias','variable','stage']),minItems=1),exprs=ref('number_exprs'),unit=string,result=ref('name'),reducer=ref('reducer')),['inputs','exprs','unit']),
  ('number_column',dict(column=ref('name'),component=ref('name')),['column'])]:variants.append(obj(dict(kind={'const':kind},**props),['kind']+req))
 d['variable']['oneOf']=[b for b in d['variable']['oneOf'] if not b.get('properties',{}).get('derived',{}).get('$ref')=='#/$defs/number_derived']
 d['number_derived']={'oneOf':variants}
 d['variable']['oneOf'].append(obj(dict(derived=ref('number_derived'),number_spec=ref('number_spec'),broadcast={'type':'boolean','default':False},labels={'type':'object','additionalProperties':string},column_names={'type':'object','additionalProperties':string}),['derived']))
 d['output']['properties']['number_columns']=dict(type='object',minProperties=1,propertyNames=ref('name'),additionalProperties=ref('variable'))
 p.write_text(json.dumps(s,indent=2,ensure_ascii=False)+'\n')
# Refresh the manifest and client copies, including shared numeric contract files.
for pkg in [root,root.parent/'dsOMOPClient']:
 if pkg!=root:
  for name in ['plan-v2.json']: (pkg/'inst/schema'/name).write_bytes((root/'inst/schema'/name).read_bytes())
 directory=pkg/'inst/schema';manifest=directory/'SHA256SUMS'
 lines=[]
 for line in manifest.read_text().splitlines():
  _,name=line.split(None,1);name=name.strip();path=directory/name
  lines.append(hashlib.sha256(path.read_bytes()).hexdigest()+'  '+name)
 manifest.write_text('\n'.join(lines)+'\n')
