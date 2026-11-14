"""A-30 category declarations: closed schemas and matching client vendoring."""
import pathlib, json, hashlib
root=pathlib.Path(__file__).resolve().parents[2]
ref=lambda x:{'$ref':'#/$defs/'+x}
obj=lambda p,required=():dict(type='object',additionalProperties=False,properties=p,required=list(required))
arr=lambda p:dict(type='array',items=p,uniqueItems=True)
enum=lambda *x:dict(enum=list(x))
string=dict(type='string',minLength=1)
key=dict(type='string',pattern=r'^(concept:(0|[1-9][0-9]*)|bin:[1-9][0-9]*|category:([1-9][0-9]*|[A-Za-z][A-Za-z0-9_]{0,63})|period:[^ ]+|flag:[01]|special:(missing|unknown))$')
for name in ['plan-v2.json','ir-v2.json']:
 p=root/'inst/schema'/name;s=json.loads(p.read_text());d=s['$defs']
 d['category_key']=key
 d['category_order']=obj(dict(kind={'const':'category_order'},keys=arr(ref('category_key')),metadata={'oneOf':[enum('identity','producer'),obj(dict(id=ref('name'),version=string,sha256=dict(type='string',pattern='^[0-9a-f]{64}$'),keys=arr(ref('category_key'))),['id','version','sha256','keys'])]}),['kind'])
 d['category_order']['oneOf']=[dict(required=['keys'],**{'not':dict(required=['metadata'])}),dict(required=['metadata'],**{'not':dict(required=['keys'])})]
 d['category_recode']=obj(dict(kind={'const':'category_recode'},map=dict(type='object',propertyNames=ref('category_key'),additionalProperties=dict(type='string',pattern='^category:[A-Za-z][A-Za-z0-9_]{0,63}$')),levels=arr(ref('category_key')),labels=arr(string),ordered=dict(type='boolean'),unmatched=enum('keep','missing')),['kind','map','levels','ordered','unmatched'])
 d['category_rollup']=obj(dict(kind=enum('none','atc','icd10','snomed','ancestors'),level={'oneOf':[dict(type='integer',minimum=1,maximum=5),enum('chapter','block')]},ancestors=arr(ref('category_key')),separation=dict(type='integer',minimum=0),distance=enum('min','max'),vocabulary=string,domain={'oneOf':[arr(ref('category_key')),ref('concept_set')]},multiple=enum('error','priority','all'),priority=arr(ref('category_key')),unmatched=enum('keep','missing')),['kind'])
 d['category_language']=obj(dict(kind={'const':'category_language'},language_concept_id=dict(type='integer',minimum=1,maximum=2147483647)),['kind','language_concept_id'])
 d['category_missing']=obj(dict(kind={'const':'category_missing'},na=enum('keep','level'),unknown=enum('keep','level','missing'),labels=obj(dict(missing=string,unknown=string),['missing','unknown'])),['kind','na','unknown','labels'])
 d['category_binary']=obj(dict(kind={'const':'category_binary'},**{'as':enum('integer','logical','factor')},labels=dict(type='array',items=string,uniqueItems=True,minItems=2,maxItems=2)),['kind','as','labels'])
 d['category_spec']=obj(dict(levels=arr(ref('category_key')),order=ref('category_order'),ordered=dict(type='boolean'),reference=ref('category_key'),recode=ref('category_recode'),rollup=ref('category_rollup'),label_style=enum('name','code','code_name','vocabulary_code','source_code'),language={'oneOf':[enum('en'),ref('category_language')]},missing=ref('category_missing'),binary=ref('category_binary')))
 s['properties']['options']['properties']['category_spec']=ref('category_spec')
 for branch in d['variable']['oneOf']:branch['properties']['category_spec']=ref('category_spec')
 variants=[obj(dict(kind={'const':'category_rows'},source=ref('source_ref'),value=ref('column_name')),['kind','source','value']),obj(dict(kind={'const':'category_indicators'},variable=ref('variable'),key=ref('category_key'),missing=enum('propagate','zero')),['kind','variable','key','missing'])]
 d['timepoint']=obj(dict(kind={'const':'timepoint'},offset=dict(type='integer'),unit=enum('days','weeks','months','years'),tolerance=dict(type='array',items=dict(type='integer'),minItems=2,maxItems=2),anchor=ref('date_source'),select=enum('closest','first','last')),['kind','offset','unit','tolerance','select'])
 d['timepoint']['properties']['tolerance'].update(prefixItems=[dict(type='integer',maximum=0),dict(type='integer',minimum=0)])
 d['timepoint']['properties']['offset'].update(minimum=-2147483647,maximum=2147483647)
 d['timepoints']=obj(dict(kind={'const':'timepoints'},anchor=ref('date_source'),reuse=dict(type='boolean'),points=dict(type='array',minItems=1,items=obj(dict(name=ref('name'),point=ref('timepoint')),['name','point']))),['kind','anchor','reuse','points'])
 d['visits']=obj(dict(kind={'const':'visits'},numbers=dict(type='array',items=dict(type='integer',minimum=1),uniqueItems=True,minItems=1),source=ref('source_ref'),window=ref('window'),select=enum('first','last','closest')),['kind','numbers','source','select'])
 variants.append(obj(dict(kind={'const':'repeated'},source=ref('source_ref'),values=dict(type='array',minItems=1,items=obj(dict(name=ref('name'),variable=ref('variable')),['name','variable'])),timepoints=ref('timepoints'),visits=ref('visits'),missing=enum('keep','skip_all','skip_any'),point=ref('name'),value=ref('name')),['kind','source','values','missing','point','value']))
 variants[-1]['oneOf']=[dict(required=['timepoints'],**{'not':dict(required=['visits'])}),dict(required=['visits'],**{'not':dict(required=['timepoints'])})]
 variants.append(obj(dict(kind={'const':'category_reduce'},variable=ref('variable'),reducer=ref('reducer')),['kind','variable','reducer']))
 variants.append(obj(dict(kind={'const':'category_column'},column=ref('name'),component=ref('name')),['kind','column']))
 d['output']['properties']['category_columns']=dict(type='object',minProperties=1,propertyNames=ref('name'),additionalProperties=ref('variable'))
 d['category_derived']={'oneOf':variants}
 d['variable']['oneOf']=[b for b in d['variable']['oneOf'] if b.get('properties',{}).get('derived',{}).get('$ref')!='#/$defs/category_derived']
 d['variable']['oneOf'].append(obj(dict(derived=ref('category_derived'),category_spec=ref('category_spec'),number_spec=ref('number_spec'),broadcast=dict(type='boolean',default=False),labels=dict(type='object',additionalProperties=string),column_names=dict(type='object',additionalProperties=string)),['derived']))
 p.write_text(json.dumps(s,indent=2,ensure_ascii=False)+'\n')
client=root.parent/'dsOMOPClient'
for name in ['plan-v2.json']:(client/'inst/schema'/name).write_bytes((root/'inst/schema'/name).read_bytes())
for pkg in [root,client]:
 directory=pkg/'inst/schema';manifest=directory/'SHA256SUMS';lines=[]
 for line in manifest.read_text().splitlines():
  _,name=line.split(None,1);name=name.strip()
  lines.append(hashlib.sha256((directory/name).read_bytes()).hexdigest()+'  '+name)
 manifest.write_text('\n'.join(lines)+'\n')
