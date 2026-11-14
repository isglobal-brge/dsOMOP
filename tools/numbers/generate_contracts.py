"""Reproducible A-29 wire probes and independent normal-form digest fixtures."""
import copy,hashlib,json,pathlib,sys
root=pathlib.Path(__file__).resolve().parents[2]
sys.path.insert(0,str(root/'tools/ws0'))
from normalize_reference import normalize
from jcs_reference import digest,jcs
from jsonschema import Draft202012Validator
schema=json.loads((root/'inst/schema/plan-v2.json').read_text())
base=dict(version=2,meta=dict(language_version='2.0',semantic_version='2.0.0',generated_by='A29-contract-fixture'),events=dict(lab=dict(table='measurement',concept_set=dict(concepts=[3000963]))),populations=dict(p=dict(kind='criteria')),variables=dict(x=dict(source=dict(event='lab'),reducer=dict(fn='mean'))),outputs=dict(o=dict(rows='p',format='wide')))
vectors=[]
def add(name,spec=None,edit=None):
 p=copy.deepcopy(base)
 if spec is not None:p['variables']['x']['number_spec']=spec
 if edit:edit(p)
 Draft202012Validator(schema).validate(p)
 n=normalize(p,schema)
 assert normalize(n,schema)==n,name+' not idempotent'
 vectors.append(dict(label=name,instance=p,normalized=n,digest_sha256=digest(n)[1]))
add('identity',{})
add('floor-shorthand',dict(rounding=dict(method='floor')))
add('floor-pipeline',dict(pipeline=[dict(kind='round',method='floor')]))
add('bins',dict(representation='bins',grid='count_1_2_5'))
add('manual-category',dict(representation='category',thresholds=[5,10]))
add('map',dict(unit=dict(kind='event_unit',spec=dict(target=8840,convert={'8713':dict(factor=1000,offset=0)})),rounding=dict(method='decimals',digits=1)))
add('recipe-default',{},lambda p:p.update(options=dict(number_spec=dict(rounding=dict(method='floor')))))
add('reset',dict(pipeline=[dict(kind='transform',method='identity')]),lambda p:p.update(options=dict(number_spec=dict(rounding=dict(method='floor')))))
expr=dict(version=1,assign=[dict(name='y',expr=dict(op='*',args=[dict(col='a'),dict(lit=2)]))])
def derived(p):
 p['variables']['x']=dict(derived=dict(kind='number_derive',inputs=[dict(alias='a',stage='canonical',variable=copy.deepcopy(base['variables']['x']))],exprs=expr,unit='coordinate:doubled'))
add('derive',edit=derived)
def columns(p):p['outputs']['o']['number_columns']=dict(copy=dict(derived=dict(kind='number_column',column='x'),number_spec=dict(rounding=dict(method='floor'))))
add('columns',edit=columns)
probes=[dict(id='Nwire'+str(i+1),schema='plan-v2',expect='valid',label=v['label'],instance=v['instance']) for i,v in enumerate(vectors)]
for label,path,value in [('unknown-spec',['variables','x','number_spec'],dict(typo=1)),('string-breaks',['variables','x','number_spec'],dict(representation='bins',breaks=['1','2'])),('code',['variables','x','number_spec'],dict(pipeline=[dict(kind='expression',exprs='x * 2')])),('unknown-step',['variables','x','number_spec'],dict(pipeline=[dict(kind='round',method='floor',typo=True)]))]:
 p=copy.deepcopy(base);owner=p
 for key in path[:-1]:owner=owner[key]
 owner[path[-1]]=value
 assert not Draft202012Validator(schema).is_valid(p)
 probes.append(dict(id='Nwire'+str(len(probes)+1),schema='plan-v2',expect='invalid',label=label,instance=p))
for repo in [root,root.parent/'dsOMOPClient']:
 directory=repo/'inst/schema'
 for name,content in [('number-vectors.json',vectors),('probes/number-probes.json',probes)]:
  (directory/name).write_text(json.dumps(content,indent=2)+'\n')
 names=[line.split(None,1)[1].strip() for line in (directory/'SHA256SUMS').read_text().splitlines()]
 for name in ['number-vectors.json','probes/number-probes.json']:
  if name not in names:names.append(name)
 (directory/'SHA256SUMS').write_text(''.join(hashlib.sha256((directory/n).read_bytes()).hexdigest()+'  '+n+'\n' for n in names))
print('generated',len(vectors),'normal forms and',len(probes),'wire probes')
