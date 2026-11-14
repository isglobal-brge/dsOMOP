"""Independent A-30 closed-wire probes and canonical normal-form fixtures."""
import copy, hashlib, json, pathlib, sys
root = pathlib.Path(__file__).resolve().parents[2]
sys.path.insert(0, str(root / 'tools/ws0'))
from normalize_reference import normalize
from jcs_reference import digest
from jsonschema import Draft202012Validator
schema = json.loads((root / 'inst/schema/plan-v2.json').read_text())
validator = Draft202012Validator(schema)
base = dict(version=2, meta=dict(language_version='2.0', semantic_version='2.0.0', generated_by='A30-contract-fixture'), events=dict(e=dict(table='observation',concept_set=dict(concepts=[45000120]))), populations=dict(p=dict(kind='criteria')), variables=dict(x=dict(source=dict(event='e'),reducer=dict(fn='last_value',value='value_as_concept_id'))), outputs=dict(o=dict(rows='p',format='wide')))
vectors=[]
def add(label,spec=None,edit=None):
    p=copy.deepcopy(base)
    if spec is not None:p['variables']['x']['category_spec']=spec
    if edit:edit(p)
    validator.validate(p)
    n=normalize(p,schema)
    assert normalize(n,schema)==n, label
    vectors.append(dict(label=label,instance=p,normalized=n,digest_sha256=digest(n)[1]))
add('custom-order',dict(levels=['concept:20','concept:10','concept:30'],order=dict(kind='category_order',keys=['concept:20','concept:10','concept:30']),ordered=True))
add('public-concept-set',dict(rollup=dict(kind='atc',level=5,domain=dict(concepts=[1,2],include_descendants=True),multiple='priority',priority=['concept:10','concept:20'],unmatched='missing')))
for na in ['keep','level']:
    for unknown in ['keep','level','missing']:
        add('missing-'+na+'-'+unknown,dict(missing=dict(kind='category_missing',na=na,unknown=unknown,labels=dict(missing='Missing',unknown='Unknown'))))
for representation in ['integer','logical','factor']:
    add('binary-'+representation,dict(binary=dict(kind='category_binary',**{'as':representation},labels=['no','yes'])),lambda p:p['variables']['x'].update(reducer=dict(fn='binary')))
add('language-style',dict(language=dict(kind='category_language',language_concept_id=4180186),label_style='code_name'))
def repeated(p):
    p['variables']['x']=dict(derived=dict(kind='repeated',source=dict(event='e'),values=[dict(name='value',variable=dict(derived=dict(kind='number_rows',source=dict(event='e'),value='value'),number_spec=dict(representation='bins',breaks=[0,5,10])))],timepoints=dict(kind='timepoints',anchor='index',reuse=False,points=[dict(name='m6',point=dict(kind='timepoint',offset=6,unit='months',tolerance=[-7,7],select='closest'))]),missing='skip_any',point='m6',value='value'))
add('repeated-bins',edit=repeated)
def columns(p):p['outputs']['o']['category_columns']=dict(copy=dict(derived=dict(kind='category_column',column='x'),category_spec=dict(ordered=True)))
add('column-default-projection',edit=columns)
probes=[dict(id='Cwire'+str(i+1),schema='plan-v2',expect='valid',label=v['label'],instance=v['instance']) for i,v in enumerate(vectors)]
for label,spec in [('unknown-field',dict(sql='select 1')),('duplicate-order',dict(order=dict(kind='category_order',keys=['concept:1','concept:1']))),('two-order-sources',dict(order=dict(kind='category_order',keys=['concept:1'],metadata='identity'))),('leading-zero',dict(levels=['concept:01'])),('executable-language',dict(language=dict(kind='category_language',language_concept_id='system()')))]:
    p=copy.deepcopy(base);p['variables']['x']['category_spec']=spec
    assert not validator.is_valid(p),label
    probes.append(dict(id='Cwire'+str(len(probes)+1),schema='plan-v2',expect='invalid',label=label,instance=p))
for repo in [root,root.parent/'dsOMOPClient']:
    directory=repo/'inst/schema'
    for name,content in [('category-vectors.json',vectors),('probes/category-probes.json',probes)]:
        (directory/name).write_text(json.dumps(content,indent=2)+'\n')
    names=[line.split(None,1)[1].strip() for line in (directory/'SHA256SUMS').read_text().splitlines()]
    for name in ['category-vectors.json','probes/category-probes.json']:
        if name not in names:names.append(name)
    (directory/'SHA256SUMS').write_text(''.join(hashlib.sha256((directory/n).read_bytes()).hexdigest()+'  '+n+'\n' for n in names))
print('generated',len(vectors),'normal forms and',len(probes),'wire probes')
