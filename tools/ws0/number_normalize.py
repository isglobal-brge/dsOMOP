"""A-29 normal form reference. The closed schema is validated before entry."""
import copy
import hashlib
from jcs_reference import jcs as canonicalize

GRIDS = {'count_1_2_5':['-Inf',0,1,2,5,10,20,50,100,'Inf'],
         'proportion_quarters':['-Inf',0,.25,.5,.75,1,'Inf'],
         'signed_decades':['-Inf',-100,-10,-1,0,1,10,100,'Inf']}
FIELDS = {'number':[], 'bins':['breaks','grid','closed','labels'],
          'category':['range','thresholds','labels','category_map','levels'],
          'flag':['op','thresholds','closed'], 'present':[], 'missing':[]}

def stages(steps):
    out=[]
    for step in copy.deepcopy(steps):
        k=step['kind']
        if k=='expression':step.setdefault('result',step['exprs']['assign'][-1]['name'])
        if k=='fixed_unit' or k=='transform' and step['method']=='affine': step.setdefault('offset',0)
        if k=='bins':
            if 'grid' in step: step['breaks']=GRIDS[step.pop('grid')]
            step.setdefault('closed','left')
        if k=='category':
            step.setdefault('range','bins' if 'category_map' in step else 'thresholds' if 'thresholds' in step else 'reference')
            if step['range']=='bins': step.setdefault('levels',['low','normal','high'])
            else: step.setdefault('labels',['low','normal','high'])
        if k=='flag' and step['op'] in ('inside','outside'): step.setdefault('closed','left')
        out.append(step)
    return out

def effective(v=None,r=None):
    v,r=copy.deepcopy(v or {}),copy.deepcopy(r or {})
    if 'pipeline' in v:return {'pipeline':stages(v['pipeline'])}
    if not v and 'pipeline' in r:return {'pipeline':stages(r['pipeline'])}
    if 'pipeline' in r:r={}
    raw=dict(r,**v)
    if 'breaks' in v:raw.pop('grid',None)
    if 'grid' in v:raw.pop('breaks',None)
    if 'thresholds' in v and 'range' not in v:raw['range']='thresholds'
    if 'category_map' in v and 'range' not in v:raw['range']='bins'
    rep=raw.get('representation','number')
    allowed=['unit','representation']+FIELDS[rep]+([] if rep in ('present','missing') else ['transform','rounding'])
    if set(v)-set(allowed):raise ValueError('number_spec_invalid: irrelevant fields')
    raw={k:x for k,x in raw.items() if k in allowed}
    pipeline=[]
    if raw.get('unit',{}).get('kind','source')!='source':pipeline.append(raw['unit'])
    for transform in raw.get('transform',[]):pipeline.append(dict(kind='transform',**transform))
    if 'rounding' in raw:pipeline.append(dict(kind='round',**raw['rounding']))
    if rep!='number':pipeline.append(dict(kind=rep,**{k:raw[k] for k in FIELDS[rep] if k in raw}))
    return {'pipeline':stages(pipeline or [{'kind':'transform','method':'identity'}])}

def overlay(high,low):
    high,low=copy.deepcopy(high or {}),copy.deepcopy(low or {})
    if 'pipeline' in high:return high
    if not high:return low
    if 'pipeline' in low:low={}
    if 'representation' in high:
        rep=high['representation']
        allowed=['unit','representation']+FIELDS[rep]+([] if rep in ('present','missing') else ['transform','rounding'])
        low={k:v for k,v in low.items() if k in allowed}
    low.update(high)
    if 'breaks' in high:low.pop('grid',None)
    if 'grid' in high:low.pop('breaks',None)
    return low

def normalize_numbers(plan):
    recipe=plan.get('options',{}).get('number_spec')
    producers=copy.deepcopy(plan.get('variables',{}))
    def norm(v,defaults=None,column_scope=False):
        d=v.get('derived',{})
        if 'inputs' in d:
            d.setdefault('result',d['exprs']['assign'][-1]['name'])
            for i in d['inputs']:i['variable']=norm(i['variable'],column_scope=column_scope)
        if d.get('kind')=='number_daily':d['input']=norm(d['input'],column_scope=column_scope)
        if 'variable' in d:d['variable']=norm(d['variable'],column_scope=column_scope)
        if d.get('kind')=='repeated':
            for entry in d['values']:entry['variable']=norm(entry['variable'],column_scope=column_scope)
        if d.get('kind')=='number_change' and d['method'] in ('slope','time_to_threshold'):
            d.setdefault('time_unit','days')
            if d['method']=='time_to_threshold':
                d.setdefault('op','ge');d.setdefault('reference',{'kind':'first_observation'})
        spec=v.get('number_spec')
        if column_scope and d.get('kind')=='number_column':
            spec=overlay(spec,without_source(producers.get(d['column'],{}).get('number_spec')))
            defaults=without_source(defaults)
        source=v.get('source',d.get('source',{}))
        eff=effective(spec,defaults) if spec is not None or defaults is not None else {}
        maps=[s for s in eff.get('pipeline',[]) if s['kind']=='event_unit']
        if maps and 'event' in source:
            ev=copy.deepcopy(plan['events'][source['event']])
            if ev.get('table') not in ('measurement','observation'):
                if spec is not None:raise ValueError('number_unit_conflict')
                return v
            ev['unit']=maps[0]['spec']
            payload=canonicalize(dict(source=source["event"],event=ev))
            if isinstance(payload,str):payload=payload.encode()
            key=source['event'] if source['event'].startswith('num') and len(source['event'])==27 else 'num'+hashlib.sha256(payload).hexdigest()[:24]
            plan['events'][key]=ev;source['event']=key
        if spec is not None:v['number_spec']=effective(spec,defaults)
        return v
    for n,v in list(plan.get('variables',{}).items()):plan['variables'][n]=norm(v,recipe)
    for out in plan.get('outputs',{}).values():
        if recipe is not None:
            for owner in (out,out.get('options',{})):
                for source in owner.get('sources',[]):
                    if 'event' in source:norm({'source':source,'reducer':{'fn':'mean'}},recipe)
        if 'number_columns' in out:out['number_columns']={n:norm(v,recipe,True) for n,v in out['number_columns'].items()}
    return plan


def without_source(spec):
    if spec is None:return None
    spec=copy.deepcopy(spec)
    if spec.get('unit',{}).get('kind')=='event_unit':spec.pop('unit')
    if 'pipeline' in spec:spec['pipeline']=[s for s in spec['pipeline'] if s['kind']!='event_unit'] or [dict(kind='transform',method='identity')]
    return spec
