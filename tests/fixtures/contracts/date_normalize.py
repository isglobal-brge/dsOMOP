"""A-28 normal-form reference; schema validation precedes this function."""
import copy
KINDS = ('calendar_date', 'date_part', 'time_between', 'age_at')

def effective(variable, recipe, kind, part=None):
    variable, recipe = variable or {}, recipe or {}
    raw = dict(recipe, **variable)
    p, r = raw.get('precision', 'day'), raw.get('representation', 'auto')
    allowed = {'partial', 'invalid', 'future', 'as_of'}
    out = {'partial':'none', 'invalid':'na', 'future':'allow'}
    if kind == 'calendar_date':
        allowed |= {'precision', 'representation'}
        out.update(precision=p, representation=r)
        if p == 'relative_days': allowed.add('anchor'); out['anchor']='index'
        if p == 'custom': allowed.add('pattern')
        if p.startswith('fiscal_'): allowed.add('fiscal_start'); out['fiscal_start']=1
        if p == 'epi_week': allowed |= {'week_start', 'week_min_days'}
        if r == 'number' and p not in ('day','relative_days','custom'):
            allowed.add('numeric'); out['numeric']='index'
            if p not in ('year','fiscal_year') and raw.get('numeric','index') == 'index':
                allowed.add('origin'); out['origin']='1970-01-01'
        if r == 'factor' or r == 'auto' and p not in ('day','relative_days'):
            allowed |= {'labels','level_range'}
    elif kind == 'date_part':
        r = variable.get('representation', recipe.get('representation','auto') if recipe.get('representation','auto') in ('auto','number') else 'auto')
        raw['representation']=r;out['representation']=r;allowed.add('representation')
        if part.startswith('epi_'): allowed |= {'week_start','week_min_days'}
        if part.startswith('fiscal_'): allowed.add('fiscal_start');out['fiscal_start']=1
        if part.startswith('season'): allowed.add('hemisphere');out['hemisphere']='north'
        if part in ('month','month_name','month_abbr','weekday','weekday_name','weekday_abbr','season') and r != 'number': allowed.add('labels')
    if set(variable)-allowed: raise ValueError('date_spec_invalid: irrelevant field')
    out.update({k:v for k,v in raw.items() if k in allowed})
    return out

def normalize_dates(x):
    recipe=x.get('options',{}).get('date_spec')
    for v in x.get('variables',{}).values():
        d=v.get('derived',{});kind=d.get('kind','calendar_date')
        native=kind in KINDS and bool(d) or 'date_spec' in v or recipe is not None and v.get('reducer',{}).get('fn') in ('first_date','last_date')
        if not native: continue
        v['date_spec']=effective(v.get('date_spec'),recipe,kind,d.get('part'))
        if kind == 'age_at' and d.get('bands'): d.setdefault('age_breaks',list(range(0,101,5)))
    for o in x.get('outputs',{}).values():
        variables=x.get('variables',{})
        names=o.get('columns') or list(variables)
        native=[n for n in names if 'date_spec' in variables.get(n,{})]
        policy=o.get('options',{}).get('date_handling')
        if policy is not None and (native or recipe is not None):
            targets=policy.get('date_columns')
            if recipe is not None or not targets or set(targets)&set(native): raise ValueError('date_policy_conflict')
        if o.get('format') in ('wide','long','membership') and not native and recipe is None:
            o.setdefault('options',{}).setdefault('date_handling',{'mode':'relative','reference':'index','bin_width':None,'date_columns':None})
    return x
