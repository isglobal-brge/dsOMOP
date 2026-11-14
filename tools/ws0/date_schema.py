"""Approved A-28 additions to the closed language-2 schema."""
import copy

def apply(plan):
    D=plan['$defs']
    def ref(k): return {'$ref':'#/$defs/'+k}
    def obj(props,required=()): return {'type':'object','additionalProperties':False,'properties':props,'required':list(required)}
    def enum(x): return {'enum':x.split()}
    D['date_component_operand']={'oneOf':[obj({'kind':{'const':'integer'},'value':{'type':'integer'}},['kind','value']),obj({'kind':{'const':'column'},'name':ref('column_name')},['kind','name'])]}
    components=obj({'kind':{'const':'date_components'},'year':ref('date_component_operand'),'month':ref('date_component_operand'),'day':ref('date_component_operand'),'event':ref('source_ref'),'which':dict(enum('first last nth'),default='first'),'n':{'type':'integer','minimum':1,'default':1},'scope':dict(enum('person episode'),default='person'),'search':ref('window')},['kind','year'])
    D['date_source']={'oneOf':[ref('anchor'),components]}
    props={'precision':enum('day year semester quarter month iso_week cdc_week epi_week fiscal_year fiscal_semester fiscal_quarter fiscal_month relative_days custom'),'representation':enum('auto factor number start end'),'numeric':enum('index decimal_year'),'origin':ref('date'),'anchor':ref('date_source'),'pattern':{'type':'string','minLength':1,'maxLength':128},'labels':{'type':'object','propertyNames':{'minLength':1},'additionalProperties':{'type':'string','minLength':1}},'level_range':{'type':'array','minItems':2,'maxItems':2,'items':ref('date')},'fiscal_start':{'type':'integer','minimum':1,'maximum':12},'week_start':{'type':'integer','minimum':1,'maximum':7},'week_min_days':{'type':'integer','minimum':1,'maximum':7},'hemisphere':enum('north south'),'partial':enum('none start mid end'),'invalid':{'const':'na'},'future':enum('allow na'),'as_of':ref('date')}
    D['date_spec']=obj(props)
    plan['properties']['options']['properties']['date_spec']=ref('date_spec')
    for v in D['variable']['oneOf']: v['properties']['date_spec']=ref('date_spec')
    v=next(v for v in D['variable']['oneOf'] if 'derived' in v['properties'])
    # Broadcast is admitted only on the new native derived variants.
    new=copy.deepcopy(v);new['properties']['broadcast']={'type':'boolean','default':False}
    parts=enum('year semester quarter month month_name month_abbr day weekday weekday_name weekday_abbr iso_week iso_year day_of_year epi_week epi_year fiscal_year fiscal_quarter season month_sin month_cos weekday_sin weekday_cos season_sin season_cos')
    kinds={'calendar_date':{'reference':ref('date_source')},'date_part':{'reference':ref('date_source'),'part':parts},'time_between':{'from':ref('date_source'),'to':ref('date_source'),'unit':dict(enum('days weeks months years decimal_years'),default='days'),'absolute':{'type':'boolean','default':False},'rounding':dict(enum('none floor ceiling trunc nearest'),default='none')},'age_at':{'reference':dict(ref('date_source'),default='index'),'unit':dict(enum('years decimal_years months weeks days annual_years'),default='years'),'bands':{'type':'boolean','default':False},'age_breaks':{'type':'array','minItems':2,'items':{'type':'integer','minimum':0}}}}
    new['properties']['derived']={'oneOf':[obj(dict(kind={'const':k},**p),['kind']+({'calendar_date':['reference'],'date_part':['reference','part'],'time_between':['from','to'],'age_at':[]}[k])) for k,p in kinds.items()]}
    D['variable']['oneOf'].append(new)
    # Explicitness on the v2 wire is presence. Legacy defaults are added by the
    # shared normalizer only after effective per-column native policy resolution.
    for d in D.values():
        if isinstance(d,dict) and 'date_handling' in d.get('properties',{}): d['properties']['date_handling'].pop('default',None)
