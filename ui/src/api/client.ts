import type {
  MetricsInput, MetricsResult, DrugDose, SideEffect,
  DiseaseList, IPIResult, DICResult, HITResult,
  PHSCResult, CalciumResult, MMResult, EBMTResult,
  MDAPSSResult, CorticosteroidDoses,
} from '../types';

// ---- Backend detection ----
// 'http': Go API (local dev / shinyapps.io proxy) — fetch('api/...')
// 'shiny': Shiny WebSocket — Shiny.setInputValue / addCustomMessageHandler

let mode: 'http' | 'shiny' = 'http';

declare global {
  interface Window {
    Shiny?: {
      setInputValue: (name: string, value: unknown) => void;
      addCustomMessageHandler: (type: string, handler: (msg: unknown) => void) => void;
    };
  }
}

// ---- Shiny API ----
let callId = 0;
const pending = new Map<string, { resolve: (v: unknown) => void; reject: (e: Error) => void }>();

function setupShiny() {
  if (typeof window === 'undefined' || !window.Shiny) return;
  mode = 'shiny';

  // Shiny.addCustomMessageHandler can be called anytime — it stores the handler.
  // Shiny.setInputValue buffers messages internally until WebSocket connects.
  window.Shiny.addCustomMessageHandler('mellon_result', (msg: unknown) => {
    const m = msg as { id: string; data: unknown };
    const p = pending.get(m.id);
    if (!p) return;
    pending.delete(m.id);
    const d = m.data as Record<string, unknown> | undefined;
    d?.error ? p.reject(new Error(String(d.error))) : p.resolve(m.data);
  });
}

function callShiny(action: string, payload: unknown): Promise<unknown> {
  const id = String(++callId);
  return new Promise((resolve, reject) => {
    pending.set(id, { resolve, reject });
    window.Shiny!.setInputValue('mellon_api', { action, payload, id });
  });
}

// ---- HTTP API (Go) ----
async function callHttp(action: string, payload: unknown): Promise<unknown> {
  const res = await fetch(`api/${action}`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(payload),
  });
  if (!res.ok) throw new Error(await res.text());
  return res.json();
}

// ---- Init ----
setupShiny();

// ---- Drug name translations (Chinese → English) ----
const DRUG_EN: Record<string, string> = {
  '环磷酰胺(ctx)': 'Cyclophosphamide', '环磷酰氨(ctx)': 'Cyclophosphamide', '环磷酰氨': 'Cyclophosphamide',
  '多柔比星': 'Doxorubicin', '长春新碱(vcr)': 'Vincristine', '长春新碱': 'Vincristine',
  '泼尼松': 'Prednisone', '利妥昔单抗': 'Rituximab', '柔红霉素(dnr)': 'Daunorubicin',
  '阿糖胞苷(ara-c)': 'Cytarabine', '阿糖胞苷': 'Cytarabine', '依托泊苷': 'Etoposide',
  '顺铂': 'Cisplatin', '卡铂': 'Carboplatin', '氟达拉滨': 'Fludarabine',
  '美法仑': 'Melphalan', '白消安': 'Busulfan', '环磷酰胺': 'Cyclophosphamide',
  '异环磷酰胺': 'Ifosfamide', '吉西他滨': 'Gemcitabine', '奥沙利铂': 'Oxaliplatin',
  '硼替佐米': 'Bortezomib', '来那度胺': 'Lenalidomide', '地塞米松(dex)': 'Dexamethasone',
  '阿霉素': 'Adriamycin', '长春碱': 'Vinblastine', '博来霉素': 'Bleomycin',
  '达卡巴嗪': 'Dacarbazine', '美斯钠': 'Mesna', '维奈克拉': 'Venclexta',
  '阿扎胞苷(aza)': 'Azacytidine', '地西他滨': 'Decitabine', '克拉屈滨(cl)': 'Cladribine',
  '高三尖杉酯碱': 'Homoharringtonine', '去甲氧柔红霉素': 'Idarubicin', '米托蒽醌': 'Mitoxantrone',
  '全反式维甲酸': 'ATRA', '亚砷酸': 'Arsenic trioxide', '甲氨蝶呤mtx': 'Methotrexate',
  '6-巯基嘌呤（6-mp）': '6-MP', '左旋门冬酰胺酶(l-asp)': 'L-Asparaginase', '培门冬酶': 'Pegaspargase',
  '达雷妥尤单抗(daratumumab)': 'Daratumumab', '苯达莫司汀（bendamustine）': 'Bendamustine',
  '阿克拉霉素': 'Aclarubicin', '阿仑单抗': 'Alemtuzumab', '氮芥': 'Mechlorethamine',
  '丙卡巴肼': 'Procarbazine', '四氢叶酸钙': 'Calcium folinate', '强的松龙(pred)': 'Prednisolone',
  '塞替派': 'Thiotepa', '卡莫司汀': 'Carmustine', '克拉霉素': 'Clarithromycin',
  '抗胸腺球蛋白': 'ATG', '椎管内注射化疗药物': 'IT chemotherapy',
  '维泊妥珠单抗': 'Polatuzumab vedotin', '维布妥昔单抗': 'Brentuximab vedotin',
  '长春花碱': 'Vinblastine',
};
export function translateDrug(name: string, lang: string) {
  if (lang !== 'en') return name;
  return DRUG_EN[name] || name;
}

// ---- Corticosteroid translations ----
const CS_EN: Record<string, string> = {
  '可的松': 'Cortisone', '氢化可的松': 'Hydrocortisone', '强的松': 'Prednisone',
  '强的松龙': 'Prednisolone', '甲强龙': 'Methylprednisolone', '曲安西龙': 'Triamcinolone',
  '倍他米松': 'Betamethasone', '地塞米松': 'Dexamethasone', '氯地米松': 'Beclomethasone',
};
export function translateCS(name: string, lang: string) {
  if (lang !== 'en') return name;
  return CS_EN[name] || name;
}

// ---- Normalization ----
function n(r: Record<string, unknown>): DrugDose {
  return {
    name: (r['药名']||r.name||'') as string,
    type: (r['类型']||r.type||'') as string,
    ref_dose: (r['推荐值(mg/m2)']||r.ref_dose||0) as number,
    calc_dose: (r['计算量']||r.calc_dose||0) as number,
    min_dose: (r['最小值']||r.min_dose||0) as number,
    max_dose: (r['最大值']||r.max_dose||0) as number,
    unit: (r['单位']||r.unit||'') as string,
  };
}
function se(r: Record<string, unknown>): SideEffect {
  return {
    Drug: (r['药名']||r.Drug||r['Drugs']||'') as string,
    SideEffect: (r['副作用']||r.SideEffect||r['副作用/SideEffect']||'') as string,
    Measures: (r['可能的解救措施']||r.Measures||r['可能的解救措施/Measures']||'') as string,
  };
}

// ---- Public API ----
export const api = {
  calculateAllMetrics: (i: MetricsInput) => (mode==='shiny'?callShiny:callHttp)('calculate_all_metrics',i) as Promise<MetricsResult>,
  listRegimens:        ()           => (mode==='shiny'?callShiny:callHttp)('list_regimens',{}) as Promise<DiseaseList>,
  calculateRegimen:    async (r:string,b:number,w:number) => { const d=await (mode==='shiny'?callShiny:callHttp)('calculate_regimen',{regimen:r,bsa:b,weight:w}); return Array.isArray(d)?(d as Array<Record<string,unknown>>).map(n):[]; },
  lookupSideEffects:   async (ns:string[],l:string) => { const d=await (mode==='shiny'?callShiny:callHttp)('lookup_side_effects',{drug_names:ns,lang:l}); return Array.isArray(d)?(d as Array<Record<string,unknown>>).map(se):[]; },
  scoreAGVHD:      (s:number,l:number,g:number) => (mode==='shiny'?callShiny:callHttp)('score_agvhd',{skin:s,liver:l,gastric:g}) as Promise<{score:number}>,
  scoreIPI:        (a:number,e:number,n:number,x:number,d:number) => (mode==='shiny'?callShiny:callHttp)('score_ipi',{age_ipi:a,ecog:e,ann_arbor:n,extranodal:x,ldh:d}) as Promise<IPIResult>,
  scoreICANS:      (c:number,w:number,l:number,a:number,n:number) => (mode==='shiny'?callShiny:callHttp)('score_icans',{count:c,write:w,listen:l,attention:a,named:n}) as Promise<{grade:number}>,
  scoreMMFull:     (h:number,c:number,b:number,m:number,j:string,g:number,a:number) => (mode==='shiny'?callShiny:callHttp)('score_mm_full',{hgb:h,serum_ca:c,bone_image:b,m_protein:m,serum_jg:j,b2mg:g,albumin:a}) as Promise<MMResult>,
  scoreDIC:        (p:number,f:number,t:number,b:number) => (mode==='shiny'?callShiny:callHttp)('score_dic',{plt:p,fdps:f,pt:t,fbg:b}) as Promise<DICResult>,
  scoreHIT:        (c:number,t:number,a:number,r:number) => (mode==='shiny'?callShiny:callHttp)('score_hit',{plt_change:c,plt_time:t,plt_agg:a,plt_reason:r}) as Promise<HITResult>,
  scoreCIT:        (g:number) => (mode==='shiny'?callShiny:callHttp)('score_cit',{plt_grade:g}) as Promise<{grade:number}>,
  scoreMDAPSS:     (i:number[]) => (mode==='shiny'?callShiny:callHttp)('score_mdapss',{mdapss_inputs:i}) as Promise<MDAPSSResult>,
  scoreEBMT:       (i:number[]) => (mode==='shiny'?callShiny:callHttp)('score_ebmt',{ebmt_inputs:i}) as Promise<EBMTResult>,
  phscBefore:      (w:number,c:number) => (mode==='shiny'?callShiny:callHttp)('phsc_before',{pb_wbc:w,pb_cd34:c}) as Promise<PHSCResult>,
  phscAfter:       (w:number,c:number,v:number,wt:number) => (mode==='shiny'?callShiny:callHttp)('phsc_after',{col_wbc:w,col_cd34:c,col_vol:v,weight:wt}) as Promise<PHSCResult>,
  convertCorticosteroid: (n:string,d:number) => (mode==='shiny'?callShiny:callHttp)('convert_corticosteroid',{tpz_name:n,tpz_dose:d}) as Promise<CorticosteroidDoses>,
  adjustCalcium:   (s:number,n:number,p:number) => (mode==='shiny'?callShiny:callHttp)('adjust_calcium',{serum_calcium:s,normal_albumin:n,patient_albumin:p}) as Promise<CalciumResult>,
};
