import { useState } from 'react';
import { Radio, Table } from 'antd';
import { useLanguage } from '../../i18n';
import { api } from '../../api/client';
import ScoringToolShell from './ScoringToolShell';
import type { MDAPSSResult } from '../../types';

export default function MDAPSSForm() {
  const { t } = useLanguage();
  const [vals, setVals] = useState<number[]>([0, 0, 0, 2, 1, 1, 0, 0]);
  const [result, setResult] = useState<MDAPSSResult | null>(null);
  const [loading, setLoading] = useState(false);

  const set = (i: number, v: number) => { const n = [...vals]; n[i] = v; setVals(n); };

  const submit = async () => {
    setLoading(true);
    try { const r = await api.scoreMDAPSS(vals); setResult(r); }
    catch { /* ignore */ }
    finally { setLoading(false); }
  };

  const fields = [
    { label: t('mdapss.1'), opts: [{v:2,l:t('mdapss.yes')},{v:0,l:t('mdapss.no')}] },
    { label: t('mdapss.2'), opts: [{v:1,l:t('mdapss.age1')},{v:2,l:t('mdapss.age2')},{v:0,l:t('mdapss.age3')}] },
    { label: t('mdapss.3'), opts: [{v:1,l:t('mdapss.blast1')},{v:2,l:t('mdapss.blast2')}] },
    { label: t('mdapss.4'), opts: [{v:1,l:t('mdapss.plt1')},{v:2,l:t('mdapss.plt2')},{v:3,l:t('mdapss.plt3')}] },
    { label: t('mdapss.5'), opts: [{v:2,l:t('mdapss.hb1')},{v:0,l:t('mdapss.hb2')}] },
    { label: t('mdapss.6'), opts: [{v:2,l:t('mdapss.wbc1')},{v:0,l:t('mdapss.wbc2')}] },
    { label: t('mdapss.7'), opts: [{v:3,l:t('mdapss.yes')},{v:0,l:t('mdapss.no')}] },
    { label: t('mdapss.8'), opts: [{v:1,l:t('mdapss.yes')},{v:0,l:t('mdapss.no')}] },
  ];

  return (
    <ScoringToolShell title={t('mdapss.title')} onSubmit={submit} loading={loading}
      result={result ? <Table dataSource={[{...result,key:1}]}
        columns={['积分','分组','中位生存月','三年生存率'].map(k => ({title:k,dataIndex:k,key:k,render:(v:any)=>k==='三年生存率'?`${v}%`:v}))}
        pagination={false} size="small" /> : undefined}>
      {fields.map((f, i) => (
        <div key={i} style={{ marginBottom: 6 }}>
          <span style={{ fontSize: 12, fontWeight: 500 }}>{f.label}</span>
          <Radio.Group value={vals[i]} onChange={e => set(i, e.target.value)} size="small" style={{ marginLeft: 8 }}>
            {f.opts.map(o => <Radio.Button key={o.v} value={o.v} style={{ fontSize: 11 }}>{o.l}</Radio.Button>)}
          </Radio.Group>
        </div>
      ))}
    </ScoringToolShell>
  );
}
