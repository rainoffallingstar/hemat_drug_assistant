import { useState } from 'react';
import { Radio } from 'antd';
import { useLanguage } from '../../i18n';
import { api } from '../../api/client';
import ScoringToolShell from './ScoringToolShell';

export default function EBMTForm() {
  const { t } = useLanguage();
  const [vals, setVals] = useState<number[]>([0, 1, 0, 0, 0]);
  const [result, setResult] = useState<number | null>(null);
  const [loading, setLoading] = useState(false);

  const set = (i: number, v: number) => { const n = [...vals]; n[i] = v; setVals(n); };

  const submit = async () => {
    setLoading(true);
    try { const r = await api.scoreEBMT(vals); setResult(r.score); }
    catch { /* ignore */ }
    finally { setLoading(false); }
  };

  const fields = [
    { label: t('ebmt.age'), opts: [{v:0,l:t('ebmt.age0')},{v:1,l:t('ebmt.age1')},{v:2,l:t('ebmt.age2')}] },
    { label: t('ebmt.stage'), opts: [{v:0,l:t('ebmt.stage0')},{v:1,l:t('ebmt.stage1')},{v:2,l:t('ebmt.stage2')}] },
    { label: t('ebmt.time'), opts: [{v:0,l:t('ebmt.time0')},{v:1,l:t('ebmt.time1')}] },
    { label: t('ebmt.donor'), opts: [{v:0,l:t('ebmt.donor0')},{v:1,l:t('ebmt.donor1')}] },
    { label: t('ebmt.sex'), opts: [{v:0,l:t('ebmt.sex0')},{v:1,l:t('ebmt.sex1')}] },
  ];

  return (
    <ScoringToolShell title={t('ebmt.title')} onSubmit={submit} loading={loading}
      result={result !== null ? `EBMT risk score: ${result}` : undefined}>
      {fields.map((f, i) => (
        <div key={i} style={{ marginBottom: 8 }}>
          <div style={{ fontWeight: 600, marginBottom: 4 }}>{f.label}</div>
          <Radio.Group value={vals[i]} onChange={e => set(i, e.target.value)}>
            {f.opts.map(o => <Radio key={o.v} value={o.v}>{o.l}</Radio>)}
          </Radio.Group>
        </div>
      ))}
    </ScoringToolShell>
  );
}
