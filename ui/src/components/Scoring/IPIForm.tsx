import { useState } from 'react';
import { Radio } from 'antd';
import { useLanguage } from '../../i18n';
import { api } from '../../api/client';
import ScoringToolShell from './ScoringToolShell';

export default function IPIForm() {
  const { t } = useLanguage();
  const [age, setAge] = useState(0);
  const [ecog, setEcog] = useState(0);
  const [ann, setAnn] = useState(1);
  const [ext, setExt] = useState(0);
  const [ldh, setLdh] = useState(0);
  const [result, setResult] = useState<string | null>(null);
  const [loading, setLoading] = useState(false);

  const submit = async () => {
    setLoading(true);
    try {
      const r = await api.scoreIPI(age, ecog, ann, ext, ldh);
      setResult(t('ipi.result').replace('{score}', String(r.score)).replace('{risk}', r.risk));
    } catch { /* ignore */ }
    finally { setLoading(false); }
  };

  return (
    <ScoringToolShell title={t('ipi.title')} onSubmit={submit} loading={loading} result={result ?? undefined}>
      <Field label={t('ipi.age')} value={age} onChange={setAge}
        opts={[{v:0,l:t('ipi.age0')},{v:1,l:t('ipi.age1')}]} />
      <Field label={t('ipi.ecog')} value={ecog} onChange={setEcog}
        opts={[0,1,2,3,4,5].map(v => ({v, l:String(v)}))} />
      <Field label={t('ipi.ann')} value={ann} onChange={setAnn}
        opts={['I','II','III','IV'].map((l,i) => ({v:i+1, l}))} />
      <Field label={t('ipi.extranodal')} value={ext} onChange={setExt}
        opts={[{v:0,l:t('ipi.extranodal0')},{v:1,l:t('ipi.extranodal1')}]} />
      <Field label={t('ipi.ldh')} value={ldh} onChange={setLdh}
        opts={[{v:0,l:t('ipi.ldh0')},{v:1,l:t('ipi.ldh1')}]} />
    </ScoringToolShell>
  );
}

function Field({ label, value, onChange, opts }: { label: string; value: number; onChange: (v: number) => void; opts: { v: number; l: string }[] }) {
  return (
    <div style={{ marginBottom: 10 }}>
      <div style={{ fontWeight: 600, marginBottom: 4 }}>{label}</div>
      <Radio.Group value={value} onChange={e => onChange(e.target.value)}>
        {opts.map(o => <Radio key={o.v} value={o.v}>{o.l}</Radio>)}
      </Radio.Group>
    </div>
  );
}
