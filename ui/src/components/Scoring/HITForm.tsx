import { useState } from 'react';
import { Radio } from 'antd';
import { useLanguage } from '../../i18n';
import { api } from '../../api/client';
import ScoringToolShell from './ScoringToolShell';

export default function HITForm() {
  const { t } = useLanguage();
  const [c, setC] = useState(0);
  const [tm, setTm] = useState(0);
  const [a, setA] = useState(0);
  const [r, setR] = useState(0);
  const [result, setResult] = useState<string | null>(null);
  const [loading, setLoading] = useState(false);

  const submit = async () => {
    setLoading(true);
    try { const res = await api.scoreHIT(c, tm, a, r); setResult(res.interpretation); }
    catch { /* ignore */ }
    finally { setLoading(false); }
  };

  const opts = [{v:0,l:t('hit.score0')},{v:1,l:t('hit.score1')},{v:2,l:t('hit.score2')}];

  return (
    <ScoringToolShell title={t('hit.title')} onSubmit={submit} loading={loading} result={result ?? undefined}>
      <F label={t('hit.pltChange')} v={c} s={setC} o={opts} />
      <F label={t('hit.pltTime')} v={tm} s={setTm} o={opts} />
      <F label={t('hit.pltAgg')} v={a} s={setA} o={opts} />
      <F label={t('hit.pltReason')} v={r} s={setR} o={[{v:0,l:t('hit.reason0')},{v:1,l:t('hit.reason1')},{v:2,l:t('hit.reason2')}]} />
    </ScoringToolShell>
  );
}

function F({ label, v, s, o }: { label: string; v: number; s: (v: number) => void; o: { v: number; l: string }[] }) {
  return (
    <div style={{ marginBottom: 10 }}>
      <div style={{ fontWeight: 600, marginBottom: 4 }}>{label}</div>
      <Radio.Group value={v} onChange={e => s(e.target.value)}>
        {o.map(x => <Radio key={x.v} value={x.v}>{x.l}</Radio>)}
      </Radio.Group>
    </div>
  );
}
