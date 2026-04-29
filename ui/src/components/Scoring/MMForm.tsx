import { useState } from 'react';
import { Radio } from 'antd';
import { useLanguage } from '../../i18n';
import { api } from '../../api/client';
import ScoringToolShell from './ScoringToolShell';

export default function MMForm() {
  const { t } = useLanguage();
  const [hgb, setHgb] = useState(1);
  const [serumCa, setSerumCa] = useState(1);
  const [bone, setBone] = useState(1);
  const [mProtein, setMProtein] = useState(1);
  const [serumJG, setSerumJG] = useState(1);
  const [b2mg, setB2mg] = useState(1);
  const [alb, setAlb] = useState(1);
  const [result, setResult] = useState<string | null>(null);
  const [loading, setLoading] = useState(false);

  const submit = async () => {
    setLoading(true);
    try {
      const r = await api.scoreMMFull(hgb, serumCa, bone, mProtein, String(serumJG), b2mg, alb);
      setResult(r.full);
    } catch { /* ignore */ }
    finally { setLoading(false); }
  };

  return (
    <ScoringToolShell title={t('mm.title')} onSubmit={submit} loading={loading} result={result ?? undefined}>
      <F label={t('mm.hgb')} v={hgb} s={setHgb} o={[{v:1,l:t('mm.hgb1')},{v:2,l:t('mm.hgb2')},{v:3,l:t('mm.hgb3')}]} />
      <F label={t('mm.serumCa')} v={serumCa} s={setSerumCa} o={[{v:1,l:t('mm.serumCa1')},{v:2,l:t('mm.serumCa2')}]} />
      <F label={t('mm.bone')} v={bone} s={setBone} o={[{v:1,l:t('mm.bone1')},{v:2,l:t('mm.bone2')},{v:3,l:t('mm.bone3')}]} />
      <F label={t('mm.mProtein')} v={mProtein} s={setMProtein} o={[{v:1,l:t('mm.mProtein1')},{v:2,l:t('mm.mProtein2')},{v:3,l:t('mm.mProtein3')}]} />
      <F label={t('mm.serumJG')} v={serumJG} s={setSerumJG} o={[{v:1,l:t('mm.serumJG1')},{v:2,l:t('mm.serumJG2')}]} />
      <F label={t('mm.b2mg')} v={b2mg} s={setB2mg} o={[{v:1,l:t('mm.b2mg1')},{v:2,l:t('mm.b2mg2')},{v:3,l:t('mm.b2mg3')}]} />
      <F label={t('mm.albumin')} v={alb} s={setAlb} o={[{v:1,l:t('mm.albumin1')},{v:2,l:t('mm.albumin2')}]} />
    </ScoringToolShell>
  );
}

function F({ label, v, s, o }: { label: string; v: number; s: (v: number) => void; o: { v: number; l: string }[] }) {
  return (
    <div style={{ marginBottom: 8 }}>
      <div style={{ fontWeight: 600, fontSize: 13, marginBottom: 4 }}>{label}</div>
      <Radio.Group size="small" value={v} onChange={e => s(e.target.value)}>
        {o.map(x => <Radio key={x.v} value={x.v}>{x.l}</Radio>)}
      </Radio.Group>
    </div>
  );
}
