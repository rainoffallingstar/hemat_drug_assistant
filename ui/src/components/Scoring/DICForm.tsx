import { useState } from 'react';
import { Radio } from 'antd';
import { useLanguage } from '../../i18n';
import { api } from '../../api/client';
import ScoringToolShell from './ScoringToolShell';

export default function DICForm() {
  const { t } = useLanguage();
  const [plt, setPlt] = useState(0);
  const [fdps, setFdps] = useState(0);
  const [pt, setPt] = useState(0);
  const [fbg, setFbg] = useState(0);
  const [result, setResult] = useState<string | null>(null);
  const [loading, setLoading] = useState(false);

  const submit = async () => {
    setLoading(true);
    try { const r = await api.scoreDIC(plt, fdps, pt, fbg); setResult(r.interpretation); }
    catch { /* ignore */ }
    finally { setLoading(false); }
  };

  return (
    <ScoringToolShell title={t('dic.title')} onSubmit={submit} loading={loading} result={result ?? undefined}>
      <F label={t('dic.plt')} v={plt} s={setPlt} o={[{v:0,l:t('dic.plt0')},{v:1,l:t('dic.plt1')},{v:2,l:t('dic.plt2')}]} />
      <F label={t('dic.fdps')} v={fdps} s={setFdps} o={[{v:0,l:t('dic.fdps0')},{v:1,l:t('dic.fdps1')},{v:2,l:t('dic.fdps2')}]} />
      <F label={t('dic.pt')} v={pt} s={setPt} o={[{v:0,l:t('dic.pt0')},{v:1,l:t('dic.pt1')},{v:2,l:t('dic.pt2')}]} />
      <F label={t('dic.fbg')} v={fbg} s={setFbg} o={[{v:0,l:t('dic.fbg0')},{v:1,l:t('dic.fbg1')}]} />
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
