import { useState } from 'react';
import { Radio } from 'antd';
import { useLanguage } from '../../i18n';
import { api } from '../../api/client';
import ScoringToolShell from './ScoringToolShell';

export default function AGVHDForm() {
  const { t } = useLanguage();
  const [skin, setSkin] = useState(1);
  const [liver, setLiver] = useState(1);
  const [gastric, setGastric] = useState(1);
  const [result, setResult] = useState<number | null>(null);
  const [loading, setLoading] = useState(false);

  const submit = async () => {
    setLoading(true);
    try { const r = await api.scoreAGVHD(skin, liver, gastric); setResult(r.score); }
    catch { /* ignore */ }
    finally { setLoading(false); }
  };

  const desc = (field: string, grade: number) => t(`${field}${grade}`);

  return (
    <ScoringToolShell title={t('agvhd.title')} onSubmit={submit} loading={loading}
      result={result !== null ? `${t('agvhd.result')}${result}` : undefined}>
      {(['skin', 'liver', 'gastric'] as const).map((field, idx) => (
        <div key={field} style={{ marginBottom: 12 }}>
          <div style={{ fontWeight: 600, marginBottom: 4 }}>{t(`agvhd.${field}`)}</div>
          <Radio.Group value={[skin, liver, gastric][idx]} onChange={e => [setSkin, setLiver, setGastric][idx](e.target.value)}>
            {[0,1,2,3,4].map(g => <Radio key={g} value={g}>{g}</Radio>)}
          </Radio.Group>
          <div style={{ fontSize: 12, color: 'var(--text-muted)', marginTop: 2 }}>
            {desc(`agvhd.${field}`, [skin, liver, gastric][idx])}
          </div>
        </div>
      ))}
    </ScoringToolShell>
  );
}
