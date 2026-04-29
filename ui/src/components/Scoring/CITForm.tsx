import { useState } from 'react';
import { Radio } from 'antd';
import { useLanguage } from '../../i18n';
import { api } from '../../api/client';
import ScoringToolShell from './ScoringToolShell';

export default function CITForm() {
  const { t } = useLanguage();
  const [grade, setGrade] = useState(1);
  const [result, setResult] = useState<number | null>(null);
  const [loading, setLoading] = useState(false);

  const submit = async () => {
    setLoading(true);
    try { const r = await api.scoreCIT(grade); setResult(r.grade); }
    catch { /* ignore */ }
    finally { setLoading(false); }
  };

  return (
    <ScoringToolShell title={t('cit.title')} onSubmit={submit} loading={loading}
      result={result !== null ? `CIT grade: ${result}` : undefined}>
      <div style={{ fontWeight: 600, marginBottom: 4 }}>{t('cit.label')}</div>
      <Radio.Group value={grade} onChange={e => setGrade(e.target.value)}>
        {[1,2,3,4].map(v => <Radio key={v} value={v}>{t(`cit.${v}`)}</Radio>)}
      </Radio.Group>
    </ScoringToolShell>
  );
}
