import { useState } from 'react';
import { Radio, Input } from 'antd';
import { useLanguage } from '../../i18n';
import { api } from '../../api/client';
import ScoringToolShell from './ScoringToolShell';

export default function CalciumForm() {
  const { t } = useLanguage();
  const [serumCa, setSerumCa] = useState('10');
  const [normalAlb, setNormalAlb] = useState('4');
  const [patientAlb, setPatientAlb] = useState('3.9');
  const [result, setResult] = useState<string | null>(null);
  const [loading, setLoading] = useState(false);

  const submit = async () => {
    setLoading(true);
    try {
      const r = await api.adjustCalcium(parseFloat(serumCa), parseFloat(normalAlb), parseFloat(patientAlb));
      setResult(t('calcium.result').replace('{value}', r.adjusted.toFixed(2)));
    } catch (e: any) { setResult(e.message || t('scoring.error')); }
    finally { setLoading(false); }
  };

  return (
    <ScoringToolShell title={t('calcium.title')} onSubmit={submit} loading={loading} result={result ?? undefined}>
      <div style={{ marginBottom: 8 }}>
        <div style={{ fontWeight: 600, marginBottom: 4 }}>{t('calcium.serumCa')}</div>
        <Input style={{ width: 160 }} value={serumCa} onChange={e => setSerumCa(e.target.value)} />
      </div>
      <div style={{ marginBottom: 8 }}>
        <div style={{ fontWeight: 600, marginBottom: 4 }}>{t('calcium.normalAlb')}</div>
        <Radio.Group value={normalAlb} onChange={e => setNormalAlb(e.target.value)}>
          <Radio value="4">4 g/dL</Radio>
          <Radio value="4.4">4.4 g/dL</Radio>
        </Radio.Group>
      </div>
      <div>
        <div style={{ fontWeight: 600, marginBottom: 4 }}>{t('calcium.patientAlb')}</div>
        <Input style={{ width: 160 }} value={patientAlb} onChange={e => setPatientAlb(e.target.value)} />
      </div>
    </ScoringToolShell>
  );
}
