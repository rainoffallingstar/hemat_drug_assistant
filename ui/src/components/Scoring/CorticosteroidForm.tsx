import { useState } from 'react';
import { Radio, Input, Table } from 'antd';
import { useLanguage } from '../../i18n';
import { api, translateCS } from '../../api/client';
import ScoringToolShell from './ScoringToolShell';

const drugs = ['可的松', '氢化可的松', '强的松', '强的松龙', '甲强龙', '曲安西龙', '倍他米松', '地塞米松', '氯地米松'];

export default function CorticosteroidForm() {
  const { t, lang } = useLanguage();
  const [name, setName] = useState('地塞米松');
  const [dose, setDose] = useState('2');
  const [result, setResult] = useState<Record<string, number> | null>(null);
  const [loading, setLoading] = useState(false);

  const submit = async () => {
    setLoading(true);
    try { const r = await api.convertCorticosteroid(name, parseFloat(dose)); setResult(r.doses); }
    catch { /* ignore */ }
    finally { setLoading(false); }
  };

  return (
    <ScoringToolShell title={t('tpz.title')} onSubmit={submit} loading={loading}
      result={result ? <Table
        dataSource={Object.entries(result).map(([drug, d]) => ({ drug: translateCS(drug, lang), dose: d.toFixed(2), unit: t('tpz.unit') }))}
        columns={[{ title: t('tpz.name'), dataIndex: 'drug' }, { title: t('tpz.dose'), dataIndex: 'dose' }, { title: t('tpz.unit'), dataIndex: 'unit' }]}
        pagination={false} size="small" /> : undefined}>
      <div style={{ marginBottom: 8 }}>
        <div style={{ fontWeight: 600, marginBottom: 4 }}>{t('tpz.name')}</div>
        <Radio.Group value={name} onChange={e => setName(e.target.value)} style={{ display: 'flex', flexWrap: 'wrap' }}>
          {drugs.map(d => <Radio.Button key={d} value={d}>{translateCS(d, lang)}</Radio.Button>)}
        </Radio.Group>
      </div>
      <div>
        <div style={{ fontWeight: 600, marginBottom: 4 }}>{t('tpz.dose')}</div>
        <Input style={{ width: 120 }} value={dose} onChange={e => setDose(e.target.value)} />
      </div>
    </ScoringToolShell>
  );
}
