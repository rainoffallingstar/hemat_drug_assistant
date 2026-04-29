import { useState } from 'react';
import { Radio, Table } from 'antd';
import { useLanguage } from '../../i18n';
import { api } from '../../api/client';
import ScoringToolShell from './ScoringToolShell';

const cartDataZh = [
  { 神志: '自发觉醒', 抽搐: 'NA', 运动能力: 'NA', 脑水肿: 'NA' },
  { 神志: '声音唤醒', 抽搐: 'NA', 运动能力: 'NA', 脑水肿: 'NA' },
  { 神志: '触觉刺激唤醒', 抽搐: '局部或全身发作，医疗干预解决', 运动能力: 'NA', 脑水肿: '局限性水肿' },
  { 神志: '不能唤醒或强刺激唤醒', 抽搐: '长时间癫痫发作(>5分钟)', 运动能力: '深部局灶性运动无力', 脑水肿: '弥漫性水肿' },
];
const cartDataEn = [
  { Consciousness: 'Spontaneous', Seizure: 'NA', Motor: 'NA', Edema: 'NA' },
  { Consciousness: 'Voice arouses', Seizure: 'NA', Motor: 'NA', Edema: 'NA' },
  { Consciousness: 'Tactile arouses', Seizure: 'Focal/generalized, intervention needed', Motor: 'NA', Edema: 'Focal edema on imaging' },
  { Consciousness: 'Stupor or coma', Seizure: 'Life-threatening (>5 min)', Motor: 'Deep focal motor weakness', Edema: 'Diffuse edema on imaging' },
];

export default function ICANSForm() {
  const { t, lang } = useLanguage();
  const [attention, setAttention] = useState(4);
  const [named, setNamed] = useState(3);
  const [listen, setListen] = useState(1);
  const [write, setWrite] = useState(1);
  const [count, setCount] = useState(1);
  const [grade, setGrade] = useState<number | null>(null);
  const [loading, setLoading] = useState(false);

  const submit = async () => {
    setLoading(true);
    try { const r = await api.scoreICANS(count, write, listen, attention, named); setGrade(r.grade); }
    catch { /* ignore */ }
    finally { setLoading(false); }
  };

  const cartRow = grade ? (lang === 'zh' ? cartDataZh : cartDataEn)[grade - 1] : null;

  return (
    <ScoringToolShell title={t('icans.title')} onSubmit={submit} loading={loading}
      result={grade !== null ? `${t('icans.grade')}: ${grade}` : undefined}>
      <Field label={t('icans.attention')} value={attention} onChange={setAttention} max={4} />
      <Field label={t('icans.named')} value={named} onChange={setNamed} max={3} />
      <Field label={t('icans.listen')} value={listen} onChange={setListen} max={1} />
      <Field label={t('icans.write')} value={write} onChange={setWrite} max={1} />
      <Field label={t('icans.count')} value={count} onChange={setCount} max={1} />
      {cartRow && (
        <Table dataSource={[cartRow]} columns={Object.keys(cartRow).map(k => ({ title: k, dataIndex: k, key: k }))}
          pagination={false} size="small" style={{ marginTop: 8 }} />
      )}
    </ScoringToolShell>
  );
}

function Field({ label, value, onChange, max }: { label: string; value: number; onChange: (v: number) => void; max: number }) {
  return (
    <div style={{ marginBottom: 10 }}>
      <div style={{ fontWeight: 600, marginBottom: 4 }}>{label}</div>
      <Radio.Group value={value} onChange={e => onChange(e.target.value)}>
        {Array.from({ length: max + 1 }, (_, i) => <Radio key={i} value={max - i}>{max - i}</Radio>)}
      </Radio.Group>
    </div>
  );
}
