import { useState } from 'react';
import { Input, Card, Button } from 'antd';
import { useLanguage } from '../../i18n';
import { useTheme } from '../../App';
import { api } from '../../api/client';

export default function PHSCForm() {
  const { t } = useLanguage();
  const { current: theme } = useTheme();
  const [pbWbc, setPbWbc] = useState('10');
  const [pbCd34, setPbCd34] = useState('5');
  const [colWbc, setColWbc] = useState('10');
  const [colCd34, setColCd34] = useState('5');
  const [colVol, setColVol] = useState('200');
  const [before, setBefore] = useState<string | null>(null);
  const [after, setAfter] = useState<string | null>(null);

  const submitBefore = async () => {
    try {
      const r = await api.phscBefore(parseFloat(pbWbc), parseFloat(pbCd34));
      setBefore(t('phsc.beforeResult').replace('{count}', r.count.toFixed(1)));
    } catch { /* ignore */ }
  };

  const submitAfter = async () => {
    try {
      const r = await api.phscAfter(parseFloat(colWbc), parseFloat(colCd34), parseFloat(colVol), 50);
      setAfter(t('phsc.afterResult').replace('{count}', r.count.toFixed(1)));
    } catch { /* ignore */ }
  };

  const rs: React.CSSProperties = {
    background: theme.tokens.resultBg, borderColor: theme.tokens.resultBorder, color: theme.tokens.resultText,
  };

  return (
    <div className="mellon-phsc-grid">
      <Card title={t('phsc.beforeLabel')} size="small" className="mellon-scoring-card">
        <div className="mellon-phsc-fields">
          <F label={t('phsc.pbWbc')}><Input value={pbWbc} onChange={e => setPbWbc(e.target.value)} /></F>
          <F label={t('phsc.pbCd34')}><Input value={pbCd34} onChange={e => setPbCd34(e.target.value)} /></F>
        </div>
        {before && <div className="mellon-result-panel" style={rs}>{before}</div>}
        <div style={{ textAlign: 'right', marginTop: 14 }}>
          <Button type="primary" onClick={submitBefore}>{t('scoring.submit')}</Button>
        </div>
      </Card>

      <Card title={t('phsc.afterLabel')} size="small" className="mellon-scoring-card">
        <div className="mellon-phsc-fields">
          <F label={t('phsc.colWbc')}><Input value={colWbc} onChange={e => setColWbc(e.target.value)} /></F>
          <F label={t('phsc.colCd34')}><Input value={colCd34} onChange={e => setColCd34(e.target.value)} /></F>
          <F label={t('phsc.colVol')}><Input value={colVol} onChange={e => setColVol(e.target.value)} /></F>
        </div>
        {after && <div className="mellon-result-panel" style={rs}>{after}</div>}
        <div style={{ textAlign: 'right', marginTop: 14 }}>
          <Button type="primary" onClick={submitAfter}>{t('scoring.submit')}</Button>
        </div>
      </Card>
    </div>
  );
}

function F({ label, children }: { label: string; children: React.ReactNode }) {
  return (
    <div>
      <div className="mellon-field-label">{label}</div>
      {children}
    </div>
  );
}
