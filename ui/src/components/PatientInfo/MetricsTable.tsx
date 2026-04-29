import { Card } from 'antd';
import { useLanguage } from '../../i18n';
import type { PatientInfoState } from '../../hooks/usePatientInfo';
import type { MetricsResult } from '../../types';

interface Props {
  metrics: MetricsResult | null;
  info: PatientInfoState;
}

export default function MetricsTable({ metrics, info }: Props) {
  const { t } = useLanguage();

  const items = [
    { label: t('metrics.weight'), value: info.weight ? `${info.weight} kg` : '--' },
    { label: t('metrics.height'), value: info.height ? `${info.height} cm` : '--' },
    { label: t('metrics.bmi'), value: metrics ? metrics.bmi.toFixed(2) : '--' },
    { label: t('metrics.bsa'), value: metrics ? `${metrics.bsa.toFixed(2)} m²` : '--' },
    { label: t('metrics.ccr'), value: metrics && metrics.ccr > 0 ? `${metrics.ccr.toFixed(2)} ml/min` : '--' },
  ];

  return (
    <Card title={t('metrics.title')} size="small" className="mellon-soft-card">
      <div className="mellon-metric-grid">
        {items.map((item) => (
          <div key={item.label} className="mellon-metric-tile">
            <span className="mellon-metric-label">{item.label}</span>
            <strong className="mellon-metric-value">{item.value}</strong>
          </div>
        ))}
      </div>
    </Card>
  );
}
