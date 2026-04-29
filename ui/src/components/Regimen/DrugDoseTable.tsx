import { Table } from 'antd';
import { useLanguage } from '../../i18n';
import { translateDrug } from '../../api/client';
import type { DrugDose } from '../../types';

interface Props { doses: DrugDose[] | null; }

export default function DrugDoseTable({ doses }: Props) {
  const { t, lang } = useLanguage();

  const columns = [
    { title: t('regimen.drug'), dataIndex: 'name', key: 'drug', render: (v: string) => <strong>{translateDrug(v, lang)}</strong>, width: 160 },
    { title: t('regimen.refDose'), dataIndex: 'ref_dose', key: 'ref', render: (v: number) => v > 0 ? v.toFixed(1) : '-' },
    { title: t('regimen.calcDose'), dataIndex: 'calc_dose', key: 'calc', render: (v: number) => v.toFixed(2) },
    { title: t('regimen.min'), dataIndex: 'min_dose', key: 'min', render: (v: number) => v > 0 ? v.toFixed(2) : '-' },
    { title: t('regimen.max'), dataIndex: 'max_dose', key: 'max', render: (v: number) => v > 0 ? v.toFixed(2) : '-' },
    { title: t('regimen.unit'), dataIndex: 'unit', key: 'unit', width: 90 },
  ];

  return (
    <div className="mellon-section">
      <h3 className="mellon-section-title">{t('regimen.title')}</h3>
      {doses && doses.length > 0 ? (
        <div className="mellon-data-table">
          <Table dataSource={doses.map((d, i) => ({ ...d, key: i }))} columns={columns} pagination={false} size="middle" />
        </div>
      ) : (
        <p className="mellon-empty-state">{t('regimen.empty')}</p>
      )}
      <div className="mellon-warning" style={{ marginTop: 14 }}>{t('regimen.warning')}</div>
    </div>
  );
}
