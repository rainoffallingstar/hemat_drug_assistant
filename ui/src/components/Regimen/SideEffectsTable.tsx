import { Table, Empty } from 'antd';
import { useLanguage } from '../../i18n';
import { translateDrug } from '../../api/client';
import type { SideEffect } from '../../types';

interface Props { effects: SideEffect[] | null; }

export default function SideEffectsTable({ effects }: Props) {
  const { t, lang } = useLanguage();

  return (
    <div className="mellon-section">
      <h3 className="mellon-section-title">{t('sideEffects.title')}</h3>
      {effects && effects.length > 0 ? (
        <div className="mellon-data-table">
          <Table
            dataSource={effects.map((e, i) => ({ ...e, key: i }))}
            columns={[
              { title: t('sideEffects.drug'), dataIndex: 'Drug', key: 'drug', width: 160, render: (v: string) => translateDrug(v, lang) },
              { title: t('sideEffects.effect'), dataIndex: 'SideEffect', key: 'effect', width: 260 },
              { title: t('sideEffects.measures'), dataIndex: 'Measures', key: 'measures', width: 360 },
            ]}
            pagination={false} size="middle"
          />
        </div>
      ) : (
        <Empty description={t('sideEffects.empty')} image={Empty.PRESENTED_IMAGE_SIMPLE} />
      )}
    </div>
  );
}
