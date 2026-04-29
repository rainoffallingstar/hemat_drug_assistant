import { Card, Typography, Divider } from 'antd';
import { useLanguage } from '../../i18n';

const { Paragraph, Text, Title } = Typography;

export default function AboutTab() {
  const { t } = useLanguage();

  return (
    <div>
      <Card title={t('about.title')} size="small" style={{ marginBottom: 16 }}>
        <Paragraph>{t('about.content')}</Paragraph>
      </Card>
      <Card title="Formula" size="small" style={{ marginBottom: 16 }}>
        <Paragraph>{t('about.formula')}</Paragraph>
      </Card>
      <Card title="References" size="small" style={{ marginBottom: 16 }}>
        <Paragraph>{t('about.references')}</Paragraph>
      </Card>
    </div>
  );
}
