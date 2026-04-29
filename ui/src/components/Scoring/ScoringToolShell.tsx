import { Button } from 'antd';
import { useLanguage } from '../../i18n';
import { useTheme } from '../../App';

interface Props {
  title: string;
  children: React.ReactNode;
  onSubmit: () => void;
  result?: React.ReactNode;
  loading?: boolean;
}

export default function ScoringToolShell({ title, children, onSubmit, result, loading }: Props) {
  const { t } = useLanguage();
  const { current: theme } = useTheme();

  return (
    <div className="mellon-scoring-card">
      <h4>{title}</h4>
      {children}
      {result && (
        <div className="mellon-result-panel" style={{
          background: theme.tokens.resultBg, borderColor: theme.tokens.resultBorder, color: theme.tokens.resultText,
        }}>{result}</div>
      )}
      <div style={{ marginTop: 16, textAlign: 'right' }}>
        <Button type="primary" onClick={onSubmit} loading={loading} size="middle">
          {t('scoring.submit')}
        </Button>
      </div>
    </div>
  );
}
