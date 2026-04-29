import { lazy, Suspense, useState, useEffect, useCallback } from 'react';
import { Layout, Tabs, Input, Select, Radio, Switch, Collapse } from 'antd';
import { SettingOutlined } from '@ant-design/icons';
import { useLanguage } from '../../i18n';
import { useTheme } from '../../App';
import { themes } from '../../themes';
import { usePatientInfo } from '../../hooks/usePatientInfo';
import { api } from '../../api/client';
import type { DiseaseList, DrugDose, SideEffect } from '../../types';

const { Content, Footer } = Layout;

const DrugDoseTable = lazy(() => import('../Regimen/DrugDoseTable'));
const SideEffectsTable = lazy(() => import('../Regimen/SideEffectsTable'));
const AGVHDForm = lazy(() => import('../Scoring/AGVHDForm'));
const IPIForm = lazy(() => import('../Scoring/IPIForm'));
const ICANSForm = lazy(() => import('../Scoring/ICANSForm'));
const MMForm = lazy(() => import('../Scoring/MMForm'));
const PHSCForm = lazy(() => import('../Scoring/PHSCForm'));
const DICForm = lazy(() => import('../Scoring/DICForm'));
const HITForm = lazy(() => import('../Scoring/HITForm'));
const CITForm = lazy(() => import('../Scoring/CITForm'));
const CorticosteroidForm = lazy(() => import('../Scoring/CorticosteroidForm'));
const MDAPSSForm = lazy(() => import('../Scoring/MDAPSSForm'));
const EBMTForm = lazy(() => import('../Scoring/EBMTForm'));
const CalciumForm = lazy(() => import('../Scoring/CalciumForm'));

const SCORING = [
  { key: 'hit',     Comp: HITForm },
  { key: 'dic',     Comp: DICForm },
  { key: 'cit',     Comp: CITForm },
  { key: 'tpz',     Comp: CorticosteroidForm },
  { key: 'mdapss',  Comp: MDAPSSForm },
  { key: 'ebmt',    Comp: EBMTForm },
  { key: 'calcium', Comp: CalciumForm },
];

function LazyPanel({ children }: { children: React.ReactNode }) {
  return <Suspense fallback={<div style={{ padding: 24 }} />}>{children}</Suspense>;
}

export default function AppLayout() {
  const { t, lang, setLanguage } = useLanguage();
  const { current: theme, setTheme } = useTheme();
  const { info, metrics, updateInfo } = usePatientInfo();

  const [regimen, setRegimen] = useState('Pola-R-CHP');
  const [diseases, setDiseases] = useState<DiseaseList>({});
  const [doses, setDoses] = useState<DrugDose[] | null>(null);
  const [sideEffects, setSideEffects] = useState<SideEffect[] | null>(null);
  const [showAdv, setShowAdv] = useState(false);

  useEffect(() => {
    const load = async () => {
      for (let i = 0; i < 3; i++) {
        try { const d = await api.listRegimens(); setDiseases(d); return; }
        catch { if (i < 2) await new Promise(r => setTimeout(r, 500 * (i + 1))); }
      }
    };
    const t = setTimeout(load, 300);
    return () => clearTimeout(t);
  }, []);

  const recalcDoses = useCallback(async (reg: string, bsa: number, w: number) => {
    try {
      const d = await api.calculateRegimen(reg, bsa, w);
      setDoses(d);
      const names = d.map((x: DrugDose) => x.name);
      const se = await api.lookupSideEffects(names, 'zh'); // drug names are Chinese in DB
      setSideEffects(se && se.length > 0 ? se : []);
    } catch { /* ignore */ }
  }, []);

  useEffect(() => { if (metrics) recalcDoses(regimen, metrics.bsa, parseFloat(info.weight) || 50); }, [metrics]);

  const handleRegimenChange = useCallback((reg: string) => {
    setRegimen(reg);
    if (metrics) recalcDoses(reg, metrics.bsa, parseFloat(info.weight) || 50);
  }, [metrics, info.weight, recalcDoses]);

  const genderOpts = [
    { label: t('gender.common'), value: '通用' },
    { label: t('gender.female'), value: '女性' },
    { label: t('gender.male'), value: '男性' },
  ];
  const regimenOpts = Object.entries(diseases).map(([category, regs]) => ({
    label: category,
    options: (regs || []).map(r => ({ label: r, value: r })),
  }));

  const tabItems = [
    { key: 'regimen',   label: t('tabs.recommendation'), children: <LazyPanel><DrugDoseTable doses={doses} /></LazyPanel> },
    { key: 'sidefx',    label: t('tabs.sideEffects'),    children: <LazyPanel><SideEffectsTable effects={sideEffects} /></LazyPanel> },
    { key: 'agvhd',     label: t('tabs.agvhd'),          children: <LazyPanel><AGVHDForm /></LazyPanel> },
    { key: 'ipi',       label: t('tabs.ipi'),            children: <LazyPanel><IPIForm /></LazyPanel> },
    { key: 'icans',     label: t('tabs.icans'),          children: <LazyPanel><ICANSForm /></LazyPanel> },
    { key: 'mm',        label: t('tabs.mm'),             children: <LazyPanel><MMForm /></LazyPanel> },
    { key: 'phsc',      label: t('tabs.phsc'),           children: <LazyPanel><PHSCForm /></LazyPanel> },
    { key: 'more',      label: t('tabs.more'),           children: <div className="mellon-tool-grid">{SCORING.map(({ key, Comp }) => <div key={key}><LazyPanel><Comp /></LazyPanel></div>)}</div> },
  ];

  const shellStyle = {
    '--app-bg': theme.tokens.colorBgLayout,
    '--surface': theme.tokens.colorBgContainer,
    '--border': theme.tokens.colorBorder,
    '--border-secondary': theme.tokens.colorBorderSecondary,
    '--text': theme.tokens.colorText,
    '--text-muted': theme.tokens.colorTextSecondary,
    '--primary': theme.tokens.colorPrimary,
    '--primary-soft': `${theme.tokens.colorPrimary}15`,
    '--font-sans': "'Plus Jakarta Sans', 'Segoe UI', sans-serif",
    '--font-serif': "'Cormorant Garamond', Georgia, serif",
  } as React.CSSProperties;

  return (
    <div className="mellon-shell" style={shellStyle}>
      <Layout>
        <Layout className="mellon-main">
          <Content className="mellon-content">
            {/* ---- Page header ---- */}
            <header className="mellon-page-header">
              <h1 className="mellon-page-title">{t('app.title')}</h1>
              <p className="mellon-page-desc">{t('hero.description')}</p>

              {/* Controls bar */}
              <div className="mellon-page-controls" style={{ marginBottom: 18 }}>
                <Switch size="small" checkedChildren="En" unCheckedChildren="中" checked={lang === 'en'}
                  onChange={c => setLanguage(c ? 'en' : 'zh')} />
                <Select size="small" value={theme.name} onChange={setTheme} style={{ width: 100 }}
                  options={themes.map(t => ({ label: t.label, value: t.name }))} />
                <span style={{ fontSize: 12, color: theme.tokens.colorTextSecondary, cursor: 'pointer' }}
                  onClick={() => setShowAdv(!showAdv)}>
                  <SettingOutlined /> {t('sidebar.advance')}
                </span>
              </div>

              {/* Patient input row */}
              <div className="mellon-input-row">
                <div className="mellon-field">
                  <label className="mellon-field-label">{t('sidebar.weight')}</label>
                  <Input value={info.weight} onChange={e => updateInfo('weight', e.target.value)} />
                </div>
                <div className="mellon-field">
                  <label className="mellon-field-label">{t('sidebar.height')}</label>
                  <Input value={info.height} onChange={e => updateInfo('height', e.target.value)} />
                </div>
                <div className="mellon-field-wide">
                  <label className="mellon-field-label">{t('sidebar.regimen')}</label>
                  <Select showSearch value={regimen} onChange={handleRegimenChange}
                    options={regimenOpts} optionFilterProp="label" style={{ width: '100%' }} />
                </div>
                <div className="mellon-field">
                  <label className="mellon-field-label">{t('sidebar.gender')}</label>
                  <Radio.Group size="small" value={info.gender} onChange={e => updateInfo('gender', e.target.value)}
                    options={genderOpts} optionType="button" buttonStyle="solid" />
                </div>
              </div>

              {/* Advanced */}
              {showAdv && (
                <div style={{ display: 'flex', gap: 14, marginTop: 14 }}>
                  <div style={{ flex: '1 1 120px' }}>
                    <label className="mellon-field-label">{t('sidebar.age')}</label>
                    <Input size="small" value={info.age} onChange={e => updateInfo('age', e.target.value)} />
                  </div>
                  <div style={{ flex: '1 1 120px' }}>
                    <label className="mellon-field-label">{t('sidebar.creatinine')}</label>
                    <Input size="small" value={info.scr} onChange={e => updateInfo('scr', e.target.value)} />
                  </div>
                </div>
              )}

              {/* Metrics bar */}
              <div className="mellon-metrics-bar">
                <div className="mellon-stat">
                  <span className="mellon-stat-label">{t('metrics.weight')}</span>
                  <span className="mellon-stat-value">{info.weight} kg</span>
                </div>
                <div className="mellon-stat">
                  <span className="mellon-stat-label">{t('metrics.height')}</span>
                  <span className="mellon-stat-value">{info.height} cm</span>
                </div>
                <div className="mellon-stat">
                  <span className="mellon-stat-label">{t('metrics.bmi')}</span>
                  <span className="mellon-stat-value">{metrics ? metrics.bmi.toFixed(2) : '--'}</span>
                </div>
                <div className="mellon-stat">
                  <span className="mellon-stat-label">{t('metrics.bsa')}</span>
                  <span className="mellon-stat-value">{metrics ? `${metrics.bsa.toFixed(2)} m²` : '--'}</span>
                </div>
                <div className="mellon-stat">
                  <span className="mellon-stat-label">{t('metrics.ccr')}</span>
                  <span className="mellon-stat-value">{metrics && metrics.ccr > 0 ? `${metrics.ccr.toFixed(0)} ml/min` : '--'}</span>
                </div>
              </div>
            </header>

            {/* ---- Tabs ---- */}
            <Tabs className="mellon-tabs" defaultActiveKey="regimen" items={tabItems} />
          </Content>

          {/* ---- Footer ---- */}
          <Footer className="mellon-footer">
            <div className="mellon-footer-text">
              {t('app.footer')}
            </div>
          </Footer>
        </Layout>
      </Layout>
    </div>
  );
}
