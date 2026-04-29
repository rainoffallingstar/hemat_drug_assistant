import { useState, createContext, useContext } from 'react';
import { ConfigProvider, App as AntApp, theme as antTheme } from 'antd';
import zhCN from 'antd/locale/zh_CN';
import enUS from 'antd/locale/en_US';
import { useLanguage } from './i18n';
import { themes, type ThemePreset } from './themes';
import AppLayout from './components/Layout/AppLayout';

interface ThemeContextType {
  current: ThemePreset;
  setTheme: (name: string) => void;
}

export const ThemeContext = createContext<ThemeContextType>({
  current: themes[0],
  setTheme: () => {},
});

export function useTheme() {
  return useContext(ThemeContext);
}

export default function App() {
  const { lang } = useLanguage();
  const [themeName, setThemeName] = useState('claude');
  const current = themes.find(t => t.name === themeName) || themes[0];

  return (
    <ThemeContext.Provider value={{ current, setTheme: setThemeName }}>
      <ConfigProvider
        theme={{
          token: {
            colorPrimary: current.tokens.colorPrimary,
            colorPrimaryHover: current.tokens.colorPrimaryHover,
            colorLink: current.tokens.colorPrimary,
            colorLinkHover: current.tokens.colorPrimaryHover,
            colorBgLayout: current.tokens.colorBgLayout,
            colorBgContainer: current.tokens.colorBgContainer,
            colorText: current.tokens.colorText,
            colorTextSecondary: current.tokens.colorTextSecondary,
            colorBorder: current.tokens.colorBorder,
            colorBorderSecondary: current.tokens.colorBorderSecondary,
            borderRadius: 10,
            borderRadiusLG: 14,
            borderRadiusSM: 8,
            fontFamily: "'Plus Jakarta Sans', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif",
            fontSize: 14,
          },
          algorithm: current.isDark ? antTheme.darkAlgorithm : antTheme.defaultAlgorithm,
        }}
        locale={lang === 'zh' ? zhCN : enUS}
      >
        <AntApp>
          <AppLayout />
        </AntApp>
      </ConfigProvider>
    </ThemeContext.Provider>
  );
}
