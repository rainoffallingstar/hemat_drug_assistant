export interface ThemePreset {
  name: string;
  label: string;
  isDark: boolean;
  tokens: {
    colorPrimary: string;
    colorPrimaryHover: string;
    colorBgLayout: string;
    colorBgContainer: string;
    colorText: string;
    colorTextSecondary: string;
    colorBorder: string;
    colorBorderSecondary: string;
    resultBg: string;
    resultBorder: string;
    resultText: string;
  };
}

export const themes: ThemePreset[] = [
  {
    name: 'claude', label: 'Claude', isDark: false,
    tokens: {
      colorPrimary: '#C2680A', colorPrimaryHover: '#9A5208',
      colorBgLayout: '#FAF7F2', colorBgContainer: '#FFFFFF',
      colorText: '#2D2B26', colorTextSecondary: '#787569',
      colorBorder: '#E8E2D5', colorBorderSecondary: '#F0EBE0',
      resultBg: '#F7F4EE', resultBorder: '#EBE4D5', resultText: '#5C574E',
    },
  },
  {
    name: 'dark', label: 'Dark', isDark: true,
    tokens: {
      colorPrimary: '#F59E0B', colorPrimaryHover: '#D97706',
      colorBgLayout: '#161618', colorBgContainer: '#252527',
      colorText: '#E5E5E5', colorTextSecondary: '#999',
      colorBorder: '#3A3A3C', colorBorderSecondary: '#2C2C2E',
      resultBg: '#1E1E20', resultBorder: '#3A3A3C', resultText: '#CCC',
    },
  },
  {
    name: 'ocean', label: 'Ocean', isDark: false,
    tokens: {
      colorPrimary: '#0D9488', colorPrimaryHover: '#0F766E',
      colorBgLayout: '#F0F6F7', colorBgContainer: '#F8FCFD',
      colorText: '#1E293B', colorTextSecondary: '#64748B',
      colorBorder: '#D0DDE2', colorBorderSecondary: '#E4EEF2',
      resultBg: '#EDF5F6', resultBorder: '#D0DDE2', resultText: '#334155',
    },
  },
  {
    name: 'forest', label: 'Forest', isDark: false,
    tokens: {
      colorPrimary: '#16A34A', colorPrimaryHover: '#15803D',
      colorBgLayout: '#F2F6EE', colorBgContainer: '#F8FBF6',
      colorText: '#1A2E1A', colorTextSecondary: '#4A6B4A',
      colorBorder: '#D0DBCC', colorBorderSecondary: '#E2EAE0',
      resultBg: '#F0F5EC', resultBorder: '#D0DBCC', resultText: '#3A5A3A',
    },
  },
  {
    name: 'rose', label: 'Rose', isDark: false,
    tokens: {
      colorPrimary: '#DC2626', colorPrimaryHover: '#B91C1C',
      colorBgLayout: '#FEF7F8', colorBgContainer: '#FFFCFD',
      colorText: '#2D1520', colorTextSecondary: '#8B5E6B',
      colorBorder: '#F0D0D8', colorBorderSecondary: '#FBE8ED',
      resultBg: '#FDF3F5', resultBorder: '#F0D0D8', resultText: '#5C3A45',
    },
  },
];
