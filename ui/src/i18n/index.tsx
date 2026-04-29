import React, { createContext, useContext, useState, useCallback } from 'react';
import zh from './zh';
import en from './en';

export type Language = 'zh' | 'en';

const strings: Record<Language, Record<string, string>> = { zh, en };

interface LanguageContextType {
  lang: Language;
  t: (key: string) => string;
  setLanguage: (lang: Language) => void;
}

const LanguageContext = createContext<LanguageContextType>({
  lang: 'zh',
  t: (key: string) => key,
  setLanguage: () => {},
});

function getStoredLang(): Language {
  try {
    const v = localStorage.getItem('mellon-lang');
    if (v === 'zh' || v === 'en') return v;
  } catch { /* ignore */ }
  return 'zh';
}

export function LanguageProvider({ children }: { children: React.ReactNode }) {
  const [lang, setLang] = useState<Language>(getStoredLang);

  const setLanguage = useCallback((l: Language) => {
    setLang(l);
    try { localStorage.setItem('mellon-lang', l); } catch { /* ignore */ }
  }, []);

  const t = useCallback(
    (key: string) => strings[lang][key] ?? key,
    [lang]
  );

  return (
    <LanguageContext.Provider value={{ lang, t, setLanguage }}>
      {children}
    </LanguageContext.Provider>
  );
}

export function useLanguage() {
  return useContext(LanguageContext);
}
