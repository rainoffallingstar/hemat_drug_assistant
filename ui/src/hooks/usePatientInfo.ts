import { useState, useCallback, useRef, useEffect } from 'react';
import { api } from '../api/client';
import type { MetricsResult } from '../types';

export interface PatientInfoState {
  weight: string; height: string; age: string; scr: string; gender: string;
}

const DEFAULT: PatientInfoState = { weight: '50', height: '170', age: '', scr: '', gender: '通用' };

async function retry<T>(fn: () => Promise<T>, maxRetries = 3): Promise<T> {
  for (let i = 0; i < maxRetries; i++) {
    try { return await fn(); }
    catch (e) {
      if (i === maxRetries - 1) throw e;
      await new Promise(r => setTimeout(r, 500 * (i + 1)));
    }
  }
  throw new Error('unreachable');
}

export function usePatientInfo() {
  const [info, setInfo] = useState<PatientInfoState>(DEFAULT);
  const [metrics, setMetrics] = useState<MetricsResult | null>(null);
  const [loading, setLoading] = useState(false);
  const debounceRef = useRef<ReturnType<typeof setTimeout>>();
  const mountedRef = useRef(false);

  const doCalculate = useCallback(async (info: PatientInfoState) => {
    const w = parseFloat(info.weight);
    const h = parseFloat(info.height);
    if (!w || !h || w <= 0 || h <= 0) return;
    setLoading(true);
    try {
      const a = parseFloat(info.age) || 0;
      const s = parseFloat(info.scr) || 0;
      const result = await retry(() => api.calculateAllMetrics({
        weight: w, height: h, gender: info.gender, age: a, scr: s,
      }));
      if (mountedRef.current) setMetrics(result);
    } catch (e) {
      console.error('Metrics calculation failed:', e);
    } finally {
      if (mountedRef.current) setLoading(false);
    }
  }, []);

  useEffect(() => { mountedRef.current = true; return () => { mountedRef.current = false; }; }, []);

  // Initial calculation with short delay for Shiny WebSocket to connect
  useEffect(() => {
    const t = setTimeout(() => doCalculate(DEFAULT), 300);
    return () => clearTimeout(t);
  }, [doCalculate]);

  const updateInfo = useCallback((field: keyof PatientInfoState, value: string) => {
    setInfo(prev => {
      const next = { ...prev, [field]: value };
      if (debounceRef.current) clearTimeout(debounceRef.current);
      debounceRef.current = setTimeout(() => doCalculate(next), 400);
      return next;
    });
  }, [doCalculate]);

  return { info, metrics, loading, updateInfo };
}
