export interface MetricsInput {
  weight: number;
  height: number;
  gender: string;
  age?: number;
  scr?: number;
}

export interface MetricsResult {
  bmi: number;
  bsa: number;
  ccr: number;
}

export interface DrugDose {
  name: string;
  type: string;
  ref_dose: number;
  calc_dose: number;
  min_dose: number;
  max_dose: number;
  unit: string;
}

export interface SideEffect {
  Drug: string;
  SideEffect: string;
  Measures: string;
}

export interface DiseaseList {
  [category: string]: string[];
}

export interface ScoreResult {
  score: number;
  risk?: string;
  interpretation?: string;
  grade?: number;
}

export interface MDAPSSResult {
  积分: string;
  分组: string;
  中位生存月: number;
  三年生存率: number;
}

export interface CorticosteroidDoses {
  doses: Record<string, number>;
}

export interface PatientInfo {
  weight: string;
  height: string;
  age: string;
  scr: string;
  gender: string;
}

export interface IPIResult {
  score: number;
  risk: string;
}

export interface DICResult {
  score: number;
  interpretation: string;
}

export interface HITResult {
  score: number;
  interpretation: string;
}

export interface PHSCResult {
  count: number;
}

export interface CalciumResult {
  adjusted: number;
}

export interface MMResult {
  ds_stage: string;
  iss_stage: string;
  full: string;
}

export interface EBMTResult {
  score: number;
}

export type Language = 'zh' | 'en';
