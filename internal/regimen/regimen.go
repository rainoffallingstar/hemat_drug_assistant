package regimen

import (
	"encoding/json"
	"os"
)

type Drug struct {
	Name     string  `json:"药名"`
	Type     string  `json:"类型"`
	RefDose  float64 `json:"推荐值(mg/m2)"`
	CalcDose float64 `json:"计算量"`
	Min      float64 `json:"最小值"`
	Max      float64 `json:"最大值"`
	Unit     string  `json:"单位"`
}

type DrugResult struct {
	Name     string  `json:"name"`
	Type     string  `json:"type"`
	RefDose  float64 `json:"ref_dose"`
	CalcDose float64 `json:"calc_dose"`
	Min      float64 `json:"min_dose"`
	Max      float64 `json:"max_dose"`
	Unit     string  `json:"unit"`
}

type RegimenDB map[string][]Drug

func LoadRegimens(path string) (RegimenDB, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	var db RegimenDB
	err = json.Unmarshal(data, &db)
	return db, err
}

func CalculateDoses(drugs []Drug, bsa, weight float64) []DrugResult {
	results := make([]DrugResult, len(drugs))
	for i, d := range drugs {
		df := d
		if df.Type == "monoclone" {
			df.CalcDose = df.CalcDose + df.RefDose
		} else if df.RefDose != 0 && df.Type == "weight" {
			df.CalcDose = df.RefDose * weight
		} else if df.RefDose == 0 && df.Type != "monoclone" && df.Type != "weight" {
			df.Min = df.Min * bsa
			df.Max = df.Max * bsa
		} else if df.RefDose == 0 && df.Type == "weight" {
			df.Min = df.Min * weight
			df.Max = df.Max * weight
		} else {
			df.CalcDose = df.RefDose * bsa
		}
		// Scale min dose for range display (e.g. doxorubicin 40-50mg/m²)
		if df.RefDose != 0 && df.Min > 0 {
			df.Min = df.Min * bsa
		}
		// Apply max dose cap (e.g. vincristine max 2mg)
		if df.Max > 0 && df.RefDose != 0 && df.CalcDose > df.Max {
			df.CalcDose = df.Max
		}
		results[i] = DrugResult{
			Name:     df.Name,
			Type:     df.Type,
			RefDose:  df.RefDose,
			CalcDose: df.CalcDose,
			Min:      df.Min,
			Max:      df.Max,
			Unit:     df.Unit,
		}
	}
	return results
}

func GetRegimenNames(db RegimenDB) []string {
	names := make([]string, 0, len(db))
	for k := range db {
		names = append(names, k)
	}
	return names
}
