package database

import (
	"encoding/json"
	"os"
	"strings"
)

type DrugEntry struct {
	DrugCN     string      `json:"药名"`
	DrugEN     string      `json:"Drugs"`
	SideEffect interface{} `json:"副作用/SideEffect"`
	Measures   interface{} `json:"可能的解救措施/Measures"`
}

type DrugRow struct {
	Drug       string
	SideEffect string
	Measures   string
}

func LoadDrugDB(path string) ([]DrugEntry, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	var db []DrugEntry
	err = json.Unmarshal(data, &db)
	return db, err
}

func LookupSideEffects(dataDir string, drugNames []string, lang string) ([]DrugRow, error) {
	db, err := LoadDrugDB(dataDir + "/database.json")
	if err != nil {
		return nil, err
	}

	// Build a set for exact matching (like R's %in%)
	lookupSet := make(map[string]bool)
	for _, name := range drugNames {
		lookupSet[strings.ToLower(name)] = true
	}

	seen := make(map[string]bool)
	var rows []DrugRow

	for _, entry := range db {
		var drugKey string
		if lang == "en" {
			drugKey = strings.ToLower(entry.DrugEN)
		} else {
			drugKey = strings.ToLower(entry.DrugCN)
		}

		if !lookupSet[drugKey] {
			continue
		}
		if seen[drugKey] {
			continue
		}
		seen[drugKey] = true

		se := ""
		if entry.SideEffect != nil {
			s, ok := entry.SideEffect.(string)
			if ok {
				se = s
			}
		}

		m := ""
		if entry.Measures != nil {
			ms, ok := entry.Measures.(string)
			if ok {
				m = ms
			}
		}

		var drugName string
		if lang == "en" {
			drugName = entry.DrugEN
		} else {
			drugName = entry.DrugCN
		}

		rows = append(rows, DrugRow{
			Drug:       drugName,
			SideEffect: se,
			Measures:   m,
		})
	}
	return rows, nil
}
