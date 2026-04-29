package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"os"

	"github.com/rainoffallingstar/mellon/internal/database"
	"github.com/rainoffallingstar/mellon/internal/metrics"
	"github.com/rainoffallingstar/mellon/internal/regimen"
	"github.com/rainoffallingstar/mellon/internal/scoring"
)

// Request is the JSON input from stdin.
type Request struct {
	Action   string   `json:"action"`
	Weight   float64  `json:"weight"`
	Height   float64  `json:"height"`
	Gender   string   `json:"gender"`
	Age      float64  `json:"age"`
	Scr      float64  `json:"scr"`
	BSA      float64  `json:"bsa"`
	Regimen  string   `json:"regimen"`

	// Scoring inputs
	Skin       int `json:"skin"`
	Liver      int `json:"liver"`
	Gastric    int `json:"gastric"`

	AgeIPI        int `json:"age_ipi"`
	ECOG          int `json:"ecog"`
	AnnArbor      int `json:"ann_arbor"`
	Extranodal    int `json:"extranodal"`
	LDH           int `json:"ldh"`

	Count     int `json:"count"`
	Write     int `json:"write"`
	Listen    int `json:"listen"`
	Attention int `json:"attention"`
	Named     int `json:"named"`

	HGB       int `json:"hgb"`
	SerumCa   int `json:"serum_ca"`
	BoneImage int `json:"bone_image"`
	MProtein  int `json:"m_protein"`
	SerumJG   string `json:"serum_jg"`
	B2MG      int `json:"b2mg"`
	Albumin   int `json:"albumin"`

	PLT      int `json:"plt"`
	FDPs     int `json:"fdps"`
	PT       int `json:"pt"`
	FBG      int `json:"fbg"`

	PLTChange int `json:"plt_change"`
	PLTTime   int `json:"plt_time"`
	PLTAgg    int `json:"plt_agg"`
	PLTReason int `json:"plt_reason"`

	PLTGrade int `json:"plt_grade"`

	MDAPSSInputs []int `json:"mdapss_inputs"`
	EBMTInputs   []int `json:"ebmt_inputs"`

	PBWBC  float64 `json:"pb_wbc"`
	PBCD34 float64 `json:"pb_cd34"`

	ColWBC   float64 `json:"col_wbc"`
	ColCD34  float64 `json:"col_cd34"`
	ColVol   float64 `json:"col_vol"`

	TpzName string  `json:"tpz_name"`
	TpzDose float64 `json:"tpz_dose"`

	SerumCalcium    float64 `json:"serum_calcium"`
	NormalAlbumin   float64 `json:"normal_albumin"`
	PatientAlbumin  float64 `json:"patient_albumin"`

	DrugNames []string `json:"drug_names"`
	Lang      string   `json:"lang"`
}

func main() {
	dataDir := flag.String("data-dir", "data", "Path to JSON data files")
	flag.Parse()

	var req Request
	decoder := json.NewDecoder(os.Stdin)
	if err := decoder.Decode(&req); err != nil {
		fmt.Fprintf(os.Stderr, "error parsing input: %v\n", err)
		os.Exit(1)
	}

	result, err := dispatch(*dataDir, req)
	if err != nil {
		fmt.Fprintf(os.Stderr, "error: %v\n", err)
		os.Exit(1)
	}

	js, _ := json.Marshal(result)
	os.Stdout.Write(js)
	os.Stdout.Write([]byte("\n"))
}

func dispatch(dataDir string, req Request) (interface{}, error) {
	switch req.Action {
	// --- Metrics ---
	case "bmi":
		return metrics.CalculateBMI(req.Weight, req.Height), nil
	case "bsa":
		return metrics.CalculateBSA(req.Weight, req.Height, req.Gender), nil
	case "ccr":
		return metrics.CalculateCCR(req.Age, req.Weight, req.Scr, req.Gender), nil

	// --- Metrics batch ---
	case "calculate_all_metrics":
		return map[string]float64{
			"bmi": metrics.CalculateBMI(req.Weight, req.Height),
			"bsa": metrics.CalculateBSA(req.Weight, req.Height, req.Gender),
			"ccr": metrics.CalculateCCR(req.Age, req.Weight, req.Scr, req.Gender),
		}, nil

	// --- Regimen ---
	case "list_regimens":
		db, err := regimen.LoadRegimens(dataDir + "/regmen_list.json")
		if err != nil {
			return nil, err
		}
		return regimen.GetRegimenNames(db), nil

	case "calculate_regimen":
		db, err := regimen.LoadRegimens(dataDir + "/regmen_list.json")
		if err != nil {
			return nil, err
		}
		drugs, ok := db[req.Regimen]
		if !ok {
			return nil, fmt.Errorf("regimen not found: %s", req.Regimen)
		}
		return regimen.CalculateDoses(drugs, req.BSA, req.Weight), nil

	// --- Side Effects ---
	case "lookup_side_effects":
		return database.LookupSideEffects(dataDir, req.DrugNames, req.Lang)

	// --- Scoring ---
	case "score_agvhd":
		return scoring.ScoreAGVHD(req.Skin, req.Liver, req.Gastric), nil

	case "score_ipi":
		score := scoring.ScoreIPI(req.AgeIPI, req.ECOG, req.AnnArbor, req.Extranodal, req.LDH)
		return map[string]interface{}{
			"score": score,
			"risk":  scoring.IPIRisk(score),
		}, nil

	case "score_icans":
		return scoring.ScoreICANS(req.Count, req.Write, req.Listen, req.Attention, req.Named), nil

	case "score_mm_ds":
		return scoring.ScoreMMDS(req.HGB, req.SerumCa, req.BoneImage, req.MProtein), nil

	case "score_mm_iss":
		return scoring.ScoreMMISS(req.B2MG, req.Albumin), nil

	case "score_mm_full":
		return scoring.ScoreMMFull(req.HGB, req.SerumCa, req.BoneImage, req.MProtein,
			req.SerumJG, req.B2MG, req.Albumin), nil

	case "score_dic":
		score := scoring.ScoreDIC(req.PLT, req.FDPs, req.PT, req.FBG)
		return map[string]interface{}{
			"score":          score,
			"interpretation": scoring.DICInterpretation(score),
		}, nil

	case "score_hit":
		score := scoring.ScoreHIT4Ts(req.PLTChange, req.PLTTime, req.PLTAgg, req.PLTReason)
		return map[string]interface{}{
			"score":          score,
			"interpretation": scoring.HITInterpretation(score),
		}, nil

	case "score_cit":
		return scoring.ScoreCIT(req.PLTGrade), nil

	case "score_mdapss":
		return scoring.ScoreMDAPSS(dataDir, req.MDAPSSInputs)

	case "score_ebmt":
		return scoring.ScoreEBMT(req.EBMTInputs), nil

	// --- PHSC ---
	case "phsc_before":
		return scoring.CountPHSCBefore(req.PBWBC, req.PBCD34), nil

	case "phsc_after":
		return scoring.CountPHSCAfter(req.ColWBC, req.ColCD34, req.ColVol, req.Weight), nil

	// --- Corticosteroid ---
	case "convert_corticosteroid":
		return scoring.ConvertCorticosteroid(dataDir, req.TpzName, req.TpzDose)

	// --- Calcium ---
	case "adjust_calcium":
		return scoring.AdjustCalcium(req.SerumCalcium, req.NormalAlbumin, req.PatientAlbumin)

	default:
		return nil, fmt.Errorf("unknown action: %s", req.Action)
	}
}
