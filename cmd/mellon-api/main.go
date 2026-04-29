package main

import (
	"encoding/json"
	"flag"
	"log"
	"net/http"
	"os"
	"strconv"

	"github.com/rainoffallingstar/mellon/internal/database"
	"github.com/rainoffallingstar/mellon/internal/metrics"
	"github.com/rainoffallingstar/mellon/internal/regimen"
	"github.com/rainoffallingstar/mellon/internal/scoring"
)

var dataDir string
var dbCache regimen.RegimenDB

func main() {
	port := flag.Int("port", 8099, "HTTP server port")
	flag.StringVar(&dataDir, "data-dir", "data", "Path to JSON data directory")
	wwwDir := flag.String("www", "", "Path to static web root (React SPA)")
	flag.Parse()

	var err error
	dbCache, err = regimen.LoadRegimens(dataDir + "/regmen_list.json")
	if err != nil {
		log.Fatalf("Failed to load regimens: %v", err)
	}

	mux := http.NewServeMux()
	mux.HandleFunc("/api/health", handleHealth)
	mux.HandleFunc("/api/bmi", handleBMI)
	mux.HandleFunc("/api/bsa", handleBSA)
	mux.HandleFunc("/api/ccr", handleCCR)
	mux.HandleFunc("/api/calculate_all_metrics", handleAllMetrics)
	mux.HandleFunc("/api/list_regimens", handleListRegimens)
	mux.HandleFunc("/api/calculate_regimen", handleCalculateRegimen)
	mux.HandleFunc("/api/lookup_side_effects", handleLookupSideEffects)
	mux.HandleFunc("/api/score_agvhd", handleScoreAGVHD)
	mux.HandleFunc("/api/score_ipi", handleScoreIPI)
	mux.HandleFunc("/api/score_icans", handleScoreICANS)
	mux.HandleFunc("/api/score_mm_full", handleScoreMMFull)
	mux.HandleFunc("/api/score_dic", handleScoreDIC)
	mux.HandleFunc("/api/score_hit", handleScoreHIT)
	mux.HandleFunc("/api/score_cit", handleScoreCIT)
	mux.HandleFunc("/api/score_mdapss", handleScoreMDAPSS)
	mux.HandleFunc("/api/score_ebmt", handleScoreEBMT)
	mux.HandleFunc("/api/phsc_before", handlePHSCBefore)
	mux.HandleFunc("/api/phsc_after", handlePHSCAfter)
	mux.HandleFunc("/api/convert_corticosteroid", handleCorticosteroid)
	mux.HandleFunc("/api/adjust_calcium", handleAdjustCalcium)

	// Serve React SPA static files if www directory provided
	if *wwwDir != "" && dirExists(*wwwDir) {
		fs := http.FileServer(http.Dir(*wwwDir))
		mux.Handle("/", fs)
		log.Printf("Serving static files from %s", *wwwDir)
	}

	handler := corsMiddleware(mux)
	log.Printf("mellon API listening on :%d (data: %s)", *port, dataDir)
	log.Fatal(http.ListenAndServe(":"+strconv.Itoa(*port), handler))
}

func dirExists(path string) bool {
	info, err := os.Stat(path)
	return err == nil && info.IsDir()
}

func corsMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Access-Control-Allow-Origin", "*")
		w.Header().Set("Access-Control-Allow-Methods", "POST, GET, OPTIONS")
		w.Header().Set("Access-Control-Allow-Headers", "Content-Type")
		if r.Method == "OPTIONS" {
			w.WriteHeader(200)
			return
		}
		next.ServeHTTP(w, r)
	})
}

func writeJSON(w http.ResponseWriter, data interface{}) {
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(data)
}

func writeError(w http.ResponseWriter, msg string, code int) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(code)
	json.NewEncoder(w).Encode(map[string]string{"error": msg})
}

func parseBody(r *http.Request, v interface{}) error {
	defer r.Body.Close()
	return json.NewDecoder(r.Body).Decode(v)
}

// --- Handlers ---

func handleHealth(w http.ResponseWriter, r *http.Request) {
	writeJSON(w, map[string]string{"status": "ok"})
}

type metricsReq struct {
	Weight float64 `json:"weight"`
	Height float64 `json:"height"`
	Gender string  `json:"gender"`
	Age    float64 `json:"age"`
	Scr    float64 `json:"scr"`
}

func handleBMI(w http.ResponseWriter, r *http.Request) {
	var req metricsReq
	if err := parseBody(r, &req); err != nil {
		writeError(w, err.Error(), 400)
		return
	}
	writeJSON(w, metrics.CalculateBMI(req.Weight, req.Height))
}

func handleBSA(w http.ResponseWriter, r *http.Request) {
	var req metricsReq
	if err := parseBody(r, &req); err != nil {
		writeError(w, err.Error(), 400)
		return
	}
	writeJSON(w, metrics.CalculateBSA(req.Weight, req.Height, req.Gender))
}

func handleCCR(w http.ResponseWriter, r *http.Request) {
	var req metricsReq
	if err := parseBody(r, &req); err != nil {
		writeError(w, err.Error(), 400)
		return
	}
	writeJSON(w, metrics.CalculateCCR(req.Age, req.Weight, req.Scr, req.Gender))
}

func handleAllMetrics(w http.ResponseWriter, r *http.Request) {
	var req metricsReq
	if err := parseBody(r, &req); err != nil {
		writeError(w, err.Error(), 400)
		return
	}
	ccr := 0.0
	if req.Age > 0 && req.Scr > 0 {
		ccr = metrics.CalculateCCR(req.Age, req.Weight, req.Scr, req.Gender)
	}
	writeJSON(w, map[string]float64{
		"bmi": metrics.CalculateBMI(req.Weight, req.Height),
		"bsa": metrics.CalculateBSA(req.Weight, req.Height, req.Gender),
		"ccr": ccr,
	})
}

func handleListRegimens(w http.ResponseWriter, r *http.Request) {
	data, err := os.ReadFile(dataDir + "/disease_list.json")
	if err != nil {
		writeError(w, err.Error(), 500)
		return
	}
	var diseaseList map[string][]string
	if err := json.Unmarshal(data, &diseaseList); err != nil {
		writeError(w, err.Error(), 500)
		return
	}
	writeJSON(w, diseaseList)
}

type regimenReq struct {
	Regimen string  `json:"regimen"`
	BSA     float64 `json:"bsa"`
	Weight  float64 `json:"weight"`
}

func handleCalculateRegimen(w http.ResponseWriter, r *http.Request) {
	var req regimenReq
	if err := parseBody(r, &req); err != nil {
		writeError(w, err.Error(), 400)
		return
	}
	drugs, ok := dbCache[req.Regimen]
	if !ok {
		writeError(w, "regimen not found: "+req.Regimen, 404)
		return
	}
	writeJSON(w, regimen.CalculateDoses(drugs, req.BSA, req.Weight))
}

type sideEffectsReq struct {
	DrugNames []string `json:"drug_names"`
	Lang      string   `json:"lang"`
}

func handleLookupSideEffects(w http.ResponseWriter, r *http.Request) {
	var req sideEffectsReq
	if err := parseBody(r, &req); err != nil {
		writeError(w, err.Error(), 400)
		return
	}
	result, err := database.LookupSideEffects(dataDir, req.DrugNames, req.Lang)
	if err != nil {
		writeError(w, err.Error(), 500)
		return
	}
	writeJSON(w, result)
}

// --- Scoring handlers ---

type agvhdReq struct {
	Skin    int `json:"skin"`
	Liver   int `json:"liver"`
	Gastric int `json:"gastric"`
}

func handleScoreAGVHD(w http.ResponseWriter, r *http.Request) {
	var req agvhdReq
	if err := parseBody(r, &req); err != nil {
		writeError(w, err.Error(), 400)
		return
	}
	writeJSON(w, map[string]int{"score": scoring.ScoreAGVHD(req.Skin, req.Liver, req.Gastric)})
}

type ipiReq struct {
	AgeIPI     int `json:"age_ipi"`
	ECOG       int `json:"ecog"`
	AnnArbor   int `json:"ann_arbor"`
	Extranodal int `json:"extranodal"`
	LDH        int `json:"ldh"`
}

func handleScoreIPI(w http.ResponseWriter, r *http.Request) {
	var req ipiReq
	if err := parseBody(r, &req); err != nil {
		writeError(w, err.Error(), 400)
		return
	}
	score := scoring.ScoreIPI(req.AgeIPI, req.ECOG, req.AnnArbor, req.Extranodal, req.LDH)
	writeJSON(w, map[string]interface{}{
		"score": score,
		"risk":  scoring.IPIRisk(score),
	})
}

type icansReq struct {
	Count     int `json:"count"`
	Write     int `json:"write"`
	Listen    int `json:"listen"`
	Attention int `json:"attention"`
	Named     int `json:"named"`
}

func handleScoreICANS(w http.ResponseWriter, r *http.Request) {
	var req icansReq
	if err := parseBody(r, &req); err != nil {
		writeError(w, err.Error(), 400)
		return
	}
	writeJSON(w, map[string]int{"grade": scoring.ScoreICANS(req.Count, req.Write, req.Listen, req.Attention, req.Named)})
}

type mmReq struct {
	HGB       int    `json:"hgb"`
	SerumCa   int    `json:"serum_ca"`
	BoneImage int    `json:"bone_image"`
	MProtein  int    `json:"m_protein"`
	SerumJG   string `json:"serum_jg"`
	B2MG      int    `json:"b2mg"`
	Albumin   int    `json:"albumin"`
}

func handleScoreMMFull(w http.ResponseWriter, r *http.Request) {
	var req mmReq
	if err := parseBody(r, &req); err != nil {
		writeError(w, err.Error(), 400)
		return
	}
	ds := scoring.ScoreMMDS(req.HGB, req.SerumCa, req.BoneImage, req.MProtein)
	iss := scoring.ScoreMMISS(req.B2MG, req.Albumin)
	writeJSON(w, map[string]string{
		"ds_stage":  ds,
		"iss_stage": iss,
		"full":      "该病人是：" + ds + req.SerumJG + "亚型" + iss,
	})
}

type dicReq struct {
	PLT  int `json:"plt"`
	FDPS int `json:"fdps"`
	PT   int `json:"pt"`
	FBG  int `json:"fbg"`
}

func handleScoreDIC(w http.ResponseWriter, r *http.Request) {
	var req dicReq
	if err := parseBody(r, &req); err != nil {
		writeError(w, err.Error(), 400)
		return
	}
	score := scoring.ScoreDIC(req.PLT, req.FDPS, req.PT, req.FBG)
	writeJSON(w, map[string]interface{}{
		"score":          score,
		"interpretation": scoring.DICInterpretation(score),
	})
}

type hitReq struct {
	PLTChange int `json:"plt_change"`
	PLTTime   int `json:"plt_time"`
	PLTAgg    int `json:"plt_agg"`
	PLTReason int `json:"plt_reason"`
}

func handleScoreHIT(w http.ResponseWriter, r *http.Request) {
	var req hitReq
	if err := parseBody(r, &req); err != nil {
		writeError(w, err.Error(), 400)
		return
	}
	score := scoring.ScoreHIT4Ts(req.PLTChange, req.PLTTime, req.PLTAgg, req.PLTReason)
	writeJSON(w, map[string]interface{}{
		"score":          score,
		"interpretation": scoring.HITInterpretation(score),
	})
}

type citReq struct {
	PLTGrade int `json:"plt_grade"`
}

func handleScoreCIT(w http.ResponseWriter, r *http.Request) {
	var req citReq
	if err := parseBody(r, &req); err != nil {
		writeError(w, err.Error(), 400)
		return
	}
	writeJSON(w, map[string]int{"grade": scoring.ScoreCIT(req.PLTGrade)})
}

type mdapssReq struct {
	Inputs []int `json:"mdapss_inputs"`
}

func handleScoreMDAPSS(w http.ResponseWriter, r *http.Request) {
	var req mdapssReq
	if err := parseBody(r, &req); err != nil {
		writeError(w, err.Error(), 400)
		return
	}
	result, err := scoring.ScoreMDAPSS(dataDir, req.Inputs)
	if err != nil {
		writeError(w, err.Error(), 500)
		return
	}
	writeJSON(w, result)
}

type ebmtReq struct {
	Inputs []int `json:"ebmt_inputs"`
}

func handleScoreEBMT(w http.ResponseWriter, r *http.Request) {
	var req ebmtReq
	if err := parseBody(r, &req); err != nil {
		writeError(w, err.Error(), 400)
		return
	}
	writeJSON(w, map[string]int{"score": scoring.ScoreEBMT(req.Inputs)})
}

type phscBeforeReq struct {
	PBWBC  float64 `json:"pb_wbc"`
	PBCD34 float64 `json:"pb_cd34"`
}

func handlePHSCBefore(w http.ResponseWriter, r *http.Request) {
	var req phscBeforeReq
	if err := parseBody(r, &req); err != nil {
		writeError(w, err.Error(), 400)
		return
	}
	writeJSON(w, map[string]float64{"count": scoring.CountPHSCBefore(req.PBWBC, req.PBCD34)})
}

type phscAfterReq struct {
	ColWBC  float64 `json:"col_wbc"`
	ColCD34 float64 `json:"col_cd34"`
	ColVol  float64 `json:"col_vol"`
	Weight  float64 `json:"weight"`
}

func handlePHSCAfter(w http.ResponseWriter, r *http.Request) {
	var req phscAfterReq
	if err := parseBody(r, &req); err != nil {
		writeError(w, err.Error(), 400)
		return
	}
	writeJSON(w, map[string]float64{"count": scoring.CountPHSCAfter(req.ColWBC, req.ColCD34, req.ColVol, req.Weight)})
}

type tpzReq struct {
	Name string  `json:"tpz_name"`
	Dose float64 `json:"tpz_dose"`
}

func handleCorticosteroid(w http.ResponseWriter, r *http.Request) {
	var req tpzReq
	if err := parseBody(r, &req); err != nil {
		writeError(w, err.Error(), 400)
		return
	}
	result, err := scoring.ConvertCorticosteroid(dataDir, req.Name, req.Dose)
	if err != nil {
		writeError(w, err.Error(), 500)
		return
	}
	writeJSON(w, map[string]interface{}{"doses": result})
}

type calciumReq struct {
	SerumCalcium   float64 `json:"serum_calcium"`
	NormalAlbumin  float64 `json:"normal_albumin"`
	PatientAlbumin float64 `json:"patient_albumin"`
}

func handleAdjustCalcium(w http.ResponseWriter, r *http.Request) {
	var req calciumReq
	if err := parseBody(r, &req); err != nil {
		writeError(w, err.Error(), 400)
		return
	}
	result, err := scoring.AdjustCalcium(req.SerumCalcium, req.NormalAlbumin, req.PatientAlbumin)
	if err != nil {
		writeError(w, err.Error(), 400)
		return
	}
	writeJSON(w, map[string]float64{"adjusted": result})
}
