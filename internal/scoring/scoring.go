package scoring

import (
	"encoding/json"
	"fmt"
	"os"
)

// --- aGVHD ---

func ScoreAGVHD(skin, liver, gastric int) int {
	s := skin + liver + gastric
	if s == 0 {
		return 0
	}
	if s <= 2 && liver == 0 && gastric == 0 {
		return 1
	}
	if skin == 4 || liver == 4 {
		return 4
	}
	if liver == 2 || liver == 3 || gastric == 2 || gastric == 3 || gastric == 4 {
		return 3
	}
	return 2
}

// --- IPI Lymphoma ---

func ScoreIPI(age, ecog, ann, extranodal, ldh int) int {
	pECOG := 0
	if ecog >= 2 {
		pECOG = 1
	}
	pAnn := 0
	if ann > 2 {
		pAnn = 1
	}
	return age + extranodal + pECOG + pAnn + ldh
}

// -- IPI risk category ---

func IPIRisk(score int) string {
	switch {
	case score < 2:
		return "低危"
	case score == 2:
		return "中低危"
	case score == 3:
		return "中高危"
	default:
		return "高危"
	}
}

// --- CAR-T ICANS ---
// Higher score = better function. Maps to grades 1-4.

func ScoreICANS(count, write, listen, attention, named int) int {
	score := count + write + listen + attention + named
	switch {
	case score >= 7:
		return 1
	case score >= 3:
		return 2
	case score >= 1:
		return 3
	default:
		return 4
	}
}

// --- Myeloma DS staging ---

func ScoreMMDS(hgb, serumCa, boneImage, mProtein int) string {
	if hgb == 1 && serumCa == 1 && boneImage == 1 && mProtein == 1 {
		return "DS I期"
	}
	if hgb == 3 || serumCa == 2 || boneImage == 2 || mProtein == 2 {
		return "DS III期"
	}
	return "DS II期"
}

// --- Myeloma ISS staging ---

func ScoreMMISS(b2mg, albumin int) string {
	if b2mg == 1 && albumin == 1 {
		return "ISS I期"
	}
	if b2mg == 3 {
		return "ISS III期"
	}
	return "ISS II期"
}

// --- Myeloma full result ---

func ScoreMMFull(hgb, serumCa, boneImage, mProtein int, serumJG string, b2mg, albumin int) string {
	ds := ScoreMMDS(hgb, serumCa, boneImage, mProtein)
	iss := ScoreMMISS(b2mg, albumin)
	return fmt.Sprintf("该病人是：%s%s亚型%s", ds, serumJG, iss)
}

// --- DIC ---

func ScoreDIC(plt, fdps, pt, fbg int) int {
	return plt + fdps + pt + fbg
}

func DICInterpretation(score int) string {
	if score >= 5 {
		return "积分>=5分，符合典型DIC"
	}
	return "积分<5分，提示非典型DIC"
}

// --- HIT 4Ts ---

func ScoreHIT4Ts(pltChange, pltTime, pltAgg, pltReason int) int {
	return pltChange + pltTime + pltAgg + pltReason
}

func HITInterpretation(score int) string {
	switch {
	case score <= 3:
		return "0-3分,轻度怀疑:诊断HIT概率很低"
	case score >= 6:
		return "6-8分,高度怀疑:抗体阴性者诊断HIT概率为16%,抗体阳性者诊断HIT概率为98%。"
	default:
		return "4-5分,中度怀疑:抗体阴性者诊断HIT概率为0.6%,抗体阳性者诊断HIT概率为58.2%"
	}
}

// --- CIT ---

func ScoreCIT(grade int) int {
	return grade
}

// --- MDAPSS ---

type MDAPSSRow struct {
	ScoreRange  string  `json:"积分"`
	Group       string  `json:"分组"`
	MedianSurv  float64 `json:"中位生存月"`
	ThreeYrSurv float64 `json:"三年生存率"`
	Index       int     `json:"index"`
}

func ScoreMDAPSS(dataDir string, inputs []int) (MDAPSSRow, error) {
	data, err := os.ReadFile(dataDir + "/mdapss_refertable.json")
	if err != nil {
		return MDAPSSRow{}, err
	}
	var ref []MDAPSSRow
	if err := json.Unmarshal(data, &ref); err != nil {
		return MDAPSSRow{}, err
	}

	sum := 0
	for _, v := range inputs {
		sum += v
	}
	idxFix := sum
	if idxFix > 9 {
		idxFix = 9
	}

	for _, row := range ref {
		if row.Index >= idxFix {
			return row, nil
		}
	}
	return ref[len(ref)-1], nil
}

// --- EBMT ---

func ScoreEBMT(inputs []int) int {
	sum := 0
	for _, v := range inputs {
		sum += v
	}
	return sum
}

// --- PHSC counts ---

func CountPHSCBefore(pbWBC, pbCD34 float64) float64 {
	return 10 * pbWBC * pbCD34
}

func CountPHSCAfter(colWBC, colCD34, colVol, weight float64) float64 {
	return colWBC / 10 * colCD34 * colVol / weight
}

// --- Corticosteroid conversion ---

type TpzEntry struct {
	Name     string  `json:"tpznames"`
	BaseDose float64 `json:"basedose"`
}

func ConvertCorticosteroid(dataDir, drugName string, dose float64) (map[string]float64, error) {
	data, err := os.ReadFile(dataDir + "/tpz_refertable.json")
	if err != nil {
		return nil, err
	}
	var ref []TpzEntry
	if err := json.Unmarshal(data, &ref); err != nil {
		return nil, err
	}

	var baseDose float64
	drugNames := make([]string, len(ref))
	for i, r := range ref {
		drugNames[i] = r.Name
		if r.Name == drugName {
			baseDose = r.BaseDose
		}
	}

	times := dose / baseDose
	result := make(map[string]float64)
	for _, r := range ref {
		result[r.Name] = r.BaseDose * times
	}
	return result, nil
}

// --- Calcium adjustment ---

func AdjustCalcium(serumCa, normalAlb, patientAlb float64) (float64, error) {
	if patientAlb > normalAlb {
		return 0, fmt.Errorf("serum albumin should be lower than normal albumin")
	}
	return serumCa + 0.8*(normalAlb-patientAlb), nil
}
