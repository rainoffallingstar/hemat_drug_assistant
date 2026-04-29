package scoring

import (
	"testing"
)

func TestScoreAGVHD(t *testing.T) {
	tests := []struct {
		skin, liver, gastric, expected int
	}{
		{0, 0, 0, 0},
		{1, 0, 0, 1},
		{1, 1, 0, 2},
		{1, 2, 0, 3},
		{4, 0, 0, 4},
		{0, 4, 0, 4},
		{3, 0, 0, 2}, // sum=3, liver=0, gastric=0 → grade 2
	}
	for _, tc := range tests {
		got := ScoreAGVHD(tc.skin, tc.liver, tc.gastric)
		if got != tc.expected {
			t.Errorf("AGVHD(%d,%d,%d) = %d, want %d",
				tc.skin, tc.liver, tc.gastric, got, tc.expected)
		}
	}
}

func TestScoreIPI(t *testing.T) {
	if got := ScoreIPI(0, 0, 1, 0, 0); got != 0 {
		t.Errorf("IPI low risk = %d, want 0", got)
	}
	if got := ScoreIPI(1, 2, 3, 1, 1); got != 5 {
		t.Errorf("IPI high risk = %d, want 5", got)
	}
}

func TestScoreICANS(t *testing.T) {
	if got := ScoreICANS(4, 3, 1, 1, 1); got != 1 {
		t.Errorf("ICANS perfect = %d, want 1", got)
	}
	if got := ScoreICANS(3, 0, 0, 0, 0); got != 2 {
		t.Errorf("ICANS mild = %d, want 2", got)
	}
	if got := ScoreICANS(0, 0, 0, 0, 0); got != 4 {
		t.Errorf("ICANS severe = %d, want 4", got)
	}
}

func TestScoreDIC(t *testing.T) {
	if got := ScoreDIC(2, 2, 2, 1); got != 7 {
		t.Errorf("DIC(2,2,2,1) = %d, want 7", got)
	}
}

func TestScoreHIT4Ts(t *testing.T) {
	if got := ScoreHIT4Ts(2, 2, 2, 2); got != 8 {
		t.Errorf("HIT(2,2,2,2) = %d, want 8", got)
	}
}

func TestScoreMMDS(t *testing.T) {
	if got := ScoreMMDS(1, 1, 1, 1); got != "DS I期" {
		t.Errorf("MM DS low = %s, want DS I期", got)
	}
	if got := ScoreMMDS(1, 1, 2, 1); got != "DS III期" {
		t.Errorf("MM DS high = %s, want DS III期", got)
	}
	if got := ScoreMMDS(1, 2, 1, 1); got != "DS III期" {
		t.Errorf("MM DS ca-elevated = %s, want DS III期", got)
	}
}

func TestScoreMMISS(t *testing.T) {
	if got := ScoreMMISS(1, 1); got != "ISS I期" {
		t.Errorf("ISS low = %s, want ISS I期", got)
	}
	if got := ScoreMMISS(3, 1); got != "ISS III期" {
		t.Errorf("ISS high = %s, want ISS III期", got)
	}
}

func TestCountPHSC(t *testing.T) {
	if got := CountPHSCBefore(10, 5); got != 500 {
		t.Errorf("PHSC before = %f, want 500", got)
	}
	if got := CountPHSCAfter(10, 5, 200, 50); got != 20 {
		t.Errorf("PHSC after = %f, want 20", got)
	}
}

func TestAdjustCalcium(t *testing.T) {
	got, err := AdjustCalcium(10, 4, 3.9)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	expected := 10.0 + 0.8*(4.0-3.9)
	if got != expected {
		t.Errorf("AdjustCalcium = %f, want %f", got, expected)
	}

	_, err = AdjustCalcium(10, 4, 5.0)
	if err == nil {
		t.Error("expected error for patient albumin > normal albumin")
	}
}
