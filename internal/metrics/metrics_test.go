package metrics

import (
	"math"
	"testing"
)

func TestCalculateBMI(t *testing.T) {
	result := CalculateBMI(70, 175)
	expected := 70.0 / math.Pow(1.75, 2)
	if math.Abs(result-expected) > 1e-6 {
		t.Errorf("BMI(70,175) = %f, want %f", result, expected)
	}
}

func TestCalculateBSA(t *testing.T) {
	// Mosteller
	result := CalculateBSA(50, 170, "Common")
	expected := math.Sqrt(170.0 * 50.0 / 3600.0)
	if math.Abs(result-expected) > 1e-6 {
		t.Errorf("BSA(50,170,Common) = %f, want %f", result, expected)
	}

	// Female
	female := CalculateBSA(50, 170, "Female")
	femaleExp := 0.00586*170 + 0.0126*50 - 0.0461
	if math.Abs(female-femaleExp) > 1e-6 {
		t.Errorf("BSA(50,170,Female) = %f, want %f", female, femaleExp)
	}

	// Male
	male := CalculateBSA(50, 170, "Male")
	maleExp := 0.00607*170 + 0.0127*50 - 0.0698
	if math.Abs(male-maleExp) > 1e-6 {
		t.Errorf("BSA(50,170,Male) = %f, want %f", male, maleExp)
	}
}

func TestCalculateCCR(t *testing.T) {
	result := CalculateCCR(30, 70, 80, "Common")
	expected := (140.0 - 30) * 70 / 0.818 / 80
	if math.Abs(result-expected) > 1e-6 {
		t.Errorf("CCR(30,70,80,Common) = %f, want %f", result, expected)
	}
}
