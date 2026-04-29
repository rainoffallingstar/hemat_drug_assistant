package metrics

import "math"

// BMI = weight(kg) / (height(cm)/100)^2
func CalculateBMI(weight, height float64) float64 {
	return weight / math.Pow(height/100, 2)
}

// BSA — Mosteller formula by default, gender-specific Zhao formulas for male/female
func CalculateBSA(weight, height float64, gender string) float64 {
	switch gender {
	case "Common", "通用":
		return math.Sqrt(height * weight / 3600)
	case "Female", "女性":
		return 0.00586*height + 0.0126*weight - 0.0461
	default:
		return 0.00607*height + 0.0127*weight - 0.0698
	}
}

// CCR — Cockcroft-Gault with gender adjustments
func CalculateCCR(age, weight, scr float64, gender string) float64 {
	switch gender {
	case "Common", "通用":
		return (140 - age) * weight / 0.818 / scr
	case "Female", "女性":
		return 0.85 * 1.23 * (140 - age) * weight / scr
	default:
		return 1.23 * (140 - age) * weight / scr
	}
}
