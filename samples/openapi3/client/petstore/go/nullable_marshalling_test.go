package main

import (
	"encoding/json"
	"fmt"
	"testing"

	sw "go-petstore"
)

func TestNullableMarshalling(t *testing.T) {
	var fv float32 = 1.1
	nc := sw.NullableClass{
		IntegerProp: *sw.NewNullableInt32(nil),
		NumberProp:  *sw.NewNullableFloat32(&fv),
		// BooleanProp is also nullable, but we leave it unset, so it shouldn't be in the JSON
	}
	res, err := json.Marshal(nc)
	if err != nil {
		t.Errorf("Error while marshalling structure with Nullables: %v", err)
	}
	expected := `{"integer_prop":null,"number_prop":1.1}`
	assertStringsEqual(t, expected, string(res))
	// try unmarshalling now
	var unc sw.NullableClass
	err = json.Unmarshal(res, &unc)
	if err != nil {
		t.Errorf("Error while unmarshalling structure with Nullables: %v", err)
	}
	if unc.BooleanProp.IsSet() || unc.BooleanProp.Get() != nil {
		t.Errorf("Unmarshalled BooleanProp not empty+unset: %+v", unc.BooleanProp)
	}
	if !unc.IntegerProp.IsSet() || unc.IntegerProp.Get() != nil {
		t.Errorf("Unmarshalled IntegerProp not explicit null: %+v", unc.IntegerProp)
	}
	if !unc.NumberProp.IsSet() || *unc.NumberProp.Get() != fv {
		t.Errorf("Unmarshalled NumberProp not set to value 1.1: %+v", unc.NumberProp)
	}

	// change the values a bit to make sure the Set/Unset methods work correctly
	nc.IntegerProp.Unset()
	bv := false
	nc.BooleanProp.Set(&bv)
	res, err = json.Marshal(nc)
	if err != nil {
		t.Errorf("Error while marshalling structure with Nullables: %v", err)
	}
	expected = `{"boolean_prop":false,"number_prop":1.1}`
	assertStringsEqual(t, expected, string(res))
	// try unmarshalling now
	var unc2 sw.NullableClass
	err = json.Unmarshal(res, &unc2)
	if err != nil {
		t.Errorf("Error while unmarshalling structure with Nullables: %v", err)
	}
	if !unc2.BooleanProp.IsSet() || *unc2.BooleanProp.Get() != false {
		t.Errorf("Unmarshalled BooleanProp not empty+unset: %+v", unc2.BooleanProp)
	}
	if unc2.IntegerProp.IsSet() || unc2.IntegerProp.Get() != nil {
		t.Errorf("Unmarshalled IntegerProp not explicit null: %+v", unc2.IntegerProp)
	}
	if !unc.NumberProp.IsSet() || *unc.NumberProp.Get() != fv {
		t.Errorf("Unmarshalled NumberProp not set to value 1.1: %+v", unc.NumberProp)
	}
}

func assertStringsEqual(t *testing.T, expected, actual string) {
	if expected != actual {
		t.Errorf(fmt.Sprintf("`%s` (expected) != `%s` (actual)", expected, actual))
	}
}
