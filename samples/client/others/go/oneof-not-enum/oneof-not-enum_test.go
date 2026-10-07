package openapi

import (
	"encoding/json"
	"testing"
)

func TestStringEnumScope(t *testing.T) {
	cases := []struct {
		name    string
		model   interface{}
		payload string
		valid   bool
	}{
		{"Known accepts known", &Known{}, `{"kind":"known"}`, true},
		{"Known rejects other", &Known{}, `{"kind":"other"}`, false},
		{"Other rejects known", &Other{}, `{"kind":"known"}`, false},
		{"Other accepts other", &Other{}, `{"kind":"other"}`, true},
		{"Known accepts KIND", &Known{}, `{"KIND":"known"}`, true},
		{"Other rejects KIND", &Other{}, `{"KIND":"known"}`, false},
		{"ordinary enum accepts unknown", &OrdinaryEnum{}, `{"status":"future"}`, true},
	}
	for _, test := range cases {
		t.Run(test.name, func(t *testing.T) {
			err := json.Unmarshal([]byte(test.payload), test.model)
			if (err == nil) != test.valid {
				t.Errorf("UnmarshalJSON error = %v, valid = %v", err, test.valid)
			}
		})
	}

	var known TaggedUnion
	if err := json.Unmarshal([]byte(`{"kind":"known"}`), &known); err != nil || known.Known == nil || known.Other != nil {
		t.Errorf("known union variant: %+v, error: %v", known, err)
	}
	var other TaggedUnion
	if err := json.Unmarshal([]byte(`{"kind":"other"}`), &other); err != nil || other.Other == nil || other.Known != nil {
		t.Errorf("other union variant: %+v, error: %v", other, err)
	}
}
