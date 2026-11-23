package openapi

import (
	"encoding/json"
	"testing"
)

func TestOneOfEnumAndNullablePattern(t *testing.T) {
	tests := []struct {
		name, input, want string
		invalid           bool
	}{
		{"file", `{"kind":"file","path":"/x"}`, "file", false},
		{"folder", `{"kind":"folder"}`, "folder", false},
		{"null path", `{"kind":"file","path":null}`, "file", false},
		{"missing path", `{"kind":"file"}`, "file", false},
		{"invalid path", `{"kind":"file","path":"bad"}`, "", true},
		{"invalid enum", `{"kind":"other"}`, "", true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			var got Metadata
			err := json.Unmarshal([]byte(tt.input), &got)
			if (err != nil) != tt.invalid {
				t.Fatalf("error = %v", err)
			}
			if err != nil {
				return
			}
			if (got.File != nil) != (tt.want == "file") || (got.Folder != nil) != (tt.want == "folder") {
				t.Fatalf("wrong variant: %#v", got)
			}
		})
	}
}

func TestOneOfNestedNullablePattern(t *testing.T) {
	for _, tc := range []struct {
		input   string
		invalid bool
	}{
		{`{"child":{"path":"/ok"}}`, false},
		{`{"child":{"path":null}}`, false},
		{`{"child":{"path":"bad"}}`, true},
	} {
		t.Run(tc.input, func(t *testing.T) {
			var got NestedUnion
			err := json.Unmarshal([]byte(tc.input), &got)
			if (err != nil) != tc.invalid {
				t.Fatalf("error = %v", err)
			}
		})
	}
}
