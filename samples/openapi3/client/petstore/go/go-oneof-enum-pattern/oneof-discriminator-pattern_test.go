package openapi

import (
	"encoding/json"
	"testing"
)

func TestDiscriminatorNullablePattern(t *testing.T) {
	cases := []struct {
		input   string
		invalid bool
	}{
		{`{"kind":"file","path":"/ok"}`, false},
		{`{"kind":"file","path":null}`, false},
		{`{"kind":"file"}`, false},
		{`{"kind":"file","path":"bad"}`, true},
		{`{"kind":"folder"}`, false},
	}
	for _, c := range cases {
		t.Run(c.input, func(t *testing.T) {
			var dst DiscriminatedMetadata
			err := json.Unmarshal([]byte(c.input), &dst)
			if (err != nil) != c.invalid {
				t.Fatalf("error = %v", err)
			}
		})
	}
}
