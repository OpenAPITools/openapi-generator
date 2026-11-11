package headerrepro

import (
	"context"
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
)

func TestJSONHeaderASCII(t *testing.T) {
	cases := []struct{ name, path, want string }{
		{"ascii", `/plain`, `{"path":"/plain"}`},
		{"bmp", `/é`, `{"path":"/\u00e9"}`},
		{"supplementary", `/😀`, `{"path":"/\ud83d\ude00"}`},
		{"unicode separators", "/\u2028\u2029", `{"path":"/\u2028\u2029"}`},
		{"delete", "/\x7f", `{"path":"/\u007f"}`},
		{"escaping", "/a\\b\"\n", `{"path":"/a\\b\"\n"}`},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			value := NewHeaderArg()
			value.SetPath(tc.path)
			got, err := parameterToJSONHeaderValue(value)
			if err != nil {
				t.Fatal(err)
			}
			if got != tc.want {
				t.Errorf("got %q, want %q", got, tc.want)
			}
			var decoded map[string]string
			if err := json.Unmarshal([]byte(got), &decoded); err != nil {
				t.Fatal(err)
			}
			if decoded["path"] != tc.path {
				t.Errorf("round trip: got %q, want %q", decoded["path"], tc.path)
			}
			for _, c := range got {
				if c >= 0x7f {
					t.Errorf("header is not ASCII-safe: %q", got)
					break
				}
			}
		})
	}
	if got, err := parameterToJSONHeaderValue(nil); err != nil || got != "null" {
		t.Errorf("nil = %q, %v", got, err)
	}
	if _, err := parameterToJSONHeaderValue(make(chan int)); err == nil {
		t.Error("expected marshal error")
	}
}

func TestJSONHeaderASCIIOnWire(t *testing.T) {
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if got, want := r.Header.Get("X-Json-Arg"), `{"path":"/\u00e9\ud83d\ude00"}`; got != want {
			t.Errorf("header = %q, want %q", got, want)
		}
		if got := r.Header.Get("X-Vendor-Json"); !strings.Contains(got, `\u00e9`) {
			t.Errorf("vendor header = %q", got)
		}
		if got := r.Header.Get("X-Plain-Arg"); got != "optional" {
			t.Errorf("optional plain header = %q", got)
		}
		if got := r.Header.Get("X-Required-Plain"); got != "plain" {
			t.Errorf("plain header = %q", got)
		}
		w.WriteHeader(http.StatusNoContent)
	}))
	defer server.Close()
	cfg := NewConfiguration()
	cfg.Servers = ServerConfigurations{{URL: server.URL}}
	arg := NewHeaderArg()
	arg.SetPath("/é😀")
	response, err := NewAPIClient(cfg).DefaultAPI.TestJsonHeader(context.Background()).XJsonArg(*arg).XVendorJson(*arg).XRequiredPlain("plain").XPlainArg("optional").Execute()
	if err != nil {
		t.Fatal(err)
	}
	defer response.Body.Close()
	if response.StatusCode != http.StatusNoContent {
		t.Fatalf("status = %d", response.StatusCode)
	}
}
