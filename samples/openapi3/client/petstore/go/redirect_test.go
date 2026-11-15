package main

import (
	"context"
	"net/http"
	"net/http/cookiejar"
	"net/http/httptest"
	"net/url"
	"sync"
	"testing"

	sw "go-petstore"
)

// recorder is the redirect target: it keeps the headers and query of every
// request it receives and answers with an empty JSON object.
type recorder struct {
	mu   sync.Mutex
	seen map[string]*http.Request
}

func (rec *recorder) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	rec.mu.Lock()
	rec.seen[r.URL.Path] = r.Clone(context.Background())
	rec.mu.Unlock()
	w.Header().Set("Content-Type", "application/json")
	w.Write([]byte(`{}`))
}

func (rec *recorder) get(path string) *http.Request {
	rec.mu.Lock()
	defer rec.mu.Unlock()
	return rec.seen[path]
}

// redirecting answers every request with a 302 to target, keeping path and
// query, so anything the client puts on the request would reach the target.
func redirecting(target string) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		http.Redirect(w, r, target+"/landed"+r.URL.Path+"?"+r.URL.RawQuery, http.StatusFound)
	})
}

// Server A is addressed as localhost and server B as 127.0.0.1, both on
// loopback: for net/http that is a cross-host redirect, whereas two servers
// addressed by their URLs would both be 127.0.0.1 and nothing would be stripped.
func newClientAgainst(t *testing.T, serverURL string) *sw.APIClient {
	t.Helper()
	u, err := url.Parse(serverURL)
	if err != nil {
		t.Fatal(err)
	}
	cfg := sw.NewConfiguration()
	cfg.Servers = sw.ServerConfigurations{{URL: "http://localhost:" + u.Port()}}
	cfg.AddDefaultHeader("X-Default-Secret", "DEFAULT_MUST_NOT_LEAK")
	return sw.NewAPIClient(cfg)
}

func apiKeyContext() context.Context {
	return context.WithValue(context.Background(), sw.ContextAPIKeys, map[string]sw.APIKey{
		"api_key":       {Key: "HEADER_KEY_MUST_NOT_LEAK"},
		"api_key_query": {Key: "QUERY_KEY_MUST_NOT_LEAK"},
	})
}

func TestCredentialsDoNotFollowCrossOriginRedirect(t *testing.T) {
	rec := &recorder{seen: map[string]*http.Request{}}
	target := httptest.NewServer(rec)
	defer target.Close()
	origin := httptest.NewServer(redirecting(target.URL))
	defer origin.Close()

	client := newClientAgainst(t, origin.URL)
	ctx := apiKeyContext()

	// api_key travels in a header on this operation.
	client.PetAPI.GetPetById(ctx, 1).Execute()
	got := rec.get("/landed/pet/1")
	if got == nil {
		t.Fatal("the redirect target was not reached")
	}
	if v := got.Header.Get("api_key"); v != "" {
		t.Errorf("api_key header followed a cross-origin redirect: %q", v)
	}
	if v := got.Header.Get("X-Default-Secret"); v != "" {
		t.Errorf("default header followed a cross-origin redirect: %q", v)
	}
	if got.Header.Get("User-Agent") == "" {
		t.Errorf("non-credential headers must still follow the redirect")
	}

	// api_key_query travels in the query on this operation.
	client.FakeClassnameTags123API.TestClassname(ctx).Client(sw.Client{}).Execute()
	got = rec.get("/landed/fake_classname_test")
	if got == nil {
		t.Fatal("the redirect target was not reached")
	}
	if v := got.URL.Query().Get("api_key_query"); v != "" {
		t.Errorf("api_key_query followed a cross-origin redirect: %q", v)
	}
	// net/http names the previous URL, query string included, in Referer.
	if v := got.Header.Get("Referer"); v != "" {
		t.Errorf("Referer followed a cross-origin redirect: %q", v)
	}
}

func TestCredentialsFollowSameOriginRedirect(t *testing.T) {
	rec := &recorder{seen: map[string]*http.Request{}}
	mux := http.NewServeMux()
	mux.Handle("/landed/", rec)
	var self *httptest.Server
	mux.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		// redirect within the same origin the client was configured with
		u, _ := url.Parse(self.URL)
		http.Redirect(w, r, "http://localhost:"+u.Port()+"/landed"+r.URL.Path+"?"+r.URL.RawQuery, http.StatusFound)
	})
	self = httptest.NewServer(mux)
	defer self.Close()

	client := newClientAgainst(t, self.URL)
	ctx := apiKeyContext()

	client.PetAPI.GetPetById(ctx, 1).Execute()
	got := rec.get("/landed/pet/1")
	if got == nil {
		t.Fatal("the redirect target was not reached")
	}
	if v := got.Header.Get("api_key"); v != "HEADER_KEY_MUST_NOT_LEAK" {
		t.Errorf("api_key header must follow a same-origin redirect, got %q", v)
	}
	if v := got.Header.Get("X-Default-Secret"); v != "DEFAULT_MUST_NOT_LEAK" {
		t.Errorf("default header must follow a same-origin redirect, got %q", v)
	}

	client.FakeClassnameTags123API.TestClassname(ctx).Client(sw.Client{}).Execute()
	got = rec.get("/landed/fake_classname_test")
	if got == nil {
		t.Fatal("the redirect target was not reached")
	}
	if v := got.URL.Query().Get("api_key_query"); v != "QUERY_KEY_MUST_NOT_LEAK" {
		t.Errorf("api_key_query must follow a same-origin redirect, got %q", v)
	}
	if got.Header.Get("Referer") == "" {
		t.Errorf("Referer must follow a same-origin redirect")
	}
}

// A chain that leaves the origin and comes back must not regain the
// credentials: net/http rebuilds every redirected request from the original.
func TestCredentialsStayStrippedWhenChainReturnsToOrigin(t *testing.T) {
	rec := &recorder{seen: map[string]*http.Request{}}
	var origin, other *httptest.Server
	originMux := http.NewServeMux()
	originMux.Handle("/landed/", rec)
	originMux.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		http.Redirect(w, r, other.URL+"/bounce"+r.URL.Path+"?"+r.URL.RawQuery, http.StatusFound)
	})
	origin = httptest.NewServer(originMux)
	defer origin.Close()
	other = httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		u, _ := url.Parse(origin.URL)
		http.Redirect(w, r, "http://localhost:"+u.Port()+"/landed"+r.URL.Path[len("/bounce"):]+"?"+r.URL.RawQuery, http.StatusFound)
	}))
	defer other.Close()

	client := newClientAgainst(t, origin.URL)
	ctx := apiKeyContext()

	client.PetAPI.GetPetById(ctx, 1).Execute()
	got := rec.get("/landed/pet/1")
	if got == nil {
		t.Fatal("the chain did not come back to the origin")
	}
	if v := got.Header.Get("api_key"); v != "" {
		t.Errorf("api_key came back with the chain: %q", v)
	}
	if v := got.Header.Get("X-Default-Secret"); v != "" {
		t.Errorf("default header came back with the chain: %q", v)
	}

	client.FakeClassnameTags123API.TestClassname(ctx).Client(sw.Client{}).Execute()
	got = rec.get("/landed/fake_classname_test")
	if got == nil {
		t.Fatal("the chain did not come back to the origin")
	}
	if v := got.URL.Query().Get("api_key_query"); v != "" {
		t.Errorf("api_key_query came back with the chain: %q", v)
	}
}

// Only the credential leaves the query. The other segments reach the target
// byte for byte and in order, including one that url.ParseQuery would reject,
// so a signed URL is not re-encoded behind the caller's back.
func TestOnlyTheCredentialLeavesTheQuery(t *testing.T) {
	rec := &recorder{seen: map[string]*http.Request{}}
	target := httptest.NewServer(rec)
	defer target.Close()
	origin := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		// a Location whose query mixes the credential with segments that
		// url.Values.Encode would re-order, re-escape or drop
		http.Redirect(w, r, target.URL+"/landed"+r.URL.Path+"?z=%2F&"+r.URL.RawQuery+"&bad=%zz&flag", http.StatusFound)
	}))
	defer origin.Close()

	client := newClientAgainst(t, origin.URL)
	client.FakeClassnameTags123API.TestClassname(apiKeyContext()).Client(sw.Client{}).Execute()
	got := rec.get("/landed/fake_classname_test")
	if got == nil {
		t.Fatal("the redirect target was not reached")
	}
	if want := "z=%2F&bad=%zz&flag"; got.URL.RawQuery != want {
		t.Errorf("query reached the target as %q, want %q", got.URL.RawQuery, want)
	}
}

// A cookie jar set on http.DefaultClient is kept by the client built here.
// net/http consults the jar after the redirect policy ran, but a jar hands
// out only the cookies of the request's host, so the origin's cookies do not
// follow a redirect to another host.
func TestJarCookiesStayOnTheirHost(t *testing.T) {
	jar, err := cookiejar.New(nil)
	if err != nil {
		t.Fatal(err)
	}
	saved := http.DefaultClient.Jar
	http.DefaultClient.Jar = jar
	defer func() { http.DefaultClient.Jar = saved }()

	rec := &recorder{seen: map[string]*http.Request{}}
	target := httptest.NewServer(rec)
	defer target.Close()
	var mu sync.Mutex
	originSaw := ""
	origin := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		mu.Lock()
		originSaw = r.Header.Get("Cookie")
		mu.Unlock()
		http.Redirect(w, r, target.URL+"/landed"+r.URL.Path, http.StatusFound)
	}))
	defer origin.Close()

	u, _ := url.Parse(origin.URL)
	originURL, _ := url.Parse("http://localhost:" + u.Port())
	jar.SetCookies(originURL, []*http.Cookie{{Name: "session", Value: "JAR_COOKIE_MUST_NOT_LEAK"}})

	client := newClientAgainst(t, origin.URL)
	client.PetAPI.GetPetById(context.Background(), 1).Execute()

	mu.Lock()
	defer mu.Unlock()
	if originSaw != "session=JAR_COOKIE_MUST_NOT_LEAK" {
		t.Fatalf("the jar did not send its cookie to the origin, got %q", originSaw)
	}
	got := rec.get("/landed/pet/1")
	if got == nil {
		t.Fatal("the redirect target was not reached")
	}
	if v := got.Header.Get("Cookie"); v != "" {
		t.Errorf("jar cookie followed a cross-origin redirect: %q", v)
	}
}

func TestCustomHTTPClientKeepsItsOwnRedirectPolicy(t *testing.T) {
	cfg := sw.NewConfiguration()
	custom := &http.Client{CheckRedirect: func(*http.Request, []*http.Request) error {
		return http.ErrUseLastResponse
	}}
	cfg.HTTPClient = custom
	client := sw.NewAPIClient(cfg)
	if client.GetConfig().HTTPClient != custom {
		t.Fatal("a supplied http.Client must be used as given")
	}
}
