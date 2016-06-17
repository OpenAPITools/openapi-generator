package io.swagger.codegen.ignore.rules;

class Part {
    private final IgnoreLineParser.Token token;
    private final String value;

    public Part(IgnoreLineParser.Token token, String value) {
        this.token = token;
        this.value = value;
    }

    public Part(IgnoreLineParser.Token token) {
        this.token = token;
        this.value = token.getPattern();
    }

    public IgnoreLineParser.Token getToken() {
        return token;
    }

    public String getValue() {
        return value;
    }
}