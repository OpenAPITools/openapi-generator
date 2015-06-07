package io.swagger.codegen;

public class CliOption {
    private final String opt;
    private String description;

    public CliOption(String opt, String description) {
        this.opt = opt;
        this.description = description;
    }

    public String getOpt() {
        return opt;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }
}
