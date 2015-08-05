package io.swagger.codegen.languages;

public class CodeGenStatus {
  public enum Status {
    UNRUN, SUCCESSFUL, FAILED
  };

  private Status status;

  public CodeGenStatus(Status status) {
    this.status = status;
  }

  public Status getStatus() {
    return status;
  }

  public void setStatus(Status status) {
    this.status = status;
  }
}
