package org.openapitools.codegen.templating;

import java.util.*;

import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenSecurity;

public class OperationsBundle extends BaseBundle{

    private List<CodegenOperation> operation = new ArrayList<>();
    private String className;
    private String pathPrefix;
    private String controllerName;
    private String symfonyService;
    private Set<CodegenSecurity> authMethods = new HashSet<>();
    private String classnameSnakeUpperCase;
    private String classnameSnakeLowerCase;
    private String apiFilename;
    private String interfacesToImplement;
    private String pathPattern;
    private List<CodegenOperationBundle> operationsByPath = new ArrayList<>();

    // getters and setters. Each setter puts the value in the underlying Map

    public String getPathPrefix() {
        return pathPrefix;
    }

    public void setPathPrefix(String pathPrefix) {
        this.pathPrefix = pathPrefix;
        put("pathPrefix", pathPrefix);
    }

    public List<CodegenOperation> getOperation() {
        return this.operation;
    }

    public void setOperation(List<CodegenOperation> operation) {
        this.operation = operation;
        put("operation", operation);
    }


    public String getClassname() {
        return className;
    }

    public void setClassname(String className) {
        this.className = className;
        put("className", className);
    }


    public String getControllerName() {
        return controllerName;
    }

    public void setControllerName(String controllerName) {
        this.controllerName = controllerName;
        put("controllerName", controllerName);
    }


    public Set<CodegenSecurity> getAuthMethods() {
        return authMethods;
    }

    public void setAuthMethods(Set<CodegenSecurity> authMethods) {
        this.authMethods = authMethods;
        put("authMethods", authMethods);
    }


    public String getClassnameSnakeUpperCase() {
        return classnameSnakeUpperCase;
    }

    public void setClassnameSnakeUpperCase(String classnameSnakeUpperCase) {
        this.classnameSnakeUpperCase = classnameSnakeUpperCase;
        put("classnameSnakeUpperCase", classnameSnakeUpperCase);
    }


    public String getClassnameSnakeLowerCase() {
        return classnameSnakeLowerCase;
    }

    public void setClassnameSnakeLowerCase(String classnameSnakeLowerCase) {
        this.classnameSnakeLowerCase = classnameSnakeLowerCase;
        put("classnameSnakeLowerCase", classnameSnakeLowerCase);
    }


    public String getApiFilename() {
        return apiFilename;
    }

    public void setApiFilename(String apiFilename) {
        this.apiFilename = apiFilename;
        put("apiFilename", apiFilename);
    }


    public String getInterfacesToImplement() {
        return interfacesToImplement;
    }

    public void setInterfacesToImplement(String interfacesToImplement) {
        this.interfacesToImplement = interfacesToImplement;
        put("interfacesToImplement", interfacesToImplement);
    }


    public String getPathPattern() {
        return pathPattern;
    }

    public void setPathPattern(String pathPattern) {
        this.pathPattern = pathPattern;
        put("pathPattern", pathPattern);
    }


    public List<CodegenOperationBundle> getOperationsByPath() {
        return operationsByPath;
    }

    public void setOperationsByPath(List<CodegenOperationBundle> operationsByPath) {
        this.operationsByPath = operationsByPath;
        put("operationsByPath", operationsByPath);
    }
}
