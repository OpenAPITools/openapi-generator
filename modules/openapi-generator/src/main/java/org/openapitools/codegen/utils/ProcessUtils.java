package org.openapitools.codegen.utils;

import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.CodegenSecurity;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class ProcessUtils {

    /**
     * Add x-index extension to the model's properties
     *
     * @param models       List of models
     * @param initialIndex starting index to use
     */
    public static void addIndexToProperties(List<Object> models, int initialIndex) {
        for (Object _mo : models) {
            Map<String, Object> mo = (Map<String, Object>) _mo;
            CodegenModel cm = (CodegenModel) mo.get("model");

            int i = initialIndex;
            for (CodegenProperty var : cm.vars) {
                var.vendorExtensions.put("x-index", i);
                i++;
            }

            int j = initialIndex;
            for (CodegenProperty var : cm.allVars) {
                var.vendorExtensions.put("x-index", j);
                j++;
            }

        }

    }

    /**
     * Add x-index extension to the model's properties
     *
     * @param models List of models
     */
    public static void addIndexToProperties(List<Object> models) {
        addIndexToProperties(models, 0);
    }


    /**
     * Returns true if the specified OAS model has at least one operation with the HTTP basic
     * security scheme.
     *
     * @param authMethods List of auth methods.
     * @return True if at least one operation has HTTP basic security scheme defined
     */
    public static boolean hasHttpBasicMethods(List<CodegenSecurity> authMethods) {
        if (authMethods != null && !authMethods.isEmpty()) {
            for (CodegenSecurity cs : authMethods) {
                if (Boolean.TRUE.equals(cs.isBasicBasic)) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Returns true if the specified OAS model has at least one operation with API keys.
     *
     * @param authMethods List of auth methods.
     * @return True if at least one operation has API key security scheme defined
     */
    public static boolean hasApiKeyMethods(List<CodegenSecurity> authMethods) {
        if (authMethods != null && !authMethods.isEmpty()) {
            for (CodegenSecurity cs : authMethods) {
                if (Boolean.TRUE.equals(cs.isApiKey)) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Returns true if the specified OAS model has at least one operation with the HTTP signature
     * security scheme.
     * The HTTP signature scheme is defined in https://datatracker.ietf.org/doc/draft-cavage-http-signatures/
     *
     * @param authMethods List of auth methods.
     * @return True if at least one operation has HTTP signature security scheme defined
     */
    public static boolean hasHttpSignatureMethods(List<CodegenSecurity> authMethods) {
        if (authMethods != null && !authMethods.isEmpty()) {
            for (CodegenSecurity cs : authMethods) {
                if (Boolean.TRUE.equals(cs.isHttpSignature)) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Returns true if the specified OAS model has at least one operation with OAuth.
     *
     * @param authMethods List of auth methods.
     * @return True if at least one operation has OAuth security scheme defined
     */
    public static boolean hasOAuthMethods(List<CodegenSecurity> authMethods) {
        for (CodegenSecurity cs : authMethods) {
            if (Boolean.TRUE.equals(cs.isOAuth)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Returns a list of OAuth Codegen security objects
     *
     * @param authMethods List of auth methods.
     * @return A list of OAuth Codegen security objects
     */
    public static List<CodegenSecurity> getOAuthMethods(List<CodegenSecurity> authMethods) {
        List<CodegenSecurity> oauthMethods = new ArrayList<>();

        for (CodegenSecurity cs : authMethods) {
            if (Boolean.TRUE.equals(cs.isOAuth)) {
                oauthMethods.add(cs);
            }
        }

        return oauthMethods;
    }

    /**
     * Returns true if the specified OAS model has at least one operation with HTTP bearer.
     *
     * @param authMethods List of auth methods.
     * @return True if at least one operation has HTTP bearer security scheme defined
     */
    public static boolean hasHttpBearerMethods(List<CodegenSecurity> authMethods) {
        if (authMethods != null && !authMethods.isEmpty()) {
            for (CodegenSecurity cs : authMethods) {
                if (Boolean.TRUE.equals(cs.isBasicBasic)) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Returns true if at least one operation has OAuth security schema defined
     *
     * @param objs Map of operations
     * @return True if at least one operation has OAuth security schema defined
     */
    public static boolean hasOAuthMethods(Map<String, Object> objs) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        if (operations != null) {
            List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
            for (CodegenOperation operation : ops) {
                if (operation.authMethods != null && !operation.authMethods.isEmpty()) {
                    for (CodegenSecurity cs : operation.authMethods) {
                        if (Boolean.TRUE.equals(cs.isOAuth)) {

                            return true;
                        }
                    }
                }
            }
        }

        return false;
    }

    /**
     * Returns true if at least one operation has Bearer security schema defined
     *
     * @param objs Map of operations
     * @return True if at least one operation has Bearer security schema defined
     */
    public static boolean hasHttpBearerMethods(Map<String, Object> objs) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        if (operations != null) {
            List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
            for (CodegenOperation operation : ops) {
                if (operation.authMethods != null && !operation.authMethods.isEmpty()) {
                    for (CodegenSecurity cs : operation.authMethods) {
                        if (Boolean.TRUE.equals(cs.isBasicBearer)) {
                            return true;
                        }
                    }
                }
            }
        }
        return false;
    }

    /**
     * Returns true if at least one operation has HTTP basic security schema defined
     *
     * @param objs Map of operations
     * @return True if at least one operation has HTTP basic security schema defined
     */
    public static boolean hasHttpBasicMethods(Map<String, Object> objs) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        if (operations != null) {
            List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
            for (CodegenOperation operation : ops) {
                if (operation.authMethods != null && !operation.authMethods.isEmpty()) {
                    for (CodegenSecurity cs : operation.authMethods) {
                        if (Boolean.TRUE.equals(cs.isBasicBearer)) {
                            return true;
                        }
                    }
                }
            }
        }
        return false;
    }

    /**
     * Returns true if at least one operation has HTTP signature security schema defined
     *
     * @param objs Map of operations
     * @return True if at least one operation has HTTP signature security schema defined
     */
    public static boolean hasHttpSignatureMethods(Map<String, Object> objs) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        if (operations != null) {
            List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
            for (CodegenOperation operation : ops) {
                if (operation.authMethods != null && !operation.authMethods.isEmpty()) {
                    for (CodegenSecurity cs : operation.authMethods) {
                        if (Boolean.TRUE.equals(cs.isHttpSignature)) {
                            return true;
                        }
                    }
                }
            }
        }
        return false;
    }

    /**
     * Returns true if at least one operation has API key security schema defined
     *
     * @param objs Map of operations
     * @return True if at least one operation has API key security schema defined
     */
    public static boolean hasApiKeyAuthMethods(Map<String, Object> objs) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        if (operations != null) {
            List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
            for (CodegenOperation operation : ops) {
                if (operation.authMethods != null && !operation.authMethods.isEmpty()) {
                    for (CodegenSecurity cs : operation.authMethods) {
                        if (Boolean.TRUE.equals(cs.isApiKey)) {
                            return true;
                        }
                    }
                }
            }
        }
        return false;
    }
}
