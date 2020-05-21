package org.openapitools.codegen.utils;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.security.SecurityScheme;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.CodegenSecurity;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class ProcessUtils {

    private static Boolean hasOAuthMethod;
    private static Boolean hasHttpBasicMethod;
    private static Boolean hasHttpSignatureMethod;
    private static Boolean hasHttpBearerMethod;
    private static Boolean hasApiKeyMethod;

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
     * Returns true if the specified OAS model has at least one operation with HTTP bearer authentication.
     *
     * @param openAPI An instance of OpenAPI
     * @return True if at least one operation has HTTP bearer security scheme defined
     */
    public static boolean hasHttpBearerMethods(OpenAPI openAPI) {
        if (hasHttpBearerMethod == null) {
            processAuthMethods(openAPI);
        }
        return Boolean.TRUE.equals(hasHttpBearerMethod);
    }

    /**
     * Returns true if the specified OAS model has at least one operation with OAuth authentication.
     *
     * @param openAPI An instance of OpenAPI
     * @return True if at least one operation has OAuth security scheme defined
     */
    public static boolean hasOAuthMethods(OpenAPI openAPI) {
        if (hasOAuthMethod == null) {
            processAuthMethods(openAPI);
        }
        return Boolean.TRUE.equals(hasOAuthMethod);
    }

    /**
     * Returns true if the specified OAS model has at least one operation with HTTP basic authentication.
     *
     * @param openAPI An instance of OpenAPI
     * @return True if at least one operation has HTTP basic security scheme defined
     */
    public static boolean hasHttpBasicMethods(OpenAPI openAPI) {
        if (hasHttpBasicMethod == null) {
            processAuthMethods(openAPI);
        }
        return Boolean.TRUE.equals(hasHttpBasicMethod);
    }

    /**
     * Returns true if the specified OAS model has at least one operation with HTTP signature authentication.
     *
     * @param openAPI An instance of OpenAPI
     * @return True if at least one operation has HTTP signature security scheme defined
     */
    public static boolean hasHttpSignatureMethods(OpenAPI openAPI) {
        if (hasHttpSignatureMethod == null) {
            processAuthMethods(openAPI);
        }
        return Boolean.TRUE.equals(hasHttpSignatureMethod);
    }

    /**
     * Returns true if the specified OAS model has at least one operation with API key authentication.
     *
     * @param openAPI An instance of OpenAPI
     * @return True if at least one operation has API key security scheme defined
     */
    public static boolean hasApiKeyMethods(OpenAPI openAPI) {
        if (hasApiKeyMethod == null) {
            processAuthMethods(openAPI);
        }
        return Boolean.TRUE.equals(hasApiKeyMethod);
    }

    /**
     * Update various static variables to indicate whether the spec contains certain security definitions.
     *
     * @param openAPI An instance of OpenAPI
     */
    private static void processAuthMethods(OpenAPI openAPI) {
        if (openAPI != null) {
            final Map<String, SecurityScheme> securitySchemes = openAPI.getComponents() != null ? openAPI.getComponents().getSecuritySchemes() : null;
            if (securitySchemes != null) {
                for (Map.Entry<String, SecurityScheme> scheme : securitySchemes.entrySet()) {
                    switch (scheme.getValue().getType()) {
                        case OAUTH2:
                            hasOAuthMethod = true;
                            break;
                        case HTTP:
                            if ("basic".equals(scheme.getValue().getScheme())) {
                                hasHttpBasicMethod = true;
                            } else if ("signature".equals(scheme.getValue().getScheme())) {
                                hasHttpSignatureMethod = true;
                            } else if ("bearer".equals(scheme.getValue().getScheme())) {
                                hasHttpBearerMethod = true;
                            } else {
                                throw new RuntimeException("Unknown HTTP security definition type: " + scheme.getValue().getScheme());
                            }
                            break;
                        case APIKEY:
                            hasApiKeyMethod = true;
                            break;
                        case OPENIDCONNECT:
                            throw new RuntimeException("OPENIDCONNECT security scheme not yet supported. Please report the issue to OpenAPI Generator.");
                        default:
                            throw new RuntimeException("Security scheme type not yet supported: " + scheme.getValue().getType());

                    }
                }
            }
        }
    }
}

