package org.openapitools.codegen.utils;

import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.CodegenSecurity;

import java.util.List;
import java.util.Map;

public class ProcessUtils {

    /**
     * Add x-index extension to the model's properties
     *
     * @param models List of models
     */
    public static void addIndexToProperties(List<Object> models) {
        for (Object _mo : models) {
            Map<String, Object> mo = (Map<String, Object>) _mo;
            CodegenModel cm = (CodegenModel) mo.get("model");

            int i = 0;
            for (CodegenProperty var : cm.vars) {
                var.vendorExtensions.put("x-index", i);
                i++;
            }

            int j = 0;
            for (CodegenProperty var : cm.allVars) {
                var.vendorExtensions.put("x-index", j);
                j++;
            }

        }

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
                        if (cs.isOAuth) {
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
    public static boolean hasBearerMethods(Map<String, Object> objs) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        if (operations != null) {
            List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
            for (CodegenOperation operation : ops) {
                if (operation.authMethods != null && !operation.authMethods.isEmpty()) {
                    for (CodegenSecurity cs : operation.authMethods) {
                        if (cs.isBasicBearer) {
                            return true;
                        }
                    }
                }
            }
        }

        return false;
    }

}
