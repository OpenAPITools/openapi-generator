package org.openapitools.codegen.utils;

import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenProperty;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * This class holds data to add to `oneOf` members. Let's consider this example:
 *
 * Foo:
 *   properties:
 *     x:
 *       oneOf:
 *         - $ref: "#/components/schemas/One
 *         - $ref: "#/components/schemas/Two
 *     y:
 *       type: string
 * One:
 *   properties:
 *     z:
 *       type: string
 * Two:
 *   properties:
 *     a:
 *       type: string
 *
 * In codegens that use this mechanism, `Foo` will become an interface and `One` will
 * become its implementing class. This class carries all data necessary to properly modify
 * the implementing class model. Specifically:
 *
 * * Interfaces that the implementing classes have to implement (in the example above, `One` and `Two` will implement `Foo`)
 * * Properties that need to be added to implementing classes (as `Foo` is interface, the `y` property will get pushed
 *   to implementing classes `One` and `Two`)
 * * Imports that need to be added to implementing classes (e.g. if type of property `y` needs a specific import, it
 *   needs to be added to `One` and `Two` because of the above point)
 */
public class OneOfImplementorAdditionalData {
    private String implementorName;
    private List<String> additionalInterfaces = new ArrayList<String>();
    private List<CodegenProperty> additionalProps = new ArrayList<CodegenProperty>();
    private List<Map<String, String>> additionalImports = new ArrayList<Map<String, String>>();
    private final Logger LOGGER = LoggerFactory.getLogger(OneOfImplementorAdditionalData.class);

    public OneOfImplementorAdditionalData(String implementorName) {
        this.implementorName = implementorName;
    }

    public String getImplementorName() {
        return implementorName;
    }

    /**
     * Add data from a given CodegenModel that the oneOf implementor should implement. For example:
     *
     * @param cm model that the implementor should implement
     * @param modelsImports imports of the given `cm`
     */
    public void addFromInterfaceModel(CodegenModel cm, List<Map<String, String>> modelsImports) {
        // Add cm as implemented interface
        additionalInterfaces.add(cm.classname);

        // Add all vars defined on cm
        // a "oneOf" model (cm) by default inherits all properties from its "interfaceModels",
        // but we only want to add properties defined on cm itself
        List<CodegenProperty> toAdd = new ArrayList<CodegenProperty>(cm.vars);
        // note that we can't just toAdd.removeAll(m.vars) for every interfaceModel,
        // as they might have different value of `hasMore` and thus are not equal
        List<String> omitAdding = new ArrayList<String>();
        if (cm.interfaceModels != null) {
            for (CodegenModel m : cm.interfaceModels) {
                for (CodegenProperty v : m.vars) {
                    omitAdding.add(v.baseName);
                }
            }
        }
        for (CodegenProperty v : toAdd) {
            if (!omitAdding.contains(v.baseName)) {
                additionalProps.add(v.clone());
            }
        }

        // Add all imports of cm
        for (Map<String, String> importMap : modelsImports) {
            // we're ok with shallow clone here, because imports are strings only
            additionalImports.add(new HashMap<String, String>(importMap));
        }
    }

    /**
     * Adds stored data to given implementing model
     *
     * @param cc CodegenConfig running this operation
     * @param implcm the implementing model
     * @param implImports imports of the implementing model
     * @param addInterfaceImports whether or not to add the interface model as import (will vary by language)
     */
    @SuppressWarnings("unchecked")
    public void addToImplementor(CodegenConfig cc, CodegenModel implcm, List<Map<String, String>> implImports, boolean addInterfaceImports) {
        implcm.getVendorExtensions().putIfAbsent("x-implements", new ArrayList<String>());

        // Add implemented interfaces
        for (String intf : additionalInterfaces) {
            List<String> impl = (List<String>) implcm.getVendorExtensions().get("x-implements");
            impl.add(intf);
            if (addInterfaceImports) {
                // Add imports for interfaces
                implcm.imports.add(intf);
                Map<String, String> importsItem = new HashMap<String, String>();
                importsItem.put("import", cc.toModelImport(intf));
                implImports.add(importsItem);
            }
        }

        // Add oneOf-containing models properties - we need to properly set the hasMore values to make rendering correct
        implcm.vars.addAll(additionalProps);

        // Add imports
        for (Map<String, String> oneImport : additionalImports) {
            // exclude imports from this package - these are imports that only the oneOf interface needs
            if (!implImports.contains(oneImport) && !oneImport.getOrDefault("import", "").startsWith(cc.modelPackage())) {
                implImports.add(oneImport);
            }
        }
    }

    @Override
    public String toString() {
        return "OneOfImplementorAdditionalData{" +
                "implementorName='" + implementorName + '\'' +
                ", additionalInterfaces=" + additionalInterfaces +
                ", additionalProps=" + additionalProps +
                ", additionalImports=" + additionalImports +
                '}';
    }
}
