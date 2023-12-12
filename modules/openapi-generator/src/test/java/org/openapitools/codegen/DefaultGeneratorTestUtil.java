package org.openapitools.codegen;

import org.openapitools.codegen.model.ModelMap;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class DefaultGeneratorTestUtil {

    /**
     * To facilitate access to package-private generateModels() in test code
     *
     * @return
     */
    public static Map<String, CodegenModel> generateModels(DefaultGenerator defaultGenerator)
    {
        defaultGenerator.configureGeneratorProperties();

        List<ModelMap> modelMapList = new LinkedList<>();
        defaultGenerator.generateModels(new LinkedList<>(), modelMapList, new LinkedList<>());
        return modelMapList.stream().collect(Collectors.toMap(modelMap -> modelMap.getModel().name, modelMap -> modelMap.getModel()));
    }
}
