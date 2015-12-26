package io.swagger;

import java.io.*;
import java.util.*;

import io.swagger.client.*;
import io.swagger.client.api.*;
import io.swagger.client.model.*;

public class PetstoreProfiling {
    public int total = 5;
    public Long newPetId = 50003L;
    public String outputFile = "./petstore_profiling.output";

    public void callApis(int index, List<Map<String, String>> results) {
        long start;

        try {
            PetApi petApi = new PetApi();

            /* ADD PET */
            Pet pet = new Pet();
            pet.setId(newPetId);
            pet.setName("profiler");
            pet.setStatus(Pet.StatusEnum.AVAILABLE);
            pet.setPhotoUrls(Arrays.asList("http://profiler.com"));
            // new tag
            Tag tag = new Tag();
            tag.setId(newPetId); // use the same id as pet
            tag.setName("profile tag 1");
            // new category
            Category category = new Category();
            category.setId(newPetId); // use the same id as pet
            category.setName("profile category 1");

            pet.setTags(Arrays.asList(tag));
            pet.setCategory(category);

            /* ADD PET */
            start = System.nanoTime();
            petApi.addPet(pet);
            results.add(buildResult(index, "ADD PET", System.nanoTime() - start));

            /* GET PET */
            start = System.nanoTime();
            pet = petApi.getPetById(newPetId);
            results.add(buildResult(index, "GET PET", System.nanoTime() - start));

            /* UPDATE PET WITH FORM */
            start = System.nanoTime();
            petApi.updatePetWithForm(String.valueOf(newPetId), "new profiler", "sold");
            results.add(buildResult(index, "UPDATE PET", System.nanoTime() - start));

            /* DELETE PET */
            start = System.nanoTime();
            petApi.deletePet(newPetId, "special-key");
            results.add(buildResult(index, "DELETE PET", System.nanoTime() - start));
        } catch (ApiException e) {
            System.out.println("Caught error: " + e.getMessage());
            System.out.println("HTTP response headers: " + e.getResponseHeaders());
            System.out.println("HTTP response body: " + e.getResponseBody());
            System.out.println("HTTP status code: " + e.getCode());
        }
    }

    public void run() {
        System.out.printf("Running profiling... (total: %s)\n", total);

        List<Map<String, String>> results = new ArrayList<Map<String, String>>();
        for (int i = 0; i < total; i++) {
            callApis(i, results);
        }
        writeResultsToFile(results);

        System.out.printf("Profiling results written to %s\n", outputFile);
    }

    private Map<String, String> buildResult(int index, String name, long time) {
        Map<String, String> result = new HashMap<String, String>();
        result.put("index", String.valueOf(index));
        result.put("name", name);
        result.put("time", String.valueOf(time / 1000000000.0));
        return result;
    }

    private void writeResultsToFile(List<Map<String, String>> results) {
        try {
            File file = new File(outputFile);
            PrintWriter writer = new PrintWriter(file);
            String command = "mvn compile test-compile exec:java -Dexec.classpathScope=test -Dexec.mainClass=\"io.swagger.PetstoreProfiling\"";
            writer.println("# To run the profiling:");
            writer.printf("#   %s\n\n", command);
            for (Map<String, String> result : results) {
                writer.printf("%s: %s => %s\n", result.get("index"), result.get("name"), result.get("time"));
            }
            writer.close();
        } catch (FileNotFoundException e) {
            throw new RuntimeException(e);
        }
    }

    public static void main(String[] args) {
        final PetstoreProfiling profiling = new PetstoreProfiling();
        profiling.run();
    }
}
