package com.example.mapped;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.validation.constraints.Pattern;
import org.springframework.lang.Nullable;

/**
 * Handwritten production model used through the Category schema mapping.
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class Category {
    private @Nullable Long id;
    private @Nullable String name;

    public Category() {
    }

    public Category(@Nullable Long id, @Nullable String name) {
        this.id = id;
        this.name = name;
    }

    @JsonProperty("id")
    public @Nullable Long getId() {
        return id;
    }

    public void setId(@Nullable Long id) {
        this.id = id;
    }

    @JsonProperty("name")
    @Pattern(regexp = "^[a-zA-Z0-9]+[a-zA-Z0-9\\.\\-_]*[a-zA-Z0-9]+$")
    public @Nullable String getName() {
        return name;
    }

    public void setName(@Nullable String name) {
        this.name = name;
    }
}
