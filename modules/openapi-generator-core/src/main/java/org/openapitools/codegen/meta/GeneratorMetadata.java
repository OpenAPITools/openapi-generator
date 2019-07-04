package org.openapitools.codegen.meta;

/**
 * Represents metadata about a generator.
 */
public class GeneratorMetadata {
    private Stability stability;
    private String generationMessage;

    private GeneratorMetadata(Builder builder) {
        stability = builder.stability;
        generationMessage = builder.generationMessage;
    }

    /**
     * Creates a new builder object for {@link GeneratorMetadata}.
     *
     * @return A new builder instance.
     */
    public static Builder newBuilder() {
        return new Builder();
    }

    /**
     * Creates a new builder object for {@link GeneratorMetadata}, accepting another instance from which to copy properties.
     *
     * @param copy An existing instance to copy defaults from
     *
     * @return A new builder instance, with values preset to those of 'copy'.
     */
    public static Builder newBuilder(GeneratorMetadata copy) {
        Builder builder = new Builder();
        if (copy != null) {
            builder.stability = copy.getStability();
            builder.generationMessage = copy.getGenerationMessage();
        }
        return builder;
    }

    /**
     * Returns a message which can be displayed during generation.
     *
     * @return A message, if defined.
     */
    public String getGenerationMessage() {
        return generationMessage;
    }

    /**
     * Returns an enum describing the stability index of the generator.
     *
     * @return The defined stability index.
     */
    public Stability getStability() {
        return stability;
    }

    /**
     * {@code GeneratorMetadata} builder static inner class.
     */
    public static final class Builder {
        private Stability stability;
        private String generationMessage;

        private Builder() {
        }

        /**
         * Sets the {@code stability} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param stability the {@code stability} to set
         * @return a reference to this Builder
         */
        public Builder stability(Stability stability) {
            this.stability = stability;
            return this;
        }

        /**
         * Sets the {@code generationMessage} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param generationMessage the {@code generationMessage} to set
         * @return a reference to this Builder
         */
        public Builder generationMessage(String generationMessage) {
            this.generationMessage = generationMessage;
            return this;
        }

        /**
         * Returns a {@code GeneratorMetadata} built from the parameters previously set.
         *
         * @return a {@code GeneratorMetadata} built with parameters of this {@code GeneratorMetadata.Builder}
         */
        public GeneratorMetadata build() {
            return new GeneratorMetadata(this);
        }
    }
}
