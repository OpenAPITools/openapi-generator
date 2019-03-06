from yaml.constructor import SafeConstructor


class ExtendedSafeConstructor(SafeConstructor):

    def construct_mapping(self, node, deep=False):
        """While yaml supports integer keys, these are not valid in
        json, and will break jsonschema. This method coerces all keys
        to strings.
        """
        mapping = super(ExtendedSafeConstructor, self).construct_mapping(
            node, deep)

        return {
            (str(key) if isinstance(key, int) else key): mapping[key]
            for key in mapping
        }
