import 'package:petstore_api/_internal.dart';

var $examplesRandom = Random(123);

class ExampleContext {
  ExampleContext({
    Random? random,
    AggregatedDiscriminatorsResult? discriminators,
    Map<String, Uint8List>? fileCache,
    Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ModelReflection>>?
        discriminatorExampleResults,
  })  : discriminators = discriminators ?? {},
        fileCache = fileCache ?? {},
        discriminatorExampleResults = discriminatorExampleResults ?? {},
        random = random ?? $examplesRandom;

  final Random random;
  final AggregatedDiscriminatorsResult discriminators;
  final Map<String, Uint8List> fileCache;
  final Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ModelReflection>>
      discriminatorExampleResults;

  T exampleEnum<T>(List<T> values) {
    return values.elementAt(random.nextInt(values.length));
  }

  int exampleCode(int min, int max) {
    return random.nextInt(max - min) + min;
  }

  String exampleMimeType(String pattern) {
    return MediaType.parse(pattern).fillDefaults().toString();
  }

  MapEntry<DiscriminatorValue, ModelReflection>? exampleDiscriminator(
    Map<DiscriminatorValue, ModelReflection<dynamic>> discriminatorMappings,
  ) {
    if (discriminatorMappings.isEmpty) {
      return null;
    }
    final pickResult = random.nextInt(discriminatorMappings.length);
    return discriminatorMappings.entries.elementAt(pickResult);
  }
}
