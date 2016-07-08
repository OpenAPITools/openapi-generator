part of api;

const _delimiters = const {'csv': ',', 'ssv': ' ', 'tsv': '\t', 'pipes': '|'};

// port from Java version
List<QueryParam> _convertParametersForCollectionFormat(
  String collectionFormat, String name, dynamic value) {
  var params = [];

  // preconditions
  if (name == null || name.isEmpty || value == null) return params;

  if (value is! List) {
    params.add(new QueryParam(name, value as String));
    return params;
  }

  List<String> values = value as List<String>;

  // get the collection format
  collectionFormat = (collectionFormat == null || collectionFormat.isEmpty)
                     ? "csv"
                     : collectionFormat; // default: csv

  if (collectionFormat == "multi") {
    return values.map((v) => new QueryParam(name, v));
  }

  String delimiter = _delimiters[collectionFormat] ?? ",";

  params.add(new QueryParam(name, values.join(delimiter)));
  return params;
}
