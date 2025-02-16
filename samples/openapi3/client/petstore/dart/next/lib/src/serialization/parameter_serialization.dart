import 'package:std_uritemplate/std_uritemplate.dart';

abstract class OpenApiParameterSerialization {
  final bool explode;
  final String parameterName;

  const OpenApiParameterSerialization({
    required this.parameterName,
    required this.explode,
  });

  String get appendExplode => explode ? '*' : '';

  String get oldParameterPlaceHolder => '{$parameterName}';
}

// in: path ------------------

abstract class OpenApiParameterSerializationPath
    extends OpenApiParameterSerialization {
  const OpenApiParameterSerializationPath({
    required super.explode,
    required super.parameterName,
  });

  String modifyTemplateToRFC6570(String template);

  String expand(String template, Object? value) {
    final modified = modifyTemplateToRFC6570(template);
    return StdUriTemplate.expand(modified, {
      parameterName: value,
    });
  }

  factory OpenApiParameterSerializationPath.fromStyle(String style, {
    required bool explode,
    required String parameterName,
  }) {
    return switch (style) {
      'simple' => OpenApiParameterSerializationPathSimple(parameterName: parameterName, explode: explode),
      'label' => OpenApiParameterSerializationPathLabel(parameterName: parameterName, explode: explode),
      'matrix' => OpenApiParameterSerializationPathMatrix(parameterName: parameterName, explode: explode),
      _ => throw UnimplementedError('Invalid style value for path.')
    };
  }
}

class OpenApiParameterSerializationPathSimple
    extends OpenApiParameterSerializationPath {
  const OpenApiParameterSerializationPathSimple({
    required super.explode,
    required super.parameterName,
  });

  @override
  String modifyTemplateToRFC6570(String template) {
    return template.replaceAll(
      oldParameterPlaceHolder,
      '{$parameterName$appendExplode}',
    );
  }
}

class OpenApiParameterSerializationPathLabel
    extends OpenApiParameterSerializationPath {
  const OpenApiParameterSerializationPathLabel({
    required super.explode,
    required super.parameterName,
  });

  @override
  String modifyTemplateToRFC6570(String template) {
    return template.replaceAll(
      oldParameterPlaceHolder,
      '{.$parameterName$appendExplode}',
    );
  }
}

class OpenApiParameterSerializationPathMatrix
    extends OpenApiParameterSerializationPath {
  const OpenApiParameterSerializationPathMatrix({
    required super.explode,
    required super.parameterName,
  });

  @override
  String modifyTemplateToRFC6570(String template) {
    return template.replaceAll(
      oldParameterPlaceHolder,
      '{;$parameterName$appendExplode}',
    );
  }
}

// in: query ------------------

abstract class OpenApiParameterSerializationQuery
    extends OpenApiParameterSerialization {
  const OpenApiParameterSerializationQuery({
    required super.explode,
    required super.parameterName,
    required this.allowEmptyValue,
  });

  factory OpenApiParameterSerializationQuery.fromStyle(String style, {
    required bool explode,
    required String parameterName,
    required bool allowEmptyValue,
  }) {
    return switch (style) {
      'form' => OpenApiParameterSerializationQueryForm(parameterName: parameterName, explode: explode, allowEmptyValue: allowEmptyValue),
      'spaceDelimited' => OpenApiParameterSerializationQuerySpaceDelimited(parameterName: parameterName, explode: explode, allowEmptyValue: allowEmptyValue),
      'pipeDelimited' => OpenApiParameterSerializationQueryPipeDelimited(parameterName: parameterName, explode: explode, allowEmptyValue: allowEmptyValue),
      'deepObject' => OpenApiParameterSerializationQueryDeepObject(parameterName: parameterName, explode: explode, allowEmptyValue: allowEmptyValue),
      _ => throw UnimplementedError('Invalid style value for path.')
    };
  }

  final bool allowEmptyValue;

  /// The templates passed here are valid [Uri]s missing query parameters
  Uri expandUri(Uri template, Object? value) {
    if ((value == null ||
            (value is String && value.isEmpty) ||
            (value is Iterable && value.isEmpty)) &&
        !allowEmptyValue) {
      return template;
    }

    final key = Uri.encodeComponent(parameterName);
    var expanded = StdUriTemplate.expand('{?$key$appendExplode}', {
      key: value,
    });
    if (expanded.startsWith('?')) {
      expanded = expanded.substring(1);
    }
    final uriWithExpanded = Uri(query: expanded);

    return template.replace(queryParameters: {
      ...template.queryParameters,
      ...uriWithExpanded.queryParameters,
    });
  }
}

// Default behavior is the same as [OpenApiParameterSerializationQuery].
class OpenApiParameterSerializationQueryForm
    extends OpenApiParameterSerializationQuery {
  const OpenApiParameterSerializationQueryForm({
    required super.explode,
    required super.parameterName,
    required super.allowEmptyValue,
  });
}

class OpenApiParameterSerializationQuerySpaceDelimited
    extends OpenApiParameterSerializationQuery {
  const OpenApiParameterSerializationQuerySpaceDelimited({
    required super.explode,
    required super.parameterName,
    required super.allowEmptyValue,
  });

  @override
  Uri expandUri(
    Uri template,
    Object? value,
  ) {
    if (value is Iterable) {
      value = value.join(' ');
    }

    return super.expandUri(template, value);
  }
}

class OpenApiParameterSerializationQueryPipeDelimited
    extends OpenApiParameterSerializationQuery {
  const OpenApiParameterSerializationQueryPipeDelimited({
    required super.explode,
    required super.parameterName,
    required super.allowEmptyValue,
  });

  @override
  Uri expandUri(
    Uri template,
    Object? value,
  ) {
    if (value is Iterable) {
      value = value.join('|');
    }

    return super.expandUri(template, value);
  }
}

class OpenApiParameterSerializationQueryDeepObject
    extends OpenApiParameterSerializationQuery {
  const OpenApiParameterSerializationQueryDeepObject({
    required super.explode,
    required super.parameterName,
    required super.allowEmptyValue,
  });

  @override
  Uri expandUri(
    Uri template,
    Object? value,
  ) {
    if (value is Map) {
      return template.replace(queryParameters: {
        ...template.queryParameters,
        ...value.map(
          (key, v) => MapEntry('$parameterName[$key]', v),
        ),
      });
    }
    return super.expandUri(template, value);
  }
}

// in: header ------------------

class OpenApiParameterSerializationHeader
    extends OpenApiParameterSerialization {
  const OpenApiParameterSerializationHeader({
    required super.explode,
    required super.parameterName,
  });

  //Style is always "simple"
  String serialize(Object? value) {
    return StdUriTemplate.expand('{v$appendExplode}', {
      'v': value,
    });
  }
}

// in: cookie ------------------

/// WARNING: this is not properly implemented.
/// see https://github.com/OAI/OpenAPI-Specification/issues/2940
class OpenApiParameterSerializationCookie
    extends OpenApiParameterSerialization {
  const OpenApiParameterSerializationCookie({
    required super.explode,
    required super.parameterName,
  });

  String serialize(Object? value) {
    return StdUriTemplate.expand('{$parameterName$appendExplode}', {
      parameterName: value,
    });
  }
}
