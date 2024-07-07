import 'package:petstore_api/_internal.dart';

class PropertyEncodingRule {
    final String? style;
    final bool? explode;
    final bool? allowReserved;
    final MediaType? contentType;
    final Map<String, String> headers;

    const PropertyEncodingRule({
        this.style,
        this.explode,
        this.allowReserved,
        this.contentType,
        this.headers = const {}
    });
}