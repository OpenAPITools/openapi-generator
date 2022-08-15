// trick: make autocomplete kinda work for LiteralType in most IDEs:
// usually shows entries of LiteralType first in autocomplete.
type LiteralUnion<LiteralType, BaseType extends string>
    = LiteralType | (BaseType & Record<never, never>);

/**
 * Standard parameter styles defined by OpenAPI spec
 */
export type ParamStyle =
  | 'matrix'
  | 'label'
  | 'form'
  | 'simple'
  | 'spaceDelimited'
  | 'pipeDelimited'
  | 'deepObject'
  ;

/**
 * Convenience: simulate ParameterStyle enum
 */
export const ParamStyle = {
  matrix: 'matrix' as ParamStyle,
  label: 'label' as ParamStyle,
  form: 'form' as ParamStyle,
  simple: 'simple' as ParamStyle,
  spaceDelimited: 'spaceDelimited' as ParamStyle,
  pipeDelimited: 'pipeDelimited' as ParamStyle,
  deepObject: 'deepObject' as ParamStyle,
} as const;

/**
 * Standard parameter locations defined by OpenAPI spec
 */
export type ParamLocation = 'query' | 'header' | 'path' | 'cookie';

/**
 * Convenience: simulate ParameterLocation enum
 */
export const ParamLocation = {
  query: 'query' as ParamLocation,
  header: 'header' as ParamLocation,
  path: 'path' as ParamLocation,
  cookie: 'cookie' as ParamLocation,
} as const;

/**
 * The OpenAPI standard styles may be extended by custom styles by the user.
 */
export type ExtendedParamStyle = LiteralUnion<ParamStyle, string>;

/**
 * Standard types as defined in <a href="https://swagger.io/specification/#data-types">OpenAPI Specification: Data Types</a>
 */
export type StandardDataType =
  | "integer"
  | "number"
  | "boolean"
  | "string"
  | "object"
  | "array"
  ;

export type DataType = LiteralUnion<StandardDataType, string>;

/**
 * Standard formats as defined in <a href="https://swagger.io/specification/#data-types">OpenAPI Specification: Data Types</a>
 */
export type StandardDataFormat =
  | "int32"
  | "int64"
  | "float"
  | "double"
  | "byte"
  | "binary"
  | "date"
  | "date-time"
  | "password"
  ;

export type DataFormat = LiteralUnion<StandardDataFormat, string>;

/**
 * The parameter to encode.
 */
export interface Param {
  name: string;
  value: unknown;
  in: ParamLocation;
  style: ExtendedParamStyle,
  explode: boolean;
  dataType: DataType;
  dataFormat: DataFormat | undefined;
}
