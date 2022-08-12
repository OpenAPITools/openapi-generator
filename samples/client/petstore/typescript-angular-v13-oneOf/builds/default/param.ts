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
export type ExtendedParamStyle = ParamStyle | string;

/**
 * The parameter to encode.
 */
export interface Param {
  name: string;
  value: unknown;
  in: ParamLocation;
  style: ExtendedParamStyle,
  explode: boolean;
}
