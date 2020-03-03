/**
 * Filler File. Handling responses are usually similar across the application. This is where the
 * business logic should go, instead of in each operation separately.
 * @param error
 * @param code
 * @returns {{code: number, error: *}}
 */

const rejectResponse = (error, code = 500) => ({ error, code });

const successResponse = (payload, code = 200) => ({ payload, code });

module.exports = {
  rejectResponse,
  successResponse,
};
