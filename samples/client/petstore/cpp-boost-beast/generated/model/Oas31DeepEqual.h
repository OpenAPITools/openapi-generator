// ============================================================================
// Oas31DeepEqual.h - exact deep JSON equality.
//
// JSON Schema `const`, `enum`, and array `uniqueItems` all require DEEP
// structural equality of the ENTIRE JSON value, not a scalar shortcut. Per the
// ADR this must be EXACT and must never shortcut through Boost.JSON `double`:
// every JSON number is compared via ExactNumber so 1 == 1.0 == 1e0
// while 0.1000000000000000001 != 0.1. Objects compare by unordered key set;
// arrays compare positionally; null == null only.
//
// This header holds the self-contained stored-JSON-value comparator (both
// sides are real boost::json::value). The instance-aware wrapper
// `deepInstanceEqual` lives in Oas31Validator.h because the instance side
// carries the authoritative numeric LEXEME; both share ExactNumber so numeric
// equality is provably identical.
//
// HEADER-ONLY. Built under -Werror with g++ -std=c++17 -I/opt/homebrew/include.
// ============================================================================
#ifndef ORG_OPENAPITOOLS_CLIENT_MODEL_OAS31_DEEP_EQUAL_H_
#define ORG_OPENAPITOOLS_CLIENT_MODEL_OAS31_DEEP_EQUAL_H_

#include "Oas31ExactNumber.h"

#include <boost/json.hpp>

#include <cstddef>
#include <string>
#include <vector>

namespace org::openapitools::client::model::detail::schema_validation {

/// Convert a stored boost::json::value that IS a number into the EXACT
/// mathematical value. Prefers integer kinds exactly; degrades to the
/// documented (lossy-for-non-representable) double path only when the value was
/// itself only representable as double. Never used for a non-number.
inline ExactNumber exactValueOf(boost::json::value const& v) {
    if (v.is_int64()) return ExactNumber::fromInt(v.as_int64());
    if (v.is_uint64()) return ExactNumber::fromUint(v.as_uint64());
    if (v.is_double()) return ExactNumber::fromDouble(v.as_double());
    return ExactNumber();
}

/// Deep equality of two STORED JSON values. Numbers are compared exactly via
/// ExactNumber (1 == 1.0 == 1e0); objects unordered; arrays positional.
/// Returns false for any kind mismatch.
inline bool deepJsonValueEqual(boost::json::value const& a, boost::json::value const& b) {
    boost::json::kind const ka = a.kind();
    boost::json::kind const kb = b.kind();
    if (ka != kb) {
        // number kinds and null/bool/string are exact kinds; only special-case
        // nothing here: int64/uint64/double are all Json "numbers" but a
        // boost::json::value distinguishes them, so map them to a common kind.
        if ((ka == boost::json::kind::int64 || ka == boost::json::kind::uint64 ||
             ka == boost::json::kind::double_) &&
            (kb == boost::json::kind::int64 || kb == boost::json::kind::uint64 ||
             kb == boost::json::kind::double_)) {
            return exactValueOf(a) == exactValueOf(b);
        }
        return false;
    }
    switch (ka) {
        case boost::json::kind::null:      return true;
        case boost::json::kind::bool_:     return a.as_bool() == b.as_bool();
        case boost::json::kind::int64:
        case boost::json::kind::uint64:
        case boost::json::kind::double_:   return exactValueOf(a) == exactValueOf(b);
        case boost::json::kind::string:    return a.as_string() == b.as_string();
        case boost::json::kind::array: {
            boost::json::array const& aa = a.as_array();
            boost::json::array const& bb = b.as_array();
            if (aa.size() != bb.size()) return false;
            for (std::size_t i = 0; i < aa.size(); ++i)
                if (!deepJsonValueEqual(aa[i], bb[i])) return false;
            return true;
        }
        case boost::json::kind::object: {
            boost::json::object const& oa = a.as_object();
            boost::json::object const& ob = b.as_object();
            if (oa.size() != ob.size()) return false;
            for (auto const& kv : oa) {
                auto it = ob.find(kv.key());
                if (it == ob.end()) return false;
                if (!deepJsonValueEqual(kv.value(), it->value())) return false;
            }
            return true;
        }
    }
    return false;
}

} // namespace org::openapitools::client::model::detail::schema_validation

#endif // ORG_OPENAPITOOLS_CLIENT_MODEL_OAS31_DEEP_EQUAL_H_
