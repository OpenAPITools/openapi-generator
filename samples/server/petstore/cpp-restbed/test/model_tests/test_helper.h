#ifndef CPP_RESTPED_SERVER_OPEN_API_TEST_HELPER_H
#define CPP_RESTPED_SERVER_OPEN_API_TEST_HELPER_H

#include <boost/test/unit_test.hpp>
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/json_parser.hpp>
#include <sstream>

#define TEST_COMPARE_JSON(jsonA, jsonB) do { \
	std::stringstream ssA(jsonA);            \
    boost::property_tree::ptree ptA;         \
    read_json(ssA,ptA);                      \
    std::stringstream ssB(jsonB);            \
    boost::property_tree::ptree ptB;         \
    read_json(ssB,ptB);                      \
    BOOST_TEST(ptA == ptB); \
} while(0)


#endif //CPP_RESTPED_SERVER_OPEN_API_TEST_HELPER_H
