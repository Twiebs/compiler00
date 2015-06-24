/*
 * Common.hpp
 *
 *  Created on: Jun 18, 2015
 *      Author: torin
 */

#ifndef _COMMON_HPP
#define _COMMON_HPP

#include <iostream>
#include <stdint.h>

typedef int8_t int8;
typedef int16_t int16;
typedef uint8_t uint8;
typedef uint16_t uint16;
typedef uint32_t uint32;
typedef uint64_t uint64;

#define LOG_LEVEL_NONE  0
#define LOG_LEVEL_ERROR 1
#define LOG_LEVEL_INFO  2
#define LOG_LEVEL_DEBUG 3
#define LOG_LEVEL_VERBOSE 4

#define LOG_LEVEL LOG_LEVEL_VERBOSE

#if LOG_LEVEL > 0
#define LOG_ERROR(x) std::cerr << "ERROR: " << x << "\n"
#else
#define LOG_ERROR(x)
#endif

#if LOG_LEVEL > 1
#define LOG_INFO(x) std::clog << "INFO: " << x << "\n"
#else
#define LOG_INFO(x)
#endif

#if LOG_LEVEL > 2
#define LOG_DEBUG(x) std::cout << "DEBUG: " << x << "\n"
#else
#define LOG_DEBUG(x)
#endif

#if LOG_LEVEL > 3
#define LOG_VERBOSE(x) std::cout << "VERBOSE: " << x << "\n"
#else
#define LOG_VERBOSE(x)
#endif

#endif /* _COMMON_HPP */
