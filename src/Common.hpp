#pragma once
#ifndef _COMMON_HPP
#define _COMMON_HPP

#include <iostream>
#include <assert.h>
#include <stdint.h>

#define ASSERT_ON_ERROR

typedef int8_t    S8;
typedef int16_t   S16;
typedef int32_t   S32;
typedef int64_t   S64;
typedef uint8_t   U8;
typedef uint16_t  U16;
typedef uint32_t  U32;
typedef uint64_t  U64;
typedef float 	  F32;
typedef double 	  F64;

#define LOG_LEVEL_NONE    0
#define LOG_LEVEL_ERROR   1
#define LOG_LEVEL_INFO    2
#define LOG_LEVEL_DEBUG   3
#define LOG_LEVEL_VERBOSE 4

#define LOG_LEVEL LOG_LEVEL_VERBOSE

#define INTERNAL_ERROR(msg) std::cout << "INTERNAL_ERROR: " << msg << "\n";

#if LOG_LEVEL > 0
#ifdef ENABLE_COLOR_OUTPUT
#define LOG_ERROR(x) std::cout << "\x1b[31mERROR: " << x << "\033[39m\n"
#else
#define LOG_ERROR(x) std::cout << "ERROR: " << x << "\n"
#endif
#else
#define LOG_ERROR(x)
#endif

#if LOG_LEVEL > 1
#ifdef ENABLE_COLOR_OUTPUT
#define LOG_INFO(msg) std::cout << "[INFO]\033[33m" << msg << "[39m\n"
#else
#define LOG_INFO(x) std::cout << "[INFO] " << x << "\n"
#endif	//COLOR_OUTPUT
#else
#define LOG_INFO(x)
#endif

#if LOG_LEVEL > 2
#ifdef ENABLE_COLOR_OUTPUT
#define LOG_DEBUG(x) std::cout << "DEBUG: " << x << "\n"
#else
#define LOG_DEBUG(x) std::cout << "DEBUG: " << x << "\n"
#endif
#else
#define LOG_DEBUG(x)
#endif

#if LOG_LEVEL > 3
#ifdef ENABLE_COLOR_OUTPUT
#define LOG_VERBOSE(x) std::cout << "\x1b[30mVERBOSE: " << x << "\033[39m\n"
#else
#define LOG_VERBOSE(x) std::cout << "VERBOSE: " << x << "\n"
#endif
#else
#define LOG_VERBOSE(x)
#endif

#endif /* _COMMON_HPP */
