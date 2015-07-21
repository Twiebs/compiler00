#pragma once
#ifndef _COMMON_HPP
#define _COMMON_HPP

#include <iostream>
#include <assert.h>
#include <stdint.h>

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

typedef int8_t   s8;
typedef int16_t  s16;
typedef int32_t  s32;
typedef int64_t  s64;
typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef float    f32;
typedef double   f64;

#define LOG_LEVEL_NONE    0
#define LOG_LEVEL_ERROR   1
#define LOG_LEVEL_INFO    2
#define LOG_LEVEL_DEBUG   3
#define LOG_LEVEL_VERBOSE 4

#define LOG_LEVEL LOG_LEVEL_INFO

extern U32 gErrorCount;
extern std::string rootDir;

#undef ENABLE_COLOR_OUTPUT

#if LOG_LEVEL > 0
#ifdef ENABLE_COLOR_OUTPUT
#define LOG_ERROR(x) gErrorCount++; std::cout << "\x1b[31mERROR: " << x << "\033[39m\n"
#else
#define LOG_ERROR(x) gErrorCount++; std::cout << "ERROR: " << x << "\n"
#endif
#else
#define LOG_ERROR(x)
#endif

#if LOG_LEVEL > 1
#ifdef ENABLE_COLOR_OUTPUT
#define LOG_INFO(x) std::cout << "\x1b[36mINFO: " << x << "\033[39m\n"
#else
#define LOG_INFO(x) std::cout << "INFO: " << x << "\n"
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
