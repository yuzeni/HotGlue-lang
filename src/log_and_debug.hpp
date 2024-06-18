#pragma once

#include <iostream>

#include "utils.hpp"

#if HG_ENABLE_LOG_COLORS
#  define HG_BLACK_COLOR           "\033[30m"
#  define HG_RED_COLOR             "\033[31m"
#  define HG_GREEN_COLOR           "\033[32m"
#  define HG_YELLOW_COLOR          "\033[33m"
#  define HG_BLUE_COLOR            "\033[34m"
#  define HG_MAGENTA_COLOR         "\033[35m"
#  define HG_CYAN_COLOR            "\033[36m"
#  define HG_WHITE_COLOR           "\033[37m"
#  define HG_BRIGHT_BLACK_COLOR    "\033[90m"
#  define HG_BRIGHT_RED_COLOR      "\033[91m"
#  define HG_BRIGHT_GREEN_COLOR    "\033[92m"
#  define HG_BRIGHT_YELLOW_COLOR   "\033[93m"
#  define HG_BRIGHT_BLUE_COLOR     "\033[94m"
#  define HG_BRIGHT_MAGENTA_COLOR  "\033[95m"
#  define HG_BRIGHT_CYAN_COLOR     "\033[96m"
#  define HG_BRIGHT_WHITE_COLOR    "\033[97m"
#  define HG_BLACK_COLOR_BG           "\033[107m"
#  define HG_RED_COLOR_BG             "\033[107m"
#  define HG_GREEN_COLOR_BG           "\033[107m"
#  define HG_YELLOW_COLOR_BG          "\033[107m"
#  define HG_BLUE_COLOR_BG            "\033[107m"
#  define HG_MAGENTA_COLOR_BG         "\033[107m"
#  define HG_CYAN_COLOR_BG            "\033[107m"
#  define HG_WHITE_COLOR_BG           "\033[107m"
#  define HG_BRIGHT_BLACK_COLOR_BG    "\033[107m"
#  define HG_BRIGHT_RED_COLOR_BG      "\033[107m"
#  define HG_BRIGHT_GREEN_COLOR_BG    "\033[107m"
#  define HG_BRIGHT_YELLOW_COLOR_BG   "\033[107m"
#  define HG_BRIGHT_BLUE_COLOR_BG     "\033[107m"
#  define HG_BRIGHT_MAGENTA_COLOR_BG  "\033[107m"
#  define HG_BRIGHT_CYAN_COLOR_BG     "\033[107m"
#  define HG_BRIGHT_WHITE_COLOR_BG    "\033[107m"
#  define HG_END_COLOR "\033[0m"
#else
#  define HG_BLACK_COLOR           ""
#  define HG_RED_COLOR             ""
#  define HG_GREEN_COLOR           ""
#  define HG_YELLOW_COLOR          ""
#  define HG_BLUE_COLOR            ""
#  define HG_MAGENTA_COLOR         ""
#  define HG_CYAN_COLOR            ""
#  define HG_WHITE_COLOR           ""
#  define HG_BRIGHT_BLACK_COLOR    ""
#  define HG_BRIGHT_RED_COLOR      ""
#  define HG_BRIGHT_GREEN_COLOR    ""
#  define HG_BRIGHT_YELLOW_COLOR   ""
#  define HG_BRIGHT_BLUE_COLOR     ""
#  define HG_BRIGHT_MAGENTA_COLOR  ""
#  define HG_BRIGHT_CYAN_COLOR     ""
#  define HG_BRIGHT_WHITE_COLOR    ""
#  define HG_BLACK_COLOR_BG           ""
#  define HG_RED_COLOR_BG             ""
#  define HG_GREEN_COLOR_BG           ""
#  define HG_YELLOW_COLOR_BG          ""
#  define HG_BLUE_COLOR_BG            ""
#  define HG_MAGENTA_COLOR_BG         ""
#  define HG_CYAN_COLOR_BG            ""
#  define HG_WHITE_COLOR_BG           ""
#  define HG_BRIGHT_BLACK_COLOR_BG    ""
#  define HG_BRIGHT_RED_COLOR_BG      ""
#  define HG_BRIGHT_GREEN_COLOR_BG    ""
#  define HG_BRIGHT_YELLOW_COLOR_BG   ""
#  define HG_BRIGHT_BLUE_COLOR_BG     ""
#  define HG_BRIGHT_MAGENTA_COLOR_BG  ""
#  define HG_BRIGHT_CYAN_COLOR_BG     ""
#  define HG_BRIGHT_WHITE_COLOR_BG    ""
#  define HG_END_COLOR ""
#endif

#define HG_ERROR_COLOR HG_BRIGHT_RED_COLOR
#define HG_WARNING_COLOR HG_BRIGHT_YELLOW_COLOR
#define HG_INFO_COLOR HG_BRIGHT_BLACK_COLOR
#define HG_SUCCESS_COLOR HG_BRIGHT_GREEN_COLOR

enum class HG_err {
  error,
  assertion,
  IO,
  parsing,
  ambiguous,
  type,
  out_of_range,
  mem_alloc,
  not_implemented,
  SIZE
};

namespace HG_errors {

    static const int max_err_msg_size = 1024;
    static const char* err_enum_name_table[int(HG_err::SIZE)]{
	"general",
	"assertion",
	"IO",
	"parsing",
	"ambiguous",
	"type",
	"out of range",
	"bad memory allocation",
	"not implemented",
    };

    static constexpr const char* err_type_to_str(HG_err err_type)
    {
	return err_enum_name_table[int(err_type)];
    }
    
    enum class Log_type { INFO, WARNING, ERROR };
    
    template<typename... Args>
    void log(Log_type log_type, const char* msg, Args... args)
    {
	char* log_msg = new char[max_err_msg_size];
	std::snprintf(log_msg, max_err_msg_size, msg, args...);
 	switch(log_type){
	case Log_type::INFO:    std::cout << HG_INFO_COLOR    "INFO    "; break;
	case Log_type::WARNING: std::cout << HG_WARNING_COLOR "WARNING "; break;
	case Log_type::ERROR:   std::cout << HG_ERROR_COLOR   "ERROR   "; break;
	}
	std::cout << HG_END_COLOR << log_msg << '\n';
    }
}

template <typename... Args>
void hg_info(const char *msg, Args... args)
{
#if HG_LOG_INFO
    using namespace HG_errors;
    log(Log_type::INFO, msg, args...);
#endif
}

template <typename... Args>
void hg_warn(const char *msg, Args... args)
{
#if HG_LOG_WARNINGS
    using namespace HG_errors;
    log(Log_type::WARNING, msg, args...);
#endif
}

template <typename... Args>
void hg_error(HG_err err_type, const char *msg, Args... args)
{
    using namespace HG_errors;
#if HG_LOG_ERRORS
    char* log_msg = new char[max_err_msg_size];
    size_t true_size = std::snprintf(log_msg, max_err_msg_size, HG_ERROR_COLOR "%s error:" HG_END_COLOR " %s", err_type_to_str(err_type), msg);
    log(Log_type::ERROR, log_msg, args...);
    if(true_size >= max_err_msg_size)
	hg_error(HG_err::out_of_range, "Previous error message was too long and did not fit in the buffer of size %d", max_err_msg_size);
#endif
}

// Debugging macros:

#ifdef HG_PLATFORM_windows
#  define HG_DEB_break() __debugbreak()
#elif HG_PLATFORM_linux
#  define HG_DEB_break() __builtin_debugtrap()
#elif HG_PLATFORM_apple
#  define HG_DEB_break() __builtin_trap()
#endif

#if HG_DEBUG
#  define HG_DEB_error(msg, ...)					\
    do {								\
	hg_error(HG_err::error, msg " FILE:%s LINE:%d", __FILE__, __LINE__, ##__VA_ARGS__); \
	exit(1);							\
    } while (0)

#define HG_DEB_assert(x, msg, ...)                                             \
  do {                                                                         \
    if (!(x)) {                                                                \
      hg_error(HG_err::assertion,                                              \
               "'" msg "'"                                                     \
               " FILE:%s LINE:%d",                                             \
               __FILE__, __LINE__, ##__VA_ARGS__);                             \
      HG_DEB_break();                                                          \
    }                                                                          \
  } while (0)

#  define HG_DEB_not_implemented		\
    do {					\
	hg_error(HG_err::not_implemented,	\
		 "lacking implementation"	\
		 " FILE:%s LINE:%d",		\
		 __FILE__, __LINE__);		\
	HG_DEB_break();				\
    } while (0)
#else
#  define HG_DEB_error(msg, ...)
#  define HG_DEB_assert(x, msg, ...)
#  define HG_DEB_not_implemented
#endif
