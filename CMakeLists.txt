cmake_minimum_required(VERSION 3.20)
project(
  pud
  VERSION 0.0.1
  LANGUAGES C CXX)

set(PUD_JIT_PYTHON_VERSION "0.1.7")
configure_file("${PROJECT_SOURCE_DIR}/cmake/config.h.in"
               "${PROJECT_SOURCE_DIR}/include/Pud/Config/Config.h")

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
# set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
# set(CMAKE_CXX_COMPILER "/usr/bin/clang++")
# set(CMAKE_C_COMPILER "/usr/bin/clang")
set(CMAKE_BUILD_TYPE "Debug")
set(CMAKE_CXX_FLAGS_RELEASE "-O3")
set(project_version "${${PROJECT_NAME}_VERSION}")

if(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  set(CMAKE_CXX_FLAGS
      "${CMAKE_CXX_FLAGS} -pedantic -fvisibility-inlines-hidden -Wno-return-type-c-linkage -Wno-gnu-zero-variadic-macro-arguments -Wno-deprecated-declarations"
  )
else()
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-return-type")
endif()
if(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -fno-limit-debug-info")
endif()
if(CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
  # for debug of stl structure while using clang compile
  add_compile_options(-fstandalone-debug)
endif()

message(STATUS "Project '${PROJECT_NAME}', version: '${project_version}'")

option(PUD_GPU "build Pud GPU backend" ON)
option(PUD_OPT_BUILD_UNITTESTS "Build all pud unittests" ON)

# temp define: https://discourse.llvm.org/t/python-api-problem/945
# add_compile_options(-fno-rtti)

set(THREADS_PREFER_PTHREAD_FLAG ON)
find_package(Threads REQUIRED)

# ------------------------------------------------------------
# LLVM BEGIN
# ------------------------------------------------------------

# apt install llvm-15-dev libclang-15-dev
set(LLVM_DIR /usr/lib/llvm-15/lib/cmake/llvm)

find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})
# ------------------------------------------------------------
# LLVM END
# ------------------------------------------------------------


#include(${CMAKE_SOURCE_DIR}/cmake/deps.cmake)



include(GNUInstallDirs)
include(CMakePackageConfigHelpers)

include(CTest)
enable_testing()
find_package(GTest REQUIRED CONFIG)

include_directories(include)


# Create the pud C++ library
set(PUD_SRCS)
set(PUD_COMMON_SRCS)

set(STATIC_LIB_NAME ${PROJECT_NAME})
set(PUD_COMMON_LIB ${PROJECT_NAME}Common)

# pudCommon lib
file(GLOB_RECURSE PUD_COMMON_SRCS lib/Common/*.cc)
add_library(${PUD_COMMON_LIB} SHARED ${PUD_COMMON_SRCS})
target_link_libraries(${PUD_COMMON_LIB})
#add_dependencies(${PUD_COMMON_LIB} zlibstatic gc backtrace bz2 liblzma re2)

# if(PUD_GPU)
#   add_compile_definitions(PUD_GPU)
#   find_package(CUDAToolkit REQUIRED)
#   target_link_libraries(${PUD_COMMON_LIB} PRIVATE CUDA::cudart_static CUDA::cuda_driver)
# endif()