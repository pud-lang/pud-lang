cmake_minimum_required(VERSION 3.20)
project(
  pud
  VERSION 0.0.1
  LANGUAGES C CXX
)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_CXX_COMPILER "clang++")
set(CMAKE_BUILD_TYPE "Debug")
set(project_version "${${PROJECT_NAME}_VERSION}")

message(STATUS "Project '${PROJECT_NAME}', version: '${project_version}'")

option(PUD_OPT_BUILD_UNITTESTS "Build all pud unittests" ON)

# temp define: https://discourse.llvm.org/t/python-api-problem/945
# add_compile_options(-fno-rtti)

# simd ssse3
add_compile_options(-mssse3)

if (CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
  # for debug of stl structure while using clang compile
  add_compile_options(-fstandalone-debug)
endif()

#------------------------------------------------------------
# LLVM BEGIN
#------------------------------------------------------------

# apt install llvm-15-dev libclang-15-dev
# set(LLVM_DIR /usr/lib/llvm-15/lib/cmake/llvm)
# set(Clang_DIR /usr/lib/llvm-15/lib/cmake/clang)
# manually install
set(LLVM_DIR /usr/local/lib/cmake/llvm)
#set(Clang_DIR /usr/local/lib/cmake/clang)

find_package(LLVM REQUIRED CONFIG)
#find_package(Clang REQUIRED CONFIG)

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})
#------------------------------------------------------------
# LLVM END
#------------------------------------------------------------

#------------------------------------------------------------
# CMake helpers begin
#------------------------------------------------------------
include(GNUInstallDirs)
include(CMakePackageConfigHelpers)

include(CTest)
enable_testing()
find_package(GTest REQUIRED CONFIG)

include_directories(include)

#------------------------------------------------------------
# CMake helpers end
#------------------------------------------------------------

#------------------------------------------------------------
# lib make begin
#------------------------------------------------------------

# Create the pud C++ library
set(PUD_SRCS)
set(PUD_COMMON_SRCS)

set(STATIC_LIB_NAME ${PROJECT_NAME})
set(PUD_COMMON_LIB ${PROJECT_NAME}Common)

# pudCommon lib
file(GLOB_RECURSE PUD_COMMON_SRCS
    lib/Common/*.cc
  )
add_library(${PUD_COMMON_LIB} STATIC ${PUD_COMMON_SRCS})
target_link_libraries(${PUD_COMMON_LIB}
  LLVMSupport
)

#------------------------------------------------------------
# lib make end
#------------------------------------------------------------