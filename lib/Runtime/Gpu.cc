#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <vector>

#include "Pud/Runtime/Runtime.h"

#ifdef PUD_GPU

#include "cuda.h"

#define fail(err)                                                          \
  do {                                                                     \
    const char* msg;                                                       \
    cuGetErrorString((err), &msg);                                         \
    fprintf(stderr, "CUDA error at %s:%d: %s\n", __FILE__, __LINE__, msg); \
    abort();                                                               \
  } while (0)

#define check(call)            \
  do {                         \
    auto err = (call);         \
    if (err != CUDA_SUCCESS) { \
      fail(err);               \
    }                          \
  } while (0)

static std::vector<CUmodule> modules;
static CUcontext context;

void pud_nvptx_init() {
  CUdevice device;
  check(cuInit(0));
  check(cuDeviceGet(&device, 0));
  check(cuCtxCreate(&context, 0, device));
}

SEQ_FUNC void pud_nvptx_load_module(const char* filename) {
  CUmodule module;
  check(cuModuleLoad(&module, filename));
  modules.push_back(module);
}

SEQ_FUNC pud_int_t pud_nvptx_device_count() {
  int devCount;
  check(cuDeviceGetCount(&devCount));
  return devCount;
}

SEQ_FUNC pud_str_t pud_nvptx_device_name(CUdevice device) {
  char name[128];
  check(cuDeviceGetName(name, sizeof(name) - 1, device));
  auto sz = static_cast<pud_int_t>(strlen(name));
  auto* p = (char*)pud_alloc_atomic(sz);
  memcpy(p, name, sz);
  return {sz, p};
}

SEQ_FUNC pud_int_t pud_nvptx_device_capability(CUdevice device) {
  int devMajor, devMinor;
  check(cuDeviceComputeCapability(&devMajor, &devMinor, device));
  return ((pud_int_t)devMajor << 32) | (pud_int_t)devMinor;
}

SEQ_FUNC CUdevice pud_nvptx_device(pud_int_t idx) {
  CUdevice device;
  check(cuDeviceGet(&device, idx));
  return device;
}

static bool name_char_valid(char c, bool first) {
  bool ok = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || (c == '_');
  if (!first)
    ok = ok || ('0' <= c && c <= '9');
  return ok;
}

SEQ_FUNC CUfunction pud_nvptx_function(pud_str_t name) {
  CUfunction function;
  CUresult result;

  std::vector<char> clean(name.len + 1);
  for (unsigned i = 0; i < name.len; i++) {
    char c = name.str[i];
    clean[i] = (name_char_valid(c, i == 0) ? c : '_');
  }
  clean[name.len] = '\0';

  for (auto it = modules.rbegin(); it != modules.rend(); ++it) {
    result = cuModuleGetFunction(&function, *it, clean.data());
    if (result == CUDA_SUCCESS) {
      return function;
    } else if (result == CUDA_ERROR_NOT_FOUND) {
      continue;
    } else {
      break;
    }
  }

  fail(result);
  return {};
}

SEQ_FUNC void pud_nvptx_invoke(CUfunction f, unsigned int gridDimX,
                               unsigned int gridDimY, unsigned int gridDimZ,
                               unsigned int blockDimX, unsigned int blockDimY,
                               unsigned int blockDimZ,
                               unsigned int sharedMemBytes,
                               void** kernelParams) {
  check(cuLaunchKernel(f, gridDimX, gridDimY, gridDimZ, blockDimX, blockDimY,
                       blockDimZ, sharedMemBytes, nullptr, kernelParams,
                       nullptr));
}

SEQ_FUNC CUdeviceptr pud_nvptx_device_alloc(pud_int_t size) {
  if (size == 0)
    return {};

  CUdeviceptr devp;
  check(cuMemAlloc(&devp, size));
  return devp;
}

SEQ_FUNC void pud_nvptx_memcpy_h2d(CUdeviceptr devp, char* hostp,
                                   pud_int_t size) {
  if (size)
    check(cuMemcpyHtoD(devp, hostp, size));
}

SEQ_FUNC void pud_nvptx_memcpy_d2h(char* hostp, CUdeviceptr devp,
                                   pud_int_t size) {
  if (size)
    check(cuMemcpyDtoH(hostp, devp, size));
}

SEQ_FUNC void pud_nvptx_device_free(CUdeviceptr devp) {
  if (devp)
    check(cuMemFree(devp));
}

#endif /* CODON_GPU */
