#include "llvm/IR/Instructions.h"
#include "llvm/Support/CrashRecoveryContext.h"
#include <llvm/IR/Attributes.h>
#include <llvm/IR/Function.h>

#include <mutex>
#include <stdlib.h>
#include <iostream>

#include <lld/Common/Driver.h>

using namespace llvm;
using namespace lld;

// LLD seems not to be thread safe. This is terrible. We basically only allow
// single threaded access to the driver using mutexes. Each type of LLD driver
// seems to be disconnected so we use a mutex for every type.
std::mutex _mutex;

const char *lld_alloc_str(const std::string &str) {
  size_t size = str.length();
  if (size > 0) {
    char *strPtr = reinterpret_cast<char *>(malloc(size + 1));
    memcpy(strPtr, str.c_str(), size + 1);
    return strPtr;
  }
  return nullptr;
}
extern "C" {

// Enable some fast-math flags for an operation
// These flags are used for derivatives by default because they only change
// the rounding behaviour which is not relevant for automatically generated code
// (derivatives in OpneVAF)
//
// https://llvm.org/docs/LangRef.html#fast-math-flags
void LLVMSetPartialFastMath(LLVMValueRef V) {
  if (auto I = dyn_cast<Instruction>(unwrap<Value>(V))) {
    I->setHasAllowReassoc(true);
    I->setHasAllowReciprocal(true);
    I->setHasAllowContract(true);
  }
}

// Enable fast-math flags for an operation
// https://llvm.org/docs/LangRef.html#fast-math-flags
void LLVMSetFastMath(LLVMValueRef V) {
  if (auto I = dyn_cast<Instruction>(unwrap<Value>(V))) {
    I->setFast(true);
  }
}

void LLVMPurgeAttrs(LLVMValueRef V) {
  if (auto func = dyn_cast<Function>(unwrap<Value>(V))) {
    func->setAttributes(AttributeList());
  }
}

enum LldFlavor {
  Elf = 0,
  MachO = 1,
  Coff = 2,
};

struct LldInvokeResult {
  int ret_code;
  const char *messages;
};

LldInvokeResult lld_link(LldFlavor flavor, int argc, const char **argv) {
  std::string outputString, errorString;
  outputString.append("fo Ro");
  errorString.append("bar");

  raw_string_ostream outputStream(outputString);
  raw_string_ostream errorStream(errorString);

  std::vector<const char *> args(argv, argv + argc);

  bool (*link)(llvm::ArrayRef<const char *>, llvm::raw_ostream &,
               llvm::raw_ostream &, bool exitEarly, bool disableOutput);
  switch (flavor) {
  case Elf: {
    link = elf::link;
    break;
  }
  case MachO: {
    link = macho::link;
    break;
  }
  case Coff: {
    link = coff::link;
    break;
  }
  default:
    return {.ret_code = -2, .messages = nullptr};
  }

  std::unique_lock<std::mutex> lock(_mutex); // Driver is not thread save

  // ensures reporduce binaries on linux. Done here instead of the main compiler
  // because the mutex ensure thread save behaviour with regard to env variables
  putenv(strdup("ZERO_AR_DATE=1"));
  
  bool success = false;
  {
    // The crash recovery is here only to be able to recover from arbitrary
    // control flow when fatal() is called (through setjmp/longjmp or
    // __try/__except).
    llvm::CrashRecoveryContext crc;
    if (!crc.RunSafely([&]() {
          success = link(args, llvm::errs(), llvm::outs(), false, false);
        })) {

      std::string resultMessage = errorString + outputString;
      return {.ret_code = -1, .messages = lld_alloc_str(resultMessage)};
    }
  }

  { // Cleanup memory and reset everything back in pristine condition. This path
    // is only taken when LLD is in test, or when it is used as a library.
    llvm::CrashRecoveryContext crc;
    if (!crc.RunSafely([&]() { CommonLinkerContext::destroy(); })) {
      // The memory is corrupted beyond any possible recovery.
      std::string resultMessage = errorString + outputString;
      return {.ret_code = -1, .messages = lld_alloc_str(resultMessage)};
    }
  }
  std::string resultMessage = errorString + outputString;
  return {.ret_code = success?0: 1, .messages = lld_alloc_str(resultMessage)};
}
}

