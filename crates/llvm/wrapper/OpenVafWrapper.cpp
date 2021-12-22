#include "llvm/IR/Instructions.h"

using namespace llvm;

// Enable some fast-math flags for an operation
// These flags are used for derivatives by default because they only change
// the rounding behaviour which is not relevant for automatically generated code (derivatives in
// OpneVAF)
//
// https://llvm.org/docs/LangRef.html#fast-math-flags
extern "C" void LLVMSetDerivativeFastMath(LLVMValueRef V) {
  if (auto I = dyn_cast<Instruction>(unwrap<Value>(V))) {
    I->setHasAllowReassoc(true);
    I->setHasAllowReciprocal(true);
    I->setHasAllowContract(true);
  }
}
