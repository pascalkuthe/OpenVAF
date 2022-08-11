#pragma once

#include <cstdarg>
#include <cstdint>
#include <cstdlib>
#include <ostream>
#include <new>

namespace vae {
    template<typename T>
    struct Slice;

    #if defined(_WIN32)
        using NativePath = Slice<uint16_t>;
    #else
        using NativePath = Slice<uint8_t>;
    #endif
}


namespace vae {

enum class OptLevel {
  None = 0,
  Less = 1,
  Default = 2,
  Aggressive = 3,
};

using ParamFlags = uint8_t;

using ModelcardInit = void(*)(double*, int32_t*, const char**, double*, int32_t*, double*, int32_t*, ParamFlags*);

template<typename T>
union Meta {
  uint64_t stride;
  T scalar;
};

template<typename T>
struct FatPtr {
  T *ptr;
  Meta<T> meta;
};

using VaeFun = void(*)(uintptr_t, FatPtr<double>*, FatPtr<double>*, FatPtr<double>*, FatPtr<int32_t>*, const char**, FatPtr<double>*, FatPtr<int32_t>*, FatPtr<double>*, void*);

template<typename T>
struct Slice {
  T *ptr;
  uintptr_t len;
};

struct VfsEntry {
  Slice<uint8_t> name;
  Slice<uint8_t> data;
};

using Vfs = Slice<VfsEntry>;

struct Opts {
  Slice<uint8_t> model;
  NativePath cache_dir;
  Slice<NativePath> include_dirs;
  Slice<Slice<uint8_t>> macro_flags;
  Slice<Slice<uint8_t>> allow_lints;
  Slice<Slice<uint8_t>> warn_lints;
  Slice<Slice<uint8_t>> deny_lints;
  OptLevel opt_lvl;
  Slice<uint8_t> target_cpu;
  Slice<uint8_t> target;
  Slice<Slice<uint8_t>> cg_flags;
  Vfs vfs;
};

extern "C" {

///This function returns a pointer to the `functions` global
/// of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
const char *const *verilogae_functions(const void *lib);

///This function returns a pointer to the `functions.sym` global
/// of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
const char *const *verilogae_function_symbols(const void *lib);

///This function returns a pointer to the `opvars` global
/// of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
const char *const *verilogae_opvars(const void *lib);

///This function returns a pointer to the `params.real` global
/// of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
const char *const *verilogae_real_params(const void *lib);

///This function returns a pointer to the `params.unit.real` global
/// of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
const char *const *verilogae_real_param_units(const void *lib);

///This function returns a pointer to the `params.desc.real` global
/// of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
const char *const *verilogae_real_param_descriptions(const void *lib);

///This function returns a pointer to the `params.group.real` global
/// of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
const char *const *verilogae_real_param_groups(const void *lib);

///This function returns a pointer to the `params.integer` global
/// of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
const char *const *verilogae_int_params(const void *lib);

///This function returns a pointer to the `params.unit.integer` global
/// of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
const char *const *verilogae_int_param_units(const void *lib);

///This function returns a pointer to the `params.desc.integer` global
/// of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
const char *const *verilogae_int_param_descriptions(const void *lib);

///This function returns a pointer to the `params.group.integer` global
/// of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
const char *const *verilogae_int_param_groups(const void *lib);

///This function returns a pointer to the `params.string` global
/// of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
const char *const *verilogae_str_params(const void *lib);

///This function returns a pointer to the `params.unit.string` global
/// of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
const char *const *verilogae_str_param_units(const void *lib);

///This function returns a pointer to the `params.desc.string` global
/// of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
const char *const *verilogae_str_param_descriptions(const void *lib);

///This function returns a pointer to the `params.group.string` global
/// of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
const char *const *verilogae_str_param_groups(const void *lib);

///This function returns a pointer to the `nodes` global
/// of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
const char *const *verilogae_nodes(const void *lib);

///This function returns the value stored in the `functions.cnt` global
/// of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
uintptr_t verilogae_function_cnt(const void *lib);

///This function returns the value stored in the `opvars.cnt` global
/// of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
uintptr_t verilogae_opvars_cnt(const void *lib);

///This function returns the value stored in the `params.real.cnt` global
/// of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
uintptr_t verilogae_real_param_cnt(const void *lib);

///This function returns the value stored in the `params.integer.cnt` global
/// of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
uintptr_t verilogae_int_param_cnt(const void *lib);

///This function returns the value stored in the `params.string.cnt` global
/// of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
uintptr_t verilogae_str_param_cnt(const void *lib);

///This function returns the value stored in the `nodes.cnt` global
/// of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
uintptr_t verilogae_node_cnt(const void *lib);

///This function returns a pointer to the `params.real` global
/// of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
///`sym_name` must batch the schema fun.{NUM}params.real
const char *const *verilogae_real_fun_params(const void *lib, const char *fun);

///This function returns a pointer to the `params.integer` global
/// of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
///`sym_name` must batch the schema fun.{NUM}params.integer
const char *const *verilogae_int_fun_params(const void *lib, const char *fun);

///This function returns a pointer to the `params.string` global
/// of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
///`sym_name` must batch the schema fun.{NUM}params.string
const char *const *verilogae_str_fun_params(const void *lib, const char *fun);

///This function returns a pointer to the `depbreak.real` global
/// of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
///`sym_name` must batch the schema fun.{NUM}depbreak.real
const char *const *verilogae_real_fun_depbreak(const void *lib, const char *fun);

///This function returns a pointer to the `depbreak.integer` global
/// of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
///`sym_name` must batch the schema fun.{NUM}depbreak.integer
const char *const *verilogae_int_fun_depbreak(const void *lib, const char *fun);

///This function returns a pointer to the `voltages` global
/// of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
///`sym_name` must batch the schema fun.{NUM}voltages
const char *const *verilogae_fun_voltages(const void *lib, const char *fun);

///This function returns a pointer to the `currents` global
/// of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
///`sym_name` must batch the schema fun.{NUM}currents
const char *const *verilogae_fun_currents(const void *lib, const char *fun);

///This function returns a pointer to the `voltages.default` global
/// of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
///`sym_name` must batch the schema fun.{NUM}voltages.default
const double *verilogae_fun_voltage_defaults(const void *lib, const char *fun);

///This function returns a pointer to the `currents.default` global
/// of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
///`sym_name` must batch the schema fun.{NUM}currents.default
const double *verilogae_fun_current_defaults(const void *lib, const char *fun);

///This funtion returns a pointer to the `params.real.cnt` global
/// of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
uintptr_t verilogae_real_fun_param_cnt(const void *lib, const char *fun);

///This funtion returns a pointer to the `params.integer.cnt` global
/// of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
uintptr_t verilogae_int_fun_param_cnt(const void *lib, const char *fun);

///This funtion returns a pointer to the `params.string.cnt` global
/// of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
uintptr_t verilogae_str_fun_param_cnt(const void *lib, const char *fun);

///This funtion returns a pointer to the `depbreak.real.cnt` global
/// of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
uintptr_t verilogae_real_fun_depbreak_cnt(const void *lib, const char *fun);

///This funtion returns a pointer to the `depbreak.integer.cnt` global
/// of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
uintptr_t verilogae_int_fun_depbreak_cnt(const void *lib, const char *fun);

///This funtion returns a pointer to the `voltages.cnt` global
/// of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
uintptr_t verilogae_fun_voltage_cnt(const void *lib, const char *fun);

///This funtion returns a pointer to the `currents.cnt` global
/// of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
uintptr_t verilogae_fun_current_cnt(const void *lib, const char *fun);

///This funtion returns a pointer to the `voltages.default.cnt` global
/// of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
uintptr_t verilogae_fun_voltage_default_cnt(const void *lib, const char *fun);

///This funtion returns a pointer to the `currents.default.cnt` global
/// of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
uintptr_t verilogae_fun_current_default_cnt(const void *lib, const char *fun);

/// Obtains a pointer to the modelcard initialization function of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
ModelcardInit verilogae_init_modelcard(const void *lib);

/// Obtains a pointer to a model functions of a VerilogAE model loaded with `load`.
///
/// # Safety
///
/// `lib` must be a valid pointer returned by the `load` functions or `dlopen`
VaeFun verilogae_fun_ptr(const void *lib, const char *fun);

/// # Safety
/// handle must be a valid model compiled with VerilogAE
const char *verilogae_module_name(const void *lib);

/// # Safety
///
/// All required parameters must be initialized appropriately
int32_t verilogae_call_fun_parallel(VaeFun fun,
                                    uintptr_t cnt,
                                    FatPtr<double> *voltages,
                                    FatPtr<double> *currents,
                                    FatPtr<double> *real_params,
                                    FatPtr<int32_t> *int_params,
                                    const char **str_params,
                                    FatPtr<double> *real_dep_break,
                                    FatPtr<int32_t> *int_dep_break,
                                    FatPtr<double> *temp,
                                    void *out);

Opts *verilogae_new_opts();

/// # Safety
/// `opts` must be a valid pointer created with `new_opts`
void verilogae_free_opts(Opts *opts);

/// # Safety
/// * path must be valid for reads
/// * opts must be valid for reads or null
/// * opts must only contain valid data
Vfs verilogae_export_vfs(NativePath path, Opts *opts);

/// # Safety
/// * path must be valid for reads
/// * opts must be valid for reads or null
/// * opts must only contain valid data
void verilogae_free_vfs(Vfs vfs);

/// # Safety
/// * path must be valid for reads
/// * opts must be valid for reads or null
/// * opts must only contain valid data
const void *verilogae_load(NativePath path, bool full_compile, const Opts *opts);

} // extern "C"

} // namespace vae

namespace vae {
    const ParamFlags PARAM_FLAGS_MIN_INCLUSIVE = 1;
    const ParamFlags PARAM_FLAGS_MAX_INCLUSIVE = 2;
    const ParamFlags PARAM_FLAGS_INVALID = 4;
    const ParamFlags PARAM_FLAGS_GIVEN = 8;
}