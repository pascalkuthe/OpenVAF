#pragma once

#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#if defined(_WIN32)
    typedef struct VAENativePath {
      uint16_t *ptr;
      uintptr_t len;
    } VAENativePath;
#else
    typedef VAESlice_u8;
    typedef struct VAESlice_u8 VAENativePath;
#endif

#define VAE_PARAM_FLAGS_MIN_INCLUSIVE 1
#define VAE_PARAM_FLAGS_MAX_INCLUSIVE 2
#define VAE_PARAM_FLAGS_INVALID 4
#define VAE_PARAM_FLAGS_GIVEN 8


typedef enum VAEOptLevel {
  VAEOptLevel_None = 0,
  VAEOptLevel_Less = 1,
  VAEOptLevel_Default = 2,
  VAEOptLevel_Aggressive = 3,
} VAEOptLevel;

typedef uint8_t VAEParamFlags;

typedef void (*VAEModelcardInit)(double*, int32_t*, const char**, double*, int32_t*, double*, int32_t*, VAEParamFlags*);

typedef union VAEMeta_f64 {
  uint64_t stride;
  double scalar;
} VAEMeta_f64;

typedef struct VAEFatPtr_f64 {
  double *ptr;
  union VAEMeta_f64 meta;
} VAEFatPtr_f64;

typedef union VAEMeta_i32 {
  uint64_t stride;
  int32_t scalar;
} VAEMeta_i32;

typedef struct VAEFatPtr_i32 {
  int32_t *ptr;
  union VAEMeta_i32 meta;
} VAEFatPtr_i32;

typedef void (*VAEVaeFun)(uintptr_t, struct VAEFatPtr_f64*, struct VAEFatPtr_f64*, struct VAEFatPtr_f64*, struct VAEFatPtr_i32*, const char**, struct VAEFatPtr_f64*, struct VAEFatPtr_i32*, struct VAEFatPtr_f64*, void*);

typedef struct VAESlice_u8 {
  uint8_t *ptr;
  uintptr_t len;
} VAESlice_u8;

typedef struct VAESlice_NativePath {
  VAENativePath *ptr;
  uintptr_t len;
} VAESlice_NativePath;

typedef struct VAESlice_Slice_u8 {
  struct VAESlice_u8 *ptr;
  uintptr_t len;
} VAESlice_Slice_u8;

typedef struct VAEVfsEntry {
  struct VAESlice_u8 name;
  struct VAESlice_u8 data;
} VAEVfsEntry;

typedef struct VAESlice_VfsEntry {
  struct VAEVfsEntry *ptr;
  uintptr_t len;
} VAESlice_VfsEntry;

typedef struct VAESlice_VfsEntry VAEVfs;

typedef struct VAEOpts {
  struct VAESlice_u8 model;
  VAENativePath cache_dir;
  struct VAESlice_NativePath include_dirs;
  struct VAESlice_Slice_u8 macro_flags;
  struct VAESlice_Slice_u8 allow_lints;
  struct VAESlice_Slice_u8 warn_lints;
  struct VAESlice_Slice_u8 deny_lints;
  enum VAEOptLevel opt_lvl;
  struct VAESlice_u8 target_cpu;
  struct VAESlice_u8 target;
  struct VAESlice_Slice_u8 cg_flags;
  VAEVfs vfs;
} VAEOpts;

/**
 *This function returns a pointer to the `functions` global
 * of a VerilogAE model loaded with `load`.
 *
 * # Safety
 *
 * `lib` must be a valid pointer returned by the `load` functions or `dlopen`
 */
const char *const *verilogae_functions(const void *lib);

/**
 *This function returns a pointer to the `functions.sym` global
 * of a VerilogAE model loaded with `load`.
 *
 * # Safety
 *
 * `lib` must be a valid pointer returned by the `load` functions or `dlopen`
 */
const char *const *verilogae_function_symbols(const void *lib);

/**
 *This function returns a pointer to the `opvars` global
 * of a VerilogAE model loaded with `load`.
 *
 * # Safety
 *
 * `lib` must be a valid pointer returned by the `load` functions or `dlopen`
 */
const char *const *verilogae_opvars(const void *lib);

/**
 *This function returns a pointer to the `params.real` global
 * of a VerilogAE model loaded with `load`.
 *
 * # Safety
 *
 * `lib` must be a valid pointer returned by the `load` functions or `dlopen`
 */
const char *const *verilogae_real_params(const void *lib);

/**
 *This function returns a pointer to the `params.unit.real` global
 * of a VerilogAE model loaded with `load`.
 *
 * # Safety
 *
 * `lib` must be a valid pointer returned by the `load` functions or `dlopen`
 */
const char *const *verilogae_real_param_units(const void *lib);

/**
 *This function returns a pointer to the `params.desc.real` global
 * of a VerilogAE model loaded with `load`.
 *
 * # Safety
 *
 * `lib` must be a valid pointer returned by the `load` functions or `dlopen`
 */
const char *const *verilogae_real_param_descriptions(const void *lib);

/**
 *This function returns a pointer to the `params.group.real` global
 * of a VerilogAE model loaded with `load`.
 *
 * # Safety
 *
 * `lib` must be a valid pointer returned by the `load` functions or `dlopen`
 */
const char *const *verilogae_real_param_groups(const void *lib);

/**
 *This function returns a pointer to the `params.integer` global
 * of a VerilogAE model loaded with `load`.
 *
 * # Safety
 *
 * `lib` must be a valid pointer returned by the `load` functions or `dlopen`
 */
const char *const *verilogae_int_params(const void *lib);

/**
 *This function returns a pointer to the `params.unit.integer` global
 * of a VerilogAE model loaded with `load`.
 *
 * # Safety
 *
 * `lib` must be a valid pointer returned by the `load` functions or `dlopen`
 */
const char *const *verilogae_int_param_units(const void *lib);

/**
 *This function returns a pointer to the `params.desc.integer` global
 * of a VerilogAE model loaded with `load`.
 *
 * # Safety
 *
 * `lib` must be a valid pointer returned by the `load` functions or `dlopen`
 */
const char *const *verilogae_int_param_descriptions(const void *lib);

/**
 *This function returns a pointer to the `params.group.integer` global
 * of a VerilogAE model loaded with `load`.
 *
 * # Safety
 *
 * `lib` must be a valid pointer returned by the `load` functions or `dlopen`
 */
const char *const *verilogae_int_param_groups(const void *lib);

/**
 *This function returns a pointer to the `params.string` global
 * of a VerilogAE model loaded with `load`.
 *
 * # Safety
 *
 * `lib` must be a valid pointer returned by the `load` functions or `dlopen`
 */
const char *const *verilogae_str_params(const void *lib);

/**
 *This function returns a pointer to the `params.unit.string` global
 * of a VerilogAE model loaded with `load`.
 *
 * # Safety
 *
 * `lib` must be a valid pointer returned by the `load` functions or `dlopen`
 */
const char *const *verilogae_str_param_units(const void *lib);

/**
 *This function returns a pointer to the `params.desc.string` global
 * of a VerilogAE model loaded with `load`.
 *
 * # Safety
 *
 * `lib` must be a valid pointer returned by the `load` functions or `dlopen`
 */
const char *const *verilogae_str_param_descriptions(const void *lib);

/**
 *This function returns a pointer to the `params.group.string` global
 * of a VerilogAE model loaded with `load`.
 *
 * # Safety
 *
 * `lib` must be a valid pointer returned by the `load` functions or `dlopen`
 */
const char *const *verilogae_str_param_groups(const void *lib);

/**
 *This function returns a pointer to the `nodes` global
 * of a VerilogAE model loaded with `load`.
 *
 * # Safety
 *
 * `lib` must be a valid pointer returned by the `load` functions or `dlopen`
 */
const char *const *verilogae_nodes(const void *lib);

/**
 *This function returns the value stored in the `functions.cnt` global
 * of a VerilogAE model loaded with `load`.
 *
 * # Safety
 *
 * `lib` must be a valid pointer returned by the `load` functions or `dlopen`
 */
uintptr_t verilogae_function_cnt(const void *lib);

/**
 *This function returns the value stored in the `opvars.cnt` global
 * of a VerilogAE model loaded with `load`.
 *
 * # Safety
 *
 * `lib` must be a valid pointer returned by the `load` functions or `dlopen`
 */
uintptr_t verilogae_opvars_cnt(const void *lib);

/**
 *This function returns the value stored in the `params.real.cnt` global
 * of a VerilogAE model loaded with `load`.
 *
 * # Safety
 *
 * `lib` must be a valid pointer returned by the `load` functions or `dlopen`
 */
uintptr_t verilogae_real_param_cnt(const void *lib);

/**
 *This function returns the value stored in the `params.integer.cnt` global
 * of a VerilogAE model loaded with `load`.
 *
 * # Safety
 *
 * `lib` must be a valid pointer returned by the `load` functions or `dlopen`
 */
uintptr_t verilogae_int_param_cnt(const void *lib);

/**
 *This function returns the value stored in the `params.string.cnt` global
 * of a VerilogAE model loaded with `load`.
 *
 * # Safety
 *
 * `lib` must be a valid pointer returned by the `load` functions or `dlopen`
 */
uintptr_t verilogae_str_param_cnt(const void *lib);

/**
 *This function returns the value stored in the `nodes.cnt` global
 * of a VerilogAE model loaded with `load`.
 *
 * # Safety
 *
 * `lib` must be a valid pointer returned by the `load` functions or `dlopen`
 */
uintptr_t verilogae_node_cnt(const void *lib);

/**
 *This function returns a pointer to the `params.real` global
 * of a VerilogAE model loaded with `load`.
 *
 * # Safety
 *
 * `lib` must be a valid pointer returned by the `load` functions or `dlopen`
 *`sym_name` must batch the schema fun.{NUM}params.real
 */
const char *const *verilogae_real_fun_params(const void *lib, const char *fun);

/**
 *This function returns a pointer to the `params.integer` global
 * of a VerilogAE model loaded with `load`.
 *
 * # Safety
 *
 * `lib` must be a valid pointer returned by the `load` functions or `dlopen`
 *`sym_name` must batch the schema fun.{NUM}params.integer
 */
const char *const *verilogae_int_fun_params(const void *lib, const char *fun);

/**
 *This function returns a pointer to the `params.string` global
 * of a VerilogAE model loaded with `load`.
 *
 * # Safety
 *
 * `lib` must be a valid pointer returned by the `load` functions or `dlopen`
 *`sym_name` must batch the schema fun.{NUM}params.string
 */
const char *const *verilogae_str_fun_params(const void *lib, const char *fun);

/**
 *This function returns a pointer to the `depbreak.real` global
 * of a VerilogAE model loaded with `load`.
 *
 * # Safety
 *
 * `lib` must be a valid pointer returned by the `load` functions or `dlopen`
 *`sym_name` must batch the schema fun.{NUM}depbreak.real
 */
const char *const *verilogae_real_fun_depbreak(const void *lib, const char *fun);

/**
 *This function returns a pointer to the `depbreak.integer` global
 * of a VerilogAE model loaded with `load`.
 *
 * # Safety
 *
 * `lib` must be a valid pointer returned by the `load` functions or `dlopen`
 *`sym_name` must batch the schema fun.{NUM}depbreak.integer
 */
const char *const *verilogae_int_fun_depbreak(const void *lib, const char *fun);

/**
 *This function returns a pointer to the `voltages` global
 * of a VerilogAE model loaded with `load`.
 *
 * # Safety
 *
 * `lib` must be a valid pointer returned by the `load` functions or `dlopen`
 *`sym_name` must batch the schema fun.{NUM}voltages
 */
const char *const *verilogae_fun_voltages(const void *lib, const char *fun);

/**
 *This function returns a pointer to the `currents` global
 * of a VerilogAE model loaded with `load`.
 *
 * # Safety
 *
 * `lib` must be a valid pointer returned by the `load` functions or `dlopen`
 *`sym_name` must batch the schema fun.{NUM}currents
 */
const char *const *verilogae_fun_currents(const void *lib, const char *fun);

/**
 *This function returns a pointer to the `voltages.default` global
 * of a VerilogAE model loaded with `load`.
 *
 * # Safety
 *
 * `lib` must be a valid pointer returned by the `load` functions or `dlopen`
 *`sym_name` must batch the schema fun.{NUM}voltages.default
 */
const double *verilogae_fun_voltage_defaults(const void *lib, const char *fun);

/**
 *This function returns a pointer to the `currents.default` global
 * of a VerilogAE model loaded with `load`.
 *
 * # Safety
 *
 * `lib` must be a valid pointer returned by the `load` functions or `dlopen`
 *`sym_name` must batch the schema fun.{NUM}currents.default
 */
const double *verilogae_fun_current_defaults(const void *lib, const char *fun);

/**
 *This funprefix_with_name = falsection returns a pointer to the `params.real.cnt` global
 * of a VerilogAE model loaded with `load`.
 *
 * # Safety
 *
 * `lib` must be a valid pointer returned by the `load` functions or `dlopen`
 */
uintptr_t verilogae_real_fun_param_cnt(const void *lib, const char *fun);

/**
 *This funprefix_with_name = falsection returns a pointer to the `params.integer.cnt` global
 * of a VerilogAE model loaded with `load`.
 *
 * # Safety
 *
 * `lib` must be a valid pointer returned by the `load` functions or `dlopen`
 */
uintptr_t verilogae_int_fun_param_cnt(const void *lib, const char *fun);

/**
 *This funprefix_with_name = falsection returns a pointer to the `params.string.cnt` global
 * of a VerilogAE model loaded with `load`.
 *
 * # Safety
 *
 * `lib` must be a valid pointer returned by the `load` functions or `dlopen`
 */
uintptr_t verilogae_str_fun_param_cnt(const void *lib, const char *fun);

/**
 *This funprefix_with_name = falsection returns a pointer to the `depbreak.real.cnt` global
 * of a VerilogAE model loaded with `load`.
 *
 * # Safety
 *
 * `lib` must be a valid pointer returned by the `load` functions or `dlopen`
 */
uintptr_t verilogae_real_fun_depbreak_cnt(const void *lib, const char *fun);

/**
 *This funprefix_with_name = falsection returns a pointer to the `depbreak.integer.cnt` global
 * of a VerilogAE model loaded with `load`.
 *
 * # Safety
 *
 * `lib` must be a valid pointer returned by the `load` functions or `dlopen`
 */
uintptr_t verilogae_int_fun_depbreak_cnt(const void *lib, const char *fun);

/**
 *This funprefix_with_name = falsection returns a pointer to the `voltages.cnt` global
 * of a VerilogAE model loaded with `load`.
 *
 * # Safety
 *
 * `lib` must be a valid pointer returned by the `load` functions or `dlopen`
 */
uintptr_t verilogae_fun_voltage_cnt(const void *lib, const char *fun);

/**
 *This funprefix_with_name = falsection returns a pointer to the `currents.cnt` global
 * of a VerilogAE model loaded with `load`.
 *
 * # Safety
 *
 * `lib` must be a valid pointer returned by the `load` functions or `dlopen`
 */
uintptr_t verilogae_fun_current_cnt(const void *lib, const char *fun);

/**
 *This funprefix_with_name = falsection returns a pointer to the `voltages.default.cnt` global
 * of a VerilogAE model loaded with `load`.
 *
 * # Safety
 *
 * `lib` must be a valid pointer returned by the `load` functions or `dlopen`
 */
uintptr_t verilogae_fun_voltage_default_cnt(const void *lib, const char *fun);

/**
 *This funprefix_with_name = falsection returns a pointer to the `currents.default.cnt` global
 * of a VerilogAE model loaded with `load`.
 *
 * # Safety
 *
 * `lib` must be a valid pointer returned by the `load` functions or `dlopen`
 */
uintptr_t verilogae_fun_current_default_cnt(const void *lib, const char *fun);

/**
 * Obtains a pointer to the modelcard initialization function of a VerilogAE model loaded with `load`.
 *
 * # Safety
 *
 * `lib` must be a valid pointer returned by the `load` functions or `dlopen`
 */
VAEModelcardInit verilogae_init_modelcard(const void *lib);

/**
 * Obtains a pointer to a model functions of a VerilogAE model loaded with `load`.
 *
 * # Safety
 *
 * `lib` must be a valid pointer returned by the `load` functions or `dlopen`
 */
VAEVaeFun verilogae_fun_ptr(const void *lib, const char *fun);

/**
 * # Safety
 * handle must be a valid model compiled with VerilogAE
 */
const char *verilogae_module_name(const void *lib);

/**
 * # Safety
 *
 * All required parameters must be initialized appropriately
 */
int32_t verilogae_call_fun_parallel(VAEVaeFun fun,
                                    uintptr_t cnt,
                                    struct VAEFatPtr_f64 *voltages,
                                    struct VAEFatPtr_f64 *currents,
                                    struct VAEFatPtr_f64 *real_params,
                                    struct VAEFatPtr_i32 *int_params,
                                    const char **str_params,
                                    struct VAEFatPtr_f64 *real_dep_break,
                                    struct VAEFatPtr_i32 *int_dep_break,
                                    struct VAEFatPtr_f64 *temp,
                                    void *out);

struct VAEOpts *verilogae_new_opts(void);

/**
 * # Safety
 * `opts` must be a valid pointer created with `new_opts`
 */
void verilogae_free_opts(struct VAEOpts *opts);

/**
 * # Safety
 * * path must be valid for reads
 * * opts must be valid for reads or null
 * * opts must only contain valid data
 */
VAEVfs verilogae_export_vfs(VAENativePath path, const struct VAEOpts *opts);

/**
 * # Safety
 * * path must be valid for reads
 * * opts must be valid for reads or null
 * * opts must only contain valid data
 */
void verilogae_free_vfs(VAEVfs vfs);

/**
 * # Safety
 * * path must be valid for reads
 * * opts must be valid for reads or null
 * * opts must only contain valid data
 */
const void *verilogae_load(VAENativePath path, bool full_compile, const struct VAEOpts *opts);