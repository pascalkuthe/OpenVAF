#include <math.h>
#include <stdio.h>
#ifdef OSDI_0_3
#include "header/osdi_0_3.h"
#endif

// no header was included explictly so just use the newest version
#ifndef OSDI_VERSION_MAJOR_CURR
#include "header/osdi_0_3.h"
#endif

#include "stdlib.h"
#include "string.h"

char *concat(const char *s1, const char *s2) {
  const size_t len1 = strlen(s1);
  const size_t len2 = strlen(s2);
  char *result = malloc(len1 + len2 + 1);
  if (result == NULL) {
    return NULL;
  }
  memcpy(result, s1, len1);
  memcpy(result + len1, s2, len2 + 1);
  return result;
}

typedef void (*osdi_log_ptr)(void *handle, char *msg, uint32_t lvl);
extern osdi_log_ptr osdi_log;

double simparam(void *params_, void *handle, uint32_t *flags, char *name) {
  OsdiSimParas *params = params_;
  for (int i = 0; params->names[i]; i++) {
    if (strcmp(params->names[i], name) == 0) {
      return params->vals[i];
    }
  }
  *flags |= EVAL_RET_FLAG_FATAL;
  char *msg = concat("unkown $simparam", name);
  if (msg == NULL) {
    osdi_log(handle, "unkown $simparam %s", LOG_LVL_FATAL | LOG_FMT_ERR);
  } else {
    osdi_log(handle, msg, LOG_LVL_FATAL);
  }
  return 0.0;
}

double simparam_opt(void *params_, char *name, double default_val) {
  OsdiSimParas *params = params_;
  for (int i = 0; params->names[i]; i++) {
    if (strcmp(params->names[i], name) == 0) {
      return params->vals[i];
    }
  }
  return default_val;
}

extern int strcmp(const char *__s1, const char *__s2);

char *simparam_str(void *params_, void *handle, uint32_t *flags, char *name) {
  OsdiSimParas *params = params_;
  for (int i = 0; params->names[i]; i++) {
    if (strcmp(params->names_str[i], name) == 0) {
      return params->names_str[i];
    }
  }
  *flags |= EVAL_RET_FLAG_FATAL;

  char *msg = concat("unkown $simparam_str", name);
  if (msg == NULL) {
    osdi_log(handle, "unkown $simparam_str %s", LOG_LVL_FATAL | LOG_FMT_ERR);
  } else {
    osdi_log(handle, msg, LOG_LVL_FATAL);
  }

  return "�";
}

void push_error(OsdiInitError **dst, uint32_t *len, uint32_t *cap,
                OsdiInitError err) {
  if (*dst == NULL) {
    *cap = 8;
    *dst = malloc(8 * sizeof(OsdiInitError));
  } else if (*cap <= *len) {
    *cap = 2 * (*len);
    *dst = realloc(*dst, *cap * sizeof(OsdiInitError));
  }

  (*dst)[*len] = err;
  *len += 1;
}

void push_invalid_param_err(void **dst, uint32_t *len, uint32_t *cap,
                            uint32_t param) {
  OsdiInitError err = (OsdiInitError){
      .code = INIT_ERR_OUT_OF_BOUNDS,
      .payload =
          (OsdiInitErrorPayload){
              .parameter_id = param,
          },
  };

  push_error((OsdiInitError **)dst, len, cap, err);
}

void bound_step(double *dst, double val) { *dst = val; }

#define FMT_OFF 6
#define NUM_FMT 11
const char FMT_CHARS[NUM_FMT] = {'a', 'f', 'p', 'n', 'u', 'm',
                                 ' ', 'k', 'M', 'G', 'T'};
const double EXP[NUM_FMT] = {1e18, 1e15, 1e12, 1e9,  1e6,  1e3,
                             1,    1e-3, 1e-6, 1e-9, 1e-12};
int fmt_char_idx(double val) {
  int exp = ((int)log(val)) / 3;
  int pos = exp + NUM_FMT;

  if (pos < 0) {
    return 0;
  }
  if (pos >= NUM_FMT) {
    return NUM_FMT - 1;
  }
  return pos;
}

char *fmt_binary(int val) {
  int len = 32 - __builtin_clz(val);
  char *res = malloc(len + 1);
  res[len] = '\0';
  if (len == 0) {
    return res;
  }
  for (int i = 1; i < len + 1; i++) {
    if (val & 1) {
      res[len - i] = '1';
    } else {
      res[len - i] = '0';
    }
    val >>= 1;
  }

  return res;
}

void lim_discontinuity(int *flags) { *flags |= EVAL_RET_FLAG_LIM; }

double store_lim(void *sim_info_, int idx, double val) {
  OsdiSimInfo *sim_info = (OsdiSimInfo *)sim_info_;
  sim_info->next_state[idx] = val;
  return val;
}

int analysis(void *sim_info_, char *name) {
  OsdiSimInfo *sim_info = (OsdiSimInfo *)sim_info_;
  uint32_t flags = sim_info->flags;
  return ((flags & ANALYSIS_AC) && strcmp(name, "ac")) ||
         ((flags & ANALYSIS_DC) && strcmp(name, "dc")) ||
         ((flags & ANALYSIS_NOISE) && strcmp(name, "noise")) ||
         ((flags & ANALYSIS_TRAN) && strcmp(name, "tran")) ||
         ((flags & ANALYSIS_IC) && strcmp(name, "ic")) ||
         ((flags & ANALYSIS_STATIC) && strcmp(name, "static")) ||
         ((flags & ANALYSIS_NODESET) && strcmp(name, "nodeset"));
}

double store_delay(void *sim_info_, double *dst, double val) {
  OsdiSimInfo *sim_info = (OsdiSimInfo *)sim_info_;
  if (sim_info->flags & ANALYSIS_IC) {
    *dst = val;
    return val;
  }

  return *dst;
}
