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

int log_msg(void *handle, uint32_t lvl, const char *restrict fmt, ...) {
  FILE *dst = osdi_init_log_message(handle, lvl);
  va_list ap;
  va_start(ap, fmt);
  int ret = vfprintf(dst, fmt, ap);
  osdi_finish_log_message(handle, dst, lvl);
  va_end(ap);
  return ret;
}

double simparam(void *params_, void *handle, uint32_t *flags, char *name) {
  OsdiSimParas *params = params_;
  for (int i = 0; params->names[i]; i++) {
    if (strcmp(params->names[i], name) == 0) {
      return params->vals[i];
    }
  }
  *flags |= EVAL_RET_FLAG_FATAL;
  log_msg(handle, LOG_LVL_FATAL, "unkown $simparam %s", name);
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

char *simparam_str(void *params_, void *handle, uint32_t *flags, char *name) {
  OsdiSimParas *params = params_;
  for (int i = 0; params->names[i]; i++) {
    if (strcmp(params->names_str[i], name) == 0) {
      return params->names_str[i];
    }
  }
  *flags |= EVAL_RET_FLAG_FATAL;
  log_msg(handle, LOG_LVL_FATAL, "unkown $simparam_str %s", name);
  return "";
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

  printf("len %i\n", *len);

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
