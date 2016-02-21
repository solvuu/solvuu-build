#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <stdio.h>
#include "foo_stub.h"

value foo_stub(value unit) {
  printf("foo! %d\n", K);
  return 0;
}
