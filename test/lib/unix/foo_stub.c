#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <stdio.h>

value foo_stub(value unit) {
  printf("foo!\n");
  return 0;
}
