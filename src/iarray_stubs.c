#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <string.h>

CAMLprim value Base_iarray_to_array_of_immediates(value src, value len) {
  /* Allocate a local array and copy all elements from an iarray to it.
     This is safe for immediate types (int64-sized) since they don't require
     caml_modify for GC tracking. */
  mlsize_t n = Long_val(len);
  value dst = caml_alloc_local(n, 0);
  memcpy((void *)dst, (const void *)src, n * sizeof(value));
  return dst;
}
