#include "types.h"
#include "values.h"

type_t val_typeof(val_t x)
{
  switch (x) {
  case 0:
  case 1:
    return T_BOOL;
  }
  return T_INT;
}

int64_t val_unwrap_int(val_t x)
{
  return x < 0 ? x : x - 2;
}
val_t val_wrap_int(int64_t i)
{
  return i < 0 ? i : i + 2;
}
int val_unwrap_bool(val_t x)
{
  return x == val_true;
}
val_t val_wrap_bool(int b)
{
  return b ? val_true : val_false;
}
