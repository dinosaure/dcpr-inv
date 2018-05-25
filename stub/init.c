#include <caml/callback.h>

__attribute__ ((__constructor___))
void
init(void)
{
  char *caml_argv[1] = { NULL };
  caml_startup(caml_argv);
}
