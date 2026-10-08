#include <stdlib.h>
#include "HsFFI.h"
#include "rofi_hoogle_hs.h"

void hs_hoogle_search_init(void) {
  int argc = 2;
  char* argv[] = { "+RTS", "-A64m", NULL };
  char **pargv = argv;
  hs_init(&argc, &pargv);
  hs_search_init();
}

void hs_hoogle_search_end(void) { hs_exit(); }
