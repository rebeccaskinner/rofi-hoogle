#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <stdint.h>

/* glib */
#include <gmodule.h>

/* rofi */
#include <rofi/mode.h>
#include <rofi/helper.h>
#include <rofi/mode-private.h>

#include <rofi_hoogle_hs.h>

G_MODULE_EXPORT Mode mode;

#define UNUSED __attribute__((unused))

static int hs_runtime_is_started = 0;

static void start_hs_runtime() {
  if (hs_runtime_is_started) {
    return;
  }
  hs_hoogle_search_init();
  hs_runtime_is_started = 1;
}

static void stop_hs_runtime() {
  if (!hs_runtime_is_started) {
    return;
  }
  hs_hoogle_search_end();
  hs_runtime_is_started = 0;
}

typedef struct {
  char **array;
  unsigned int array_length;
} hoogle_mode_private_data;

static void get_hoogle_plugin(UNUSED Mode *sw) {}

static int hoogle_plugin_mode_init(UNUSED Mode *sw) {
  start_hs_runtime();
  if (NULL == mode_get_private_data(sw)) {
    hoogle_mode_private_data *private_data = g_malloc0(sizeof(*private_data));
    mode_set_private_data(sw, (void*) private_data);
    get_hoogle_plugin(sw);
  }
  return TRUE;
}

static unsigned int hoogle_plugin_get_num_entries(const Mode *sw) {
  const hoogle_mode_private_data *private_data =
      (const hoogle_mode_private_data *)mode_get_private_data(sw);
  return private_data->array_length;

  //const hoogle_search_state_t *search_state = mode_get_private_data(sw);

}

static ModeMode hoogle_plugin_mode_result(
  Mode *sw,
  int mretv,
  UNUSED char **input,
  UNUSED unsigned int selected_line
  ) {
  ModeMode retv = MODE_EXIT;
  hoogle_mode_private_data *private_data = (hoogle_mode_private_data*) mode_get_private_data(sw);

  if ( mretv & MENU_NEXT ) {retv = NEXT_DIALOG;}
  else if ( mretv & MENU_PREVIOUS ) {retv = PREVIOUS_DIALOG;}
  else if ( mretv & MENU_QUICK_SWITCH ) {retv = ( mretv & MENU_LOWER_MASK );}
  else if ( mretv & MENU_OK ) {retv = RELOAD_DIALOG;}
  else if ( (mretv & MENU_ENTRY_DELETE) == MENU_ENTRY_DELETE ) {retv = RELOAD_DIALOG;}
  return retv;
}

static void hoogle_plugin_mode_destroy(Mode *sw) {
  hoogle_mode_private_data *private_data = (hoogle_mode_private_data*) mode_get_private_data(sw);
  if (NULL != private_data) {
    g_free(private_data);
    mode_set_private_data(sw, NULL);
  }
  stop_hs_runtime();
}

int skip_not_ready(const char* input) {
  int paren_count = 0;
  int last_space = 0;
  int total_len = 0;
  for (const char *s = input; *s != 0; s++) {
    switch (*s) {
    case ' ':
      last_space++;
      break;
    case '(':
      last_space = 0;
      paren_count++;
      break;
    case ')':
      last_space = 0;
      paren_count--;
      break;
    default:
      last_space = 0;
      break;
    }
    total_len++;
  }

  // If the parentheses are unbalanced then never try to autocomplete.
  // If they are balanced, autocomplete if the string ends with a
  // double-space, or if it does not end with a space at all but is at
  // least 15 characters long.
  int do_process = (paren_count == 0) && ((last_space >= 2) || ((last_space == 0) && total_len >= 15));
  printf("do_process: %d, total_len: %d, last_space: %d, paren_count: %d\n", do_process, total_len, last_space, paren_count);
  return !do_process;
}

void debug_hoogle_search_state(const hoogle_search_state_t *state) {
  if (NULL == state) {
    printf("search state is NULL\n");
    return;
  }
  printf("%d search results\n", state->result_count);
  for (hoogle_result_set_t *result = state->results; result != NULL; result = result->next) {
    printf("result info:\n");
    printf("\tsearch_result_url: %s\n", result->search_result.search_result_url);
    printf("\tsearch_result_name: %s\n", result->search_result.search_result_name);
    printf("\tsearch_result_type: %s\n", result->search_result.search_result_type);
    printf("\tsearch_result_item: %s\n", result->search_result.search_result_item);
    printf("\tsearch_result_doc_preview: %s\n", result->search_result.search_result_doc_preview);
    printf("\n\n");
  }
}


static char *hoogle_plugin_preprocess_input(UNUSED Mode *sw, const char *input) {
  /* hoogle_mode_private_data *private_data = (hoogle_mode_private_data*) mode_get_private_data(sw); */
  /* // skip search when there are unbalanced parentheses */
  /* if (skip_not_ready(input)) { */
  /*   printf("input not ready, skipping"); */
  /*   return g_strdup(input); */
  /* } */

  /* printf("starting to preprocess input\n"); */
  /* char* result = search_hoogle(input); */
  /* printf("input: %s\n", input); */
  /* printf("result: %s\n", result); */
  /* printf("finished preprocessing input\n"); */
  hoogle_search_state_t *result_state = hs_preprocess_input(input);
  debug_hoogle_search_state(result_state);
  return g_strdup(input);
}



static char *hoogle_plugin_get_display_value(
  const Mode *sw,
  UNUSED unsigned int selected_line,
  UNUSED int *state,
  UNUSED GList **attr_list,
  int get_entry
  ) {
  hoogle_mode_private_data *private_data = (hoogle_mode_private_data *) mode_get_private_data(sw);
  return get_entry ? g_strdup("n/a") : NULL;
}

static int hoogle_plugin_token_match(
  UNUSED const Mode *sw,
  UNUSED rofi_int_matcher **tokens,
  UNUSED unsigned int index
  ) {
  return TRUE;
}

/* formats and displays a mardown rendering of the final result */
static char *hoogle_plugin_get_message(UNUSED const Mode *sw) {
  return g_strdup("this is a message\nand another message");
}

Mode mode = {
  .abi_version = ABI_VERSION,
  .name = "hoogle",
  .cfg_name_key = "display-hoogle",
  ._init = hoogle_plugin_mode_init,
  ._get_num_entries = hoogle_plugin_get_num_entries,
  ._result = hoogle_plugin_mode_result,
  ._destroy = hoogle_plugin_mode_destroy,
  ._token_match = hoogle_plugin_token_match,
  ._get_display_value = hoogle_plugin_get_display_value,
  ._get_message = hoogle_plugin_get_message,
  ._get_completion = NULL,
  ._preprocess_input = hoogle_plugin_preprocess_input,
  .private_data = NULL,
  .free = NULL,
};
