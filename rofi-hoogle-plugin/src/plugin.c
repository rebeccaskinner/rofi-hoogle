#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <stdint.h>

#define STR_OR(s,def) ((NULL == s) ? (def) : (s))

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

static hoogle_search_result_t* get_nth_result(hoogle_search_state_t *state, unsigned int offset) {
  if (NULL == state) { return NULL; }
  if (offset >= state->result_count) { return NULL; }
  return &state->results[offset];
}

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
    hoogle_search_state_t *initial = hs_initial_state();
    if (NULL != initial) {
      mode_set_private_data(sw, initial);
    } else {
      get_hoogle_plugin(sw);
    }
  }
  return TRUE;
}

static unsigned int hoogle_plugin_get_num_entries(const Mode *sw) {
  printf("checking number of entries...\n");
  hoogle_search_state_t *private_data = mode_get_private_data(sw);
  if (NULL == private_data) {
    return 0;
  }
  return private_data->result_count;
}

static ModeMode hoogle_plugin_mode_result(
  Mode *sw,
  int mretv,
  UNUSED char **input,
  unsigned int selected_line
  ) {
  printf("hoogle_plugin_mode_result\n");
  ModeMode retv = MODE_EXIT;
  hoogle_search_state_t *private_data = mode_get_private_data(sw);


  if ( mretv & MENU_NEXT ) {retv = NEXT_DIALOG;}
  else if ( mretv & MENU_PREVIOUS ) {retv = PREVIOUS_DIALOG;}
  else if ( mretv & MENU_QUICK_SWITCH ) {retv = ( mretv & MENU_LOWER_MASK );}
  else if ( mretv & MENU_OK ) {
    hoogle_search_result_t *result = get_nth_result(private_data, selected_line);
    if (NULL == result) {
      return MODE_EXIT;
    }

    char* command = g_strdup_printf("xdg-open %s", result->search_result_primary_url);
    g_spawn_command_line_sync(command, NULL, NULL, NULL, NULL);
    // `command` intentionally not freed: we return MODE_EXIT and rofi exits.
    retv = MODE_EXIT;
  }
  else if ( (mretv & MENU_ENTRY_DELETE) == MENU_ENTRY_DELETE ) {retv = RELOAD_DIALOG;}
  return retv;
}

static void hoogle_plugin_mode_destroy(UNUSED Mode *sw) {
  // private_data (and the Haskell-side lastResults pointer) is not freed:
  // mode_destroy runs as rofi exits, so the OS reclaims it.
  stop_hs_runtime();
}

void debug_hoogle_search_state(const hoogle_search_state_t *state) {
  if (NULL == state) {
    printf("search state is NULL\n");
    return;
  }
  printf("%d search results\n", state->result_count);
  for (unsigned int i = 0; i < state->result_count; i++) {
    const hoogle_search_result_t *result = &state->results[i];
    printf("result info for: %s\n", result->search_result_name);
    printf("url: %s\n", result->search_result_primary_url);
    printf("package: %s\n", STR_OR(result->search_result_primary_package, "no package"));
    printf("module: %s\n\n", STR_OR(result->search_result_primary_module, "no module"));
    printf("%d secondary matches\n", result->secondary_search_result_count);
    printf("\n\n");
  }
}

extern void rofi_view_reload(void);

static void swap_result_buffer(const void *old_state) {
  static const void *last_known_state = NULL;
  if (old_state != last_known_state) {
    printf("flipping buffer");
    last_known_state = old_state;
    rofi_view_reload();
  }
}

static char *hoogle_plugin_preprocess_input(Mode *sw, const char *input) {
  hoogle_search_state_t *result_state = hs_preprocess_input(input);
  if (NULL != result_state) {
    mode_set_private_data(sw, NULL);
    mode_set_private_data(sw, result_state);
  }
  swap_result_buffer(result_state);
  return g_strdup(input);
}

static char *hoogle_plugin_get_display_value(
  const Mode *sw,
  unsigned int selected_line,
  UNUSED int *state,
  UNUSED GList **attr_list,
  int get_entry
  ) {

  *state |= 8;
  if (!get_entry) { return NULL; }
  hoogle_search_state_t *private_data = mode_get_private_data(sw);
  if (NULL == private_data) {
    printf("calling hoogle_plugin_get_display_value...no private data, exiting...\n");
    return g_strdup("n/a");
  }
  hoogle_search_result_t *result = get_nth_result(private_data, selected_line);
  if (NULL == result) {
    printf("cannot get %d result", selected_line);
    return g_strdup("n/a");
  }
  const char *package = result->search_result_primary_package;
  if (NULL == package) {
    return g_markup_printf_escaped(
      "<span font_weight=\"bold\">%s</span>",
      result->search_result_name
      );
  }
  return g_markup_printf_escaped(
    "<span font_weight=\"bold\">%s</span>  <span font_weight=\"light\" alpha=\"70%%\">(%s)</span>",
    result->search_result_name,
    package
    );
}

static int hoogle_plugin_token_match(
  UNUSED const Mode *sw,
  UNUSED rofi_int_matcher **tokens,
  UNUSED unsigned int index
  ) {
  return TRUE;
}

/* formats and displays a mardown rendering of the final result */
static char *hoogle_plugin_get_message(const Mode *sw) {
   hoogle_search_state_t *private_data = mode_get_private_data(sw);
   if (NULL != private_data && NULL != private_data->message) {
     return g_markup_escape_text(private_data->message, -1);
   }
   return g_strdup("search must be at least 15 characters (end with two spaces to search early)");
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
