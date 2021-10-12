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


static hoogle_result_set_t* get_nth_result(hoogle_search_state_t *state, unsigned int offset) {
  printf("Getting the %d result\n", offset);
  hoogle_result_set_t *result = NULL;
  if (NULL == state) {return NULL;}
  if (offset >= state->result_count) { return NULL; }
  result = state->results;
  for (; offset > 0; offset--) {
    if (NULL == result) {
      printf("unexpected null at offset %d\n", offset);
      return NULL;
    }
    result = result->next;
  }
  return result;
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

static void get_hoogle_plugin(UNUSED Mode *sw) {}

static int hoogle_plugin_mode_init(UNUSED Mode *sw) {
  start_hs_runtime();
  if (NULL == mode_get_private_data(sw)) {
    get_hoogle_plugin(sw);
  }
  return TRUE;
}

static unsigned int hoogle_plugin_get_num_entries(const Mode *sw) {
  printf("checking number of entries...\n");
  hoogle_search_state_t *private_data = mode_get_private_data(sw);
  if (NULL == private_data) {
    return 0;
  }
  printf("getting result count...\n");
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
    hoogle_result_set_t *result = get_nth_result(private_data, selected_line);
    if (NULL == result) {
      return MODE_EXIT;
    }

    char* command = g_strdup_printf("xdg-open %s", result->search_result.search_result_primary_url);
    g_spawn_command_line_sync(command, NULL, NULL, NULL, NULL);
    retv = MODE_EXIT;
  }
  else if ( (mretv & MENU_ENTRY_DELETE) == MENU_ENTRY_DELETE ) {retv = RELOAD_DIALOG;}
  return retv;
}

static void hoogle_plugin_mode_destroy(UNUSED Mode *sw) {
  stop_hs_runtime();
}

extern void rofi_view_reload(void);

static void swap_result_buffer(const void *old_state) {
  static const void *last_known_state = NULL;
  if (old_state != last_known_state) {
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
  printf("hoogle_plugin_get_display_value...\n");

  *state |= 8;
  if (!get_entry) { return NULL; }

  printf("getting private data...\n");
  hoogle_search_state_t *private_data = mode_get_private_data(sw);
  printf("got private data...\n");

  if (NULL == private_data) {
    printf("calling hoogle_plugin_get_display_value...no private data, exiting...\n");
    return g_strdup("n/a");
  }

  printf("getting nth result...\n");
  hoogle_result_set_t *result = get_nth_result(private_data, selected_line);
  printf("got nth result...\n");

  if (NULL == result) {
    printf("cannot get %d result\n", selected_line);
    return g_strdup("n/a");
  }

  printf("got a result, showing it...\n");

  return(
    g_markup_printf_escaped(
      "<span font_weight=\"bold\">%s</span>\r<span size=\"x-small\" font_weight=\"light\">%s %s (and %d more...)</span>",
      result->search_result.search_result_name,
      STR_OR(result->search_result.search_result_primary_package, ""),
      STR_OR(result->search_result.search_result_primary_module,""),
      result->search_result.secondary_search_result_count
      )
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
  if (NULL == private_data) {
    return NULL;
  }

  if (0 < private_data->result_count) {
    return NULL;
  }

  /* query_info_t qinfo = private_data->last_query_info; */

  /* if (qinfo.total_length == 0) { */
  /*   return NULL; */
  /* } */

  /* if (qinfo.paren_count >= 1) { */
  /*   return g_strdup("Missing closing paren: ')'"); */
  /* } else if (qinfo.paren_count < 0) { */
  /*   return g_strdup("Missing opening paren: '('"); */
  /* } */

  /* if (qinfo.bracket_count >= 1) { */
  /*   return g_strdup("Missing closing bracket: ']'"); */
  /* } else if (qinfo.bracket_count < 0) { */
  /*   return g_strdup("Missing opening bracket: '['"); */
  /* } */

  /* if (qinfo.total_length <= 15 && qinfo.trailing_spaces < 2) { */
  /*   return g_strdup_printf( */
  /*     "Type %d more characters to auto-search\r(you can add two trailing spaces to your query to search immediately)", */
  /*     (15 - qinfo.total_length) */
  /*     ); */
  /* } */

  return g_strdup("Sorry, no results found for that query");
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
