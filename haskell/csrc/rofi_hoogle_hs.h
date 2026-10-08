#ifndef __ROFI_HOOGLE_HS_H__
#define __ROFI_HOOGLE_HS_H__

void hs_hoogle_search_init(void);
void hs_hoogle_search_end(void);
char* search_hoogle(const char *search_string);

struct hoogle_secondary_result {
  char *secondary_result_url;
  char *secondary_result_package; // may be NULL
  char *secondary_result_module;  // may be NULL
  struct hoogle_secondary_result *next;
};

typedef struct hoogle_secondary_result hoogle_secondary_result_t;

struct hoogle_search_result {
  char *search_result_name;
  char *search_result_primary_url;
  char *search_result_primary_package; // may be NULL
  char *search_result_primary_module;  // may be NULL
  int secondary_search_result_count;
  hoogle_secondary_result_t *secondary_results;
};

typedef struct hoogle_search_result hoogle_search_result_t;

struct hoogle_search_state {
  unsigned int result_count;
  char *message; // shown in place of the usage hint; NULL if none
  hoogle_search_result_t results[];
};

typedef struct hoogle_search_state hoogle_search_state_t;

hoogle_search_state_t* hs_preprocess_input(const char* input);

// Spawns the search worker and waits for the Hoogle database to load.
// Must be called once after hs_init.
void hs_search_init(void);

// Returns a state surfacing the DB load error if hs_search_init failed,
// otherwise NULL. Used by mode_init to populate private_data on startup.
hoogle_search_state_t* hs_initial_state(void);

#endif
