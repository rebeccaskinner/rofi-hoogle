#ifndef __ROFI_HOOGLE_HS_H__
#define __ROFI_HOOGLE_HS_H__
#include <stdint.h>

void hs_hoogle_search_init(void);
void hs_hoogle_search_end(void);
char* search_hoogle(const char *search_string);

struct query_info {
  int16_t paren_count;
  int16_t bracket_count;
  int16_t trailing_spaces;
  int16_t total_length;
};

typedef struct query_info query_info_t;

struct hoogle_secondary_result {
  char *secondary_result_url;
  char *secondary_result_package; // may be NULL
  char *secondary_result_module;  // may be NULL
  struct hoogle_secondary_result *next;
}__attribute__((packed));

typedef struct hoogle_secondary_result hoogle_secondary_result_t;

struct hoogle_search_result {
  char *search_result_name;
  char *search_result_primary_url;
  char *search_result_primary_package; // may be NULL
  char *search_result_primary_module;  // may be NULL
  uint32_t secondary_search_result_count;
  hoogle_secondary_result_t *secondary_results;
} __attribute__((packed));

typedef struct hoogle_search_result hoogle_search_result_t;

struct hoogle_result_set {
  hoogle_search_result_t search_result;
  struct hoogle_result_set *next;
}__attribute__((packed));

typedef struct hoogle_result_set hoogle_result_set_t;

struct hoogle_search_state {
  hoogle_result_set_t *results;
  uint32_t result_count;
//  struct query_info last_query_info;
};

typedef struct hoogle_search_state hoogle_search_state_t;

hoogle_search_state_t* hs_preprocess_input(const char* input);

#endif
