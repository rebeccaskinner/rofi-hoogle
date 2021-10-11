#ifndef __ROFI_HOOGLE_HS_H__
#define __ROFI_HOOGLE_HS_H__

void hs_hoogle_search_init(void);
void hs_hoogle_search_end(void);
char* search_hoogle(const char *search_string);

struct hoogle_search_result {
  char *search_result_url;
  char *search_result_name;
  char *search_result_type;
  char *search_result_item;
  char *search_result_doc_preview;
};

typedef struct hoogle_search_result hoogle_search_result_t;

struct hoogle_result_set {
  hoogle_search_result_t search_result;
  struct hoogle_result_set *next;
};

typedef struct hoogle_result_set hoogle_result_set_t;

struct hoogle_search_state {
  hoogle_result_set_t *results;
  unsigned int result_count;
};

typedef struct hoogle_search_state hoogle_search_state_t;

hoogle_search_state_t* hs_preprocess_input(const char* input);

#endif
