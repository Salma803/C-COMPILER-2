#include <stdio.h>

/*@ requires the size of tab is at least max(i,j)+1
  assigns tab[i], tab[j]
  ensures swap the content of t[i] and t[j]
 */
void swap(int *tab, int i, int j) {
  int tmp;
  tmp = tab[i];
  tab[i] = tab[j];
  tab[j] = tmp;
}

/*@requires tab is of size s
  assigns tab[0], tab[1], ..., tab[s-1]
  ensures sort tab */
void selection_sort(int *tab, int s) {
  int i, j, m;
  /*@loop invariant 
    for all 0 <= k < l < i, tab[k] <= tab[l]
    for all 0 <= k < i <= l, tab[k] <= tab[l]
 */
  for (i = 0; i < s - 1; i += 1) {
    m = i; // m is the index of the minimum value from i
    /*@ loop invariant
      for i <= k < j tab[k] >= tab[m] */
    for (j = i + 1; j < s; j += 1)
      if (tab[j] < tab[m]) m = j;
    swap(tab, i, m);
  }
}

/*@ requires tab is of size s and tab is sorted
  assigns nothing
  ensures returns the index of a case containing e in tab
  or -1 if tab does not contain e */
int dicho_indexof(int e, int *tab, int s) {
  int low, up, m;
  low = 0;
  up = s;
  /* loop variant up - low
     invariant 
     if there exists k st 0 <= k < s and tab[k] == e,
     then there exists k st low <= k < up and tab[k] == e
   */
  while (low < up) {
    m = (low + up) / 2;
    if (e == tab[m]) return m;
    else if (e < tab[m]) up = m;
    else low = m + 1;
  }
  return -1;
}

int main() {
  int *t;
  selection_sort(t, 10);
  dicho_indexof(8, t, 10);
  return 0;
}
