#include <stdio.h>
#include <string.h>
#include <time.h>
#include "regex.h"


void respeedtest(int numreps, const char* patstr, const char* candstr)
{
  regex_t comp;
  regmatch_t matches[10];
  int nomatch, err;
  time_t start, finish;
  int bMatched = -1;
  int candlen;
  float elapsed;
  int i;

  if (err = regcomp(&comp, patstr, REG_EXTENDED)) {
    char error[256];
    regerror(err,&comp,error,255);
    printf("Error in regcomp: %s\n",error);
    return;
  }

  fprintf(stdout, "\nTiming %s\n", patstr); fflush(stdout);
  time(&start);
  candlen = strlen(candstr);
  for (i = 0; i < numreps; ++i)
    bMatched=!regexec(&comp,candstr,10,matches,0);
  time(&finish);
  elapsed = difftime(finish, start);
  fprintf(stdout, "\nRE match\t: %d secs, %d/sec, \"%s\" --> \"%s\"\n", (int)elapsed, (int)((float)numreps/elapsed), patstr, candstr);

  regfree(&comp);

  if (!bMatched)
    fprintf(stdout, "didn't match!\n");
}

int foo;

void strcmpspeedtest(int numreps, const char* patstr, const char* candstr, char* cmpname, int (*cmpfxn)(const char* a, const char* b))
{
  time_t start, finish;
  float elapsed;
  int i, j;

  fprintf(stdout, "\nTiming %s\n", cmpname); fflush(stdout);
  time(&start);
  for (i = 0; i < numreps; ++i)
    foo = (*cmpfxn)(patstr, candstr);
  time(&finish);
  elapsed = difftime(finish, start);
  fprintf(stdout, "\n%s\t: %d secs, %d/sec, \"%s\" --> \"%s\"\n", cmpname, (int)elapsed, (int)((float)numreps/elapsed), patstr, candstr);
  if (foo != 0)
    fprintf(stdout, "didn't match!\n");
}

void speedtest()
{
  char* candstr = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABD";
  /*
    char* candstr = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABD";
  */

  const int numreps = 1000000;

  respeedtest(numreps, "A*BD", candstr);
  respeedtest(numreps, "(A|A)*BD", candstr);
  respeedtest(numreps, "(A|B)*BD", candstr);
  respeedtest(numreps, "(B|A)*BD", candstr);
  respeedtest(numreps, "((A*B)|(AC))D", candstr);
  respeedtest(numreps, "((A*B)|(A*C))D", candstr);
  respeedtest(numreps, "[Aa]*[Bb][Dd]", candstr);
  strcmpspeedtest(numreps, candstr, candstr, "strcmp", strcmp);
  strcmpspeedtest(numreps, candstr, candstr, "stricmp", stricmp);
}



void main()
{
  fprintf(stdout, "\nStarting speed test\n");
  speedtest();
  fprintf(stdout, "\ndone\n");
}
