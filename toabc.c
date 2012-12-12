/*
 * toabc.c - part of abc2abc - program to manipulate abc files.
 * Copyright (C) 1999 James Allwright
 * e-mail: J.R.Allwright@westminster.ac.uk
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
 *
 */

/* back-end for outputting (possibly modified) abc */

#define VERSION "1.70 December 01 2012"

/* for Microsoft Visual C++ 6.0 or higher */
#ifdef _MSC_VER
#define ANSILIBS
#endif

#include "abc.h"
#include "parseabc.h"
#include <stdio.h>

/* define USE_INDEX if your C libraries have index() instead of strchr() */
#ifdef USE_INDEX
#define strchr index
#endif

#ifdef ANSILIBS
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#else
extern char* strchr();
#endif

#define MAX_VOICES 30
/* should be plenty! */

programname fileprogram = ABC2ABC;
extern int oldchordconvention; /* for handling +..+ chords */

struct fract {
  int num;
  int denom;
};
struct fract barlen; /* length of a bar as given by the time signature */
struct fract unitlen; /* unit length as given by the L: field */
struct fract count; /* length of bar so far */
struct fract prevcount; /* length of bar before last increment */
struct fract tuplefactor; /* factor associated with a tuple (N */
struct fract breakpoint; /* used to break bar into beamed sets of notes */
int barno; /* number of bar within tune */
int newspacing; /* was -s option selected ? */
int barcheck, repcheck; /* indicate -b and -r options selected */
int echeck; /* was error-checking turned off ? (-e option) */
int newbreaks; /* was -n option selected ? */
int nodouble_accidentals;
int totalnotes, notecount; 
int bars_per_line; /* number supplied after -n option */
int barcount;
int expect_repeat;
int tuplenotes, barend;
int xinhead, xinbody; /* are we in head or body of abc tune ? */
int inmusic; /* are we in a line of notes (in the tune body) ? */
int startline, blankline;
int transpose; /* number of semitones to transpose by (-t option) */
struct fract lenfactor; /* fraction to scale note lengths; -v,-d options */
int newkey; /* key after transposition (expressed as no. of sharps) */
int lines; /* used by transposition */
int orig_key_number; /* used for gchord transposition */
int new_key_number;  /* used for gchord transposition */
int oldtable[7], newtable[7]; /* for handling transposition */
int inchord; /* are we in a chord [ ] ? */
int ingrace; /* are we in a grace note set { } ? */
int chordcount; /* number of notes or rests in current chord */
int inlinefield; /* boolean - are we in [<field>: ] ? */
int cleanup; /* boolean to indicate -u option (update notation) */
char tmp[2000]; /* buffer to hold abc output being assembled */
int output_on = 1;  /* if 0 suppress output */
int passthru = 0; /* output original abc file [SS] 2011-06-07 */
int selected_voice = -1; /* no voice was selected */
int newrefnos; /* boolean for -X option (renumber X: fields) */
int newref; /* next new number for X: field */
int useflats=0; /* flag associated with nokey.*/ 
int adapt_useflats_to_gchords = 1; /* experimental flag */
int usekey = 0;
int drumchan=0; /* flag to suppress transposition */
int noplus; /* flag for outputting !..! instructions instead of +...+ */

extern int nokey; /* signals no key signature assumed */
extern int voicecodes ;  /* from parseabc.c */
extern char voicecode[16][30]; /*for interpreting V: string */
 
struct voicetype { /* information needed for each voice */
  int number; /* voice number from V: field */
  int barcount;
  int foundbar;
  struct abctext* currentline;
  int bars_remaining;
  int bars_complete;
  int drumchan;
} voice[MAX_VOICES];
int voicecount, this_voice, next_voice;
enum abctype {field, bar, barline};
/* linestat is used by -n for deciding when to generate a newline */
enum linestattype {fresh, midmusic, endmusicline, postfield};
enum linestattype linestat; 
/* struct abctext is used to store abc lines for re-formatting (-n option) */
struct lyricwords{
  struct lyricwords* nextverse;
  char* words;
};  
struct abctext{ /* linked list used to store output before re-formatting */
  struct abctext* next;
  char* text;
  enum abctype type;
  int notes;
  struct lyricwords* lyrics;
};
struct abctext* head;
struct abctext* tail;

extern char *mode[];
extern int modekeyshift[];
int basemap[7], workmap[7]; /* for -nokey and pitchof() */
int  workmul[7];
void copymap();
void printpitch(int);
void setup_sharps_flats (int sf);
int pitchof(char note,int accidental,int mult,int octave);


static int purgespace(p)
char* p;
/* if string p is empty or consists of spaces, set p to the empty string */
/* and return 1, otherwise return 0. Used to test tmp */
/* part of new linebreak option (-n) */
{
  int blank;
  char *s;

  blank = 1;
  s = p;
  while (*s != '\0') {
    if (*s != ' ') blank = 0;
    s = s + 1;
  };
  if (blank) {
    *p = '\0';
  };
  return(blank);
}

int zero_barcount(foundbar)
/* initialize bar counter for abctext elements */
/* part of new linebreak option (-n) */
int *foundbar;
{
  *foundbar = 0;
  return(0);
}

int new_barcount(type, foundbar, oldcount)
enum abctype type;
int *foundbar;
int oldcount;
/* work out whether we have reached the end of a bar in abctext elements */
/* and increment barcount if we have */
/* part of new linebreak option (-n) */
{
  int new_value;

  new_value = oldcount;
  if (type == bar) {
    *foundbar = 1;
  };
  if ((type == barline) && (*foundbar == 1)) {
    new_value = new_value + 1;
    *foundbar = 0;
  };
  return(new_value);
}


static void setline(t)
enum linestattype t;
/* generates newline, or continuation, or nothing at all */
/* part of new linebreak option (-n) */
{
  if ((t == fresh) && ((linestat == postfield) || (linestat == endmusicline))) {
    printf("\n");
  };
  if ((t == fresh) && (linestat == midmusic)) {
    printf("\\\n");
  };
  linestat = t;
}

static int flush_abctext(bars, termination)
int bars;
enum linestattype termination;
/* outputs up to the specified number of bars of stored music */
/* and frees up the storage allocated for those bars. */
/* returns the number of bars actually output */
/* part of new linebreak option (-n) */
{
  struct abctext *p, *nextp;
  struct lyricwords *q, *r;
  struct lyricwords *barlyrics;
  int count, donewords, wordline;
  int i, foundtext;
  int foundbar;

  /* printf("flush_abctext called\n"); */
  /* print music */
  p = head;
  count = zero_barcount(&foundbar);
  while ((p != NULL) && (count < bars)) {
    if (p->type == field) {
      setline(fresh);
    };
    printf("%s", p->text);
    if (p->type == field) {
      setline(postfield);
      setline(fresh);
    } else {
      setline(midmusic);
    };
    count = new_barcount(p->type, &foundbar, count);
    if ((count == bars) && (p->type == barline)) {
      setline(endmusicline);
    };
    p = p->next;
  };
  if (linestat == midmusic) {
    setline(termination);
  };
  if (bars > 0) {
    /* print out any w: lines */
    donewords = 0;
    wordline = 0;
    while (donewords == 0) {
      p = head;
      foundtext = 0;
      count = zero_barcount(&foundbar);
      while ((p != NULL) && (count < bars)) {
        barlyrics = p->lyrics;
        for (i=0; i<wordline; i++) {
          if (barlyrics != NULL) {
            barlyrics = barlyrics->nextverse;
          };
        };
        if (barlyrics != NULL) {
          if (foundtext == 0) {
            setline(fresh);
            printf("w:");
            foundtext = 1;
          };
          printf("%s",barlyrics->words);
        };
        count = new_barcount(p->type, &foundbar, count);
        p = p->next;
      };
      if (foundtext == 0) {
        donewords = 1;
      } else {
        setline(postfield);
        setline(fresh);
      };
      wordline = wordline + 1;
    };
  };
  /* move head on and free up space used by stuff printed out */
  count = zero_barcount(&foundbar);
  p = head;
  foundbar = 0;
  while ((p != NULL) && (count < bars)) {
    if (p != NULL) {
      free(p->text);
      q = p->lyrics;
      while (q != NULL) {
        free(q->words);
        r = q->nextverse;
        free(q);
        q = r;
      };
      count = new_barcount(p->type, &foundbar, count);
      nextp = p->next;
      free(p);
      p = nextp;
    };
    head = p;
  };
  if (head == NULL) {
    tail = NULL;
  };
  return(count);
}

void complete_bars(v)
/* mark all bars as completed (i.e. having associated w: fields parsed) */
/* and out put all music lines which contain the full set of bars */
/* part of new linebreak option (-n) */
struct voicetype *v;
{
  int bars_done;

  v->bars_complete = v->bars_complete + v->barcount;
  v->barcount = 0;
  while (v->bars_complete > v->bars_remaining) {
    bars_done = flush_abctext(v->bars_remaining, endmusicline);
    setline(fresh);
    v->bars_complete = v->bars_complete - bars_done;
    v->bars_remaining = v->bars_remaining - bars_done;
    if (v->bars_remaining == 0) {
      v->bars_remaining = bars_per_line;
    };
  };
}

void complete_all(v, termination)
struct voicetype *v;
enum linestattype termination;
/* output all remaining music and fields */
/* part of new linebreak option (-n) */
{
  int bars_done;

  complete_bars(v);
  bars_done = flush_abctext(v->bars_remaining+1, termination);
  v->bars_complete = v->bars_complete - bars_done;
  v->bars_remaining = v->bars_remaining - bars_done;
  if (v->bars_remaining == 0) {
    v->bars_remaining = bars_per_line;
  };
  head = NULL;
  tail = NULL;
  voice[this_voice].currentline = NULL;
}

static struct abctext* newabctext(t)
enum abctype t;
/* called at newlines and barlines */
/* adds current output text to linked list structure */
/* part of new linebreak option (-n) */
{
  struct abctext* p;

  if (output_on == 0) {
    p = NULL;
    return(p);
  };
  if (newbreaks) {
/*
    if ((t == field) && (!xinbody || (this_voice != next_voice))) {
*/
    if (t == field) {
      complete_all(&voice[this_voice], midmusic);
      this_voice = next_voice;
    };
    p = (struct abctext*) checkmalloc(sizeof(struct abctext));
    p->text = addstring(tmp);
    tmp[0] = '\0';
    p->next = NULL;
    p->type = t;
    p->lyrics = NULL;
    if (t == bar) {
      p->notes = notecount;
      totalnotes = totalnotes + notecount;
      notecount = 0;
    } else {
      p->notes = 0;
    };
    if (xinbody) {
      voice[this_voice].barcount = new_barcount(t, 
                       &voice[this_voice].foundbar, 
                        voice[this_voice].barcount);
    };
    if (head == NULL) {
      head = p;
      tail = p;
    } else {
      tail->next = p;
      tail = p;
    };
    if ((t != field) && (voice[this_voice].currentline == NULL)) {
      voice[this_voice].currentline = p;
    };
  } else {
    printf("%s", tmp);  /* output to stdout is here */
    tmp[0] = '\0';
    p = NULL;
  };
  inmusic = 1;
  return(p);
}

static int nextnotes()
/* return the number of notes in the next bar */
/* part of new linebreak option (-n) */
{
  int n, got;
  struct abctext* p;

  p = head;
  n = 100;
  got = 0;
  while ((p != NULL) && (!got)) {
    if (p->type == bar) {
      n = p->notes;
      got = 1;
    } else {
      p = p->next;
    };
  };
  return(n);
}

static void reduce(a, b)
int *a, *b;
{
  int t, n, m;

  /* find HCF using Euclid's algorithm */
  if (*a > *b) {
    n = *a;
    m = *b;
  } else {
    n = *b;
    m = *a;
  };
  while (m != 0) {
    t = n % m;
    n = m;
    m = t;
  };
  *a = *a/n;
  *b = *b/n;
}


static void addunits(n, m)
int n, m;
/* add fraction n/m to count */
{
  prevcount = count;  /* in case of chord extension eg [CE]3/2 */
  count.num = n*count.denom + count.num*(m*unitlen.denom);
  count.denom = (m*unitlen.denom)*count.denom;
  reduce(&count.num, &count.denom);
}

static void repudiate_lastaddunits()
{
  count = prevcount;
}

void event_init(argc, argv, filename)
int argc;
char* argv[];
char** filename;
/* routine called on program start-up */
{
  int targ, narg;

  if ((getarg("-h", argc, argv) != -1) || (argc < 2)) {
    printf("abc2abc version %s\n",VERSION);
    printf("Usage: abc2abc <filename> [-s] [-n X] [-b] [-r] [-e] [-t X]\n");
    printf("       [-u] [-d] [-v] [-V X] [-ver] [-X n]\n");
    printf("  -s for new spacing\n");
    printf("  -n X to re-format the abc with a new linebreak every X bars\n");
    printf("  -b to remove bar checking\n");
    printf("  -r to remove repeat checking\n");
    printf("  -e to remove all error reports\n");
    printf("  -t X to transpose X semitones\n");
    printf("  -nda No double accidentals in guitar chords\n");
    printf("  -nokeys No key signature. Use sharps\n");
    printf("  -nokeyf No key signature. Use flats\n");
    printf("  -u to update notation ([] for chords and () for slurs)\n");
    printf("  -usekey n Use key signature sf (sharps/flats)\n");
    printf("  -d to notate with doubled note lengths\n");
    printf("  -v to notate with halved note lengths\n");
    printf("  -V X to output only voice X\n");
    printf("  -P X restricts action to voice X, leaving other voices intact\n");
    printf("  -ver  prints version number and exits\n");
    printf("  -X n renumber the all X: fields as n, n+1, ..\n");
    printf("  -OCC old chord convention (eg. +CE+)\n");
    /*printf("  -noplus use !...! instead of +...+ for instructions\n");
     [SS] 2012-06-04
    */

    exit(0);
  } else {
    *filename = argv[1];
  };
  nodouble_accidentals = 0;   /* use correct guitar chords */
  if (getarg("-ver",argc,argv) != -1) {
	  printf("%s\n",VERSION);
	  exit(0);
  }
  if (getarg("-u", argc, argv) == -1) {
    cleanup = 0;
  } else {
    cleanup = 1;
    oldchordconvention = 1;
  };
  if (getarg("-s", argc, argv) == -1) {
    newspacing = 0;
  } else {
    newspacing = 1;
  };
  narg = getarg("-X", argc, argv);
  if (narg == -1) {
    newrefnos = 0;
  } else {
    newrefnos = 1;
    if (narg < argc) {
      newref = readnumf(argv[narg]);
    } else {
      newref = 1;
    };
  };
  if (getarg("-e", argc, argv) == -1) {
    echeck = 1;
  } else {
    echeck = 0;
  };
  narg = getarg("-n", argc, argv);
  if (narg == -1) {
    newbreaks = 0;
  } else {
    newbreaks = 1;
    if (narg >= argc) {
      event_error("No value for bars per line after -n");
      bars_per_line = 4;
    } else {
      bars_per_line = readnumf(argv[narg]);
      if (bars_per_line < 1) {
        bars_per_line = 4;
      };
    };
  };
  if (getarg("-b", argc, argv) != -1) {
    barcheck = 0;
  } else {
    barcheck = 1;
  };
  if (getarg("-r", argc, argv) != -1) {
    repcheck = 0;
  } else {
    repcheck = 1;
  };
  if (getarg("-v", argc, argv) != -1) {
    lenfactor.num = 1;
    lenfactor.denom = 2;
  } else {
    if (getarg("-d", argc, argv) != -1) {
      lenfactor.num = 2;
      lenfactor.denom = 1;
    } else {
      lenfactor.num = 1;
      lenfactor.denom = 1;
    };
  };
  targ = getarg("-t", argc, argv);
  if (targ == -1) {
    transpose = 0;
  } else {
    if (targ >= argc) {
      event_error("No tranpose value supplied");
    } else {
      if (*argv[targ] == '-') {
        transpose = -readnumf(argv[targ]+1);
      } else if (*argv[targ] == '+') {
          transpose = readnumf(argv[targ]+1);
      } else {
          transpose = readnumf(argv[targ]);
        };
    };
  };
  targ = getarg("-nda",argc,argv);
  if (targ != -1) nodouble_accidentals = 1;

  targ = getarg("-nokeys",argc,argv);
  if (targ != -1) nokey=1;

  targ = getarg("-nokeyf",argc,argv);
  if (targ != -1) {nokey=1; useflats=1;}

  targ = getarg("-V", argc, argv);
  if (targ != -1) {
    selected_voice  = readnumf(argv[targ]);
  };

  targ = getarg("-P", argc, argv);   /* [SS] 2011-06-07 */
  if (targ != -1) {
    selected_voice = readnumf(argv[targ]);
    passthru = 1;
    }

  targ = getarg("-usekey",argc,argv);
  if (targ != -1) {
     usekey = readsnumf(argv[targ]);
     nokey = 1;
     if (usekey < 0) useflats=1;
     if (usekey <-5) usekey = -5;
     if (usekey >5) usekey = 5;
     setup_sharps_flats (usekey);
     }
  if (getarg("-OCC",argc,argv) != -1) oldchordconvention=1;
  /*if (getarg("-noplus",argc,argv) != -1) noplus = 1; [SS] 2012-06-04*/


  /* printf("%% output from abc2abc\n"); */
  startline = 1;
  blankline = 0;
  xinbody =0;
  inmusic = 0;
  inchord = 0;
  ingrace = 0;
  head = NULL;
  tail = NULL;
  tmp[0] = '\0';
  totalnotes = 0;
}

void emit_string(s)
char *s;
/* output string */
{
  if (output_on) {
    strcpy(tmp+strlen(tmp), s);
  };
}

void emit_char(ch)
char ch;
/* output single character */
{
  char *place;

  if (output_on) {
    place = tmp+strlen(tmp);
    *place = ch;
    *(place+1) = '\0';
  };
}

void emit_int(n)
int n;
/* output integer */
{
  if (output_on) {
    sprintf(tmp+strlen(tmp), "%d", n);
  };
}

void emit_string_sprintf(s1, s2)
char *s1;
char *s2;
/* output string containing string expression %s */
{
  if (output_on) {
    sprintf(tmp+strlen(tmp), s1, s2);
  };
}

void emit_int_sprintf(s, n)
char *s;
int n;
/* output string containing int expression %d */
{
  if (output_on) {
    sprintf(tmp+strlen(tmp), s, n);
  };
}

void unemit_inline()
/* remove previously output start of inline field */
/* needed for -V voice selection option           */
{
  int len;

  len = strlen(tmp);
  if ((len > 0) && (tmp[len-1] == '[')) {
    tmp[len-1] = '\0'; /* delete last character */
  } else {
    event_error("Internal error - Could not delete [");
  };
}

static void close_newabc()
/* output all remaining abc_text elements */
/* part of new linebreak option (-n) */
{
  if (newbreaks) {
    complete_all(&voice[this_voice], endmusicline);
    if (linestat == midmusic) setline(endmusicline);
    setline(fresh);
  };
}

void event_eof()
{
  close_newabc();
}

void event_blankline()
{
  output_on = 1;
  close_newabc();
/*  if (newbreaks) [SS] 2006-09-23 */  printf("\n");
  xinbody = 0;
  xinhead = 0;
  parseroff();
  blankline = 1;
}

void event_text(p)
char *p;
{
  emit_string_sprintf("%%%s", p);
  inmusic = 0;
}

void event_reserved(p)
char p;
{
  emit_char(p);
  inmusic = 0;
}

void event_tex(s)
char *s;
{
  emit_string(s);
  inmusic = 0;
}

void event_linebreak()
{
  if (!output_on && passthru) print_inputline(); /* [SS] 2011-06-07*/
  if (newbreaks) {
    if (!purgespace(tmp)) {
      if (inmusic) {
        newabctext(bar);
      } else {
        newabctext(field); 
      };
    };
  } else {
    newabctext(bar);
    if (output_on) {
      printf("\n"); /* linefeed to stdout is here */
    };
    /* don't output new line if voice is already suppressed
       otherwise we will get lots of blank lines where we
       are suppressing output. [SS] feb-10-2002.
     */
  };
}

void event_startmusicline()
/* encountered the start of a line of notes */
{
  voice[this_voice].currentline = NULL;
  complete_bars(&voice[this_voice]);
}

void event_endmusicline(endchar)
char endchar;
/* encountered the end of a line of notes */
{
}

void event_error(s)
char *s;
{
  if (echeck && output_on) {     /* [SS] 2011-04-14 */
   printf("\n%%Error : %s\n", s);
  };
}

void event_warning(s)
char *s;
{
  if (echeck && output_on) {    /* [SS] 2011-04-14 */
   printf("\n%%Warning : %s\n", s);
  };
}

void event_comment(s)
char *s;
{
  if (newbreaks && (!purgespace(tmp))) {
    if (inmusic) {
      newabctext(bar);
    } else {
      newabctext(field);
    };
  };
  emit_string_sprintf("%%%s", s);
  inmusic = 0;
}

void event_specific(package, s)
char *package, *s;
{
  char command[40];
  int ch;
  char *p;
  emit_string("%%");
  emit_string(package);
  emit_string(s);
  inmusic = 0;
/* detect drum channel by searching for %%MIDI channel 10 */
  if (strcmp(package,"MIDI") != 0) return;
  p = s;
  skipspace(&p);
  readstr(command, &p, 40);
  if (strcmp(command, "channel") != 0) return;
  skipspace(&p);
  ch = readnump(&p);
  if(ch == 10) {
               voice[next_voice].drumchan = 1;
               drumchan = 1;
               }
/*  printf("event_specific: next_voice = %d\n",next_voice); */
}

void event_info(f)
/* handles info field I: */
char *f;
{
  emit_string_sprintf("I:%s", f);
  inmusic = 0;
}


void event_field(k, f)
char k;
char *f;
{
  emit_char(k);
  emit_char(':');
  emit_string(f);
  inmusic = 0;
}

struct abctext* getbar(place)
struct abctext *place;
/* find first element in list which is a bar of music */
{
  struct abctext *newplace;

  newplace = place;
  while ((newplace != NULL) &&
         ((newplace->type != bar) || 
          (newplace->notes == 0))) {
    newplace = newplace->next;
  };
  return(newplace);    
}

struct abctext* getnextbar(place)
struct abctext *place;
/* find next element in list which is a bar of music */
{
  struct abctext *newplace;

  newplace = place;
  if (newplace != NULL) {
    newplace = getbar(newplace->next);
  };
  return(newplace);
};

void append_lyrics(place, newwords)
struct abctext *place;
char *newwords;
/* add lyrics to end of lyric list associated with bar */
{
  struct lyricwords* new_words;
  struct lyricwords *new_place;

  if (place == NULL) {
    return;
  };
  /* printf("append_lyrics has %s at %s\n", newwords, place->text); */
  new_words = (struct lyricwords*)checkmalloc(sizeof(struct lyricwords));
  /* add words to bar */
  new_words->nextverse = NULL;
  new_words->words = addstring(newwords);
  if (place->lyrics == NULL) {
    place->lyrics = new_words;
  } else {
    new_place = place->lyrics;
    /* find end of list */
    while (new_place->nextverse != NULL) {
      new_place = new_place->nextverse;
    };
    new_place->nextverse = new_words;
  };
}

struct abctext* apply_bar(syll, place, notesleft, barwords)
/* advance to next bar (on finding '|' in a w: field) */
char* syll;
struct abctext *place;
int *notesleft;
struct vstring *barwords;
{
  struct abctext* new_place;

  if (place == NULL) {
    return(NULL);
  };
  new_place = place;
  addtext(syll, barwords);
  append_lyrics(place, barwords->st);
  /* go on to next bar */
  clearvstring(barwords);
  new_place = getnextbar(place);
  if (new_place != NULL) {
    *notesleft = new_place->notes;
  };
  return(new_place); 
}

struct abctext* apply_syllable(syll, place, notesleft, barwords)
/* attach syllable to appropriate place in abctext structure */
char* syll;
struct abctext *place;
int *notesleft;
struct vstring *barwords;
{
  struct abctext* new_place;
  char msg[80];

  if (place == NULL) {
    sprintf(msg, "Cannot find note to match \"%s\"", syll);
    event_error(msg);
    return(NULL);
  };
  new_place = place;
  addtext(syll, barwords);
  *notesleft = *notesleft - 1;
  if (*notesleft == 0) {
    append_lyrics(place, barwords->st);
    /* go on to next bar */
    clearvstring(barwords);
    new_place = getnextbar(place);
    if (new_place != NULL) {
      *notesleft = new_place->notes;
    };
  };
  return(new_place); 
}

void parse_words(p)
char* p;
/* Break up a line of lyrics (w: ) into component syllables */
{
  struct vstring syll;
  struct vstring barwords;
  char* q;
  unsigned char ch;
  int errors;
  int found_hyphen;

  struct abctext *place;
  int notesleft;

  if (!xinbody) {
    event_error("w: field outside tune body");
    return;
  };
  place = getbar(voice[this_voice].currentline);
  if (place == NULL) {
    event_error("No music to match w: line to");
    return;
  };
  notesleft = voice[this_voice].currentline->notes;
  initvstring(&barwords);
  errors = 0;
  if (place == NULL) {
    event_error("No notes to match words");
    return;
  };
  initvstring(&syll);
  q = p;
  skipspace(&q);
  while (*q != '\0') {
    found_hyphen = 0;
    clearvstring(&syll);
    ch = *q;
    while(ch=='|') {
      addch('|', &syll);
      addch(' ', &syll);
      place = apply_bar(syll.st, place, &notesleft, &barwords);
      clearvstring(&syll);
      q++;
      ch = *q;
    };
    /* PCC seems to require (ch != ' ') on the next line */
    /* presumably PCC's version of ispunct() thinks ' ' is punctuation */
    while (((ch>127)||isalnum(ch)||ispunct(ch))&&(ch != ' ')&&
           (ch != '_')&&(ch != '-')&&(ch != '*')&& (ch != '|')) {
      if ((ch == '\\') && (*(q+1)=='-')) {
        addch('\\', &syll);
        ch = '-';
        q++;
      };
      /* syllable[i] = ch; */
      addch(ch, &syll);
      q++;
      ch = *q;
    };
    skipspace(&q);
    if (ch == '-') {
      found_hyphen = 1;
      addch(ch, &syll);
      while (isspace(ch)||(ch=='-')) {
        q++;
        ch = *q;
      };
    };
    if (syll.len > 0) {
      if (!found_hyphen) {
        addch(' ', &syll);
      };
      place = apply_syllable(syll.st, place, &notesleft, &barwords);
    } else {
      if (ch=='_') {
        clearvstring(&syll);
        addch('_', &syll);
        addch(' ', &syll);
        place = apply_syllable(syll.st, place, &notesleft, &barwords);
        q++;
        ch = *q;
      };
      if (ch=='*') {
        clearvstring(&syll);
        addch('*', &syll);
        addch(' ', &syll);
        place = apply_syllable(syll.st, place, &notesleft, &barwords);
        q++;
        ch = *q;
      };
    }; 
  };
  if (errors > 0) {
    event_error("Lyric line too long for music");
  } else {
    clearvstring(&syll);
  };
  freevstring(&syll);
}

void event_words(p, continuation)
char* p;
int continuation;
/* a w: field has been encountered */
{
  struct vstring afield;

  if (xinbody && newbreaks) {
    parse_words(p);
  } else {
    initvstring(&afield);
    addtext(p, &afield);
    if (continuation) {
      addch(' ', &afield);
      addch('\\', &afield);
    };
    event_field('w', afield.st);
  };
}

void event_part(s)
char* s;
{
  if (xinbody) {
    complete_bars(&voice[this_voice]);
  };
  output_on = 1;  /* [SS] 2011-04-14 */
  emit_string_sprintf("P:%s", s);
  inmusic = 0;
}

int setvoice(num)
int num;
/* we need to keep track of current voice for new linebreak handling (-n) */
/* change voice to num. If voice does not exist, start new one */
{
  int i, voice_index;

  i = 0;
  while ((i < voicecount) && (voice[i].number != num)) {
    i = i + 1;
  };
  if ((i < voicecount) && (voice[i].number == num)) {
    voice_index = i;
    drumchan = voice[voice_index].drumchan;
/*    printf("voice_index = %d drumchan = %d\n",voice_index,drumchan); */
  } else {
    voice_index = voicecount;
    if (voicecount < MAX_VOICES) {
      voicecount = voicecount + 1;
    } else {
      event_error("Number of voices exceeds static limit MAX_VOICES");
    };
    voice[voice_index].number = num;
    voice[voice_index].barcount = zero_barcount(&voice[voice_index].foundbar);
    voice[voice_index].bars_complete = 0;
    voice[voice_index].bars_remaining = bars_per_line;
    voice[voice_index].drumchan = 0;
  };
  voice[voice_index].currentline = NULL;
  return(voice_index);
}

void event_voice(n, s, vp)
int n;
char *s;
struct voice_params *vp;
{
  char output[32];
  if (xinbody) {
    next_voice = setvoice(n);
  };
  if ((selected_voice != -1) && (n != selected_voice)) {
    if ((inlinefield) && (output_on == 1)) { 
      unemit_inline();
    }; 
    /*output_on = 0; [SS] 2011-06-10 */
    if (xinbody) output_on = 0; /* [SS] 2011-06-10 */
  } else { 
    if (output_on == 0) { 
      output_on = 1; 
      if (inlinefield) { 
        emit_string("["); /* regenerate missing [ */
      }; 
    }; 
  }; 
  if (strlen(s) == 0) {
    if(voicecodes >= n) emit_string_sprintf("V:%s",voicecode[n-1]);
    else emit_int_sprintf("V:%d", n);
    if (vp->gotclef) {sprintf(output," clef=%s", vp->clefname);
	    emit_string(output);}
    if (vp->gotoctave) {sprintf(output," octave=%d", vp->octave);
	    emit_string(output);}
    if (vp->gottranspose) {sprintf(output," transpose=%d", vp->transpose);
	    emit_string(output);}
     if (vp->gotname) {sprintf(output," name=%s", vp->namestring);
            emit_string(output);}
     if (vp->gotsname) {sprintf(output," sname=%s", vp->snamestring);
            emit_string(output);}
     if( vp->gotmiddle ) { sprintf(output, " middle=%s", vp->middlestring);
            emit_string(output);}
     if( vp->gotother ) { sprintf(output, " %s", vp->other);
            emit_string(output);}  /* [SS] 2011-04-18 */
  } else {
    if(voicecodes >= n) emit_string_sprintf("V:%s",voicecode[n-1]);
    emit_int_sprintf("V:%d ", n);
    if (vp->gotclef) {sprintf(output," clef=%s", vp->clefname);
	    emit_string(output);}
    if (vp->gotoctave) {sprintf(output," octave=%d", vp->octave);
	    emit_string(output);}
    if (vp->gottranspose) {sprintf(output," transpose=%d", vp->transpose);
	    emit_string(output);}
     if (vp->gotname) {sprintf(output," name=%s", vp->namestring);
            emit_string(output);}
     if( vp->gotmiddle ) { sprintf(output, " middle=%s", vp->middlestring);
            emit_string(output);}
     if( vp->gotother ) { sprintf(output, " %s", vp->other);
            emit_string(output);} /* [SS] 2011-04-18 */
    emit_string(s);
  };
  inmusic = 0;
}

void event_length(n)
int n;
{
  struct fract newunit;

  newunit.num = lenfactor.denom;
  newunit.denom = lenfactor.num * n;
  reduce(&newunit.num, &newunit.denom);
  emit_int_sprintf("L:%d/", newunit.num);
  emit_int(newunit.denom);
  unitlen.num = 1;
  unitlen.denom = n;
  inmusic = 0;
}

void event_refno(n)
int n;
{
  if (xinbody) {
    close_newabc();
  };
  output_on = 1;
  if (newrefnos) {
    emit_int_sprintf("X:%d", newref);
    newref = newref + 1;
  } else {
    emit_int_sprintf("X:%d", n);
  };
  parseron();
  xinhead = 1;
  notecount = 0;
  unitlen.num = 0;
  unitlen.denom = 1;
  barlen.num = 0;
  barlen.denom = 1;
  inmusic = 0;
  barcount = 0;
}

void event_tempo(n, a, b, relative, pre, post)
int n, a, b;
int relative;
char *pre;
char *post;
{
  struct fract newlen;

  emit_string("Q:");
  if (pre != NULL) {
    emit_string_sprintf("\"%s\"", pre);
  };
  if (n != 0) {
    if ((a == 0) && (b == 0)) {
      emit_int(n);
    } else {
      if (relative) {
        newlen.num = a * lenfactor.num;
        newlen.denom = b * lenfactor.denom;
        reduce(&newlen.num, &newlen.denom);
        emit_int_sprintf("C%d/", newlen.num);
        emit_int(newlen.denom);
        emit_int_sprintf("=%d", n);
      } else {
        emit_int_sprintf("%d/", a);
        emit_int(b);
        emit_int_sprintf("=%d", n);
      };
    };
  };
  if (post != NULL) {
    emit_string_sprintf("\"%s\"", post);
  };
  inmusic = 0;
}

void event_timesig(n, m, checkbars)
int n, m, checkbars;
{
  if (checkbars == 1) {
    emit_int_sprintf("M:%d/", n);
    emit_int(m);
  } else {
    emit_string("M:none");
    barcheck = 0;
  };
  barlen.num = n;
  barlen.denom = m;
  breakpoint.num = n;
  breakpoint.denom = m;
  if ((n == 9) || (n == 6)) {
    breakpoint.num = 3;
    breakpoint.denom = barlen.denom;
  };
  if (n%2 == 0) {
    breakpoint.num = barlen.num/2;
    breakpoint.denom = barlen.denom;
  };
  barend = n/breakpoint.num;
  inmusic = 0;
}

static void setmap(sf, map)
int sf;
int map[7];
{
/* map[0] to map[7] corresponds to keys a to g and indicates
   whether they are flattened or sharpened. sf encodes the
   key signature by indicating the number of sharps (positive
   numbers) or flats (negative numbers)
*/
  int j;

  for (j=0; j<7; j++) {
    map[j] = 0;
  };
  if (sf >= 1) map['f'-'a'] = 1;
  if (sf >= 2) map['c'-'a'] = 1;
  if (sf >= 3) map['g'-'a'] = 1;
  if (sf >= 4) map['d'-'a'] = 1;
  if (sf >= 5) map['a'-'a'] = 1;
  if (sf >= 6) map['e'-'a'] = 1;
  if (sf >= 7) map['b'-'a'] = 1;
  if (sf <= -1) map['b'-'a'] = -1;
  if (sf <= -2) map['e'-'a'] = -1;
  if (sf <= -3) map['a'-'a'] = -1;
  if (sf <= -4) map['d'-'a'] = -1;
  if (sf <= -5) map['g'-'a'] = -1;
  if (sf <= -6) map['c'-'a'] = -1;
  if (sf <= -7) map['f'-'a'] = -1;
}

static void start_tune()
{
  parseron();
  count.num =0;
  count.denom = 1;
  barno = 0;
  tuplenotes = 0;
  expect_repeat = -1;	/* repeat from start may occur [J-FM] 2012-06-04 */
  inlinefield = 0;
  if (barlen.num == 0) {
    /* generate missing time signature */
    event_linebreak();
    event_timesig(4, 4, 1);
    inmusic = 0;
  };
  if (unitlen.num == 0) {
    if ((float) barlen.num / (float) barlen.denom < 0.75) {
      unitlen.num = 1;
      unitlen.denom = 16;
    } else {
      unitlen.num = 1;
      unitlen.denom = 8;
    };
  };
  voicecount = 0;
  this_voice = setvoice(1);
  next_voice = this_voice;
}

void compute_keysignature (int sf,int modeindex, char * keysignature)
{
char  *notes[7] = {"A","B","C","D","E","F","G"};
int sf2note[12] = {3,0,4,1,5,2,6,3,0,4,1,5};
char *flatsharp[2] = {"b","#"};
int index0,index1,index;
int map[7];

index0 = sf+5; /* -5 <sf < 4 */
index1 = sf2note[index0]; /* major key */
index =index1 + modekeyshift[modeindex]; /* key for mode */
setmap(sf,map); /* determine the flats and sharps for major key */
if (index > 6) index -=7;
strcpy(keysignature,notes[index]);
/* propogate sharp or flat to key signature of mode */
if (map[index] == -1) strcat(keysignature,flatsharp[0]);
if (map[index] == 1) strcat(keysignature,flatsharp[1]);
/* add mode name */
strcat(keysignature,mode[modeindex]);
}



/* [SS] 2011-02-15 */

/* Support functions to transpose key signature modifiers (eg. K: DPhr ^F
 * K: D exp _b_e^f ).
 * Method: we represent the notes in the key signature (with modifiers) in
 * a chromatic scale semiseq. semiseq(i) == 1 means the note is in the
 * key signature, semiseq(i) == 0 means the note is not present in the
 * key signature. semiseq(0) corresponds to A natural, semiseq(11) 
 * represents G# or Ab. We transpose the notes by shifting (with
 * rotation) semiseq() left or right. We then read back the notes
 * of the shifted sequence, ignoring all flats and sharps in the
 * new key signature corresponding to this transpose.

   modmap indicates the key modifiers using the convention in
   event_key.

    
 */

int debugsemi = 0;  /* set to one if debugging ! [SS] 2011-02-21 */
int semiseq[12];
int semiseqbase[12];

char modmap[7];
int convertnote[7] = {0,2,3,5,7,8,10};
/* needed to convert modmap to semiseq representation */

int sfpos[7] = {8, 3, 10, 5, 0, 7, 2}; /* FCGDAEB */
int sfneg[7] = {2, 7, 0, 5, 10, 3, 8};
/* needed to convert the key signature to semiseq representation */

char *semisharp[12] = {"=A", "^A", "=B", "=C", "^C", "D",
  "^D", "=E", "=F", "^F", "=G", "^G"};
char *semiflat[12] = {"=A", "_B", "=B", "=C", "_D", "=D",
  "_E", "=E", "=F", "_G", "=G", "_A"};
/* needed to convert the semiseq representation to the notes */

int modmap_not_empty (char* modmap) {
int i;
for (i=0;i<7;i++)
  if (modmap[i] != ' ') {return 1;}
return 0;
}

void print_semiseq();

void sf2semi (int sf) {
/* apply key signature to semiseq */
  int i;
  for (i=0;i<12;i++) semiseqbase[i]=0;
  for (i=0;i<7;i++) semiseqbase[sfpos[i]] = 1;
  if (sf == 0) return;
  if (sf > 0) {for (i=0;i<sf;i++) {
       semiseqbase[sfpos[i]] = 0;
       semiseqbase[sfpos[i]+1] = 1;
       }
     return;
     } else {
  sf = -sf;
  for (i=0;i<sf;i++) {
      semiseqbase[sfneg[i]] = 0;
      semiseqbase[(sfneg[i] + 11) % 12] = 1;
      }
  }
}

void note2semi (char *modmap) {
/* to convert modmap to semiseq representation */
int i;
int semi;
//for (i=0;i<12;i++) semiseq[i]=0;
for (i=0;i<8;i++) {
  semi = convertnote[i];
  switch (modmap[i]) {
    case ' ': 
      break;
    case '=': 
     semiseq[semi] = 1;
     if (semiseq[(semi+1) % 12]) semiseq[(semi+1) % 12] = 0;
     else semiseq[(semi+11) % 12] = 0;
     break;
      
    case '^': 
      semiseq[semi] = 0;
      semiseq[semi+1] = 1; 
      break;
    case '_': 
      semiseq[semi] = 0;
      semiseq[(semi+11) % 12] = 1;
      break;
    }
  }
}

void transpose_semiseq (shift) {
int i,j;
int newseq[12];
for (i=0;i<12;i++) {
  j = (i-shift+12)%12;
  newseq[i] = semiseq[j];
  }
for (i=0;i<12;i++) semiseq[i]=newseq[i];
}

void print_semiseq() {
  int i;
 if (!debugsemi) return; /* [SS] 2011-02-11 */
  printf(" A   B C   D   E F   G\n");  
  for (i=0;i<12;i++) printf(" %d",semiseq[i]);
  printf("\n");  
}

char trans_string[20];

char *pickup_accidentals_for (int sf, int explict) {
/* remove non-accidentals */
int i;
if (explict) sf2semi(0);
else sf2semi(sf);
/* [SS] 2011-02-20 */
if (debugsemi) {
  for (i=0;i<12;i++) printf(" %d",semiseqbase[i]);
  printf(" semiseqbase\n");
  }

/* everything left are accidentals */
trans_string[0] = ' ';
for (i=0;i<12;i++)
  if(semiseq[i] != semiseqbase[i] && semiseqbase[i] == 0) {
    if (sf <0) strcat(trans_string,semiflat[i]);
    else strcat(trans_string,semisharp[i]);
    }
return  trans_string;
  };

char *transpose_modmap(int oldkeysigsf, int semitranspose, char* modmap, int explict) {
  int newkeysigsf;
  int i;
  newkeysigsf = (oldkeysigsf + 7*semitranspose)%12;
  if (newkeysigsf > 6) newkeysigsf = newkeysigsf -12;
  if (newkeysigsf <-5) newkeysigsf = newkeysigsf +12;
  if (debugsemi) printf("newkeysigsf= %d\n",newkeysigsf);
  sf2semi(oldkeysigsf);
  for (i=0;i<12;i++) semiseq[i] = semiseqbase[i];
  print_semiseq();
  note2semi(modmap);
  if (debugsemi) printf("applying note2semi\n");
  print_semiseq();
  transpose_semiseq(semitranspose);
  if (debugsemi) printf("applying transpose %d\n",semitranspose);
  print_semiseq();
  return pickup_accidentals_for(newkeysigsf,explict);
  }

/* end of [SS] 2011-02-15 */

void event_key(sharps, s, modeindex, modmap, modmul, gotkey, gotclef, clefname,
          octave, xtranspose, gotoctave, gottranspose, explict)
int sharps;
char *s;
int modeindex;
char modmap[7];
int modmul[7];
int gotkey, gotclef;
char* clefname;
int octave, xtranspose, gotoctave, gottranspose;
int explict;
{
  static char* keys[12] = {"Db", "Ab", "Eb", "Bb", "F", "C", 
                           "G", "D", "A", "E", "B", "F#"};
  char signature[10];

  if (!xinbody && passthru) {print_inputline_nolinefeed(); /* [SS] 2011-06-10 */
                            if ((xinhead) && (!xinbody)) {
                                xinbody = 1;
                                start_tune();
                                };
                            inmusic = 0;
                            return;
                            }
  if (gotkey) {
    setmap(sharps, basemap); /* required by copymap and pitchof */
    setmap(sharps, oldtable);
    copymap();
    newkey = (sharps+7*transpose)%12;
    if (sharps < -5) orig_key_number = (int) keys[sharps+17][0] - (int) 'A';
    else if (sharps > 6) orig_key_number = (int)keys[sharps-7][0] - (int) 'A';
    else    orig_key_number = (int) keys[sharps+5][0] - (int) 'A';
    lines = (sharps+7*transpose)/12;
    if (newkey > 6) {
      newkey = newkey - 12;
      lines = lines + 1;
    };
    if (newkey < -5) {
      newkey = newkey + 12;
      lines = lines - 1;
    };
    setmap(newkey, newtable);  /* used by event_note1 */
    new_key_number = (int) keys[newkey+5][0] - (int) 'A';
    if  (modmap_not_empty (modmap)) {
        transpose_modmap(sharps,transpose,modmap,explict); 
        }
    
  };
  emit_string("K:");
  if (transpose == 0 && !nokey) {
    emit_string(s); 
  } else {
    if (gotkey) {
      if (!nokey) {
        /*  emit_string(keys[newkey+5]); */
        compute_keysignature(newkey,modeindex,signature); /* [SS] 2006-07-30*/
        emit_string(signature); /* [SS] 2006-07-30 */
        if (explict) emit_string(" exp"); /* 2011-02-21*/
        emit_string(trans_string); /* [SS] 2011-02-20 */
        }

      else if (usekey == 0) emit_string("none"); 
      else emit_string(keys[usekey+5]);
      if (gotclef) {
        emit_string(" ");
      };
    };
    if (gotclef) {
      emit_string_sprintf("clef=%s", clefname);
    };
    if (gotoctave) {
      emit_int_sprintf(" octave=%d", octave);
    };
    if (gottranspose) {
      emit_int_sprintf(" transpose=%d", xtranspose);
    };
  };
  if ((xinhead) && (!xinbody)) {
    xinbody = 1;
    start_tune();
  };
  inmusic = 0;
}

static void printlen(a, b)
int a, b;
{
  if (a != 1) {
    emit_int(a);
  };
  if (b != 1) {
    emit_int_sprintf("/%d", b);
  };
}

void event_spacing(n, m)
int n, m;
{
  emit_string("y");
  printlen(n, m);
}


void event_rest(decorators,n,m,type)
int n, m, type;
int decorators[DECSIZE];
{
  struct fract newlen;

  inmusic = 1;
  if( type == 1) emit_string("x");
  else emit_string("z");
  newlen.num = n * lenfactor.num;
  newlen.denom = m * lenfactor.denom;
  reduce(&newlen.num, &newlen.denom);
  printlen(newlen.num, newlen.denom);
  if (inchord) {
    chordcount = chordcount + 1;
  };
  if ((!ingrace) && (!inchord || (chordcount == 1))) {
    if (!tuplenotes) addunits(n, m);
    else {
      addunits(n*tuplefactor.num, m*tuplefactor.denom);
      tuplenotes = tuplenotes - 1;
      }
  };
}

void event_mrest(n,m)
int n, m;
{
  inmusic = 1;
  emit_string("Z");
  printlen(n,m);
  if (inchord) {
    event_error("Multiple bar rest not allowed in chord");
  };
  if (tuplenotes != 0) {
    event_error("Multiple bar rest not allowed in tuple");
  };
}

void event_bar(type, replist)
int type;
char* replist;
{
  char msg[40];

  if (!purgespace(tmp)) {
    if (inmusic) {
      newabctext(bar);
    } else {
      newabctext(field);
    };
  };
  switch(type) {
  case SINGLE_BAR:
    emit_string_sprintf("|%s", replist);
    break;
  case DOUBLE_BAR:
    emit_string("||");
    break;
  case THIN_THICK:
    emit_string("|]");
    break;
  case THICK_THIN:
    emit_string("[|");
    break;
  case BAR_REP:
    emit_string("|:");
    if (expect_repeat > 0 /* no error if first repeat [J-FM] 2012-06-04 */
     && repcheck) {
      event_error("Expecting repeat, found |:");
    };
    expect_repeat = 1;
    break;
  case REP_BAR:
    emit_string_sprintf(":|%s", replist);
    if ((!expect_repeat) && (repcheck)) {
      event_warning("No repeat expected, found :|");
    };
    expect_repeat = 0;
    break;
  case BAR1:
    emit_string("|1");
    if ((!expect_repeat) && (repcheck)) {
      event_warning("found |1 in non-repeat section");
    };
    break;
  case REP_BAR2:
    emit_string(":|2");
    if ((!expect_repeat) && (repcheck)) {
      event_warning("No repeat expected, found :|2");
    };
    expect_repeat = 0;
    break;
  case DOUBLE_REP:
    emit_string("::");
    if ((!expect_repeat) && (repcheck)) {
      event_error("No repeat expected, found ::");
    };
    expect_repeat = 1;
    break;
  };
  if ((count.num*barlen.denom != barlen.num*count.denom) &&
      (count.num != 0) && (barno != 0) && (barcheck)) {

/* [J-FM] 2012-06-04  start */
    switch (type) {
    case BAR_REP:
    case REP_BAR:
    case BAR1:
    case REP_BAR2:
    case DOUBLE_REP:
	break;		/* no error if repeat bar */
    default:
	sprintf(msg, "Bar %d is %d/%d not %d/%d", barno, 
		count.num, count.denom,
		barlen.num, barlen.denom );
	event_error(msg);
	count.num = 0;
	count.denom = 1;
	break;
    }
  } else {
	count.num = 0;
	count.denom = 1;
  }
/* [J-FM] 2012-06-04 end */

  newabctext(barline);
  barno = barno + 1;
  copymap();
}

void event_space()
{
  if (!newspacing) {
    emit_string(" ");
  };
}

void event_graceon()
{
  emit_string("{");
  ingrace = 1;
}

void event_graceoff()
{
  emit_string("}");
  ingrace = 0;
}

void event_rep1()
{
  emit_string(" [1");
}

void event_rep2()
{
  emit_string(" [2");
}

void event_playonrep(s)
char*s;
{
  emit_string_sprintf(" [%s", s);
}

void event_broken(type, n)
int type, n;
{
  int i;

  if (type == GT) {
    for (i=0; i<n; i++) {
      emit_char('>');
    };
  } else {
    for (i=0; i<n; i++) {
      emit_char('<');
    };
  };
}

void event_tuple(n, q, r)
int n, q, r;
{
  emit_int_sprintf("(%d", n);
  if (tuplenotes != 0) {
    event_error("tuple within tuple not allowed");
  };
  if (q != 0) {
    emit_int_sprintf(":%d", q);
    tuplefactor.num = q;
    tuplefactor.denom = n;
    if (r != 0) {
      emit_int_sprintf(":%d", r);
      tuplenotes = r;
    } else {
      tuplenotes = n;
    };
  } else {
    tuplenotes = n;
    tuplefactor.denom = n;
    if ((n == 2) || (n == 4) || (n == 8)) tuplefactor.num = 3;
    if ((n == 3) || (n == 6)) tuplefactor.num = 2;
    if ((n == 5) || (n == 7) || (n == 9)) {
      if ((barlen.num % 3) == 0) {
        tuplefactor.num = 3;
      } else {
        tuplefactor.num = 2;
      };
    };
  };
}

void event_startinline()
{
  emit_string("[");
  inlinefield = 1;
}

void event_closeinline()
{
  emit_string("]");
  inmusic = 1;
  inlinefield = 0;
}

void event_chord()
{
  if (cleanup) {
    if (inchord) {
      emit_string("]");
    } else {
      emit_string("[");
    };
  } else {
    emit_string("+");
  };
  inmusic = 1;
  inchord = 1 - inchord;
  chordcount = 0;
}

void event_chordon(int chorddecorators[])
{
  int i;
  for (i=0; i<DECSIZE; i++) {
    if (chorddecorators[i]) 
      emit_char(decorations[i]);
    }
  emit_string("[");
  inmusic = 1;
  inchord = 1;
  chordcount = 0;
}

void event_chordoff(int chord_n, int chord_m)
{
  char string[16];
  emit_string("]");
  if(chord_n !=1 && chord_m !=1)
     {
     sprintf(string,"%d/%d",chord_n,chord_m);
     emit_string(string);
     }
  else if(chord_n !=1)
     {
     sprintf(string,"%d",chord_n);
     emit_string(string);
     }
  else if(chord_m !=1)
     {
     sprintf(string,"/%d",chord_m);
     emit_string(string);
     }
  inmusic = 1;
  inchord = 0;
  if(chord_n !=1 || chord_m !=1)
    {
    repudiate_lastaddunits();
    addunits(chord_n,chord_m);
    }
}

static void splitstring(s, sep, handler)
char* s;
char sep;
void (*handler)();
/* this routine splits the string into fields using semi-colon */
/* and calls handler for each sub-string                       */
{
  char* out;
  char* p;
  int fieldcoming;

  p = s;
  fieldcoming = 1;
  while (fieldcoming) {
    out = p;
    while ((*p != '\0') && (*p != sep)) p = p + 1;
    if (*p == sep) {
      *p = '\0';
      p = p + 1;
    } else {
      fieldcoming = 0;
    };
    (*handler)(out);
  };
}

void event_handle_gchord(s)
/* deals with an accompaniment (guitar) chord */
/* either copies it straight out or transposes it */
char* s;
{
  char newchord[50];
  static int offset[7] = {9, 11, 0, 2, 4, 5, 7};
  static char* sharproots[12] = {"C", "C#", "D", "D#", "E", "F",
                            "F#", "G", "G#", "A", "A#", "B"};
  static char* flatroots[12] = {"C", "Db", "D", "Eb", "E", "F",
                            "Gb", "G", "Ab", "A", "Bb", "B"};
  static char* sharpbases[12] = {"c", "c#", "d", "d#", "e", "f",
                            "f#", "g", "g#", "a", "a#", "b"};
  static char* flatbases[12] = {"c", "db", "d", "eb", "e", "f",
                            "gb", "g", "ab", "a", "bb", "b"};
  char** roots;
  char** bases;
  int chordstart;
  int key_number;
  int old_triad_number;
  int new_triad_number;
  int triad_diff;

  if ((transpose == 0) || (*s == '_') || (*s == '^') || (*s == '<') ||
      (*s == '>') || (*s == '@')) {
    emit_string_sprintf("\"%s\"", s);
  } else {
    char* p;
    int pitch;
    int j;

    if (newkey >= 0) {
      roots = sharproots;
      bases = sharpbases;
    } else {
      roots = flatroots;
      bases = flatbases;
    };
    p = s;
    chordstart = 1;
    j = 0;
    while (*p != '\0') {
      if (chordstart) {
        if ((*p >= 'A') && (*p <= 'G')) {
          key_number = (int) *p - ((int) 'A');
	  old_triad_number = key_number - orig_key_number+1;
	  if (old_triad_number < 1) old_triad_number += 7;
          pitch = (offset[key_number] + transpose)%12;
          p = p + 1;
          if (*p == 'b') {
            pitch = pitch - 1;
            p = p + 1;
            };
          if (*p == '#') {
            pitch = pitch + 1;
            p = p + 1;
            };
          pitch = (pitch + 12)%12;
	  key_number = (int) roots[pitch][0] - (int) 'A';
	  new_triad_number = key_number - new_key_number +1;
	  if (new_triad_number < 1) new_triad_number += 7;
	  triad_diff = new_triad_number - old_triad_number;
	  if (!nodouble_accidentals && (triad_diff == -1 || triad_diff == 6)) {
		  /*      printf("*** %d  old chord = %s (%d) new chord = %s (%d)\n",
		        triad_diff,s,old_triad_number,roots[pitch],new_triad_number); */
	                pitch = pitch+1;
                        pitch = (pitch+12)%12;
                        strcpy(&newchord[j],roots[pitch]);
                        j = strlen(newchord);
                        strcpy(&newchord[j],"b");
                        j = j+1;
                        if (adapt_useflats_to_gchords) useflats=1;
	                } else
          if (!nodouble_accidentals && (triad_diff  ==1 || triad_diff == -6)) {
  	         /*	   printf("*** %d old chord = %s (%d) new chord = %s (%d)\n",
		         triad_diff,s,old_triad_number,roots[pitch],new_triad_number);  */

                        pitch = pitch-1;
                        pitch = (pitch+12)%12;
                        strcpy(&newchord[j],roots[pitch]);
                        j = strlen(newchord);
                        strcpy(&newchord[j],"#");
                        j = j+1;
                        if (adapt_useflats_to_gchords) useflats=0;
                        } else  
/* no extra flats or sharps needed */
                        {
                        strcpy(&newchord[j], roots[pitch]);
                        j = strlen(newchord);
                        }

          chordstart = 0;
        } else {
          if ((*p >= 'a') && (*p <= 'g')) {
            key_number = (int) *p - ((int) 'a');
	    old_triad_number = key_number - orig_key_number+1;
	    if (old_triad_number < 1) old_triad_number += 7;
            pitch = (offset[key_number] + transpose)%12;
            p = p + 1;
            if (*p == 'b') {
              pitch = pitch - 1;
              p = p + 1;
              };
            if (*p == '#') {
              pitch = pitch + 1;
              p = p + 1;
              };
            pitch = (pitch + 12)%12;
	    key_number = (int) bases[pitch][0] - (int) 'a';
	    new_triad_number = key_number - new_key_number +1;
	    if (new_triad_number < 1) new_triad_number += 7;
	    triad_diff = new_triad_number - old_triad_number;
	  if (!nodouble_accidentals && (triad_diff == -1 || triad_diff == 6)) {
          /*              printf("*** %d  old chord = %s (%d) new chord = %s (%d)\n",
			 triad_diff,s,old_triad_number,bases[pitch],new_triad_number); */
	                pitch = pitch+1;
                        pitch = (pitch+12)%12;
			strcpy(&newchord[j],bases[pitch]);
			j = strlen(newchord);
			strcpy(&newchord[j],"b");
			j = j+1;
	                } else
          if (!nodouble_accidentals && (triad_diff  ==1 || triad_diff == -6)) {
	/*	        printf("*** %d old chord = %s (%d) new chord = %s (%d)\n",
			 triad_diff,s,old_triad_number,bases[pitch],new_triad_number);*/
			 pitch = pitch-1;
			 pitch = (pitch+12)%12;
			 strcpy(&newchord[j],bases[pitch]);
			 j = strlen(newchord);
			 strcpy(&newchord[j],"#");
			 j = j+1;
                         } else
	                 {
                         strcpy(&newchord[j], bases[pitch]);
                         j = strlen(newchord);
			 }
            chordstart = 0;
          } else {
            if (isalpha(*p)) {
              chordstart = 0;
            };
            newchord[j] = *p;
            p = p + 1;
            j = j + 1;
            newchord[j] = '\0';
          };
        };
      } else {
        if ((*p == '/') || (*p == '(') || (*p == ' ')) {
          chordstart = 1;
        };

        newchord[j] = *p;
        p = p + 1;
        j = j + 1;
        newchord[j] = '\0';
      };
      if (j >= 49) {
        event_error("guitar chord contains too much text");
        while (*p != '\0') {
          p = p + 1;
        };
      };
    };
    emit_string_sprintf("\"%s\"", newchord);
  };
}

void event_gchord(s)
char* s;
{
  splitstring(s, ';', event_handle_gchord);
}

void event_instruction(s)
char* s;
{
  if (oldchordconvention || noplus) emit_string_sprintf("!%s!", s);
  else emit_string_sprintf("+%s+", s);
}

void event_slur(t)
int t;
{
  if (cleanup) {
    if (t) {
      emit_string("(");
    } else {
      emit_string(")");
    };
  } else {
    emit_string("s");
  };
}

void event_sluron(t)
int t;
{
  emit_string("(");
}

void event_sluroff(t)
int t;
{
  emit_string(")");
}

void event_tie()
{
  emit_string("-");
}

void event_lineend(ch, n)
char ch;
int n;
{
  int i;

  if (!newbreaks) {
    for (i = 0; i<n; i++) {
      emit_char(ch);
    };
  };
}



void event_note1(decorators, xaccidental, xmult, xnote, xoctave, n, m)
int decorators[DECSIZE];
int xmult;
char xaccidental, xnote;
int xoctave, n, m;
{
  int t;
  struct fract barpoint;
  struct fract newlen;
  int mult;
  char accidental, note;
  int octave;

  mult = 0;  /* [SS] 2006-10-27 */
  if (transpose == 0 || drumchan) {
    accidental = xaccidental;
    mult = xmult;
    note = xnote;
    octave = xoctave;
  } else {
    int val, newval;
    int acc;
    char *anoctave = "cdefgab";

    octave = xoctave;
    val = (int) ((long) strchr(anoctave, xnote) - (long) anoctave);
    newval = val + lines;
    octave = octave + (newval/7);
    newval = newval % 7;
    if (newval < 0) {
      newval = newval + 7;
      octave = octave - 1;
    };
    note = *(anoctave+newval);
    if (xaccidental == ' ') {
      accidental = ' ';
    } else {
      switch (xaccidental) {
      case '_':
        acc = -xmult;
        break;
      case '^':
        acc = xmult;
        break;
      case '=':
        acc = 0;
        break;
      default:
        event_error("Internal error");
      };
      acc = acc - oldtable[(int)anoctave[val] - (int)'a'] + 
                  newtable[(int)anoctave[newval] - (int)'a'];
      mult = 1;
      accidental = '=';
      if (acc > 0) {
        accidental = '^';
        mult = acc;
      };
      if (acc < 0) {
        accidental = '_';
        mult = -acc;
      };
    };
  };    
  if (!ingrace) {
    notecount = notecount + 1;
  };
  for (t=0; t<DECSIZE; t++) {
    if (decorators[t]) {
      emit_char(decorations[t]);
    };
  };
  if (mult == 2) {
    emit_char(accidental);
  };
  if (accidental != ' ') {
    emit_char(accidental);
  };
  if (octave >= 1) {
    emit_char(note);
    t = octave;
    while (t > 1) {
      emit_string("'");
      t = t - 1;
    };
  } else {
    emit_char((char) ((int)note + 'C' - 'c'));
    t = octave;
    while (t < 0) {
      emit_string(",");
      t = t + 1;
    };
  };
  newlen.num = n * lenfactor.num;
  newlen.denom = m * lenfactor.denom;
  reduce(&newlen.num, &newlen.denom);
  printlen(newlen.num, newlen.denom);
  if (inchord) {
    chordcount = chordcount + 1;
  };
  if ((!ingrace) && (!inchord || (chordcount == 1))) {
    if (tuplenotes == 0) {
      addunits(n, m);
    } else {
      addunits(n*tuplefactor.num, m*tuplefactor.denom);
      tuplenotes = tuplenotes - 1;
    };
  };
  if (newspacing) {
    barpoint.num = count.num * breakpoint.denom;
    barpoint.denom = breakpoint.num * count.denom;
    reduce(&barpoint.num, &barpoint.denom);
    if ((barpoint.denom == 1) && (barpoint.num != 0) && 
        (barpoint.num != barend)) {
      emit_string(" ");
    };
  };
}

/* these functions are here to satisfy the linker */
void event_microtone(int dir, int a, int b)
{
}

void event_normal_tone()
{
}



int accidental_to_code (char xaccidental)
{
 switch (xaccidental) {
 case ' ':
   return 10;
   break;
 case '_':
   return -1;
   break;
 case '^':
   return 1;
   break;
  case '=':
    return 0;
    break;
  default:
    return 10;
  }
}


void event_note2(decorators, xaccidental, xmult, xnote, xoctave, n, m)
/* this function is called if flag nokey is set */
int decorators[DECSIZE];
int xmult;
char xaccidental, xnote;
int xoctave, n, m;
{
  int t;
  struct fract barpoint;
  struct fract newlen;

  int acc,assumed_acc;
  int propogate;
  int val;
  char *anoctave = "cdefgab";
  int midipitch;

 for (t=0; t<DECSIZE; t++) 
    if (decorators[t]) emit_char(decorations[t]);

  acc = accidental_to_code(xaccidental);
  if (acc == -1 || acc == 1) acc = xmult*acc;
  val = (int) ((long) strchr(anoctave, xnote) - (long) anoctave);

  /* if no accidental precedes, then get accidental from key signature */
  assumed_acc = oldtable[(int)anoctave[val] - (int)'a']; 
  propogate=0;
  midipitch = pitchof(xnote, acc, xmult, xoctave);
  if (drumchan) printpitch(midipitch);
  else printpitch(midipitch+transpose);

  if (!ingrace) {
    notecount = notecount + 1;
  };

  newlen.num = n * lenfactor.num;
  newlen.denom = m * lenfactor.denom;
  reduce(&newlen.num, &newlen.denom);
  printlen(newlen.num, newlen.denom);
  if (inchord) {
    chordcount = chordcount + 1;
  };
  if ((!ingrace) && (!inchord || (chordcount == 1))) {
    if (tuplenotes == 0) {
      addunits(n, m);
    } else {
      addunits(n*tuplefactor.num, m*tuplefactor.denom);
      tuplenotes = tuplenotes - 1;
    };
  };
  if (newspacing) {
    barpoint.num = count.num * breakpoint.denom;
    barpoint.denom = breakpoint.num * count.denom;
    reduce(&barpoint.num, &barpoint.denom);
    if ((barpoint.denom == 1) && (barpoint.num != 0) && 
        (barpoint.num != barend)) {
      emit_string(" ");
    };
  };
}


void event_note(decorators, xaccidental, xmult, xnote, xoctave, n, m)
int decorators[DECSIZE];
int xmult;
char xaccidental, xnote;
int xoctave, n, m;
{
if (nokey)
  event_note2(decorators, xaccidental, xmult, xnote, xoctave, n, m);
else
  event_note1(decorators, xaccidental, xmult, xnote, xoctave, n, m);
}


void event_abbreviation(char symbol, char *string, char container)
/* a U: field has been found in the abc */
{
  if (container == '!') {
    emit_string("U:");
    emit_char(symbol);
    emit_string_sprintf(" = !%s!", string);
  } else {
    emit_string("U:");
    emit_char(symbol);
    emit_string_sprintf(" = %s", string);
  };
  inmusic = 0;
}

void event_acciaccatura()
{
/* to handle / in front of note in grace notes eg {/A} */
/* abcm2ps compatability feature [SS] 2005-03-28 */
emit_string("/");
}

void event_split_voice ()
{
/* code contributed by Frank Meisshaert 2012-05-31 */
char msg[40];
  emit_string("&");
  if ((count.num*barlen.denom != barlen.num*count.denom) &&
      (count.num != 0) && (barno != 0) && (barcheck)) {
    sprintf(msg, "Bar %d is %d/%d not %d/%d", barno, 
           count.num, count.denom,
           barlen.num, barlen.denom );
    event_error(msg);
  };
  count.num = 0;
  count.denom = 1;
}
 

/* The following functions provide an alternative
   method for transposing. The note is converted
   to MIDI representation. It is transposed and
   then converted back. These functions were
   borrowed from store.c and midi2abc with
   some minor modifications.
*/


int pitchof(char note, int accidental, int mult, int octave)
/* finds MIDI pitch value for a given note taking into account the  */
/* key signature if any and propogation of accidentals across a bar.*/
/* accidental = 0  implies a natural specified.                     */
/* accidental = 1  implies one or more sharps indicated by mult.    */
/* accidental =-1  implies one or more flats indicated by mult.     */
/* accidental = 10 nothing specified, determine from context        */
{
  int p;
  int mul, noteno;
  static int scale[7] = {0, 2, 4, 5, 7, 9, 11};
  char *anoctave = "cdefgab";
  int middle_c = 60;

  p = (int) ((long) strchr(anoctave, note) - (long) anoctave);
  p = scale[p];
  mul = mult;
  noteno = (int)note - 'a';
  if (accidental==10) {        /* propagate accidentals  */
    mul = workmul[noteno];
  } else {
      workmap[noteno] = accidental;
      workmul[noteno] = mul;
  };
  p = p + workmap[noteno]*workmul[noteno];
  return p + 12*octave + middle_c;
}

int lastaccidental[7];

void copymap()
/* sets up working map at the start of each bar */
{
  int j;

  for (j=0; j<7; j++) {
    workmap[j] = basemap[j];
    workmul[j] = 1;
    lastaccidental[j] = 0;
  };
}

#define MIDDLE 72


int sharpmap[12] =  { 0, 0, 1, 1, 2, 3, 3, 4, 4, 5, 5, 6};
int flatmap[12]  =  { 0, 1, 1, 2, 2, 3, 4, 4, 5, 5, 6, 6};
int sharpsym[12] =  { 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0};
int flatsym[12]  =  { 0,-1, 0,-1, 0, 0,-1, 0,-1, 0,-1, 0};
char hikey[7]    =  {'c','d','e','f','g','a','b'};
char lowkey[7]   =  {'C','D','E','F','G','A','B'};
char symlet[3]   =  {'=','^','_'};


void setup_sharps_flats (int sf)
{
if (sf >=1) {sharpsym[6] = 0; sharpsym[5] = 2;}
if (sf >=2) {sharpsym[1] = 0; sharpsym[0] = 2;}
if (sf >=3) {sharpsym[8] = 0; sharpsym[7] = 2;}
if (sf >=4) {sharpsym[3] = 0; sharpsym[2] = 2;}
if (sf >=5) {sharpsym[10]= 0; sharpsym[9] = 2;}
if (sf <= -1) {flatsym[10] = 0; flatsym[11] = 2;}
if (sf <= -2) {flatsym[3]  = 0; flatsym[4]  =2;}
if (sf <= -3) {flatsym[8]  = 0; flatsym[9]  =2;}
if (sf <= -4) {flatsym[1]  = 0; flatsym[2]  =2;}
if (sf <= -5) {flatsym[6]  = 0; flatsym[6]  =2;}
}



void printpitch(int pitch)
/* convert midi pitch value to abc note */
{
int p;
char keylet,symlet;
int keynum,symcod;
char string[16];
p = pitch%12;
if (useflats)
 {keynum = flatmap[p];
  symcod = flatsym[p];
 } else
 {keynum = sharpmap[p];
  symcod = sharpsym[p];
 }

if (pitch<MIDDLE)
  keylet = lowkey[keynum];
else
  keylet = hikey[keynum];

switch (symcod) {
  case 0:
   symlet = '=';
   break;
  case 1:
   symlet = '^';
   break;
  case -1:
   symlet = '_';
   break;
  case 2:
   symlet = '=';
   break;
}
 
if (lastaccidental[keynum] == symcod)
   sprintf(string,"%c",keylet);
else {
   sprintf(string,"%c%c", symlet, keylet);
   lastaccidental[keynum] = symcod;
   }
  emit_string(string);

p = pitch;
while (p >= MIDDLE + 12) {
    emit_string("'");
    p = p - 12;
    };
while (p < MIDDLE - 12) {
    emit_string(",");
    p = p + 12;
    };
}


int main(argc,argv)
int argc;
char *argv[];
{
  char *filename;

  oldchordconvention = 0; /* for handling +..+ chords */
  noplus = 1;  /* [SS] 2012-06-04 */

  /*for (i=0;i<DECSIZE;i++) decorators_passback[i]=0; */

  event_init(argc, argv, &filename);
  if (argc < 2) {
    /* printf("argc = %d\n", argc); */
  } else {
    init_abbreviations();
    parsefile(filename);
    free_abbreviations();
  };
  return(0);
}
