/* abcmatch.c 
This contains the main program and core functions of the program to
compare abc files. 

The abc file may either be a single tune or a compilation
of tunes. The program searches for specific bars in the
file and returns its positions in the file. There are
various matching criteria ranging from exact to approximate
match. The program assumes there is a file called match.abc
containing a description of the bars to be matched. Though
you can run abcmatch as a stand alone program, it was 
really designed to run with a graphical user interface such
as runabc.tcl (version 1.59 or higher).


Limitations: 
tied notes longer than 8 quarter notes are ignored.
*/



#define VERSION "1.43 August 08 2012"
#include <stdio.h>
#include <stdlib.h>
#include "abc.h"
#include "parseabc.h"

#define MAX(A, B) ((A) > (B) ? (A) : (B))
#define MIN(A, B) ((A) < (B) ? (A) : (B))

int* checkmalloc(int);
extern int getarg(char *, int, char **);
void free_feature_representation();

/* variables shared with abcparse.c and abcstore.c */
/* many of these variables are used in event_int which has
   been moved to this file.
*/
extern int *pitch,  *num, *denom, *pitchline;
extern char  **atext,**words;
extern featuretype *feature;
extern int notes;
extern int note_unit_length;
extern int time_num,time_denom;
extern int sf, mi;
extern FILE *fp;
extern int xrefno;
extern int check, nowarn, noerror, verbose, maxnotes, xmatch,dotune;
extern int maxtexts, maxwords;

/* midipitch: pitch in midi units of each note in the abc file. A pitch
   of zero is reserved for rest notes. Bar lines are signaled with -1000.
   If contour matching is used, then all the pitch differences are offsetted
   by 256 to avoid the likelihood of a zero difference mistaken for a
   rest.
   notelength: represents the length of a note. A quarter note is 24 units
   (assuming L:1/4 in abc file) and all other note sizes are proportional.
   Thus a half note is 48 units, a sixteenth note is 6 units.
   nnotes: the number of notes in the midipitch array.
   nbars:  the number of bar lines in the midipitch array.
   barlineptr: indicates the index in midipitch, notelength of the start
   of bar lines.
*/

/* data structure for input to matcher. */
int imidipitch[2000]; /* pitch-barline midi note representation of input tune */
int inotelength[2000]; /* notelength representation of input tune */
int  innotes; /*number of notes in imidipitch,inotelength representation */
int  inbars; /*number of bars in input tune */
int ibarlineptr[600]; /*pointers to bar lines in imidipitch */
int itimesig_num,itimesig_denom;
int imaxnotes = 2000; /* maximum limits of this program */
int imaxbars  = 200;
int resolution = 12; /* default to 1/8 note resolution */
int anymode = 0; /* default to matching all bars */
int ignore_simple = 0; /* ignore simple bars */
int con = 0; /* default for no contour matching */
int qnt = 0; /* pitch difference quantization flag */
int brief = 0;  /* set brief to 1 if brief mode */
int cthresh =3; /* minimum number of common bars to report */ 
int fileindex = -1;
int phist,lhist;  /* flags for computing pitch or length histogram */

/* data structure for matcher (template) (usually match.abc)*/
int mmidipitch[1000]; /*pitch-barline representation for template */
int mnotelength[1000]; /*note lengths for template */
int  mnnotes; /* number of notes in template */
int  mnbars;  /* number of bar lines in template */
int mbarlineptr[300]; /* I don't expect 300 bar lines, but lets play safe */
int mtimesig_num,mtimesig_denom;
int mmaxnotes = 1000; /* maximum limits of this program */
int mmaxbars  = 300;

int pitch_histogram[128];
int length_histogram[144];

/* compute the midi offset for the key signature. Since
   we do not have negative indices in arrays, sf2midishift[7]
   corresponds to no flats/no sharps, ie. C major scale.
   If sf is the number of sharps (when positive) and the number of
   flats when negative, then sf2midishift[7+i] is the offset in
   semitones from C. Thus G is 7, D is 2, Bb is 10 etc.
*/
int sf2midishift[] = {11, 6, 1, 8, 3, 10, 5, 0, 7, 2, 9, 4, 11, 6, 1, }; 


/* when using a resolution greater than 0, time resolution
 * is reduced by a certain factor. The resolution is reduced
 * in the arrays ipitch_samples and mpitch_samples. We need
 * more room in mpitch_samples for the template, since we
 * use this template over and over. Therefore we compute it
 * and store it for the entire template. On the other hand, the
 * input tune is always changing, so we only store one  bar at
 * a time for the input tune.
 */

int ipitch_samples[400],isamples;
int mpitch_samples[4000],msamples[160]; /* maximum number of bars 160 */





void make_note_representation (int *nnotes, int *nbars, int maxnotes, int maxbars,
 int *timesig_num, int *timesig_denom, int *barlineptr, int *notelength,
 int *midipitch)
/* converts between the feature,pitch,num,denom representation to the
   midipitch,notelength,... representation. This simplification does
   not preserve chords, decorations, grace notes etc.
*/
{
float fract;
int i;
int skip_rests,multiplier,inchord,ingrace;
int maxpitch;
*nnotes = 0;
*nbars  = 0;
inchord = 0;
ingrace = 0;
skip_rests = 0;
*timesig_num   = time_num;
*timesig_denom = time_denom;
multiplier = (int) 24.0;
barlineptr[*nbars] =0;
for (i=0;i<notes;i++) {
  switch (feature[i]) {
    case NOTE:
      if (inchord) maxpitch = MAX(maxpitch,pitch[i]);
      else if (ingrace) break;
      else {
        midipitch[*nnotes] = pitch[i];
        fract = (float) num[i] / (float) denom[i];
        notelength[*nnotes] = (int) (fract * multiplier + 0.01);      
        (*nnotes)++;
           }
      break;
    case TNOTE:
      midipitch[*nnotes] = pitch[i];
      fract = (float) num[i] / (float) denom[i];
      notelength[*nnotes] = (int) (fract * multiplier + 0.01);      
      (*nnotes)++;
      skip_rests = 2;
      break;
    case REST:
      if(skip_rests >0) {skip_rests--;break;} else    /* for handling tied notes */
       {midipitch[*nnotes] = pitch[i];
       fract = (float) num[i] / (float) denom[i];
       notelength[*nnotes] = (int) (fract * multiplier + 0.01);      
       (*nnotes)++;
       break;}
    case CHORDON:
      inchord = 1;
      maxpitch = 0;
      break;
    case CHORDOFF:
      inchord = 0;
      midipitch[*nnotes] = maxpitch;
      fract = (float) num[i] / (float) denom[i];
      notelength[*nnotes] = (int) (fract * multiplier + 0.01);      
      (*nnotes)++;
      break;
    case GRACEON:
      ingrace = 1;
      break;
    case GRACEOFF:
      ingrace = 0;
      break;
    case DOUBLE_BAR:
    case SINGLE_BAR:
    case REP_BAR:
    case REP_BAR2:
    case BAR_REP:
      midipitch[*nnotes] =  -1000;
      notelength[*nnotes] = -1000;
      (*nnotes)++;
      (*nbars)++;
      barlineptr[*nbars] = *nnotes;
      break;
    case TIME:
      *timesig_num =   num[i];
      *timesig_denom = denom[i];
      break;
    default:
      break;
   }
  if (*nnotes > 2000) {printf("ran out of space for midipitch\n");  exit(0);}
  if (*nbars  > 599)  {printf("ran out of space for barlineptr\n"); exit(0);}
 }
midipitch[*nnotes+1] = -1000;  /* in case a final bar line is missing */
/*printf("refno =%d  %d notes %d bar lines  %d/%d time-signature %d sharps\n"
,xrefno,(*nnotes),(*nbars),(*timesig_num),(*timesig_denom),sf);*/
}


/* This function is not used yet. */
int quantize5(int pitch) 
{
if (pitch < -4) return -2;
if (pitch < -1) return -1;
if (pitch > 4) return 2;
if (pitch > 1) return 1;
return 0;
}

void compute_pitch_contour(int nnotes, int * midipitch , int qntflag)
{
/* computes the pitch difference between adjacent musical note in 
   midipitch array. To avoid confusion between the rest indication = 0
   we add an offset of 256 to the difference if the note is not a rest.
*/
int lastpitch,newpitch;
int i;
lastpitch = -1;
for (i=0;i<nnotes;i++)
  {
  if (midipitch[i] == -1000 || midipitch[i] == 0) continue; /* ignore all bar line  and rest indications */
  if (lastpitch <0) {
      lastpitch = midipitch[i];
      midipitch[i] = -1;
      continue;} /* -1 means unknown and matches anything */

  else {newpitch = midipitch[i];
        midipitch[i] = midipitch[i] - lastpitch;
        lastpitch = newpitch;
        if (qntflag > 0) midipitch[i] = quantize5(midipitch[i]);
        midipitch[i] += 256;
        } 
  }
}


void init_histograms()
{
int i;
for (i=0;i<128;i++) pitch_histogram[i] =0;
for (i=0;i<144;i++) length_histogram[i] =0;
}
 

void compute_note_histograms()
{
int i,index;
for (i=0;i<innotes;i++)
 {
 index =  imidipitch[i];
 pitch_histogram[index]++;
 index =  inotelength[i];
 if (index > 143) index =143;
 length_histogram[index]++;
 }
}


void print_length_histogram()
{
int i;
printf("\n\nlength histogram\n");
for (i=0;i<144;i++)
  if(length_histogram[i] > 0)
     printf("%d %d\n",i,length_histogram[i]);
}


void print_pitch_histogram()
{
int i;
printf("\n\npitch_histogram\n");
for (i=0;i<128;i++)
  if(pitch_histogram[i] > 0)
     printf("%d %d\n",i,pitch_histogram[i]);
}





int make_bar_image (int bar_number, int resolution, int *pitch_samples,
   int *barlineptr, int *notelength, int nnotes, int delta_pitch, int *midipitch)
{
/* the function returns the midipitch at regular time interval
   for bar %d xref %d\n,bar_number,xrefnos
   (set by resolution) for a particular bar. Thus if you have
   the notes CDEF in a bar (L=1/8), then integrated_length
   will contain 12,24,36,48. If resolution is set to 6, then
   pitch_samples will return the pitch at time units 0, 6, 12,...
   mainly CCDDEEFF or 60,60,62,62,64,64,65,65 in midipitch.
   The pitch is shifted by delta_pitch to account for a different
   key signature in the matching template.
*/
int integrated_length[50]; /* maximum of 50 notes in a bar */
/* integrated_length is the number of time units in the bar after note i;
*/
int offset,lastnote,lastpulse,lastsample;
int i,j,t;
offset = barlineptr[bar_number];
/* double bar is always placed at the beginning of the tune */

i = 1;
integrated_length[0] = notelength[offset];
lastnote=0;
while (notelength[i+offset] != -1000) {
 if(notelength[i+offset]>288) return -1; /* don't try to handle notes longer than 2 whole */
 integrated_length[i] = integrated_length[i-1] + notelength[i+offset];
 lastnote = i;
 i++;
 if (i+offset > nnotes) {
      /* printf("make_bar_image -- running past last note for bar %d xref %d\n",bar_number,xrefno);*/
      break;}
 if (i > 49) {
	 printf("make_bar_image -- bar %d has too many notes for xref %d\n",bar_number,xrefno);
              break;
             }
 }
lastpulse = integrated_length[lastnote];
i = 0;
j = 0;
t = 0;
while  (t < lastpulse) {
  while  (t >= integrated_length[i]) i++;
  while  (t <  integrated_length[i]) {
  if (midipitch[i+offset] == 0) pitch_samples[j] = 0;  /* REST don't transpose */
  if (midipitch[i+offset] == -1) pitch_samples[j] = -1; /*also doesn't tranpose*/
  else pitch_samples[j] = midipitch[i+offset]+delta_pitch;
  j++;
  t += resolution;
  if (j >= 400) {printf("make_bar_image -- pitch_sample is out of space for bar %d xrefno = %d\n",bar_number,xrefno);
                break;
                }
  }
}
lastsample = j;
t = 0;
return lastsample;
}


/* absolute match - matches notes relative to key signature */
/* It is called if the resolution variable is set to 0.     */
/* -------------------------------------------------------- */

int match_notes (int mbar_number, int ibar_number, int delta_pitch)
{
int i,notes;
int ioffset,moffset;
ioffset = ibarlineptr[ibar_number];
moffset = mbarlineptr[mbar_number];
i = 0;
notes = 0;
if (mmidipitch[moffset] == -1000) return -1; /* in case nothing in bar */
while (mmidipitch[i+moffset] !=  -1000)
  {
  if (imidipitch[i+ioffset] == 0 && mmidipitch[i + moffset] == 0)
    {i++;  continue;} /* REST -- don't transpose */
  if (imidipitch[i+ioffset] == -1 || mmidipitch[i+moffset] == -1)
    {i++; continue;} /* unknown contour note */
  if (imidipitch[i+ioffset] != (mmidipitch[i + moffset] - delta_pitch) ) return -1;
  if (inotelength[i+ioffset] != mnotelength[i + moffset]) return -1;
  i++;
  notes++;
  }
if (imidipitch[i+ioffset] != -1000) return -1; /* in case template has fewer notes*/
if (notes > 2) return 0;
if (ignore_simple) return -1;
return 0;
}



int match_samples (int mmsamples, int * mmpitch_samples)
{
int i,dif;
int changes;
int last_sample;
if (mmsamples != isamples) return -1;
dif = 0;
changes = 0;
last_sample = ipitch_samples[0]; /* [SS] 2012-02-05 */
for (i=0;i<mmsamples;i++) {
 if (ipitch_samples[i] == -1 || mmpitch_samples[i] == -1) 
      continue; /* unknown contour note (ie. 1st note) */
 if (ipitch_samples[i] != mmpitch_samples[i]) {dif++; break;}
 if (last_sample != ipitch_samples[i]) {
    last_sample = ipitch_samples[i];
    changes++;
    }
 }
if (ignore_simple && changes < 3) return -1;
return dif;
}




int match_any_bars (int mnbars,int barnum,int delta_key, int nmatches)
{
/* This function reports any bars in match template  match a paticular
 * bar (barnum) in the tune. If a match is found then barnum is 
 * reported.  It runs in one of two modes depending on the value
 * of the variable called resolution.
 */
int kmatches;
int moffset,j,dif;
/* for every bar in match sample */
kmatches = nmatches;
moffset  = 0;
if (resolution > 0) 
  {
  isamples = make_bar_image(barnum,resolution,ipitch_samples,
     ibarlineptr, inotelength, innotes, delta_key, imidipitch);
  if(isamples <1) return kmatches;
  for(j=0;j<mnbars;j++) {
    dif = match_samples(msamples[j],mpitch_samples + moffset);
    moffset += msamples[j];
    if (dif == 0) {
         kmatches++;
        if (kmatches == 1) printf("%d %d  %d ",fileindex,xrefno,barnum-1); 
/* subtract one from bar because first bar always seems to be 2 */
    else printf(" %d ",barnum-1);
        break;
       }
   }
 }
else /* exact match */
 {
 for (j=0;j<mnbars;j++)
   {
   dif = match_notes(j,barnum,delta_key);
   if (dif == 0) {
        kmatches++;
        if (kmatches == 1) printf("%d %d  %d ",fileindex,xrefno,barnum-1); 
/* subtract one from bar because first bar always seems to be 2 */
        else printf(" %d ",barnum-1);
        break;
        } /* dif condition */
   } /*for loop */
 } /* resolution condition */
return kmatches;
}


int match_all_bars (int mnbars,int barnum,int delta_key, int nmatches)
{
/* This function tries to match all the bars in the match template
   with  the bars in the bars in the tune. All the template bars
   must match in the same sequence in order to be reported.
   It runs in one of two modes depending on the value of resolution.
*/
int kmatches;
int moffset,j,dif;
int matched_bars[20];
/* for every bar in match sample */
kmatches = 0;
moffset  = 0;
if (resolution > 0) 
      for(j=0;j<mnbars;j++) {
        isamples = make_bar_image(barnum+j,resolution,ipitch_samples,
         ibarlineptr, inotelength, innotes, delta_key, imidipitch);
         dif = match_samples(msamples[j],mpitch_samples + moffset);
         moffset += msamples[j];
         if (dif != 0) return nmatches;
         matched_bars[kmatches] = barnum+j-1;
         kmatches++;
         if(j>15) break;
         }
else
      for(j=0;j<mnbars;j++) {
         dif = match_notes(j,barnum+j,delta_key);
         if (dif != 0) return nmatches;
         matched_bars[kmatches] = barnum+j-1;
         kmatches++;
         if(j>15) break;
         }
      
if (nmatches == 0) printf("%d %d ",fileindex,xrefno); 
for (j=0;j<kmatches;j++)
  printf("%d ",matched_bars[j]);
return kmatches+nmatches;
}



void find_and_report_matching_bars 
/* top level matching function. Distinguishes between,
   any, all and contour modes and calls the right functions
*/
  (int mnbars, int inbars, int transpose, int anymode, int con)
  {
  int i, nmatches;
  if (con == 1) {
       compute_pitch_contour(innotes, imidipitch, qnt);
       transpose = 0; /* not applicable here */
       }
    nmatches = 0;
    if (anymode == 1)  /* match any bars in template */
      
      for (i=1;i<inbars;i++) {
           nmatches = match_any_bars(mnbars,i,transpose,nmatches); 
        }

    else   /* match all bars in template in sequence */

      for (i=1;i<=inbars-mnbars;i++) {
           nmatches = match_all_bars(mnbars,i,transpose,nmatches); 
           }

    if (nmatches > 0) printf("\n");
  }  


int find_first_matching_tune_bar (int mbarnumber, int inbars, int transpose)
/* given a bar number in the template, the function looks for
 * the first bar in the tune which matches the template.
 * Unfortunately since we are only looking for single bars,
 * it may not be useful to play around with the resolution
 * parameter.
 */
{
int i,dif;
for (i=1 ; i<inbars; i++)
  {
  dif = match_notes(mbarnumber, i, transpose);
  if (dif == 0) return i;
  }
return -1;
}



/* The next two functions are not used presently. */

int find_first_matching_template_bar (int barnumber, int mnbars, int transpose)
/* given a bar number in the tune, the function looks for
 * the first bar in the template which matches the tune bar.
 */
{
int i,dif;
for (i=1 ; i<mnbars; i++)
  {
  dif = match_notes(i, barnumber, transpose);
  if (dif == 0) return i;
  }
return -1;
}

 
int count_matched_template_bars (int mnbars, int inbars, int transpose)
{
int i, count,bar;
count = 0;
for (i=0; i<mnbars; i++)
 {
 bar = find_first_matching_tune_bar (i, inbars, transpose);
 /*if (bar >= 0) printf ("bar %d matches %d\n",bar,i);*/
 if (bar >= 0) count++;
 }
return count;
}


int count_matched_tune_bars (int mnbars, int inbars, int transpose)
/* used only by brief mode */
{
int i, count,bar;
count = 0;
for (i=0; i<inbars; i++)
 {
 bar = find_first_matching_template_bar (i, inbars, transpose);
/* if (bar >= 0) printf ("bar %d matches %d\n",bar,i); */
 if (bar >= 0) count++;
 }
return count;
}

 


void event_init(argc, argv, filename)
/* this routine is called first by abcparse.c */
int argc;
char* argv[];
char **filename;
{
  int j;

  /* look for code checking option */
  if (getarg("-c", argc, argv) != -1) {
    check = 1;
    nowarn =0;
    noerror=0;
  } else {
    check = 0;
  };
  if (getarg("-ver",argc,argv) != -1) {
     printf("%s\n",VERSION);
     exit(0);
     }
  /* look for verbose option */
  if (getarg("-v", argc, argv) != -1) {
    verbose = 1;
  } else {
    verbose = 0;
  };
  j = getarg("-r",argc,argv);
  if (j != -1) sscanf(argv[j],"%d",&resolution);
  if (getarg("-a",argc,argv) != -1) anymode = 1;
  if (getarg("-ign",argc,argv) != -1) ignore_simple = 1;
  if (getarg("-con",argc,argv) != -1) con = 1;
  if (getarg("-qnt",argc,argv) != -1) {qnt = 1; con =1;}
  j =getarg("-br",argc,argv);
  if (j != -1) {
    sscanf(argv[j],"%d",&cthresh); 
    brief=1;
    }

  phist = getarg("-pitch_hist",argc,argv);
  lhist = getarg("-length_hist",argc,argv);

  if (phist >= 0) phist = 1; else phist = 0;
  if (lhist >= 0) lhist = 1; else lhist = 0;

  if (brief == 1) resolution=0; /* do not compute msamples in main() */
  maxnotes = 3000;
  /* allocate space for notes */
  pitch = checkmalloc(maxnotes*sizeof(int));
  num = checkmalloc(maxnotes*sizeof(int));
  denom = checkmalloc(maxnotes*sizeof(int));
  pitchline = checkmalloc(maxnotes*sizeof(int));
  feature = (featuretype*) checkmalloc(maxnotes*sizeof(featuretype));
  /* and for text */
  atext = (char**) checkmalloc(maxtexts*sizeof(char*));
  words = (char**) checkmalloc(maxwords*sizeof(char*));
  if ((getarg("-h", argc, argv) != -1) || (argc < 2)) {
    printf("abcmatch version %s\n",VERSION);
    printf("Usage : abcmatch <abc file> [reference number] [-options] \n");
    printf("        [reference number] selects a tune\n");
    printf("        -c returns error and warning messages\n");
    printf("        -v selects verbose option\n");
    printf("        -r resolution for matching\n");
    printf("        -con  pitch contour match\n");
    printf("        -qnt contour quantization\n");
    printf("        -ign  ignore simple bars\n");
    printf("        -a report any matching bars (default all bars)\n");
    printf("        -br %%d only report number of matched bars when\n\
	    above given threshold\n");
    printf("        -ver returns version number\n");
    printf("        -pitch_hist pitch histogram\n");
    printf("        -length_hist pitch histogram\n");
    exit(0);
  } else {
    xmatch = 0;
    *filename = argv[1];
  };
  /* look for user-supplied output filename */
  dotune = 0;
  parseroff();
}



int main(argc,argv)
int argc;
char *argv[];
{
  char *filename;
  int i,j;
  int ikey,mkey;
  int moffset;
  int transpose;
  int mseqno;
  /* sequence number of template (match.abc) 
   * mseqno can differ from xrefnum when running count_matched_tune_bars
   * because there is no guarantee the xref numbers are in
   * sequence in the input file. Hopefully fileindex matches sequence
   * number in script calling this executable.
   */

  int kfile,count;
 
/* initialization */
  event_init(argc, argv, &filename);
  init_histograms();
  init_abbreviations();


/* get the search template from the file match.abc written
   in abc format. This file is automatically generated by
   runabc.tcl when you are using this search function.
*/
 
if (!(phist | lhist)) {  /* if not computing histograms */
  parsefile("match.abc");
  mkey = sf2midishift[sf+7];
  mseqno = xrefno; /* if -br mode, X:refno is file sequence number */
  /* xrefno was set by runabc.tcl to be file sequence number of tune */
  /*print_feature_list();*/
  make_note_representation(&mnnotes, &mnbars, mmaxnotes, mmaxbars,
      &mtimesig_num, &mtimesig_denom,mbarlineptr,mnotelength,mmidipitch);

  /* trim off any initial bar lines */
  for (i=0;i<mnbars;i++)
    {
    j = i;
    if(mmidipitch[mbarlineptr[i]] != -1000) break;
    }
  for (i=j;i<mnbars;i++)
    mbarlineptr[i-j] = mbarlineptr[i];
  mnbars -= j; 

/* if con == 1 compute the pitch differences
   int the template.
*/
 if (con == 1) compute_pitch_contour(mnnotes,mmidipitch,qnt);
  moffset=0;
/* if not exact match, i.e. resolution > 0 compute to an
   sample representation of the template.
*/
  if (resolution > 0) 
      for (i=0;i<mnbars;i++) {
        msamples[i] = make_bar_image(i,resolution,mpitch_samples+moffset,
                      mbarlineptr, mnotelength, mnnotes, 0, mmidipitch);
        moffset += msamples[i];
	if (moffset >3900) printf("abcmatch: out of room in mpitch_samples\n");
        }
   }

/* now process the input file */

  fp = fopen(filename,"rt");
  if (fp == NULL) {printf("cannot open file %s\n",filename);
                     exit(0);
                    }

  kfile = 0;
  while (!feof(fp))
     {
     fileindex++;
     startfile();
     parsetune(fp);
/*     printf("fileindex = %d xrefno =%d\n",fileindex,xrefno); */
/*     if (feof(fp)) break;                                    */
     if(notes < 10) break;
     ikey = sf2midishift[sf+7];
    /*print_feature_list();*/
    make_note_representation(&innotes, &inbars, imaxnotes, imaxbars,
      &itimesig_num, &itimesig_denom,ibarlineptr,inotelength,imidipitch);
    if (phist || lhist) compute_note_histograms();  else

/* ignore tunes which do not share the same time signature as the template */
   if (itimesig_num != mtimesig_num || itimesig_denom != mtimesig_denom) continue;
    transpose = mkey-ikey;

/* brief mode is used by the grouper in runabc.tcl */
   if (brief) {
     if(mseqno == fileindex) continue; /* don't check tune against itself */
     count = count_matched_tune_bars (mnbars,  inbars,  transpose);
     if (count >= cthresh) {
	     if(kfile == 0) printf("%d\n",mnbars);
	     printf(" %d %d\n",fileindex,count);
	     kfile++;
             }
     }

 else
/* top level matching function if not brief mode */
       find_and_report_matching_bars (mnbars, inbars, transpose, anymode, con);
  }



  free_abbreviations();
  free_feature_representation();
  fclose(fp);
  if (phist)  print_pitch_histogram();
  if (lhist)  print_length_histogram(); 
  return(0);
}
