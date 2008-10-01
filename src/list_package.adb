--
-- Change log:
--   17-Dec-2007  mgursky      add order to case, part of speech
--                             skip repeated inflected forms
--
   with CONFIG; use CONFIG;
   with STRINGS_PACKAGE; use STRINGS_PACKAGE;
   with LATIN_FILE_NAMES; use LATIN_FILE_NAMES;
   with WORD_PARAMETERS; use WORD_PARAMETERS;
   with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
   with DICTIONARY_PACKAGE; use DICTIONARY_PACKAGE;
   with ADDONS_PACKAGE; use ADDONS_PACKAGE;
   with UNIQUES_PACKAGE; use UNIQUES_PACKAGE;
   with WORD_SUPPORT_PACKAGE; use WORD_SUPPORT_PACKAGE;
   with DEVELOPER_PARAMETERS; use DEVELOPER_PARAMETERS;
   with WORD_PACKAGE; use WORD_PACKAGE;
   with EXAMPLE_LINE; use EXAMPLE_LINE;
   with DICTIONARY_FORM;
   with LIST_SWEEP;
   with PUT_STAT;
   package body LIST_PACKAGE is

      package BOOLEAN_IO is new TEXT_IO.ENUMERATION_IO(BOOLEAN);

      subtype XONS is PART_OF_SPEECH_TYPE range TACKON..SUFFIX;

      type DICTIONARY_MNPC_RECORD is record
        D_K  : DICTIONARY_KIND := DEFAULT_DICTIONARY_KIND;
        MNPC : MNPC_TYPE := NULL_MNPC;
        DE   : DICTIONARY_ENTRY := NULL_DICTIONARY_ENTRY;
      end record;
      NULL_DICTIONARY_MNPC_RECORD : DICTIONARY_MNPC_RECORD
                                  := (X, NULL_MNPC, NULL_DICTIONARY_ENTRY);

      MAX_MEANING_PRINT_SIZE : constant := 79;
      MM : INTEGER := MAX_MEANING_SIZE;
      I, J, K : INTEGER := 0;

      function TAGGED_STRING_RAW(TAG : STRING;
                                 ATTR_NAME : STRING;
                                 ATTR_VALUE : STRING;
                                 S : STRING) return STRING is
      begin
        if S'LENGTH /= 0 then
          if ATTR_NAME'LENGTH /= 0 and ATTR_VALUE'LENGTH /= 0 then
            return "<" & TAG & " " &
                   ATTR_NAME & "=""" & ATTR_VALUE & """>" &
                   S & "</" & TAG & ">";
          else
            return "<" & TAG & ">" & S & "</" & TAG & ">";
          end if;
        else
          if ATTR_NAME'LENGTH /= 0 and ATTR_VALUE'LENGTH /= 0 then
            return "<" & TAG & " " &
                   ATTR_NAME & "=""" & ATTR_VALUE & """/>";
          else
            return "";
          end if;
        end if;
      end TAGGED_STRING_RAW;

      function TAGGED_STRING(TAG : STRING; S : STRING) return STRING is
        OUT_STRING : STRING := TRIM(S);
      begin
        return TAGGED_STRING_RAW(TAG, "", "", TRIM(S));
      end TAGGED_STRING;

      function TAGGED_STRING(TAG : STRING;
                             ATTR_NAME : STRING;
                             ATTR_VALUE : STRING;
                             S : STRING) return STRING is
      begin
        return TAGGED_STRING_RAW(TAG,
                                 TRIM(ATTR_NAME),
                                 TRIM(ATTR_VALUE),
                                 TRIM(S));
      end TAGGED_STRING;

      function TAGGED_DECN(TAG : STRING;
                           T : DECN_RECORD;
                           SHOW_VAR : BOOLEAN) return STRING is
        FST: array (NATURAL range 0..9) of STRING(1..3) :=
          ("   ", "1st", "2nd", "3rd", "4th",
           "5th", "6th", "7th", "8th", "9th");
      begin
        if SHOW_VAR then
          return TAGGED_STRING(TAG, FST(T.WHICH)) &
                 TAGGED_STRING("var", FST(T.VAR));
        else
          return TAGGED_STRING(TAG, FST(T.WHICH));
        end if;
      end TAGGED_DECN;

      function CONJ_TO_XML(T : DECN_RECORD; SHOW_VAR : BOOLEAN) return STRING is
      begin
        return TAGGED_DECN("conj", T, SHOW_VAR);
      end CONJ_TO_XML;

      function DECL_TO_XML(T : DECN_RECORD; SHOW_VAR : BOOLEAN) return STRING is
      begin
        return TAGGED_DECN("decl", T, SHOW_VAR);
      end DECL_TO_XML;

      function FREQUENCY_TO_NAME(T : FREQUENCY_TYPE) return STRING is
        FREQUENCY_NAME : array (FREQUENCY_TYPE) of STRING(1..13) :=
             ("             ",  --  X
              "very frequent",  --  A
              "frequent     ",  --  B
              "common       ",  --  C
              "lesser       ",  --  D
              "uncommon     ",  --  E
              "very rare    ",  --  F
              "inscription  ",  --  I
              "graffiti     ",  --  J
              "Pliny        " );--  N
      begin
        return TRIM(FREQUENCY_NAME(T));
      end FREQUENCY_TO_NAME;

      function FREQUENCY_TO_ORDER(T : FREQUENCY_TYPE) return STRING is
        FREQUENCY_ORDER : array (FREQUENCY_TYPE) of STRING(1..1) :=
             (" ",  --  X
              "6",  --  A
              "5",  --  B
              "4",  --  C
              "3",  --  D
              "2",  --  E
              "1",  --  F
              " ",  --  I
              " ",  --  J
              " " );--  N
      begin
        return TRIM(FREQUENCY_ORDER(T));
      end FREQUENCY_TO_ORDER;

      function FREQUENCY_TO_XML(T : FREQUENCY_TYPE) return STRING is
      begin
        return TAGGED_STRING("freq",
                             "order",
                             FREQUENCY_TO_ORDER(T),
                             FREQUENCY_TO_NAME(T));
      end FREQUENCY_TO_XML;

      function INFLECTION_FREQUENCY_TO_XML(T : FREQUENCY_TYPE) return STRING is
         INFLECTION_FREQUENCY : array (FREQUENCY_TYPE) of STRING(1..13) :=
              ("             ",  --  X
               "most frequent",  --  A
               "sometime     ",  --  B
               "uncommon     ",  --  C
               "infrequent   ",  --  D
               "rare         ",  --  E
               "very rare    ",  --  F
               "inscription  ",  --  I
               "             ",  --  Not used (J)
               "             " );--  Not used (N)
      begin
        return TAGGED_STRING("freq",
                             "order",
                             FREQUENCY_TO_ORDER(T),
                             INFLECTION_FREQUENCY(T));
      end INFLECTION_FREQUENCY_TO_XML;

      function AGE_TO_NAME(T : AGE_TYPE) return STRING is
        AGE_NAME : array (AGE_TYPE) of STRING(1..8) :=
             ("        ",   --  X
              "archaic ",   --  A
              "early   ",   --  B
              "classic ",   --  C
              "late    ",   --  D
              "later   ",   --  E
              "medieval",   --  F
              "neoLatin",   --  G
              "modern  " ); --  H
      begin
        return TRIM(AGE_NAME(T));
      end AGE_TO_NAME;

      function AGE_TO_ORDER(T : AGE_TYPE) return STRING is
        AGE_ORDER : array (AGE_TYPE) of STRING(1..1) :=
             (" ",   --  X
              "1",   --  A
              "2",   --  B
              "3",   --  C
              "4",   --  D
              "5",   --  E
              "6",   --  F
              "7",   --  G
              "8" ); --  H
      begin
        return TRIM(AGE_ORDER(T));
      end AGE_TO_ORDER;

      function AGE_TO_XML(T : AGE_TYPE) return STRING is
      begin
        return TAGGED_STRING("age", "order", AGE_TO_ORDER(T), AGE_TO_NAME(T));
      end AGE_TO_XML;

      function INFLECTION_AGE_TO_XML(T : AGE_TYPE) return STRING is
         INFLECTION_AGE : array (AGE_TYPE) of STRING(1..8) :=
              ("always  ",   --  X
               "archaic ",   --  A
               "early   ",   --  B
               "classic ",   --  C
               "late    ",   --  D
               "later   ",   --  E
               "medieval",   --  F
               "scholar ",   --  G
               "modern  " ); --  H   
      begin
        return TAGGED_STRING("age",
                             "order",
                             AGE_TO_ORDER(T),
                             INFLECTION_AGE(T));
      end INFLECTION_AGE_TO_XML;

      function AREA_TO_XML(T : AREA_TYPE) return STRING is
        AREA_NAME : array (AREA_TYPE) of STRING(1..12) :=
             ("            ",   --  X
              "agriculture ",   --  A
              "bio/medical ",   --  B
              "arts        ",   --  D
              "religion    ",   --  E
              "literature  ",   --  G
              "government  ",   --  L
              "poetry      ",   --  P
              "science     ",   --  S
              "technical   ",   --  T
              "military    ",   --  W
              "mythology   " ); --  Y
      begin
        return TAGGED_STRING("area", AREA_NAME(T));
      end AREA_TO_XML;

      function GEO_TO_XML(T : GEO_TYPE) return STRING is
        GEO_NAME : array (GEO_TYPE) of STRING(1..14) :=
             ("              ",   -- X
              "Africa        ",   -- A
              "Britain       ",   -- B
              "China         ",   -- C
              "Scandinavia   ",   -- D
              "Egypt         ",   -- E
              "France/Gaul   ",   -- F
              "Germany       ",   -- G
              "Greece        ",   -- H
              "Italy/Rome    ",   -- I
              "India         ",   -- J
              "Balkans       ",   -- K
              "Netherlands   ",   -- N
              "Persia        ",   -- P
              "Near East     ",   -- Q
              "Russia        ",   -- R
              "Spain/Iberia  ",   -- S
              "Eastern Europe" ); -- U
      begin
        return TAGGED_STRING("geo", GEO_NAME(T));
      end GEO_TO_XML;

      function SOURCE_TO_XML(T : SOURCE_TYPE) return STRING is
        SOURCE_NAME : array (SOURCE_TYPE) of STRING(1..12) :=
             ("            ",   -- X
              "            ",   -- A   
              "Beeson      ",   -- B
              "Cassell's   ",   -- C       
              "Sexual Vocab",   -- D
              "Eccles.Latin",   -- E
              "Deferrari   ",   -- F
              "Gild.+Lodge ",   -- G
              "Collatinus  ",   -- H
              "Leverett    ",   -- I
              "Bracton     ",   -- J
              "Calepinus   ",   -- K
              "Lewis       ",   -- L
              "Latham      ",   -- M
              "Nelson      ",   -- N
              "Ox.Lat.Dict.",   -- O
              "Souter      ",   -- P
              "            ",   -- Q
              "Plater+White",   -- R
              "Lewis+Short ",   -- S
              "            ",   -- T
              "            ",   -- U
              "Opus Saxonis",   -- V
              "Whitaker    ",   -- W   
              "            ",   -- Y
              "            " ); -- Z
      begin
        return TAGGED_STRING("src", SOURCE_NAME(T));
      end SOURCE_TO_XML;

      function GENDER_TO_XML(T : GENDER_TYPE) return STRING is
        GENDER_NAME : array (GENDER_TYPE) of STRING(1..9) :=
             ("all      ",   -- X
              "masculine",   -- M
              "feminine ",   -- F
              "neuter   ",   -- N
              "common   " ); -- C
      begin
        return TAGGED_STRING("gend", GENDER_NAME(T));
      end GENDER_TO_XML;

      function PART_OF_SPEECH_TO_NAME(T : PART_OF_SPEECH_TYPE) return STRING is
        PART_OF_SPEECH_NAME : array (PART_OF_SPEECH_TYPE) of STRING(1..15) :=
             ("               ",   -- X
              "noun           ",   -- N
              "pronoun        ",   -- PRON
              "packon         ",   -- PACK
              "adjective      ",   -- ADJ
              "numeral        ",   -- NUM
              "adverb         ",   -- ADV
              "verb           ",   -- V
              "verb participle",   -- VPAR
              "supine         ",   -- SUPINE
              "preposition    ",   -- PREP
              "conjunction    ",   -- CONJ
              "interjection   ",   -- INTERJ
              "tackon         ",   -- TACKON
              "prefix         ",   -- PREFIX
              "suffix         " ); -- SUFFIX
      begin
        return TRIM(PART_OF_SPEECH_NAME(T));
      end PART_OF_SPEECH_TO_NAME;

      function PART_OF_SPEECH_TO_ORDER(T : PART_OF_SPEECH_TYPE) return STRING is
        PART_OF_SPEECH_ORDER : array (PART_OF_SPEECH_TYPE) of STRING(1..1) :=
             (" ",   -- X
              "5",   -- N
              " ",   -- PRON
              " ",   -- PACK
              "4",   -- ADJ
              " ",   -- NUM
              "2",   -- ADV
              "3",   -- V
              " ",   -- VPAR
              " ",   -- SUPINE
              "1",   -- PREP
              " ",   -- CONJ
              " ",   -- INTERJ
              " ",   -- TACKON
              " ",   -- PREFIX
              " " ); -- SUFFIX
      begin
        return TRIM(PART_OF_SPEECH_ORDER(T));
      end PART_OF_SPEECH_TO_ORDER;

      function PART_OF_SPEECH_TO_XML(T : PART_OF_SPEECH_TYPE) return STRING is
      begin
        return TAGGED_STRING("pofs",
                             "order",
                             PART_OF_SPEECH_TO_ORDER(T),
                             PART_OF_SPEECH_TO_NAME(T));
      end PART_OF_SPEECH_TO_XML;

      function CASE_TO_NAME(T : CASE_TYPE) return STRING is
        CASE_NAME : array (CASE_TYPE) of STRING(1..10) :=
             ("          ",   -- X
              "nominative",   -- NOM
              "vocative  ",   -- VOC
              "genitive  ",   -- GEN
              "locative  ",   -- LOC
              "dative    ",   -- DAT
              "ablative  ",   -- ABL
              "accusative" ); -- ACC
      begin
        return TRIM(CASE_NAME(T));
      end CASE_TO_NAME;

      function CASE_TO_ORDER(T : CASE_TYPE) return STRING is
        CASE_ORDER : array (CASE_TYPE) of STRING(1..1) :=
             (" ",   -- X
              "7",   -- NOM
              "1",   -- VOC
              "6",   -- GEN
              "2",   -- LOC
              "5",   -- DAT
              "3",   -- ABL
              "4" ); -- ACC
      begin
        return TRIM(CASE_ORDER(T));
      end CASE_TO_ORDER;

      function CASE_TO_XML(T : CASE_TYPE) return STRING is
      begin
        return TAGGED_STRING("case",
                             "order",
                             CASE_TO_ORDER(T),
                             CASE_TO_NAME(T));
      end CASE_TO_XML;

      function NUMBER_TO_XML(T : NUMBER_TYPE) return STRING is
        NUMBER_NAME : ARRAY (NUMBER_TYPE) of STRING(1..8) :=
             ("        ",   -- X
              "singular",   -- S
              "plural  " ); -- P
      begin
        return TAGGED_STRING("num", NUMBER_NAME(T));
      end NUMBER_TO_XML;

      function COMPARISON_TO_XML(T : COMPARISON_TYPE) return STRING is
        COMPARISON_NAME : ARRAY (COMPARISON_TYPE) of STRING(1..11) :=
             ("           ",   -- X
              "positive   ",   -- POS
              "comparative",   -- COMP
              "superlative" ); -- SUPER
      begin
        return TAGGED_STRING("comp", COMPARISON_NAME(T));
      end COMPARISON_TO_XML;

      function TENSE_VOICE_MOOD_TO_XML(T : TENSE_VOICE_MOOD_RECORD)
      return STRING is
        TENSE_NAME : ARRAY (TENSE_TYPE) of STRING(1..14) :=
             ("              ",   -- X
              "present       ",   -- PRES
              "imperfect     ",   -- IMPF
              "future        ",   -- FUT
              "perfect       ",   -- PERF
              "pluperfect    ",   -- PLUP
              "future perfect" ); -- FUTP
        VOICE_NAME : ARRAY (VOICE_TYPE) of STRING(1..7) :=
             ("       ",   -- X
              "active ",   -- ACTIVE
              "passive" ); -- PASSIVE
        MOOD_NAME : ARRAY (MOOD_TYPE) of STRING(1..11) :=
             ("           ",   -- X
              "indicative ",   -- IND
              "subjunctive",   -- SUB
              "imperative ",   -- IMP
              "infinitive ",   -- INF
              "participle " ); -- PPL
      begin
        return TAGGED_STRING("tense", TENSE_NAME(T.TENSE)) &
               TAGGED_STRING("voice", VOICE_NAME(T.VOICE)) &
               TAGGED_STRING("mood", MOOD_NAME(T.MOOD));
      end TENSE_VOICE_MOOD_TO_XML;

      function NUMERAL_SORT_TO_XML(T : NUMERAL_SORT_TYPE) return STRING is
        NUMERAL_SORT_NAME : ARRAY (NUMERAL_SORT_TYPE) of STRING(1..14) :=
             ("              ",   -- X
              "cardinal      ",   -- CARD
              "ordinal       ",   -- ORD
              "distributive  ",   -- DIST
              "numeral adverb" ); -- ADVERB
      begin
        return TAGGED_STRING("sort", NUMERAL_SORT_NAME(T));
      end NUMERAL_SORT_TO_XML;

      function PERSON_TO_XML(T : PERSON_TYPE) return STRING is
        FST: array (PERSON_TYPE range 0..3) of STRING(1..3) :=
          ("   ", "1st", "2nd", "3rd");
      begin
        return TAGGED_STRING("pers", FST(T));
      end PERSON_TO_XML;

      function VERB_KIND_TO_XML(T : VERB_KIND_TYPE) return STRING is
        VERB_KIND_NAME : ARRAY (VERB_KIND_TYPE) of STRING(1..18) :=
             ("                  ",   -- X
              "to be             ",   -- TO_BE
              "compounds of to be",   -- TO_BEING
              "taking genitive   ",   -- GEN
              "taking dative     ",   -- DAT
              "taking ablative   ",   -- ABL
              "transitive        ",   -- TRANS
              "intransitive      ",   -- INTRANS
              "impersonal        ",   -- IMPERS
              "deponent          ",   -- DEP
              "semideponent      ",   -- SEMIDEP
              "perfect definite  " ); -- PERFDEF
      begin
        return TAGGED_STRING("kind", VERB_KIND_NAME(T));
      end VERB_KIND_TO_XML;

      function CAP_STEM(S : STRING) return STRING  is
      begin
         if ALL_CAPS  then
            return UPPER_CASE(S);
         elsif CAPITALIZED  then
            return UPPER_CASE(S(S'FIRST)) & S(S'FIRST+1..S'LAST);
         else
            return S;
         end if;
      end CAP_STEM;

      function CAP_ENDING(S : STRING) return STRING  is
      begin
         if ALL_CAPS  then
            return UPPER_CASE(S);
         else
            return S;
         end if;
      end CAP_ENDING;


           procedure PUT_DICTIONARY_FLAGS(OUTPUT : TEXT_IO.FILE_TYPE;
                                          DE     : DICTIONARY_ENTRY) is
             begin

               if WORDS_MODE(SHOW_AGE)   or 
                 (AGE_TO_NAME(DE.TRAN.AGE)'LENGTH /= 0)  then  --  Not X
                  TEXT_IO.PUT(OUTPUT, "  " & AGE_TO_NAME(DE.TRAN.AGE));
                end if;
               if (WORDS_MODE(SHOW_FREQUENCY) or 
                  (DE.TRAN.FREQ >= D))  and 
                  (FREQUENCY_TO_NAME(DE.TRAN.FREQ)'LENGTH /= 0)  then
                  TEXT_IO.PUT(OUTPUT,
                              "  " & FREQUENCY_TO_NAME(DE.TRAN.FREQ));
               end if;
           end PUT_DICTIONARY_FLAGS; 



      procedure PUT_DICTIONARY_FORM(OUTPUT : TEXT_IO.FILE_TYPE;
                                    D_K    : DICTIONARY_KIND;
                                    MNPC   : DICT_IO.COUNT; 
                                    DE     : DICTIONARY_ENTRY) is
             DICTIONARY_LINE_NUMBER : INTEGER := INTEGER(MNPC);
             --DE : DICTIONARY_ENTRY := DM.DE;

      begin                               --  PUT_DICTIONARY_FORM
        TEXT_IO.PUT(OUTPUT, "<dict>");
        if WORDS_MODE(DO_DICTIONARY_FORMS)  then
          if WORDS_MDEV(DO_PEARSE_CODES) then
            TEXT_IO.PUT(OUTPUT, "02 ");
          end if;
          if DICTIONARY_FORM(DE)'LENGTH /= 0  then
            TEXT_IO.PUT(OUTPUT, DICTIONARY_FORM(DE));
          end if;
        end if;

        if WORDS_MDEV(SHOW_DICTIONARY_CODES) and then
           DE.PART.POFS not in XONS
        then
          TEXT_IO.PUT(OUTPUT, AGE_TO_XML(DE.TRAN.AGE));
          TEXT_IO.PUT(OUTPUT, AREA_TO_XML(DE.TRAN.AREA));
          TEXT_IO.PUT(OUTPUT, GEO_TO_XML(DE.TRAN.GEO));
          TEXT_IO.PUT(OUTPUT, FREQUENCY_TO_XML(DE.TRAN.FREQ));
          TEXT_IO.PUT(OUTPUT, SOURCE_TO_XML(DE.TRAN.SOURCE));
        end if;

        if WORDS_MDEV(SHOW_DICTIONARY) then
          TEXT_IO.PUT(OUTPUT, EXT(D_K) & ">");
        end if;

        if WORDS_MDEV(SHOW_DICTIONARY_LINE)  then
          if DICTIONARY_LINE_NUMBER > 0  then
            TEXT_IO.PUT(OUTPUT, "(" 
                  & TRIM(INTEGER'IMAGE(DICTIONARY_LINE_NUMBER)) & ")");
          end if;
        end if;

        --PUT_DICTIONARY_FLAGS(OUTPUT, DE);       

        TEXT_IO.PUT_LINE(OUTPUT, "</dict>");
      end PUT_DICTIONARY_FORM;


      procedure LIST_STEMS(OUTPUT   : TEXT_IO.FILE_TYPE;
                           RAW_WORD : STRING;
                           INPUT_LINE : STRING;
                           PA       : in out PARSE_ARRAY; 
                           PA_LAST  : in out INTEGER) is
         use TEXT_IO;
         use DICT_IO;


      --  The main WORD processing has been to produce an array of PARSE_RECORD
      --      type PARSE_RECORD is
      --        record
      --          STEM  : STEM_TYPE := NULL_STEM_TYPE;
      --          IR    : INFLECTION_RECORD := NULL_INFLECTION_RECORD;
      --          D_K   : DICTIONARY_KIND := DEFAULT_DICTIONARY_KIND;
      --          MNPC  : DICT_IO.COUNT := NULL_MNPC;
      --        end record;
      --  This has involved STEMFILE and INFLECTS, no DICTFILE

      --  PARSE_RECORD is put through the LIST_SWEEP procedure that does TRIMing
      --  Then, for processing for output, the data is converted to arrays of   
      --      type STEM_INFLECTION_RECORD is
      --        record
      --          STEM : STEM_TYPE          := NULL_STEM_TYPE;
      --          IR   : INFLECTION_RECORD  := NULL_INFLECTION_RECORD;
      --        end record;
      --  and
      --      type DICTIONARY_MNPC_RECORD is 
      --        record
      --          D_K  : DICTIONARY_KIND;
      --          MNPC : MNPC_TYPE;
      --          DE   : DICTIONARY_ENTRY;
      --        end record;
      --  containing the same data plus the DICTFILE data DICTIONARY_ENTRY
      --  but breaking it into two arrays allows different manipulation
      --  These are only within this routine, used to clean up the output   



      type STEM_INFLECTION_RECORD is
        record
          STEM : STEM_TYPE          := NULL_STEM_TYPE;
          IR   : INFLECTION_RECORD  := NULL_INFLECTION_RECORD;
        end record;
      NULL_STEM_INFLECTION_RECORD : STEM_INFLECTION_RECORD;

      STEM_INFLECTION_ARRAY_SIZE       : constant := 10;
      STEM_INFLECTION_ARRAY_ARRAY_SIZE : constant := 40;
      type STEM_INFLECTION_ARRAY is array (INTEGER range <>) of STEM_INFLECTION_RECORD;
      type STEM_INFLECTION_ARRAY_ARRAY is array (INTEGER range <>)
                              of STEM_INFLECTION_ARRAY(1..STEM_INFLECTION_ARRAY_SIZE);

      SRA, OSRA, NULL_SRA : STEM_INFLECTION_ARRAY(1..STEM_INFLECTION_ARRAY_SIZE)
                             := (others => (NULL_STEM_TYPE, NULL_INFLECTION_RECORD));
      SRAA, NULL_SRAA : STEM_INFLECTION_ARRAY_ARRAY(1..STEM_INFLECTION_ARRAY_ARRAY_SIZE)
                        := (others => NULL_SRA);

--      type DICTIONARY_MNPC_RECORD is record
--        D_K  : DICTIONARY_KIND := DEFAULT_DICTIONARY_KIND;
--        MNPC : MNPC_TYPE := NULL_MNPC;
--        DE   : DICTIONARY_ENTRY := NULL_DICTIONARY_ENTRY;
--      end record;
--      NULL_DICTIONARY_MNPC_RECORD : DICTIONARY_MNPC_RECORD
--                                  := (X, NULL_MNPC, NULL_DICTIONARY_ENTRY);
      DM, ODM : DICTIONARY_MNPC_RECORD := NULL_DICTIONARY_MNPC_RECORD;

      DICTIONARY_MNPC_ARRAY_SIZE : constant := 40;

      type DICTIONARY_MNPC_ARRAY is array (1..DICTIONARY_MNPC_ARRAY_SIZE)
                                       of DICTIONARY_MNPC_RECORD;
      DMA, ODMA, NULL_DMA : DICTIONARY_MNPC_ARRAY;


      --MEANING_ARRAY_SIZE : constant := 5;
      --MEANING_ARRAY : array (1..MEANING_ARRAY_SIZE) of MEANING_TYPE;

      DEA : DICTIONARY_ENTRY := NULL_DICTIONARY_ENTRY;






         W : constant STRING := RAW_WORD;
         J, J1, J2, K : INTEGER := 0;
         THERE_IS_AN_ADVERB : BOOLEAN := FALSE;







         procedure  PUT_INFLECTION(SR : STEM_INFLECTION_RECORD; 
                                   DM : DICTIONARY_MNPC_RECORD) is
         --  Handles putting ONLY_MEAN, PEARSE_CODES, CAPS, QUAL, V_KIND, FLAGS
           procedure PUT_INFLECTION_FLAGS is
           begin
             if (WORDS_MODE(SHOW_AGE)   or 
                (SR.IR.AGE /= X))  then   --  Warn even if not to show AGE
               TEXT_IO.PUT(OUTPUT, INFLECTION_AGE_TO_XML(SR.IR.AGE));
             end if; 
             if (WORDS_MODE(SHOW_FREQUENCY)  or
                (SR.IR.FREQ >= C)) then   --  Warn regardless
               TEXT_IO.PUT(OUTPUT, INFLECTION_FREQUENCY_TO_XML(SR.IR.FREQ));
             end if;
           end PUT_INFLECTION_FLAGS;

         begin
           if (not WORDS_MODE(DO_ONLY_MEANINGS) and
               not (CONFIGURATION = ONLY_MEANINGS)) then

             TEXT_IO.PUT(OUTPUT, "<infl>");

             if WORDS_MDEV(DO_PEARSE_CODES) then
               if DM.D_K = ADDONS  then
                 TEXT_IO.PUT(OUTPUT, "05 ");
               elsif DM.D_K in XXX..YYY  then
                 TEXT_IO.PUT(OUTPUT, "06 ");
               else
                 TEXT_IO.PUT(OUTPUT, "01 ");
               end if;
             end if;

             TEXT_IO.PUT(OUTPUT, "<term xml:lang=""la"">");
             TEXT_IO.PUT(OUTPUT, TAGGED_STRING("stem", SR.STEM));
             TEXT_IO.PUT(OUTPUT, TAGGED_STRING("suff", SR.IR.ENDING.SUF));
             TEXT_IO.PUT(OUTPUT, "</term>");

             if SR.IR /= NULL_INFLECTION_RECORD  then

PRINT_MODIFIED_QUAL:   
declare
  Q : QUALITY_RECORD := SR.IR.QUAL;
begin

  if (DM.D_K in GENERAL..LOCAL)  then  --  UNIQUES has no DE
    if (Q.POFS = V)    and then
       (DM.DE.PART.V.KIND = DEP)       and then
       (Q.V.TENSE_VOICE_MOOD.MOOD in IND..INF)   then
--TEXT_IO.PUT_LINE("START PRINT MODIFIED QUAL   V" );
      Q.V.TENSE_VOICE_MOOD.VOICE := X;
    elsif (Q.POFS = VPAR)    and then
          (DM.DE.PART.V.KIND = DEP)    and then
          (Q.VPAR.TENSE_VOICE_MOOD.MOOD = PPL)  then
--TEXT_IO.PUT_LINE("START PRINT MODIFIED QUAL   VPAR" );
      Q.VPAR.TENSE_VOICE_MOOD.VOICE := X;
    end if;
  end if;
  QUALITY_RECORD_IO.PUT(OUTPUT, Q);

end PRINT_MODIFIED_QUAL;

               --PUT_INFLECTION_FLAGS;
               if HAS_EXAMPLE_LINE(SR.IR, DM.DE) then
                 TEXT_IO.NEW_LINE(OUTPUT);
                 TEXT_IO.PUT(OUTPUT, "<xmpl>");
                 PUT_EXAMPLE_LINE(OUTPUT, SR.IR, DM.DE);
                 TEXT_IO.PUT(OUTPUT, "</xmpl>");
                 TEXT_IO.NEW_LINE(OUTPUT);
               end if;
             end if;
             TEXT_IO.PUT_LINE(OUTPUT, "</infl>");
           end if;   

         end PUT_INFLECTION;


         procedure PUT_FORM(SR : STEM_INFLECTION_RECORD; 
                            DM : DICTIONARY_MNPC_RECORD) is
         --  Handles PEARSE_CODES and DICTIONARY_FORM (which has FLAGS) and D_K
         --  The Pearse 02 is handled in PUT_DICTIONARY_FORM
        begin
           if (SR.IR.QUAL.POFS not in XONS)  and
              (DM.D_K in GENERAL..UNIQUE)           then
--DICTIONARY_ENTRY_IO.PUT(DM.DE);
                 PUT_DICTIONARY_FORM(OUTPUT, DM.D_K, DM.MNPC, DM.DE);
           end if;
         end PUT_FORM;



         function TRIM_BAR(S : STRING) return STRING is
         --  Takes vertical bars from begining of MEAN and TRIMs
         begin
           if S'LENGTH >3  and then S(S'FIRST..S'FIRST+3) = "||||"  then
             return TRIM(S(S'FIRST+4.. S'LAST));
           elsif S'LENGTH >2  and then S(S'FIRST..S'FIRST+2) = "|||"  then
             return TRIM(S(S'FIRST+3.. S'LAST));
           elsif S'LENGTH > 1  and then  S(S'FIRST..S'FIRST+1) = "||"  then
             return TRIM(S(S'FIRST+2.. S'LAST));
           elsif S(S'FIRST) = '|'  then
             return TRIM(S(S'FIRST+1.. S'LAST));
           else
             return TRIM(S);
           end if;
         end TRIM_BAR;




      procedure PUT_MEANING(OUTPUT : TEXT_IO.FILE_TYPE;
                            RAW_MEANING : STRING) is
      --  Handles the MM screen line limit and TRIM_BAR, then TRIMs

      begin

        TEXT_IO.PUT(OUTPUT, TRIM(HEAD(TRIM_BAR(RAW_MEANING), MM)));
      end PUT_MEANING;


      function CONSTRUCTED_MEANING(SR : STEM_INFLECTION_RECORD;
                                   DM  : DICTIONARY_MNPC_RECORD) return STRING is
      --  Constructs the meaning for NUM from NUM.SORT and NUM_VALUE
        S : STRING(1..MAX_MEANING_SIZE) := NULL_MEANING_TYPE;
        N : INTEGER := 0;
      begin
        if DM.DE.PART.POFS = NUM  then
          N := DM.DE.PART.NUM.VALUE;
          if SR.IR.QUAL.POFS = NUM  then    --  Normal parse 
            case SR.IR.QUAL.NUM.SORT is
              when CARD  =>
                S := HEAD(INTEGER'IMAGE(N) &  " - (CARD answers 'how many');", MAX_MEANING_SIZE);
              when ORD   =>
                S := HEAD(INTEGER'IMAGE(N) & "th - (ORD, 'in series'); (a/the)" & INTEGER'IMAGE(N) &
                                             "th (part) (fract w/pars?);", MAX_MEANING_SIZE);
              when DIST  =>
                S := HEAD(INTEGER'IMAGE(N) & " each/apiece/times/fold/together/at a time - 'how many each'; by " &
                          INTEGER'IMAGE(N) & "s; ", MAX_MEANING_SIZE);
              when ADVERB =>
                S := HEAD(INTEGER'IMAGE(N) & " times, on" & INTEGER'IMAGE(N) &
                                             " occasions - (ADVERB answers 'how often');", MAX_MEANING_SIZE);
              when others =>
                null;
            end case;  
          else  -- there is fix so POFS is not NUM
            S := HEAD("Number " & INTEGER'IMAGE(N), MAX_MEANING_SIZE);
          end if;
        end if;

        return S;

      end CONSTRUCTED_MEANING;


      procedure PUT_MEANING_LINE(SR : STEM_INFLECTION_RECORD;
                                 DM  : DICTIONARY_MNPC_RECORD) is
      begin
        TEXT_IO.PUT(OUTPUT, "<mean>");
        if DM.D_K not in ADDONS..PPP  then
          if WORDS_MDEV(DO_PEARSE_CODES) then
            TEXT_IO.PUT(OUTPUT, "03 ");
          end if;
          if DM.DE.PART.POFS = NUM and then DM.DE.PART.NUM.VALUE > 0 then
            TEXT_IO.PUT(OUTPUT, TRIM(CONSTRUCTED_MEANING(SR, DM)));
          elsif DM.D_K = UNIQUE  then
            PUT_MEANING(OUTPUT, UNIQUES_DE(DM.MNPC).MEAN); 
          else
            PUT_MEANING(OUTPUT, TRIM_BAR(DM.DE.MEAN)); 
          end if;
        else
          if DM.D_K = RRR  then 
            if RRR_MEANING /= NULL_MEANING_TYPE   then
              if WORDS_MDEV(DO_PEARSE_CODES) then
                TEXT_IO.PUT(OUTPUT, "03 ");
              end if;
              PUT_MEANING(OUTPUT, RRR_MEANING);      --  Roman Numeral
              RRR_MEANING := NULL_MEANING_TYPE;
            end if;
          elsif DM.D_K = NNN then 
            if NNN_MEANING /= NULL_MEANING_TYPE  then
              if WORDS_MDEV(DO_PEARSE_CODES) then
                TEXT_IO.PUT(OUTPUT, "03 ");
              end if;
              PUT_MEANING(OUTPUT, NNN_MEANING);  --  Unknown Name
              NNN_MEANING := NULL_MEANING_TYPE;
            end if;

          elsif DM.D_K = XXX  then 
            if XXX_MEANING /= NULL_MEANING_TYPE  then
              if WORDS_MDEV(DO_PEARSE_CODES) then
                TEXT_IO.PUT(OUTPUT, "06 ");
              end if;
              PUT_MEANING(OUTPUT, XXX_MEANING);  --  TRICKS
              XXX_MEANING := NULL_MEANING_TYPE;
            end if;

          elsif DM.D_K = YYY  then 
            if YYY_MEANING /= NULL_MEANING_TYPE  then
              if WORDS_MDEV(DO_PEARSE_CODES) then
                TEXT_IO.PUT(OUTPUT, "06 ");
              end if;
              PUT_MEANING(OUTPUT, YYY_MEANING);  --  Syncope
              YYY_MEANING := NULL_MEANING_TYPE;
            end if;

          elsif DM.D_K = PPP  then 
            if PPP_MEANING /= NULL_MEANING_TYPE  then
              if WORDS_MDEV(DO_PEARSE_CODES) then
                TEXT_IO.PUT(OUTPUT, "06 ");
              end if;
              PUT_MEANING(OUTPUT, PPP_MEANING); --  Compounds
              PPP_MEANING := NULL_MEANING_TYPE;
            end if;

          elsif DM.D_K = ADDONS  then 
             if WORDS_MDEV(DO_PEARSE_CODES) then
                TEXT_IO.PUT(OUTPUT, "06 ");
              end if;
              PUT_MEANING(OUTPUT, MEANS(INTEGER(DM.MNPC))); 
          end if;

        end if;
        TEXT_IO.PUT_LINE(OUTPUT, "</mean>");
      end PUT_MEANING_LINE;

      begin
      TRIMMED := FALSE;

      --  Since this procedure weeds out possible parses, if it weeds out all
      --  (or all of a class) it must fix up the rest of the parse array,
      --  e.g., it must clean out dangling prefixes and suffixes

--    --  Just to find the words with long/complicated output at the processing level 
--    --  This is done with the final PA_LAST, entering LIST_STEM, before SWEEP
--       if PA_LAST > PA_LAST_MAX   then
--         PUT_STAT("$PA_LAST_MAX    for RAW_WORD " & HEAD(RAW_WORD, 24) & "   = " & INTEGER'IMAGE(PA_LAST));
--         PA_LAST_MAX := PA_LAST;
--       end if;

--TEXT_IO.PUT_LINE("PA on entering LIST_STEMS    PA_LAST = " & INTEGER'IMAGE(PA_LAST));
--for I in 1..PA_LAST  loop
--PARSE_RECORD_IO.PUT(PA(I)); TEXT_IO.NEW_LINE;
--end loop;


         if (TEXT_IO.NAME(OUTPUT) = 
             TEXT_IO.NAME(TEXT_IO.STANDARD_OUTPUT))  then
           MM := MAX_MEANING_PRINT_SIZE;   --  to keep from overflowing screen line
                                           --  or even adding blank line
        else
           MM := MAX_MEANING_SIZE;

        end if;

      -------  The gimick of adding an ADV if there is only ADJ VOC  ----
--TEXT_IO.PUT_LINE("About to do the ADJ -> ADV kludge");
        for I in PA'FIRST..PA_LAST  loop
            if PA(I).IR.QUAL.POFS = ADV   then
               THERE_IS_AN_ADVERB := TRUE;
               exit;
            end if;
         end loop;

--TEXT_IO.PUT_LINE("In the ADJ -> ADV kludge  Checked to see if there is an ADV");

         if ((not THERE_IS_AN_ADVERB) and (WORDS_MODE(DO_FIXES)))  then
--TEXT_IO.PUT_LINE("In the ADJ -> ADV kludge  There is no ADV");
            for I in reverse PA'FIRST..PA_LAST  loop

               if PA(I).IR.QUAL.POFS = ADJ and then
                  (PA(I).IR.QUAL.ADJ = ((1, 1), VOC, S, M, POS)    or
                      ((PA(I).IR.QUAL.ADJ.CS = VOC)   and
                          (PA(I).IR.QUAL.ADJ.NUMBER = S)   and
                          (PA(I).IR.QUAL.ADJ.GENDER = M)   and
                          (PA(I).IR.QUAL.ADJ.CO = SUPER)))    then

                  J := I;

                  while J >=  PA'FIRST  loop  --Back through other ADJ cases
                     if PA(J).IR.QUAL.POFS /= ADJ  then
                        J2 := J;                          --  J2 is first (reverse) that is not ADJ
                        exit;
                     end if;
                     J := J - 1;
                  end loop;
                  while J >=  PA'FIRST  loop  --  Sweep up associated fixes
                     if PA(J).IR.QUAL.POFS not in XONS  then
                        J1 := J;                      --  J1 is first (reverse) that is not XONS
                        exit;
                     end if;
                     J := J - 1;
                  end loop;



                  for J in J1+1..J2  loop
                     PA(PA_LAST+J-J1+1) := PA(J);
                  end loop;
--TEXT_IO.PUT_LINE("In the ADJ -> ADV kludge  Ready to add PA for ADV");
                  PA_LAST := PA_LAST + J2 - J1 + 1;
                  PA(PA_LAST) := PA(J2+1);
--TEXT_IO.PUT_LINE("In the ADJ -> ADV kludge  Adding SUFFIX E ADV");
                   PA(PA_LAST) := ("e                 ",
                                         ((SUFFIX, NULL_SUFFIX_RECORD), 0, NULL_ENDING_RECORD, X, B),
                                      PPP, NULL_MNPC);
--PARSE_RECORD_IO.PUT(PA(PA_LAST)); TEXT_IO.NEW_LINE;
                   PA_LAST := PA_LAST + 1;
                  if PA(J2+1).IR.QUAL.ADJ.CO = POS   then
--TEXT_IO.PUT_LINE("In the ADJ -> ADV kludge  Adding POS for ADV");
                     PA(PA_LAST) := (PA(J2+1).STEM,
                                            ((POFS => ADV, ADV => (CO => PA(J2+1).IR.QUAL.ADJ.CO)),
                                             KEY => 0, ENDING => (1, "e      "), AGE => X, FREQ => B),
                                         PA(J2+1).D_K,
                                         PA(J2+1).MNPC);
 --PARSE_RECORD_IO.PUT(PA(PA_LAST)); TEXT_IO.NEW_LINE;
                    PPP_MEANING :=
                        HEAD("-ly; -ily;  Converting ADJ to ADV",
                             MAX_MEANING_SIZE);

                  elsif PA(J2+1).IR.QUAL.ADJ.CO = SUPER  then
                     PA(PA_LAST) := (PA(J2+1).STEM,
                     ((POFS => ADV, ADV => (CO => PA(J2+1).IR.QUAL.ADJ.CO)),
                       KEY => 0, ENDING => (2, "me     "), AGE => X, FREQ => B),
                                         PA(J2+1).D_K,
                                         PA(J2+1).MNPC);
                     PPP_MEANING :=
                        HEAD("-estly; -estily; most -ly, very -ly  Converting ADJ to ADV",
                             MAX_MEANING_SIZE);
                  end if;
 --TEXT_IO.PUT_LINE("In the ADJ -> ADV kludge  Done adding PA for ADV");
               end if;           --  PA(I).IR.QUAL.POFS = ADJ

            end loop;

         end if;           --  not THERE_IS_AN_ADVERB
-- TEXT_IO.PUT_LINE("In the ADJ -> ADV kludge  FINISHED");

         LIST_SWEEP(PA(1..PA_LAST), PA_LAST);   


--TEXT_IO.PUT_LINE("PA after leaving LIST_SWEEP    PA_LAST = "  & INTEGER'IMAGE(PA_LAST));
--for I in 1..PA_LAST  loop
--PARSE_RECORD_IO.PUT(PA(I)); TEXT_IO.NEW_LINE;
--end loop;
-- 







--               --  Does STATS
--      
--TEXT_IO.PUT_LINE("Before STATING FIXES");
     if  WORDS_MDEV(WRITE_STATISTICS_FILE)    then      --  Omit rest of output

       for I in 1..PA_LAST  loop                       --  Just to PUT_STAT
         if (PA(I).D_K = ADDONS)  then
           if PA(I).IR.QUAL.POFS = PREFIX  then
             PUT_STAT("ADDON PREFIX at "
                     & HEAD(INTEGER'IMAGE(LINE_NUMBER), 8) & HEAD(INTEGER'IMAGE(WORD_NUMBER), 4)
                     & "   " & HEAD(W, 20) & "   "  & PA(I).STEM & "  " & INTEGER'IMAGE(INTEGER(PA(I).MNPC)));
           elsif PA(I).IR.QUAL.POFS = SUFFIX  then
             PUT_STAT("ADDON SUFFIX at "
                     & HEAD(INTEGER'IMAGE(LINE_NUMBER), 8) & HEAD(INTEGER'IMAGE(WORD_NUMBER), 4)
                     & "   " & HEAD(W, 20) & "   "  & PA(I).STEM & "  " & INTEGER'IMAGE(INTEGER(PA(I).MNPC)));
           elsif PA(I).IR.QUAL.POFS = TACKON  then
             PUT_STAT("ADDON TACKON at "
                     & HEAD(INTEGER'IMAGE(LINE_NUMBER), 8) & HEAD(INTEGER'IMAGE(WORD_NUMBER), 4)
                     & "   " & HEAD(W, 20) & "   "  & PA(I).STEM & "  " & INTEGER'IMAGE(INTEGER(PA(I).MNPC)));
           end if;
         end if;
       end loop;


----    --  Just to find the words with long/complicated output at the LIST level 
----    --  This is done with the final PA_LAST, after SWEEP
----       if PA_LAST > FINAL_PA_LAST_MAX   then
----         PUT_STAT("$FINAL_PA_LAST_MAX    for RAW_WORD " & HEAD(RAW_WORD, 24) & "   = " & INTEGER'IMAGE(PA_LAST));
----         FINAL_PA_LAST_MAX := PA_LAST;
----       end if;

     end if;

--TEXT_IO.PUT_LINE("After STATING FIXES");







--  Convert from PARSE_RECORDs to DICTIONARY_MNPC_RECORD and STEM_INFLECTION_RECORD         
--TEXT_IO.PUT_LINE("Doing arrays in LIST_STEMS    PA_LAST = "  & 
--                 INTEGER'IMAGE(PA_LAST));
I := 1;           --  I cycles on PA
J := 0;           --  J indexes the number of DMA arrays  --  Initialize
SRAA := NULL_SRAA;
DMA := NULL_DMA;
CYCLE_OVER_PA:
while I <= PA_LAST  loop       --  I cycles over full PA array
--TEXT_IO.PUT_LINE("Starting loop for I    I = " & INTEGER'IMAGE(I));
ODM := NULL_DICTIONARY_MNPC_RECORD;

if PA(I).D_K = UNIQUE  then
  J := J + 1;
  SRAA(J)(1) := (PA(I).STEM, PA(I).IR);
--TEXT_IO.PUT_LINE("UNIQUE   I = " & INTEGER'IMAGE(I) & "  J = " & INTEGER'IMAGE(J)); 
  DM := NULL_DICTIONARY_MNPC_RECORD;
  DM.D_K := UNIQUE;
  DM.MNPC := PA(I).MNPC;  
  DM.DE := UNIQUES_DE(PA(I).MNPC);  
  DMA(J) := DM;
  I := I + 1;
else

case PA(I).IR.QUAL.POFS  is

when N  =>  
  OSRA := NULL_SRA;
  ODMA := NULL_DMA;
  --ODM := NULL_DICTIONARY_MNPC_RECORD;
  --DM := NULL_DICTIONARY_MNPC_RECORD;
  while PA(I).IR.QUAL.POFS = N   and
        I <= PA_LAST                   loop
--TEXT_IO.PUT_LINE("Starting loop for N    I = " & INTEGER'IMAGE(I) & "   K = " & INTEGER'IMAGE(K));
   if PA(I).MNPC  /= ODM.MNPC  then   --  Encountering new MNPC
      OSRA := SRA;
      K := 1;                  --  K indexes within the MNPCA array --  Initialize
--TEXT_IO.PUT_LINE("Starting IRA for N    I = " & INTEGER'IMAGE(I) & "   K = " & INTEGER'IMAGE(K));
      J := J + 1;             --  J indexes the number of MNPCA arrays - Next MNPCA
--TEXT_IO.PUT_LINE("Shifting J for N  I = " & INTEGER'IMAGE(I) & "   J = " & INTEGER'IMAGE(J));
      SRAA(J)(K) := (PA(I).STEM, PA(I).IR);
      DICT_IO.SET_INDEX(DICT_FILE(PA(I).D_K), PA(I).MNPC); 
      DICT_IO.READ(DICT_FILE(PA(I).D_K), DEA);
      DM := (PA(I).D_K, PA(I).MNPC, DEA);
      DMA(J) := DM;
      ODM := DM;
    else
      K := K + 1;              --  K indexes within the MNPCA array  - Next MNPC
--TEXT_IO.PUT_LINE("Continuing IRA for N  I = " & INTEGER'IMAGE(I) & "   K = " & INTEGER'IMAGE(K)
--                                                                 & "   J = " & INTEGER'IMAGE(J));
      SRAA(J)(K) := (PA(I).STEM, PA(I).IR);
    end if;

    I := I + 1;              --  I cycles over full PA array
    end loop;

when PRON  =>  
  OSRA := NULL_SRA;
  ODMA := NULL_DMA;
  --ODM := NULL_DICTIONARY_MNPC_RECORD;
  --DM := NULL_DICTIONARY_MNPC_RECORD;
  while PA(I).IR.QUAL.POFS = PRON   and
        I <= PA_LAST                   loop
    if PA(I).MNPC  /= ODM.MNPC  then   --  Encountering new MNPC
      OSRA := SRA;
      K := 1;                  --  K indexes within the MNPCA array --  Initialize
      J := J + 1;             --  J indexes the number of MNPCA arrays - Next MNPCA
      SRAA(J)(K) := (PA(I).STEM, PA(I).IR);
      DICT_IO.SET_INDEX(DICT_FILE(PA(I).D_K), PA(I).MNPC); 
      DICT_IO.READ(DICT_FILE(PA(I).D_K), DEA);
      DM := (PA(I).D_K, PA(I).MNPC, DEA);
      DMA(J) := DM;
      ODM := DM;
   else
      K := K + 1;              --  K indexes within the MNPCA array  - Next MNPC
      SRAA(J)(K) := (PA(I).STEM, PA(I).IR);
    end if;

    I := I + 1;              --  I cycles over full PA array
    end loop;

    when PACK  =>  
  OSRA := NULL_SRA;
  ODMA := NULL_DMA;
  --ODM := NULL_DICTIONARY_MNPC_RECORD;
  --DM := NULL_DICTIONARY_MNPC_RECORD;
  while PA(I).IR.QUAL.POFS = PACK   and
        I <= PA_LAST                   loop
    if PA(I).MNPC  /= ODM.MNPC  then   --  Encountering new MNPC
      OSRA := SRA;
      K := 1;                  --  K indexes within the MNPCA array --  Initialize
      J := J + 1;             --  J indexes the number of MNPCA arrays - Next MNPCA
      SRAA(J)(K) := (PA(I).STEM, PA(I).IR);
      DICT_IO.SET_INDEX(DICT_FILE(PA(I).D_K), PA(I).MNPC); 
      DICT_IO.READ(DICT_FILE(PA(I).D_K), DEA);
      DM := (PA(I).D_K, PA(I).MNPC, DEA);
      DMA(J) := DM;
      ODM := DM;
   else
      K := K + 1;              --  K indexes within the MNPCA array  - Next MNPC
      SRAA(J)(K) := (PA(I).STEM, PA(I).IR);
    end if;

    I := I + 1;              --  I cycles over full PA array
    end loop;

    when ADJ  =>  
  OSRA := NULL_SRA;
  ODMA := NULL_DMA;
  --ODM := NULL_DICTIONARY_MNPC_RECORD;
  --DM := NULL_DICTIONARY_MNPC_RECORD;
  while PA(I).IR.QUAL.POFS = ADJ   and
        I <= PA_LAST                   loop
--TEXT_IO.PUT_LINE("SRAA - ADJ");
    if PA(I).MNPC  /= ODM.MNPC  then   --  Encountering new MNPC
      OSRA := SRA;
      K := 1;                  --  K indexes within the MNPCA array --  Initialize
      J := J + 1;             --  J indexes the number of MNPCA arrays - Next MNPCA
      SRAA(J)(K) := (PA(I).STEM, PA(I).IR);
      DICT_IO.SET_INDEX(DICT_FILE(PA(I).D_K), PA(I).MNPC); 
      DICT_IO.READ(DICT_FILE(PA(I).D_K), DEA);
      DM := (PA(I).D_K, PA(I).MNPC, DEA);
      DMA(J) := DM;
      ODM := DM;
    else
      K := K + 1;              --  K indexes within the MNPCA array  - Next MNPC
      SRAA(J)(K) := (PA(I).STEM, PA(I).IR);
    end if;
--TEXT_IO.PUT_LINE("SRAA  + ADJ");
    I := I + 1;              --  I cycles over full PA array
    end loop;

     when NUM  =>  
  OSRA := NULL_SRA;
  ODMA := NULL_DMA;
  --ODM := NULL_DICTIONARY_MNPC_RECORD;
  --DM := NULL_DICTIONARY_MNPC_RECORD;
  while PA(I).IR.QUAL.POFS = NUM   and
        I <= PA_LAST                   loop
    if (PA(I).D_K = RRR)  then        --  Roman numeral
      OSRA := SRA;
      K := 1;                  --  K indexes within the MNPCA array --  Initialize
      J := J + 1;             --  J indexes the number of MNPCA arrays - Next MNPCA
      SRAA(J)(K) := (PA(I).STEM, PA(I).IR);
      --DICT_IO.SET_INDEX(DICT_FILE(PA(I).D_K), PA(I).MNPC); 
      --DICT_IO.READ(DICT_FILE(PA(I).D_K), DEA);




      DEA := NULL_DICTIONARY_ENTRY;
      DM := (PA(I).D_K, PA(I).MNPC, DEA);
      DMA(J) := DM;
      ODM := DM;
    elsif (PA(I).MNPC  /= ODM.MNPC) then    --  Encountering new MNPC    
      OSRA := SRA;
      K := 1;                  --  K indexes within the MNPCA array --  Initialize
      J := J + 1;             --  J indexes the number of MNPCA arrays - Next MNPCA
      SRAA(J)(K) := (PA(I).STEM, PA(I).IR);
      DICT_IO.SET_INDEX(DICT_FILE(PA(I).D_K), PA(I).MNPC); 
      DICT_IO.READ(DICT_FILE(PA(I).D_K), DEA);
      DM := (PA(I).D_K, PA(I).MNPC, DEA);
      DMA(J) := DM;
      ODM := DM;
    else
      K := K + 1;              --  K indexes within the MNPCA array  - Next MNPC
      SRAA(J)(K) := (PA(I).STEM, PA(I).IR);
    end if;

    I := I + 1;              --  I cycles over full PA array
    end loop;


    when V | VPAR | SUPINE  =>  
  OSRA := NULL_SRA;
  ODMA := NULL_DMA;
  --ODM := NULL_DICTIONARY_MNPC_RECORD;
  --DM := NULL_DICTIONARY_MNPC_RECORD;
  while (PA(I).IR.QUAL.POFS = V      or
         PA(I).IR.QUAL.POFS = VPAR   or
         PA(I).IR.QUAL.POFS = SUPINE)   and
        I <= PA_LAST                   loop
--TEXT_IO.PUT_LINE("Starting loop for VPAR I = " & INTEGER'IMAGE(I) & "   K = " & INTEGER'IMAGE(K));
    if (PA(I).MNPC  /= ODM.MNPC) and (PA(I).D_K /= PPP)   then   --  Encountering new MNPC
      OSRA := SRA;                                               --  But not for compound
      K := 1;                  --  K indexes within the MNPCA array --  Initialize
--TEXT_IO.PUT_LINE("Starting IRA for VPAR I = " & INTEGER'IMAGE(I) & "   K = " & INTEGER'IMAGE(K));
      J := J + 1;             --  J indexes the number of MNPCA arrays - Next MNPCA
--TEXT_IO.PUT_LINE("Shifting J for VPAR I = " & INTEGER'IMAGE(I) & "   J = " & INTEGER'IMAGE(J));
      SRAA(J)(K) := (PA(I).STEM, PA(I).IR);
      if PA(I).D_K /= PPP  then
        DICT_IO.SET_INDEX(DICT_FILE(PA(I).D_K), PA(I).MNPC); 
        DICT_IO.READ(DICT_FILE(PA(I).D_K), DEA);
      end if;     --  use previous DEA
      DM := (PA(I).D_K, PA(I).MNPC, DEA);
      DMA(J) := DM;
      ODM := DM;
    else
      K := K + 1;              --  K indexes within the MNPCA array  - Next MNPC
 --TEXT_IO.PUT_LINE("Continuing IRA for VPAR  I = " & INTEGER'IMAGE(I) & "   K = " & INTEGER'IMAGE(K)
 --                                                                      & "   J = " & INTEGER'IMAGE(J));
    SRAA(J)(K) := (PA(I).STEM, PA(I).IR);
    end if;

    I := I + 1;              --  I cycles over full PA array
    end loop;


  when others  =>  
--TEXT_IO.PUT_LINE("Others");
  OSRA := NULL_SRA;
  ODMA := NULL_DMA;
  --ODM := NULL_DICTIONARY_MNPC_RECORD;
  --DM := NULL_DICTIONARY_MNPC_RECORD;
  while I <= PA_LAST                   loop
 --TEXT_IO.PUT_LINE("Starting loop for OTHER I = " & INTEGER'IMAGE(I) & "   K = " & INTEGER'IMAGE(K));
   if (ODM.D_K  /= PA(I).D_K)  or
       (ODM.MNPC /= PA(I).MNPC)      then   --  Encountering new single (K only 1)
      OSRA := SRA;
      K := 1;                  --  K indexes within the MNPCA array --  Initialize
  --TEXT_IO.PUT_LINE("Starting IRA for OTHER I = " & INTEGER'IMAGE(I) & "   K = " & INTEGER'IMAGE(K));
    J := J + 1;             --  J indexes the number of MNPCA arrays - Next MNPCA
  --TEXT_IO.PUT_LINE("Shifting J for OTHER I = " & INTEGER'IMAGE(I) & "   J = " & INTEGER'IMAGE(J));
    SRAA(J)(K) := (PA(I).STEM, PA(I).IR);
      if PA(I).MNPC /= NULL_MNPC  then
        if PA(I).D_K = ADDONS  then
          DEA :=  NULL_DICTIONARY_ENTRY;   --  Fix for ADDONS in MEANS, not DICT_IO
        else
          DICT_IO.SET_INDEX(DICT_FILE(PA(I).D_K), PA(I).MNPC); 
          DICT_IO.READ(DICT_FILE(PA(I).D_K), DEA);
        end if;
      else                       --  Has no dictionary to read
        DEA:= NULL_DICTIONARY_ENTRY;
      end if;
      DM := (PA(I).D_K, PA(I).MNPC, DEA);
      DMA(J) := DM;
      ODM := DM;
   --else
    --  K := K + 1;              --  K indexes within the MNPCA array  - Next MNPC
    --  SRAA(J)(K) := (PA(I).STEM, PA(I).IR);
    end if;

    I := I + 1;              --  I cycles over full PA array
    exit;                    --  Since Other is only one, don't loop
  end loop;    

  end case;

  end if;      
--  --  This just for developer test, will be commented out   
--  if K > SRA_MAX  then 
--    SRA_MAX := K;
--PUT_STAT("*SRA_MAX for RAW_WORD " & HEAD(RAW_WORD, 26) & "   = " & INTEGER'IMAGE(SRA_MAX));
--  end if;
--  if J > DMA_MAX  then 
--    DMA_MAX := J;
--PUT_STAT("*DMA_MAX for RAW_WORD " & HEAD(RAW_WORD, 26) & "   = " & INTEGER'IMAGE(DMA_MAX));
--  end if;

end loop CYCLE_OVER_PA;

--TEXT_IO.PUT_LINE("Made QA");







--TEXT_IO.PUT_LINE("QA ARRAYS   FFFFFF  ======================================");
--     for J in 1..DICTIONARY_MNPC_ARRAY_SIZE  loop
--       if DMA(J) /= NULL_DICTIONARY_MNPC_RECORD  then
--         TEXT_IO.PUT(INTEGER'IMAGE(J) & "  ");
--         DICTIONARY_KIND_IO.PUT(DMA(J).D_K); TEXT_IO.PUT("  "); 
--         MNPC_IO.PUT(DMA(J).MNPC); TEXT_IO.NEW_LINE;
--       end if;
--     end loop;  
--     for J in 1..STEM_INFLECTION_ARRAY_ARRAY_SIZE  loop
--       for K in 1..STEM_INFLECTION_ARRAY_SIZE  loop
--         if SRAA(J)(K) /= NULL_STEM_INFLECTION_RECORD  then
--           TEXT_IO.PUT(INTEGER'IMAGE(J) & " " & INTEGER'IMAGE(K) & "  ");
--           QUALITY_RECORD_IO.PUT(SRAA(J)(K).IR.QUAL); TEXT_IO.NEW_LINE;
--         end if;
--       end loop;
--     end loop;  









  --  Sets + if capitalized      
      --  Strangely enough, it may enter LIST_STEMS with PA_LAST /= 0
      --  but be weeded and end up with no parse after LIST_SWEEP  -  PA_LAST = 0      
      if PA_LAST = 0  then  --  WORD failed
   --????      (DMA(1).D_K in ADDONS..YYY  and then TRIM(DMA(1).DE.STEMS(1)) /= "que")  then  --  or used FIXES/TRICKS
           if WORDS_MODE(IGNORE_UNKNOWN_NAMES)  and CAPITALIZED  then
             NNN_MEANING := HEAD(
               "Assume this is capitalized proper name/abbr",
               MAX_MEANING_SIZE);
           PA(1) := (HEAD(RAW_WORD, MAX_STEM_SIZE),
                       ((N, ((0, 0), X, X, X)), 0, NULL_ENDING_RECORD, X, X),
                        NNN, NULL_MNPC);
           PA_LAST := 1;    --  So LIST_NEIGHBORHOOD will not be called
           SRAA := NULL_SRAA;
           DMA := NULL_DMA;
           SRAA(1)(1) := (PA(1).STEM, PA(1).IR);
           DMA(1) := (NNN, 0, NULL_DICTIONARY_ENTRY);
         elsif  WORDS_MODE(IGNORE_UNKNOWN_CAPS)  and ALL_CAPS  then
           NNN_MEANING := HEAD(
             "Assume this is capitalized proper name/abbr",
             MAX_MEANING_SIZE);
           PA(1) := (HEAD(RAW_WORD, MAX_STEM_SIZE),
                    ((N, ((0, 0), X, X, X)), 0, NULL_ENDING_RECORD, X, X),
                     NNN, NULL_MNPC);
           PA_LAST := 1;
           SRAA := NULL_SRAA;
           DMA := NULL_DMA;
           SRAA(1)(1) := (PA(1).STEM, PA(1).IR);
           DMA(1) := (NNN, 0, NULL_DICTIONARY_ENTRY);
         end if;
      end if;


--                --  Does STATS
--      
----TEXT_IO.PUT_LINE("Before STATING FIXES");
--     if  WORDS_MDEV(WRITE_STATISTICS_FILE)    then      --  Omit rest of output
----            
----       for I in 1..PA_LAST  loop                       --  Just to PUT_STAT
----         if (PA(I).D_K = ADDONS)  then
----           if PA(I).IR.QUAL.POFS = PREFIX  then
----             PUT_STAT("ADDON PREFIX at "
----                     & HEAD(INTEGER'IMAGE(LINE_NUMBER), 8) & HEAD(INTEGER'IMAGE(WORD_NUMBER), 4)
----                     & "   " & HEAD(W, 20) & "   "  & PA(I).STEM);
----           elsif PA(I).IR.QUAL.POFS = SUFFIX  then
----             PUT_STAT("ADDON SUFFIX at "
----                     & HEAD(INTEGER'IMAGE(LINE_NUMBER), 8) & HEAD(INTEGER'IMAGE(WORD_NUMBER), 4)
----                     & "   " & HEAD(W, 20) & "   "  & PA(I).STEM);
----           elsif PA(I).IR.QUAL.POFS = TACKON  then
----             PUT_STAT("ADDON TACKON at "
----                     & HEAD(INTEGER'IMAGE(LINE_NUMBER), 8) & HEAD(INTEGER'IMAGE(WORD_NUMBER), 4)
----                     & "   " & HEAD(W, 20) & "   "  & PA(I).STEM);
----           end if;
----         end if;
----       end loop;
--         
--       
----    --  Just to find the words with long/complicated output at the LIST level 
----    --  This is done with the final PA_LAST, after SWEEP
--       if PA_LAST > FINAL_PA_LAST_MAX   then
--         PUT_STAT("$FINAL_PA_LAST_MAX    for RAW_WORD " & HEAD(RAW_WORD, 24) & "   = " & INTEGER'IMAGE(PA_LAST));
--         FINAL_PA_LAST_MAX := PA_LAST;
--       end if;
--       
--     end if;








if PA_LAST = 0   then

        if  WORDS_MODE(WRITE_OUTPUT_TO_FILE)      then
          if WORDS_MDEV(DO_PEARSE_CODES) then
            TEXT_IO.PUT(OUTPUT, "04 ");
          end if;
          TEXT_IO.PUT_LINE(OUTPUT, TAGGED_STRING("unknown", RAW_WORD));
        else              --  Just screen output
          if WORDS_MDEV(DO_PEARSE_CODES) then
            TEXT_IO.PUT(OUTPUT, "04 ");
          end if;
          TEXT_IO.PUT_LINE(TAGGED_STRING("unknown", RAW_WORD));
        end if;

        if WORDS_MODE(WRITE_UNKNOWNS_TO_FILE)  then
          if WORDS_MDEV(INCLUDE_UNKNOWN_CONTEXT) or
             WORDS_MDEV(DO_ONLY_INITIAL_WORD)  then
            TEXT_IO.PUT_LINE(INPUT_LINE);
            TEXT_IO.PUT_LINE(UNKNOWNS, INPUT_LINE);
          end if;
          if WORDS_MDEV(DO_PEARSE_CODES) then
            TEXT_IO.PUT(UNKNOWNS, "04 ");
          end if;
          TEXT_IO.PUT(UNKNOWNS, RAW_WORD);
          TEXT_IO.SET_COL(UNKNOWNS, 30);
          INFLECTIONS_PACKAGE.INTEGER_IO.PUT(UNKNOWNS, LINE_NUMBER, 7);
          INFLECTIONS_PACKAGE.INTEGER_IO.PUT(UNKNOWNS, WORD_NUMBER, 7);
          TEXT_IO.PUT_LINE(UNKNOWNS, "    ========   UNKNOWN    ");
        end if;
      end if;

      if PA_LAST = 0   then
        if WORDS_MODE(DO_STEMS_FOR_UNKNOWN)   then  
          if  WORDS_MODE(WRITE_OUTPUT_TO_FILE)  and then
              not WORDS_MODE(WRITE_UNKNOWNS_TO_FILE)   then  
             LIST_NEIGHBORHOOD(OUTPUT, RAW_WORD);     
           elsif  WORDS_MODE(WRITE_OUTPUT_TO_FILE)  and then
                  WORDS_MODE(WRITE_UNKNOWNS_TO_FILE)   then  
             LIST_NEIGHBORHOOD(OUTPUT, RAW_WORD);     
             LIST_NEIGHBORHOOD(UNKNOWNS, RAW_WORD);     
           elsif (NAME(CURRENT_INPUT) = NAME(STANDARD_INPUT))  then
             LIST_NEIGHBORHOOD(OUTPUT, RAW_WORD); 
          end if; 
        end if;
      end if;

      if PA_LAST = 0   then
        if WORDS_MDEV(UPDATE_LOCAL_DICTIONARY)  and  -- Don't if reading from file
          (NAME(CURRENT_INPUT) = NAME(STANDARD_INPUT))  then
          UPDATE_LOCAL_DICTIONARY_FILE;
          WORD(RAW_WORD, PA, PA_LAST);       --  Circular if you dont update!!!!!
        end if;
      end if;     

         --  Exit if UNKNOWNS ONLY (but had to do STATS above)         
         if  WORDS_MODE(DO_UNKNOWNS_ONLY)    then      --  Omit rest of output
           return;
         end if;

        J := 1;
        OSRA := NULL_SRA;
        if DMA(1) /= NULL_DICTIONARY_MNPC_RECORD then
          TEXT_IO.PUT_LINE(OUTPUT, "<word>");
          TEXT_IO.PUT_LINE(OUTPUT,
                           TAGGED_STRING("form", "xml:lang", "la", RAW_WORD));
        end if;
        OUTPUT_LOOP:
        while  DMA(J) /= NULL_DICTIONARY_MNPC_RECORD  loop

          if SRAA(J) /= OSRA  then --  Skips one identical SRA
                                   --  no matter what comes next
            if J /= 1 then
              TEXT_IO.PUT_LINE(OUTPUT, "</entry>");
            end if;
            TEXT_IO.PUT_LINE(OUTPUT, "<entry>");

--TEXT_IO.PUT_LINE("PUTting INFLECTIONS");         
            PUT_INFLECTION_ARRAY_J:
            for K in SRAA(J)'RANGE loop
              exit when SRAA(J)(K) = NULL_STEM_INFLECTION_RECORD;

              -- skip repeated inflections
              if K = 1 or else SRAA(J)(K) /= SRAA(J)(K-1) then
                PUT_INFLECTION(SRAA(J)(K), DMA(J));
                if SRAA(J)(K).STEM(1..3) = "PPL"  then
                  TEXT_IO.PUT(OUTPUT, "<note>");
                  TEXT_IO.PUT(OUTPUT, HEAD(PPP_MEANING, MM));
                  TEXT_IO.PUT_LINE(OUTPUT, "</note>");
                end if;
              end if;
            end loop PUT_INFLECTION_ARRAY_J;
            OSRA := SRAA(J);
          end if;

--TEXT_IO.PUT_LINE("PUTting FORM");         
          PUTTING_FORM:
          begin
            if J = 1  or else
               DICTIONARY_FORM(DMA(J).DE) /= DICTIONARY_FORM(DMA(J-1).DE)  then
            --  Put at first chance, skip duplicates
                 PUT_FORM(SRAA(J)(1), DMA(J));
            end if;
          end PUTTING_FORM;


--TEXT_IO.PUT_LINE("PUTting MEANING");         
         PUTTING_MEANING:
          begin
            if (DMA(J).D_K in GENERAL..UNIQUE)  then
              if (DMA(J).DE.MEAN /= DMA(J+1).DE.MEAN)  then
                --  This if handles simple multiple MEAN with same IR and FORM
                --  by anticipating duplicates and waiting until change
                PUT_MEANING_LINE(SRAA(J)(1), DMA(J));
              end if;
            else
            PUT_MEANING_LINE(SRAA(J)(1), DMA(J));
            end if;
          end PUTTING_MEANING;

          if I = PA_LAST  then
            TEXT_IO.NEW_LINE(OUTPUT);
          end if;
 --TEXT_IO.PUT_LINE("End of OUTPUT_LOOP with J = " & INTEGER'IMAGE(J));


          J := J + 1;
        end loop OUTPUT_LOOP;
 --TEXT_IO.PUT_LINE("Finished OUTPUT_LOOP");
        if J /= 1 then
          TEXT_IO.PUT_LINE(OUTPUT, "</entry>");
        end if;
        if DMA(1) /= NULL_DICTIONARY_MNPC_RECORD then
          TEXT_IO.PUT_LINE(OUTPUT, "</word>");
        end if;

-- if TRIMMED  then
--     PUT(OUTPUT, '*');
-- end if;


        exception
          when others  =>
            TEXT_IO.PUT_LINE(TAGGED_STRING("error", RAW_WORD));
            PUT_STAT("EXCEPTION LS at "
                     & HEAD(INTEGER'IMAGE(LINE_NUMBER), 8) & HEAD(INTEGER'IMAGE(WORD_NUMBER), 4)
                     & "   " & HEAD(W, 20) & "   "  & PA(I).STEM);
end LIST_STEMS;



    procedure LIST_ENTRY(OUTPUT   : TEXT_IO.FILE_TYPE;
                         D_K      : DICTIONARY_KIND;
                         MN       : DICT_IO.COUNT) is
      DE : DICTIONARY_ENTRY;
    begin
      DICT_IO.READ(DICT_FILE(D_K), DE, MN);
      TEXT_IO.PUT(OUTPUT, "=>  ");
      --TEXT_IO.PUT_LINE(OUTPUT, DICTIONARY_FORM(DE));
      PUT_DICTIONARY_FORM(OUTPUT, D_K, MN, DE);
      TEXT_IO.PUT_LINE(OUTPUT, 
                       TRIM(HEAD(DE.MEAN, MM)));  --  so it wont line wrap/put CR

    end LIST_ENTRY;





    procedure UNKNOWN_SEARCH(UNKNOWN       :  in STRING;
                             UNKNOWN_COUNT : out DICT_IO.COUNT) is

         use STEM_IO;

         D_K : constant DICTIONARY_KIND := GENERAL;
         J, J1, J2, JJ : STEM_IO.COUNT := 0;

         INDEX_ON : constant STRING := UNKNOWN;
         INDEX_FIRST, INDEX_LAST : STEM_IO.COUNT := 0;
         DS : DICTIONARY_STEM;
         FIRST_TRY, SECOND_TRY : BOOLEAN := TRUE;


         function FIRST_TWO(W : STRING) return STRING is
         --  'v' could be represented by 'u', like the new Oxford Latin Dictionary
         --  Fixes the first two letters of a word/stem which can be done right
            S : constant STRING := LOWER_CASE(W);
            SS : STRING(W'RANGE) := W;

            function UI(C : CHARACTER) return CHARACTER  is
            begin
               if (C = 'v')   then
                  return 'u';
               elsif (C = 'V')  then
                  return 'U';
               elsif (C = 'j')  then
                  return 'i';
               elsif (C = 'J')  then
                  return 'I';
               else
                  return C;
               end if;
            end UI;

         begin

            if S'LENGTH = 1  then
               SS(S'FIRST) := UI(W(S'FIRST));
            else
               SS(S'FIRST)   := UI(W(S'FIRST));
               SS(S'FIRST+1) := UI(W(S'FIRST+1));
            end if;

            return SS;
         end FIRST_TWO;





      begin

       if DICTIONARY_AVAILABLE(D_K)  then
               if not IS_OPEN(STEM_FILE(D_K))  then
                  OPEN(STEM_FILE(D_K), STEM_IO.IN_FILE,
                        ADD_FILE_NAME_EXTENSION(STEM_FILE_NAME,
                                                DICTIONARY_KIND'IMAGE(D_K)));
               end if;

         INDEX_FIRST := FIRST_INDEX(FIRST_TWO(INDEX_ON), D_K);
         INDEX_LAST  := LAST_INDEX(FIRST_TWO(INDEX_ON), D_K);

         if INDEX_FIRST > 0  and then INDEX_FIRST <= INDEX_LAST then


            J1 := STEM_IO.COUNT(INDEX_FIRST);    --######################
            J2 := STEM_IO.COUNT(INDEX_LAST);


                     FIRST_TRY := TRUE;

                     SECOND_TRY := TRUE;

                     J := (J1 + J2) / 2;

                 BINARY_SEARCH:
                     loop

                       if (J1 = J2-1) or (J1 = J2) then
                           if FIRST_TRY  then
                              J := J1;
                              FIRST_TRY := FALSE;
                           elsif SECOND_TRY  then
                              J := J2;
                              SECOND_TRY := FALSE;
                           else
                              JJ := J;
                              exit BINARY_SEARCH;
                           end if;
                        end if;

                        SET_INDEX(STEM_FILE(D_K), STEM_IO.COUNT(J));
                        READ(STEM_FILE(D_K), DS);

                        if  LTU(LOWER_CASE(DS.STEM), UNKNOWN)  then
                           J1 := J;
                           J := (J1 + J2) / 2;
                        elsif  GTU(LOWER_CASE(DS.STEM), UNKNOWN)  then
                           J2 := J;
                           J := (J1 + J2) / 2;
                        else
                           for I in reverse J1..J  loop
                              SET_INDEX(STEM_FILE(D_K), STEM_IO.COUNT(I));
                              READ(STEM_FILE(D_K), DS);

                              if EQU(LOWER_CASE(DS.STEM), UNKNOWN)  then
                                 JJ := I;


                              else
                                 exit;
                              end if;
                           end loop;

                           for I in J+1..J2  loop
                              SET_INDEX(STEM_FILE(D_K), STEM_IO.COUNT(I));
                              READ(STEM_FILE(D_K), DS);

                              if EQU(LOWER_CASE(DS.STEM), UNKNOWN)  then
                                JJ := I;


                              else
                                 exit BINARY_SEARCH;
                              end if;
                           end loop;
                           exit BINARY_SEARCH;

                        end if;
                     end loop BINARY_SEARCH;
                     J1 := JJ;
                     J2 := STEM_IO.COUNT(INDEX_LAST);


         end if;
         UNKNOWN_COUNT := DS.MNPC;

           CLOSE(STEM_FILE(D_K));  --??????
         end if;
 --TEXT_IO.PUT_LINE("Leaving LIST_NEIGHBORHOOD    UNKNOWN_SEARCH");
         end UNKNOWN_SEARCH;





    procedure LIST_NEIGHBORHOOD(OUTPUT : TEXT_IO.FILE_TYPE; 
                              INPUT_WORD : STRING) is

         D_K : constant DICTIONARY_KIND := GENERAL;
         DE : DICTIONARY_ENTRY;
         UNK_MNPC : DICT_IO.COUNT; 




     begin
--TEXT_IO.PUT_LINE("Entering LIST_NEIGHBORHOOD");

       if (TEXT_IO.NAME(OUTPUT) = 
             TEXT_IO.NAME(TEXT_IO.STANDARD_OUTPUT))  then
           MM := MAX_MEANING_PRINT_SIZE;   --  to keep from overflowing screen line
       else
           MM := MAX_MEANING_SIZE;
       end if;

         UNKNOWN_SEARCH(HEAD(INPUT_WORD, MAX_STEM_SIZE), UNK_MNPC);
--TEXT_IO.PUT_LINE("UNK_MNPC = " & INTEGER'IMAGE(INTEGER(UNK_MNPC)));
       if INTEGER(UNK_MNPC) > 0  then
         TEXT_IO.PUT_LINE(OUTPUT, 
        "----------  Entries in GENEAL Dictionary around the UNKNOWN  ----------");
         for MN in DICT_IO.COUNT(INTEGER(UNK_MNPC)-5).. 
                  DICT_IO.COUNT(INTEGER(UNK_MNPC)+3)  loop
           LIST_ENTRY(OUTPUT, D_K, MN);

         end loop;
       end if;

--TEXT_IO.PUT_LINE("Leaving LIST_NEIGHBORHOOD");

     end LIST_NEIGHBORHOOD;

   end LIST_PACKAGE;
