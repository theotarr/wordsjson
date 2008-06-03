with TEXT_IO;
with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
with DICTIONARY_PACKAGE; use DICTIONARY_PACKAGE;
package LIST_PACKAGE is

         
--  SCROLL_LINE_NUMBER : INTEGER := 0;
--  OUTPUT_SCROLL_COUNT : INTEGER := 0;
--

  function CONJ_TO_XML(T : DECN_RECORD; SHOW_VAR : BOOLEAN) return STRING;
  function DECL_TO_XML(T : DECN_RECORD; SHOW_VAR : BOOLEAN) return STRING;
  function GENDER_TO_XML(T : GENDER_TYPE) return STRING;
  function PART_OF_SPEECH_TO_XML(T : PART_OF_SPEECH_TYPE) return STRING;
  function CASE_TO_XML(T : CASE_TYPE) return STRING;
  function NUMBER_TO_XML(T : NUMBER_TYPE) return STRING;
  function COMPARISON_TO_XML(T : COMPARISON_TYPE) return STRING;
  function TENSE_VOICE_MOOD_TO_XML(T : TENSE_VOICE_MOOD_RECORD) return STRING;
  function NUMERAL_SORT_TO_XML(T : NUMERAL_SORT_TYPE) return STRING;
  function PERSON_TO_XML(T : PERSON_TYPE) return STRING;
  function VERB_KIND_TO_XML(T : VERB_KIND_TYPE) return STRING;
  
  procedure LIST_STEMS(OUTPUT   : TEXT_IO.FILE_TYPE;
                       RAW_WORD : STRING;
                       INPUT_LINE : STRING;
                       PA       : in out PARSE_ARRAY; 
                       PA_LAST  : in out INTEGER);
                       
                       
  procedure LIST_ENTRY(OUTPUT   : TEXT_IO.FILE_TYPE;
                       D_K      : DICTIONARY_KIND;
                       MN       : DICT_IO.COUNT);

                         
  procedure UNKNOWN_SEARCH(UNKNOWN       :  in STRING;
                           UNKNOWN_COUNT : out DICT_IO.COUNT);
                                  
  procedure LIST_NEIGHBORHOOD(OUTPUT : TEXT_IO.FILE_TYPE; INPUT_WORD : STRING);

end LIST_PACKAGE;
