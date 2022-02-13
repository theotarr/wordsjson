with TEXT_IO;
with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
with DICTIONARY_PACKAGE; use DICTIONARY_PACKAGE;

package EXAMPLE_LINE is

  procedure PUT_EXAMPLE_LINE(OUTPUT : TEXT_IO.FILE_TYPE;
                             IR : in INFLECTION_RECORD; 
                             DE : in DICTIONARY_ENTRY);

  function HAS_EXAMPLE_LINE(IR : in INFLECTION_RECORD; 
                            DE : in DICTIONARY_ENTRY) return BOOLEAN;

end EXAMPLE_LINE;
