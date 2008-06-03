with STRINGS_PACKAGE; use STRINGS_PACKAGE;
with DEVELOPER_PARAMETERS; use DEVELOPER_PARAMETERS;
package body LATIN_FILE_NAMES is

  function ADD_FILE_NAME_EXTENSION(NAME : in STRING;
                                   EXTENSION : in STRING := "") return STRING is
  --  This is the version that creates a DOS file name
  --  One that has a name, a '.', and an extension no longer than 3 characters
  --  Arbitarily, we also truncate the NAME to 8 characters
  --  To port to another system, one needs to do this function appropriately
    NAME_LENGTH : INTEGER := NAME'LENGTH;
    EXTENSION_LENGTH : INTEGER := EXTENSION'LENGTH;
  begin
    if EXTENSION_LENGTH > 0 then
      if NAME_LENGTH >= 8  then
        NAME_LENGTH := 8;
      end if;
      if EXTENSION'LENGTH >= 3  then
        EXTENSION_LENGTH := 3;
      end if;
      return TRIM(DATA_FILE_DIRECTORY) &
             '/' &
             NAME(1..NAME_LENGTH) &
             '.' &
             EXTENSION(1..EXTENSION_LENGTH);
    else
      return TRIM(DATA_FILE_DIRECTORY) & '/' & NAME;
    end if;

  end ADD_FILE_NAME_EXTENSION;


end LATIN_FILE_NAMES;
