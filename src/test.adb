   with Ada.Command_Line;
   with Ada.Text_IO; use Ada.Text_IO;
   with STRINGS_PACKAGE; use STRINGS_PACKAGE;
  procedure TEST is
    A : STRING := "Hello";
    B : STRING(1..5);
    C : STRING(1..10) := (others => 'C');
    D : STRING(1..20) := (others => 'D');
  begin
    B := A;                      -- B becomes "Hello"
    A(1) := 'h';                 -- A becomes "hello"
    A(2..3) := A(4..5);          -- A becomes "hlolo"
    A := B(1) & A(2..3) & "ol";  -- A becomes "Hlool"
    C(1..5) := B(1) & A(2..3) & "ol";  -- A becomes "Hlool"
    Put_Line(A);
    A(2..3) := B(2..3);
    Put_Line(A);
    Put_Line("<" & C & ">");
    D(1..C'LENGTH) := C;
    Put_Line("[" & D & "]");
  end TEST;
