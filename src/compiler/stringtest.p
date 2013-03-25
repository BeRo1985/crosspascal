program stringtest;

const ConstString1 = '"const string" ';
      ConstString2 : string = '"string" ';
      ConstString3 : ansistring = '"ansistring" ';
      ConstString4 : widestring = '"widestring" ';
      ConstString5 : Unicodestring = '"unicodestring" ';
      ConstString6 : PChar = '"Pchar" ';
      ConstString7 : shortstring = '"shortstring" ';
//      ConstString8 : string[24] = '"string[24]" '+ConstString1;

var AString: string;
    AAnsistring: ansistring;
    AWidestring: widestring;
    AUnicodestring: Unicodestring;
    APChar: PChar;
    AShortstring: shortstring;

begin
  Writeln(ConstString1);
  Writeln(ConstString2);
  Writeln(ConstString3);
  Writeln(ConstString4);
  Writeln(ConstString5);
  Writeln(ConstString6);

  Writeln('------------');

  Writeln(ConstString1 + ConstString2);
  Writeln(ConstString1 + ConstString3);
  Writeln(ConstString1 + ConstString4);
  Writeln(ConstString1 + ConstString5);

  Writeln(ConstString2 + ConstString3);
  Writeln(ConstString2 + ConstString4);
  Writeln(ConstString2 + ConstString5);

  Writeln(ConstString3 + ConstString4);
  Writeln(ConstString3 + ConstString5);
  Writeln(ConstString4 + ConstString5);

  // Writeln(ConstString1 + ConstString6);
  // Writeln(ConstString2 + ConstString6);
  // Writeln(ConstString3 + ConstString6);
  // Writeln(ConstString4 + ConstString6);
  // Writeln(ConstString5 + ConstString6);
  Writeln('-------------');

  AString := ConstString2;
  AAnsistring := Conststring3;
  AWidestring := Conststring4;
  AUnicodestring := Conststring5;
  // APChar := Conststring6;
  // AShortstring := Conststring7;

  Writeln(AString);
  Writeln(AAnsistring);
  Writeln(AWidestring);
  Writeln(AUnicodestring);

  Writeln('------------');
  AAnsistring := AString + AAnsistring + AWidestring + AUnicodestring + ConstString5;
  AUnicodestring := AAnsistring;

  Writeln(AAnsistring);
  Writeln(AUnicodestring);

  // AUnicodestring := ConstString1 + ConstString2 + ConstString3 + ConstString4 + ConstString5;
  // AShortstring := ConstString1 + ConstString2 + ConstString3 + ConstString4 + ConstString5;
  // APChar := ConstString1 + ConstString2 + ConstString3 + ConstString4 + ConstString5;

end.
