program stringtest;

const ConstString1 = '"const string" ';
      ConstString2 : string = '"string" ';
      ConstString3 : ansistring = '"ansistring" ';
      ConstString4 : widestring = '"widestring" ';
      ConstString5 : Unicodestring = '"unicodestring" ';
      ConstString6 : PChar = '"Pchar" ';
      ConstString7 : shortstring = '"shortstring" ';
      ConstString8 : string[64] = '"string[24]" '+ConstString1;

var AString: string;
    AAnsistring: ansistring;
    AWidestring: widestring;
    AUnicodestring: Unicodestring;
    APChar: PChar;
    AShortstring: shortstring;

procedure TestA(const s: String);
var s2: string;
begin
  s2 := s;
  Writeln(s2);
end;

procedure TestB(var s: String);
begin
  Write(s,' is now known as ');
  s := 'Chocolate';
  Writeln(s);
end;

procedure TestC(s: string);
begin
  Writeln(s);
  s := 'I AM FREE!';
  Writeln(s);
end;

begin
  AString := ConstString1;
  TestA(AString + AString);
  TestB(AString);
  TestC(ConstString1  + AString);

  Writeln(ConstString1);
  Writeln(ConstString2);
  Writeln(ConstString3);
  Writeln(ConstString4);
  Writeln(1);

  Writeln(ConstString5);
  Writeln(ConstString6);

  Writeln('------------');

  Writeln(ConstString1 + ConstString2);
  Writeln(ConstString1 + ConstString3);
  AString := ConstString1 + ConstString4;
  //Writeln(AString);

  Writeln(ConstString1 + ConstString5);
  Writeln(ConstString2 + ConstString3);
  Writeln(ConstString2 + ConstString4);
  Writeln(ConstString2 + ConstString5);
  Writeln(ConstString3 + ConstString4);
  Writeln(ConstString3 + ConstString5);
  Writeln(ConstString4 + ConstString5);

  Writeln(Conststring7);
  Writeln(Conststring8);
  AString := ConstString7 + ConstString8;
  Writeln(AString,'***');

  Writeln(ConstString7 + ConstString8,Length(AString));

//   Writeln(ConstString1 + string(ConstString6));
//   Writeln(ConstString2 + ConstString6);
//   Writeln(ConstString3 + ConstString6);
//   Writeln(ConstString4 + ConstString6);
//   Writeln(ConstString5 + ConstString6);

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
