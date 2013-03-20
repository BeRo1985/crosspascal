program ConvertUnicode;
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$ifdef win32}
 {$apptype console}
{$endif}
{$ifdef win64}
 {$apptype console}
{$endif}
uses SysUtils,Classes;

const MaxUnicodeChar=$10ffff;
      CountUnicodeChars=$110000;

type TUnicodeDWords=array[0..MaxUnicodeChar] of longint;

var UnicodeCategories:TUnicodeDWords;
    UnicodeScripts:TUnicodeDWords;
    UnicodeLowerCaseDeltas:TUnicodeDWords;
    UnicodeUpperCaseDeltas:TUnicodeDWords;
    UnicodeTitleCaseDeltas:TUnicodeDWords;
    Categories:TStringList;
    Scripts:TStringList;
    OutputList:TStringList;

function GetUntilSplitter(const Splitter:ansistring;var s:ansistring):ansistring;
var i:longint;
begin
 i:=pos(Splitter,s);
 if i>0 then begin
  result:=trim(copy(s,1,i-1));
  Delete(s,1,(i+length(Splitter))-1);
  s:=trim(s);
 end else begin
  result:=trim(s);
  s:='';
 end;
end;

procedure ParseBlocks;
type TUnicodeBlock=record
      Name:ansistring;
      FromChar,ToChar:longword;
     end;
var List:TStringList;
    i,j,k,FromChar,ToChar,Count:longint;
    s,p:ansistring;
    Blocks:array of TUnicodeBlock;
begin
 Blocks:=nil;
 try
  Count:=0;
  OutputList.Add('type TUnicodeBlock=record');
  OutputList.Add('      Name:ansistring;');
  OutputList.Add('      FromChar,ToChar:longword;');
  OutputList.Add('     end;');
  List:=TStringList.Create;
  try
   List.LoadFromFile(IncludeTrailingPathDelimiter('data')+'Blocks.txt');
   for i:=0 to List.Count-1 do begin
    s:=trim(List[i]);
    if (length(s)=0) or ((length(s)>0) and (s[1]='#')) then begin
     continue;
    end;
    j:=pos('#',s);
    if j>0 then begin
     s:=trim(copy(s,1,j-1));
    end;
    j:=pos(';',s);
    if j=0 then begin
     continue;
    end;
    p:=trim(copy(s,j+1,length(s)-j));
    s:=trim(copy(s,1,j-1));
    j:=pos('..',s);
    if j=0 then begin
     FromChar:=StrToInt('$'+trim(s));
     ToChar:=FromChar;
    end else begin
     FromChar:=StrToInt('$'+trim(copy(s,1,j-1)));
     ToChar:=StrToInt('$'+trim(copy(s,j+2,length(s)-(j+1))));
    end;
    if (Count+1)>=length(Blocks) then begin
     j:=1;
     k:=Count+1;
     while j<=k do begin
      inc(j,j);
     end;
     SetLength(Blocks,j);
    end;
    Blocks[Count].Name:=p;
    Blocks[Count].FromChar:=FromChar;
    Blocks[Count].ToChar:=ToChar;
    inc(Count);
   end;
   SetLength(Blocks,Count);
  finally
   List.Free;
  end;
  OutputList.Add('const UnicodeBlockCount='+IntToStr(Count)+';');
  OutputList.Add('      UnicodeBlocks:array[0..'+IntToStr(Count-1)+'] of TUnicodeBlock=(');
  for i:=0 to Count-1 do begin
   if (i+1)<Count then begin
    OutputList.Add('       (Name:'''+Blocks[i].Name+''';FromChar:'+inttostr(Blocks[i].FromChar)+';ToChar:'+inttostr(Blocks[i].ToChar)+'),');
   end else begin
    OutputList.Add('       (Name:'''+Blocks[i].Name+''';FromChar:'+inttostr(Blocks[i].FromChar)+';ToChar:'+inttostr(Blocks[i].ToChar)+'));');
   end;
  end;
  if Count=0 then begin
   OutputList.Add(');');
  end;
  OutputList.Add('');
 finally
  SetLength(Blocks,0);
 end;
end;

procedure ParseDerivedGeneralCategory;
var List:TStringList;
    i,j,ci,FromChar,ToChar,CurrentChar:longint;
    s,p:ansistring;
begin
 List:=TStringList.Create;
 try
  List.LoadFromFile(IncludeTrailingPathDelimiter('data')+'DerivedGeneralCategory.txt');
  for i:=0 to List.Count-1 do begin
   s:=trim(List[i]);
   if (length(s)=0) or ((length(s)>0) and (s[1]='#')) then begin
    continue;
   end;
   j:=pos('#',s);
   if j>0 then begin
    s:=trim(copy(s,1,j-1));
   end;
   j:=pos(';',s);
   if j=0 then begin
    continue;
   end;
   p:=trim(copy(s,j+1,length(s)-j));
   ci:=Categories.IndexOf(p);
   if ci<0 then begin
    ci:=Categories.Add(p);
   end;
   s:=trim(copy(s,1,j-1));
   j:=pos('..',s);
   if j=0 then begin
    CurrentChar:=StrToInt('$'+trim(s));
    UnicodeCategories[CurrentChar]:=ci;
   end else begin
    FromChar:=StrToInt('$'+trim(copy(s,1,j-1)));
    ToChar:=StrToInt('$'+trim(copy(s,j+2,length(s)-(j+1))));
    for CurrentChar:=FromChar to ToChar do begin
     UnicodeCategories[CurrentChar]:=ci;
    end;
   end;
  end;
 finally
  List.Free;
 end;
end;

procedure ParseScripts;
var List:TStringList;
    i,j,si,FromChar,ToChar,CurrentChar:longint;
    s,p:ansistring;
begin
 List:=TStringList.Create;
 try
  List.LoadFromFile(IncludeTrailingPathDelimiter('data')+'Scripts.txt');
  for i:=0 to List.Count-1 do begin
   s:=trim(List[i]);
   if (length(s)=0) or ((length(s)>0) and (s[1]='#')) then begin
    continue;
   end;
   j:=pos('#',s);
   if j>0 then begin
    s:=trim(copy(s,1,j-1));
   end;
   j:=pos(';',s);
   if j=0 then begin
    continue;
   end;
   p:=trim(copy(s,j+1,length(s)-j));
   si:=Scripts.IndexOf(p);
   if si<0 then begin
    si:=Scripts.Add(p);
   end;
   s:=trim(copy(s,1,j-1));
   j:=pos('..',s);
   if j=0 then begin
    CurrentChar:=StrToInt('$'+trim(s));
    UnicodeScripts[CurrentChar]:=si;
   end else begin
    FromChar:=StrToInt('$'+trim(copy(s,1,j-1)));
    ToChar:=StrToInt('$'+trim(copy(s,j+2,length(s)-(j+1))));
    for CurrentChar:=FromChar to ToChar do begin
     UnicodeScripts[CurrentChar]:=si;
    end;
   end;
  end;
 finally
  List.Free;
 end;
end;

procedure ParseUnicodeData;
var List:TStringList;
    i,j,ci,OtherChar,CurrentChar:longint;
    s,cs:ansistring;
begin
 List:=TStringList.Create;
 try
  List.LoadFromFile(IncludeTrailingPathDelimiter('data')+'UnicodeData.txt');
  for i:=ord('a') to ord('z') do begin
   UnicodeUpperCaseDeltas[i]:=longint(ord('A')-ord('a'));
  end;
  for i:=ord('A') to ord('Z') do begin
   UnicodeLowerCaseDeltas[i]:=ord('a')-ord('A');
  end;
  for i:=$ff21 to $ff3a do begin
   UnicodeLowerCaseDeltas[i]:=$ff41-$ff21;
  end;
  for i:=$ff41 to $ff5a do begin
   UnicodeUpperCaseDeltas[i]:=longint($ff21-$ff41);
  end;
  for i:=0 to List.Count-1 do begin
   s:=trim(List[i]);
   if (length(s)=0) or ((length(s)>0) and (s[1]='#')) then begin
    continue;
   end;
   j:=pos('#',s);
   if j>0 then begin
    s:=trim(copy(s,1,j-1));
   end;
   j:=pos(';',s);
   if j=0 then begin
    continue;
   end;
   CurrentChar:=StrToInt('$'+GetUntilSplitter(';',s)); // Code
   GetUntilSplitter(';',s); // Name
   begin
    cs:=GetUntilSplitter(';',s); // Class
    ci:=Categories.IndexOf(cs);
    if ci<0 then begin
     ci:=Categories.Add(cs);
    end;
    if UnicodeCategories[CurrentChar]<>ci then begin
     writeln(ErrOutput,CurrentChar,' has multiple categories?');
     UnicodeCategories[CurrentChar]:=ci;
    end;
   end;
   GetUntilSplitter(';',s); // ???
   GetUntilSplitter(';',s); // ???
   GetUntilSplitter(';',s); // ???
   GetUntilSplitter(';',s); // ???
   GetUntilSplitter(';',s); // ???
   GetUntilSplitter(';',s); // ???
   GetUntilSplitter(';',s); // ???
   GetUntilSplitter(';',s); // ???
   GetUntilSplitter(';',s); // ???
   begin
    OtherChar:=StrToIntDef('$'+GetUntilSplitter(';',s),-1); // UpperChar Code
    if (OtherChar>=0) and (OtherChar<>CurrentChar) then begin
     UnicodeUpperCaseDeltas[CurrentChar]:=OtherChar-CurrentChar;
    end;
   end;
   begin
    OtherChar:=StrToIntDef('$'+GetUntilSplitter(';',s),-1); // LowerChar Code
    if (OtherChar>=0) and (OtherChar<>CurrentChar) then begin
     UnicodeLowerCaseDeltas[CurrentChar]:=OtherChar-CurrentChar;
    end;
   end;
   begin
    OtherChar:=StrToIntDef('$'+GetUntilSplitter(';',s),-1); // TitleChar Code
    if (OtherChar>=0) and (OtherChar<>CurrentChar) then begin
     UnicodeTitleCaseDeltas[CurrentChar]:=OtherChar-CurrentChar;
    end;
   end;
  end;
 finally
  List.Free;
 end;
end;

procedure PackTable(const Table:array of longint;Level:integer;const Name:ansistring);
type TBlock=array of longint;
     TBlocks=array of TBlock;
     TIndices=array of longint;
var BestBlockSize,BlockSize,CountBlocks,CountIndices,Index,BlockPosition,Bytes,BestBytes,Bits,BestBits,EntryBytes,IndicesEntryBytes,BestIndicesEntryBytes,i,j,k:longint;
    Block:TBlock;
    Blocks:TBlocks;
    Indices:TIndices;
    BestBlocks:TBlocks;
    BestIndices:TIndices;
    OK:boolean;
    s:ansistring;
begin
 if Level<2 then begin
  Block:=nil;
  Blocks:=nil;
  Indices:=nil;
  BestBlocks:=nil;
  BestIndices:=nil;
  try
   BestBlockSize:=length(Table)*2;
   BestBits:=24;
   BlockSize:=1;
   Bits:=0;
   BestBytes:=-1;
   i:=0;
   OK:=true;
   for Index:=0 to length(Table)-1 do begin
    j:=Table[Index];
    if j<0 then begin
     OK:=false;
    end;
    j:=abs(j);
    if i<j then begin
     i:=j;
    end;
   end;
   if OK then begin
    if i<256 then begin
     EntryBytes:=1;
     s:='byte';
    end else if i<65536 then begin
     EntryBytes:=2;
     s:='word';
    end else begin
     EntryBytes:=4;
     s:='longword';
    end;
   end else begin
    if i<128 then begin
     EntryBytes:=1;
     s:='shortint';
    end else if i<32768 then begin
     EntryBytes:=2;
     s:='smallint';
    end else begin
     EntryBytes:=4;
     s:='longint';
    end;
   end;
   BestIndicesEntryBytes:=4;
   while BlockSize<length(Table) do begin
    SetLength(Block,BlockSize);
    SetLength(Blocks,(length(Table) div BlockSize)+1);
    FillChar(Block[0],BlockSize,#$ff);
    BlockPosition:=0;
    CountBlocks:=0;
    CountIndices:=0;
    for Index:=0 to length(Table)-1 do begin
     Block[BlockPosition]:=Table[Index];
     inc(BlockPosition);
     if BlockPosition=BlockSize then begin
      k:=-1;
      for i:=0 to CountBlocks-1 do begin
       OK:=true;
       for j:=0 to BlockSize-1 do begin
        if Blocks[i,j]<>Block[j] then begin
         OK:=false;
         break;
        end;
       end;
       if OK then begin
        k:=i;
        break;
       end;
      end;
      if k<0 then begin
       k:=CountBlocks;
       Blocks[CountBlocks]:=copy(Block);
       inc(CountBlocks);
      end;
      if (CountIndices+1)>=length(Indices) then begin
       i:=1;
       j:=CountIndices+1;
       while i<=j do begin
        inc(i,i);
       end;
       SetLength(Indices,i);
      end;
      Indices[CountIndices]:=k;
      inc(CountIndices);
      BlockPosition:=0;
     end;
    end;
    if CountBlocks<256 then begin
     IndicesEntryBytes:=1;
    end else if CountBlocks<65536 then begin
     IndicesEntryBytes:=2;
    end else begin
     IndicesEntryBytes:=4;
    end;
    Bytes:=((CountBlocks*BlockSize)*EntryBytes)+(CountIndices*IndicesEntryBytes);
    if (BestBytes<0) or (Bytes<=BestBytes) then begin
     BestBytes:=Bytes;
     BestBlockSize:=BlockSize;
     BestBits:=Bits;
     BestIndicesEntryBytes:=EntryBytes;
     BestBlocks:=copy(Blocks,0,CountBlocks);
     BestIndices:=copy(Indices,0,CountIndices);
    end;
    SetLength(Blocks,0);
    SetLength(Indices,0);
    inc(BlockSize,BlockSize);
    inc(Bits);
   end;
   OutputList.Add('// '+Name+': '+IntToStr(BestBytes)+' bytes, '+IntToStr(length(BestBlocks))+' blocks with '+IntToStr(BestBlockSize)+' items per '+IntToStr(EntryBytes)+' bytes and '+IntToStr(length(BestIndices))+' indices per '+IntToStr(BestIndicesEntryBytes)+' bytes');
   OutputList.Add('const '+Name+'BlockBits='+IntToStr(BestBits)+';');
   OutputList.Add('      '+Name+'BlockMask='+IntToStr((1 shl BestBits)-1)+';');
   OutputList.Add('      '+Name+'BlockSize='+IntToStr(BestBlockSize)+';');
   OutputList.Add('      '+Name+'BlockCount='+IntToStr(length(BestBlocks))+';');
   OutputList.Add('      '+Name+'BlockData:array[0..'+IntToStr(length(BestBlocks)-1)+',0..'+IntToStr(BestBlockSize-1)+'] of '+s+'=(');
   s:='';
   for i:=0 to length(BestBlocks)-1 do begin
    s:=s+'(';
    for j:=0 to BestBlockSize-1 do begin
     s:=s+IntToStr(BestBlocks[i,j]);
     if (j+1)<BestBlockSize then begin
      s:=s+',';
     end;
     if length(s)>80 then begin
      OutputList.Add(s);
      s:='';
     end;
    end;
    s:=s+')';
    if (i+1)<length(BestBlocks) then begin
     s:=s+',';
    end;
    OutputList.Add(s);
    s:='';
   end;
   if length(s)>0 then begin
    OutputList.Add(s);
    s:='';
   end;
   OutputList.Add(');');
   if Level=1 then begin
    case BestIndicesEntryBytes of
     1:begin
      s:='byte';
     end;
     2:begin
      s:='word';
     end;
     else begin
      s:='longword';
     end;
    end;
    OutputList.Add('      '+Name+'IndexCount='+IntToStr(length(BestBlocks))+';');
    OutputList.Add('      '+Name+'IndexData:array[0..'+IntToStr(length(BestIndices)-1)+'] of '+s+'=(');
    s:='';
    for i:=0 to length(BestIndices)-1 do begin
     s:=s+IntToStr(BestIndices[i]);
     if (i+1)<length(BestIndices) then begin
      s:=s+',';
     end;
     if length(s)>80 then begin
      OutputList.Add(s);
      s:='';
     end;
    end;
    if length(s)>0 then begin
     OutputList.Add(s);
     s:='';
    end;
    OutputList.Add(');');
    OutputList.Add('');
   end else begin
    OutputList.Add('');
    PackTable(BestIndices,Level+1,Name+'Index');
   end;
  finally
   SetLength(Block,0);
   SetLength(Blocks,0);
   SetLength(Indices,0);
   SetLength(BestBlocks,0);
   SetLength(BestIndices,0);
  end;
 end;
end;

var i:longint;
begin
 FillChar(UnicodeCategories,sizeof(TUnicodeDWords),#0);
 FillChar(UnicodeScripts,sizeof(TUnicodeDWords),#$0);
 FillChar(UnicodeUpperCaseDeltas,sizeof(TUnicodeDWords),#$0);
 FillChar(UnicodeLowerCaseDeltas,sizeof(TUnicodeDWords),#$0);
 FillChar(UnicodeTitleCaseDeltas,sizeof(TUnicodeDWords),#$0);
 OutputList:=TStringList.Create;
 try
  Categories:=TStringList.Create;
  Categories.Add('Cn');
  try
   Scripts:=TStringList.Create;
   Scripts.Add('Unknown');
   Scripts.Add('Common');
   try
    ParseDerivedGeneralCategory;
    ParseScripts;
    ParseUnicodeData;
    OutputList.Add('unit Unicode;');
    OutputList.Add('{$ifdef fpc}');
    OutputList.Add(' {$mode delphi}');
    OutputList.Add('{$endif}');
    OutputList.Add('interface');
    OutputList.Add('');
    ParseBlocks;
    begin
     OutputList.Add('const UnicodeCategoryIDs:array[0..'+IntToStr(Categories.Count-1)+'] of ansistring=(');
     for i:=0 to Categories.Count-1 do begin
      if (i+1)<Categories.Count then begin
       OutputList.Add(''''+Categories[i]+''',');
      end else begin
       OutputList.Add(''''+Categories[i]+'''');
      end;
     end;
     OutputList.Add(');');
     for i:=0 to Categories.Count-1 do begin
      OutputList.Add('      UnicodeCategory'+Categories[i]+'='+IntToStr(i)+';');
     end;
     OutputList.Add('      UnicodeCategoryCount='+IntToStr(Categories.Count)+';');
     OutputList.Add('      _CT_UNASSIGNED=UnicodeCategoryCn;');
     OutputList.Add('      _CT_UPPERCASE_LETTER=UnicodeCategoryLu;');
     OutputList.Add('      _CT_LOWERCASE_LETTER=UnicodeCategoryLl;');
     OutputList.Add('      _CT_TITLECASE_LETTER=UnicodeCategoryLt;');
     OutputList.Add('      _CT_MODIFIER_LETTER=UnicodeCategoryLm;');
     OutputList.Add('      _CT_OTHER_LETTER=UnicodeCategoryLo;');
     OutputList.Add('      _CT_NON_SPACING_MARK=UnicodeCategoryMn;');
     OutputList.Add('      _CT_ENCLOSING_MARK=UnicodeCategoryMe;');
     OutputList.Add('      _CT_COMBINING_SPACING_MARK=UnicodeCategoryMc;');
     OutputList.Add('      _CT_DECIMAL_DIGIT_NUMBER=UnicodeCategoryNd;');
     OutputList.Add('      _CT_LETTER_NUMBER=UnicodeCategoryNl;');
     OutputList.Add('      _CT_OTHER_NUMBER=UnicodeCategoryNo;');
     OutputList.Add('      _CT_SPACE_SEPARATOR=UnicodeCategoryZs;');
     OutputList.Add('      _CT_LINE_SEPARATOR=UnicodeCategoryZl;');
     OutputList.Add('      _CT_PARAGRAPH_SEPARATOR=UnicodeCategoryZp;');
     OutputList.Add('      _CT_CONTROL=UnicodeCategoryCc;');
     OutputList.Add('      _CT_FORMAT=UnicodeCategoryCf;');
     OutputList.Add('      _CT_PRIVATE_USE=UnicodeCategoryCo;');
     OutputList.Add('      _CT_SURROGATE=UnicodeCategoryCs;');
     OutputList.Add('      _CT_DASH_PUNCTUATION=UnicodeCategoryPd;');
     OutputList.Add('      _CT_START_PUNCTUATION=UnicodeCategoryPs;');
     OutputList.Add('      _CT_END_PUNCTUATION=UnicodeCategoryPe;');
     OutputList.Add('      _CT_INITIAL_PUNCTUATION=UnicodeCategoryPi;');
     OutputList.Add('      _CT_FINAL_PUNCTUATION=UnicodeCategoryPf;');
     OutputList.Add('      _CT_CONNECTOR_PUNCTUATION=UnicodeCategoryPc;');
     OutputList.Add('      _CT_OTHER_PUNCTUATION=UnicodeCategoryPo;');
     OutputList.Add('      _CT_MATH_SYMBOL=UnicodeCategorySm;');
     OutputList.Add('      _CT_CURRENCY_SYMBOL=UnicodeCategorySc;');
     OutputList.Add('      _CT_MODIFIER_SYMBOL=UnicodeCategorySk;');
     OutputList.Add('      _CT_OTHER_SYMBOL=UnicodeCategorySo;');
     OutputList.Add('');
    end;
    begin
     OutputList.Add('const UnicodeScriptIDs:array[0..'+IntToStr(Scripts.Count-1)+'] of ansistring=(');
     for i:=0 to Scripts.Count-1 do begin
      if (i+1)<Scripts.Count then begin
       OutputList.Add(''''+Scripts[i]+''',');
      end else begin
       OutputList.Add(''''+Scripts[i]+'''');
      end;
     end;
     OutputList.Add(');');
     for i:=0 to Scripts.Count-1 do begin
      OutputList.Add('     UnicodeScript'+Scripts[i]+'='+IntToStr(i)+';');
     end;
     OutputList.Add('     UnicodeScriptCount='+IntToStr(Scripts.Count)+';');
     OutputList.Add('');
    end;
    PackTable(UnicodeCategories,0,'UnicodeCategoryArray');
    writeln;
    PackTable(UnicodeScripts,0,'UnicodeScriptArray');
    writeln;
    PackTable(UnicodeUpperCaseDeltas,0,'UnicodeUpperCaseDeltaArray');
    PackTable(UnicodeLowerCaseDeltas,0,'UnicodeLowerCaseDeltaArray');
    PackTable(UnicodeTitleCaseDeltas,0,'UnicodeTitleCaseDeltaArray');
    OutputList.Add('implementation');
    OutputList.Add('end.');
    OutputList.SaveToFile(IncludeTrailingPathDelimiter('..')+'Unicode.pas');
   finally
    Scripts.Free;
   end;
  finally
   Categories.Free;
  end;
 finally
  OutputList.Free;
 end;
end.
