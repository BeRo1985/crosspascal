unit BeRoStream;
{$ifdef fpc}
 {$mode delphi}
 {$ifdef cpui386}
  {$define cpu386}
 {$endif}
 {$ifdef cpu386}
  {$asmmode intel}
 {$endif}
 {$ifdef cpuamd64}
  {$asmmode intel}
 {$endif}
 {$ifdef FPC_LITTLE_ENDIAN}
  {$define LITTLE_ENDIAN}
 {$else}
  {$ifdef FPC_BIG_ENDIAN}
   {$define BIG_ENDIAN}
  {$endif}
 {$endif}
 {-$pic off}
 {$define caninline}
 {$ifdef FPC_HAS_TYPE_EXTENDED}
  {$define HAS_TYPE_EXTENDED}
 {$else}
  {$undef HAS_TYPE_EXTENDED}
 {$endif}
 {$ifdef FPC_HAS_TYPE_DOUBLE}
  {$define HAS_TYPE_DOUBLE}
 {$else}
  {$undef HAS_TYPE_DOUBLE}
 {$endif}
 {$ifdef FPC_HAS_TYPE_SINGLE}
  {$define HAS_TYPE_SINGLE}
 {$else}
  {$undef HAS_TYPE_SINGLE}
 {$endif}
{$else}
 {$realcompatibility off}
 {$localsymbols on}
 {$define LITTLE_ENDIAN}
 {$ifndef cpu64}
  {$define cpu32}
 {$endif}
 {$define HAS_TYPE_EXTENDED}
 {$define HAS_TYPE_DOUBLE}
 {$define HAS_TYPE_SINGLE}
{$endif}
{$ifdef win32}
 {$define windows}
{$endif}
{$ifdef win64}
 {$define windows}
{$endif}
{$ifdef wince}
 {$define windows}
{$endif}
{$rangechecks off}
{$extendedsyntax on}
{$writeableconst on}
{$hints off}
{$booleval off}
{$typedaddress off}
{$stackframes off}
{$varstringchecks on}
{$typeinfo on}
{$overflowchecks off}
{$longstrings on}
{$openstrings on}

interface

uses HugeString;

const bsoFromBeginning=0;
      bsoFromCurrent=1;
      bsoFromEnd=2;

type PBeRoStreamData=^TBeRoStreamData;
     TBeRoStreamData=packed array[0..$7ffffffe] of byte;

     PBeRoStreamBuffer=^TBeRoStreamBuffer;
     TBeRoStreamBuffer=packed array[1..4096] of byte;

     PBeRoStream=^TBeRoStream;
     TBeRoStream=class
      private
       fPosition,fSize,fInMemorySize:longint;
       fData:PBeRoStreamData;
       fBitBuffer:longword;
       fBitBufferSize:byte;
       procedure Realloc(NewInMemorySize:longint);
       procedure Resize(NewSize:longint);
       function GetString:ansistring;
       procedure setstring(Value:ansistring);
       function GetByte(BytePosition:longint):byte;
       procedure SetByte(BytePosition:longint;Value:byte);
      public
       constructor Create;
       destructor Destroy; override;
       function Assign(Src:TBeRoStream):longint;
       function Append(Src:TBeRoStream):longint;
       function AppendFrom(Src:TBeRoStream;Counter:longint):longint;
       procedure Clear; virtual;
       function read(var Buf;Count:longint):longint; virtual;
       function ReadAt(Position:longint;var Buf;Count:longint):longint; virtual;
       function write(const Buf;Count:longint):longint; virtual;
       function SeekEx(APosition:longint):longint; virtual;
       function Seek(APosition:longint):longint; overload;
       function Seek(APosition,Origin:longint):longint; overload;
       function Position:longint; virtual;
       function Size:longint; virtual;
       procedure SetSize(NewSize:longint);
       function ReadByte:byte;
       function ReadWord:word;
       function ReadDWord:longword;
       function ReadLine:ansistring;
       function ReadString:ansistring;
       function ReadWideString:widestring;
       function ReadHugeString:THugeString;
       procedure WriteByte(Value:byte);
       function WriteByteCount(Value:byte;Count:longint):longint;
       procedure WriteWord(Value:word);
       procedure WriteDWord(Value:longword);
       procedure WriteShortInt(Value:shortint);
       procedure WriteSmallInt(Value:smallint);
       procedure WriteLongInt(Value:longint);
       procedure WriteInt64(Value:int64);
       procedure WriteBoolean(Value:boolean);
       procedure WriteLine(Line:ansistring);
       procedure WriteString(S:ansistring);
       procedure WriteDataString(S:ansistring);
       procedure WriteDataWidestring(S:widestring);
       procedure WriteDataHugeString(S:THugeString);
       procedure ResetBits;
       function ReadBit:boolean;
       function ReadBits(BitsCount:byte):longword;
       function ReadBitsSigned(BitsCount:byte):longint;
       procedure WriteBit(Value:boolean);
       procedure WriteBits(Value:longword;BitsCount:byte);
       procedure WriteBitsSigned(Value:longint;BitsCount:byte);
       procedure FlushBits;
       property Text:ansistring read GetString write setstring;
       property Bytes[BytePosition:longint]:byte read GetByte write SetByte; default;
       property BitsInBuffer:byte read fBitBufferSize;
     end;

     PBeRoDatenStream=^TBeRoDatenStream;
     TBeRoDatenStream=TBeRoStream;

     PBeRoMemoryStream=^TBeRoMemoryStream;
     TBeRoMemoryStream=TBeRoStream;

     PBeRoFileStream=^TBeRoFileStream;
     TBeRoFileStream=class(TBeRoStream)
      private
       fFile:file;
      public
       constructor Create(FileName:ansistring);
       constructor CreateNew(FileName:ansistring);
       destructor Destroy; override;
       function read(var Buf;Count:longint):longint; override;
       function write(const Buf;Count:longint):longint; override;
       function SeekEx(APosition:longint):longint; override;
       function Position:longint; override;
       function Size:longint; override;
     end;

implementation

type pbyte=^byte;

const MemoryDelta=1 shl 16;
      MemoryDeltaMask=MemoryDelta-1;

constructor TBeRoStream.Create;
begin
 inherited Create;
 fData:=nil;
 REALLOCMEM(fData,0);
 fPosition:=0;
 fSize:=0;
 fInMemorySize:=0;
 ResetBits;
end;

destructor TBeRoStream.Destroy;
begin
 REALLOCMEM(fData,0);
 fPosition:=0;
 fSize:=0;
 fInMemorySize:=0;
 inherited Destroy;
end;

function TBeRoStream.Assign(Src:TBeRoStream):longint;
var Remain,Count:longint;
    Buf:TBeRoStreamBuffer;
begin
 Clear;
 result:=0;
 Remain:=Src.Size;
 if (Seek(0)<>0) or (Src.Seek(0)<>0) then exit;
 while Remain>=sizeof(TBeRoStreamBuffer) do begin
  Count:=Src.read(Buf,sizeof(TBeRoStreamBuffer));
  write(Buf,Count);
  inc(result,Count);
  dec(Remain,sizeof(TBeRoStreamBuffer));
 end;
 Count:=Src.read(Buf,Remain);
 write(Buf,Count);
 inc(result,Count);
end;

function TBeRoStream.Append(Src:TBeRoStream):longint;
var Remain,Count:longint;
    Buf:TBeRoStreamBuffer;
begin
 result:=0;
 Remain:=Src.Size;
 if Src.Seek(0)<>0 then exit;
 while Remain>=sizeof(TBeRoStreamBuffer) do begin
  Count:=Src.read(Buf,sizeof(TBeRoStreamBuffer));
  write(Buf,Count);
  inc(result,Count);
  dec(Remain,sizeof(TBeRoStreamBuffer));
 end;
 Count:=Src.read(Buf,Remain);
 write(Buf,Count);
 inc(result,Count);
end;

function TBeRoStream.AppendFrom(Src:TBeRoStream;Counter:longint):longint;
var Remain,Count:longint;
    Buf:TBeRoStreamBuffer;
begin
 result:=0;
 Remain:=Counter;
 while Remain>=sizeof(TBeRoStreamBuffer) do begin
  Count:=Src.read(Buf,sizeof(TBeRoStreamBuffer));
  write(Buf,Count);
  inc(result,Count);
  dec(Remain,sizeof(TBeRoStreamBuffer));
 end;
 Count:=Src.read(Buf,Remain);
 write(Buf,Count);
 inc(result,Count);
end;

procedure TBeRoStream.Clear;
begin
 REALLOCMEM(fData,0);
 fPosition:=0;
 fSize:=0;
 fInMemorySize:=0;
end;

procedure TBeRoStream.Realloc(NewInMemorySize:longint);
var OldInMemorySize,Count:longint;
begin
 if NewInMemorySize>0 then begin
  NewInMemorySize:=(NewInMemorySize+MemoryDeltaMask) and not MemoryDeltaMask;
 end;
 if fInMemorySize<>NewInMemorySize then begin
  OldInMemorySize:=fInMemorySize;
  fInMemorySize:=NewInMemorySize;
  REALLOCMEM(fData,fInMemorySize);
  Count:=NewInMemorySize-OldInMemorySize;
  if Count>0 then begin
   FILLCHAR(fData^[OldInMemorySize],Count,#0);
  end;
 end;
end;

procedure TBeRoStream.Resize(NewSize:longint);
begin
 fSize:=NewSize;
 if fPosition>fSize then fPosition:=fSize;
 Realloc(fSize);
end;

function TBeRoStream.read(var Buf;Count:longint):longint;
begin
 if (fPosition>=0) and (Count>0) then begin
  result:=fSize-fPosition;
  if result>0 then begin
   if result>Count then result:=Count;
   MOVE(fData^[fPosition],Buf,result);
   inc(fPosition,result);
   exit;
  end;
 end;
 result:=0;
end;

function TBeRoStream.ReadAt(Position:longint;var Buf;Count:longint):longint;
begin
 if Seek(Position)=Position then begin
  result:=read(Buf,Count);
 end else begin
  result:=0;
 end;
end;

function TBeRoStream.write(const Buf;Count:longint):longint;
var EndPosition:longint;
begin
 if (fPosition>=0) and (Count>0) then begin
  EndPosition:=fPosition+Count;
  if EndPosition>fSize then Resize(EndPosition);
  MOVE(Buf,fData^[fPosition],Count);
  fPosition:=EndPosition;
  result:=Count;
  exit;
 end;
 result:=0;
end;

function TBeRoStream.SeekEx(APosition:longint):longint;
var AltePos,RemainSize:longint;
begin
 fPosition:=APosition;
 if fPosition<0 then fPosition:=0;
 if fPosition>fSize then begin
  AltePos:=fSize;
  RemainSize:=fPosition-fSize;
  if RemainSize>0 then begin
   Resize(fSize+RemainSize);
   FILLCHAR(fData^[AltePos],RemainSize,#0);
  end;
  result:=fPosition;
 end else begin
  result:=fPosition;
 end;
end;

function TBeRoStream.Seek(APosition:longint):longint;
begin
 result:=SeekEx(APosition);
end;

function TBeRoStream.Seek(APosition,Origin:longint):longint;
begin
 case Origin of
  bsoFromBeginning:result:=SeekEx(APosition);
  bsoFromCurrent:result:=SeekEx(Position+APosition);
  bsoFromEnd:result:=SeekEx(Size-APosition);
  else result:=SeekEx(APosition);
 end;
end;

function TBeRoStream.Position:longint;
begin
 result:=fPosition;
end;

function TBeRoStream.Size:longint;
begin
 result:=fSize;
end;

procedure TBeRoStream.SetSize(NewSize:longint);
begin
 fSize:=NewSize;
 if fPosition>fSize then fPosition:=fSize;
 REALLOCMEM(fData,fSize);
end;

function TBeRoStream.ReadByte:byte;
var B:byte;
begin
 if read(B,1)<>1 then begin
  result:=0;
 end else begin
  result:=B;
 end;
end;

function TBeRoStream.ReadWord:word;
begin
 result:=ReadByte or (ReadByte shl 8);
end;

function TBeRoStream.ReadDWord:longword;
begin
 result:=ReadWord or (ReadWord shl 16);
end;

function TBeRoStream.ReadLine:ansistring;
var C:ansichar;
begin
 result:='';
 while Position<Size do begin
  read(C,1);
  case C of
   #10,#13:begin
    if (Position<Size) and (((C=#13) and (Bytes[Position]=10)) or
                            ((C=#10) and (Bytes[Position]=13))) then begin
     read(C,1);
    end;
    break;
   end;
   else result:=result+C;
  end;
 end;
end;

function TBeRoStream.ReadString:ansistring;
var L:longword;
begin
 L:=ReadDWord;
 setlength(result,L);
 if L>0 then read(result[1],L);
end;

function TBeRoStream.ReadWideString:widestring;
var L:longword;
begin
 L:=ReadDWord;
 setlength(result,L);
 if L>0 then read(result[1],L*2);
end;

function TBeRoStream.ReadHugeString:THugeString;
var L:longword;
begin
 L:=ReadDWord;
 setlength(result,L);
 if L>0 then read(result[0],L*4);
end;

procedure TBeRoStream.WriteByte(Value:byte);
begin
 write(Value,sizeof(byte));
end;

function TBeRoStream.WriteByteCount(Value:byte;Count:longint):longint;
var Counter:longint;
begin
 result:=0;
 for Counter:=1 to Count do inc(result,write(Value,sizeof(byte)));
end;

procedure TBeRoStream.WriteWord(Value:word);
begin
 write(Value,sizeof(word));
end;

procedure TBeRoStream.WriteDWord(Value:longword);
begin
 write(Value,sizeof(longword));
end;

procedure TBeRoStream.WriteShortInt(Value:shortint);
begin
 write(Value,sizeof(shortint));
end;

procedure TBeRoStream.WriteSmallInt(Value:smallint);
begin
 write(Value,sizeof(smallint));
end;

procedure TBeRoStream.WriteLongInt(Value:longint);
begin
 write(Value,sizeof(longint));
end;

procedure TBeRoStream.WriteInt64(Value:int64);
begin
 write(Value,sizeof(int64));
end;

procedure TBeRoStream.WriteBoolean(Value:boolean);
begin
 if Value then begin
  WriteByte(1);
 end else begin
  WriteByte(0);
 end;
end;

procedure TBeRoStream.WriteLine(Line:ansistring);
const CRLF:array[1..2] of ansichar=#13#10;
begin
 if length(Line)>0 then write(Line[1],length(Line));
 write(CRLF,2);
end;

procedure TBeRoStream.WriteString(S:ansistring);
var L:longword;
begin
 L:=length(S);
 if L>0 then write(S[1],L);
end;

procedure TBeRoStream.WriteDataString(S:ansistring);
var L:longword;
begin
 L:=length(S);
 WriteDWord(L);
 if L>0 then write(S[1],L);
end;

procedure TBeRoStream.WriteDataWideString(S:widestring);
var L:longword;
begin
 L:=length(S);
 WriteDWord(L);
 if L>0 then write(S[1],L*2);
end;

procedure TBeRoStream.WriteDataHugeString(S:THugeString);
var L:longword;
begin
 L:=length(S);
 WriteDWord(L);
 if L>0 then write(S[0],L*4);
end;

procedure TBeRoStream.ResetBits;
begin
 fBitBuffer:=0;
 fBitBufferSize:=0;
end;

function TBeRoStream.ReadBit:boolean;
begin
 result:=(ReadBits(1)<>0);
end;

function TBeRoStream.ReadBits(BitsCount:byte):longword;
begin
 while fBitBufferSize<BitsCount do begin
  fBitBuffer:=(fBitBuffer shl 8) or ReadByte;
  inc(fBitBufferSize,8);
 end;
 result:=(fBitBuffer shr (fBitBufferSize-BitsCount)) and ((1 shl BitsCount)-1);
 dec(fBitBufferSize,BitsCount);
end;

function TBeRoStream.ReadBitsSigned(BitsCount:byte):longint;
begin
 result:=0;
 if BitsCount>1 then begin
  if ReadBits(1)<>0 then begin
   result:=-ReadBits(BitsCount-1);
  end else begin
   result:=ReadBits(BitsCount-1);
  end;
 end;
end;

procedure TBeRoStream.WriteBit(Value:boolean);
begin
 if Value then begin
  WriteBits(1,1);
 end else begin
  WriteBits(0,1);
 end;
end;

procedure TBeRoStream.WriteBits(Value:longword;BitsCount:byte);
begin
 fBitBuffer:=(fBitBuffer shl BitsCount) or Value;
 inc(fBitBufferSize,BitsCount);
 while fBitBufferSize>=8 do begin
  WriteByte((fBitBuffer shr (fBitBufferSize-8)) and $ff);
  dec(fBitBufferSize,8);
 end;
end;

procedure TBeRoStream.WriteBitsSigned(Value:longint;BitsCount:byte);
begin
 if BitsCount>1 then begin
  if Value<0 then begin
   WriteBits(1,1);
   WriteBits(longword(0-Value),BitsCount-1);
  end else begin
   WriteBits(0,1);
   WriteBits(longword(Value),BitsCount-1);
  end;
 end;
end;

procedure TBeRoStream.FlushBits;
begin
 if fBitBufferSize>0 then begin
  WriteByte(fBitBuffer shl (8-fBitBufferSize));
 end;
 fBitBuffer:=0;
 fBitBufferSize:=0;
end;

function TBeRoStream.GetString:ansistring;
begin
 Seek(0);
 if Size>0 then begin
  setlength(result,Size);
  read(result[1],Size);
 end else begin
  result:='';
 end;
end;

procedure TBeRoStream.setstring(Value:ansistring);
begin
 Clear;
 if length(Value)>0 then begin
  write(Value[1],length(Value));
 end;
end;

function TBeRoStream.GetByte(BytePosition:longint):byte;
var AltePosition:longint;
begin
 AltePosition:=Position;
 Seek(BytePosition);
 read(result,sizeof(byte));
 Seek(AltePosition);
end;

procedure TBeRoStream.SetByte(BytePosition:longint;Value:byte);
var AltePosition:longint;
begin
 AltePosition:=Position;
 Seek(BytePosition);
 write(Value,sizeof(byte));
 Seek(AltePosition);
end;

constructor TBeRoFileStream.Create(FileName:ansistring);
var Alt:byte;
begin
 inherited Create;
 Alt:=FileMode;
 FileMode:=0;
 ASSIGNFILE(fFile,FileName);
 {$I-}RESET(fFile,1);{$I+}
 FileMode:=Alt;
 if IOResult<>0 then {$I-}REWRITE(fFile,1);{$I+}
 if IOResult<>0 then begin
 end;
end;

constructor TBeRoFileStream.CreateNew(FileName:ansistring);
var Alt:byte;
begin
 inherited Create;
 Alt:=FileMode;
 FileMode:=2;
 ASSIGNFILE(fFile,FileName);
 {$I-}REWRITE(fFile,1);{$I+}
 FileMode:=Alt;
 if IOResult<>0 then begin
 end;
end;

destructor TBeRoFileStream.Destroy;
begin
 {$I-}CLOSEFILE(fFile);{$I+}
 if IOResult<>0 then begin
 end;
 inherited Destroy;
end;

function TBeRoFileStream.read(var Buf;Count:longint):longint;
var I:longint;
begin
 {$I-}BLOCKREAD(fFile,Buf,Count,I);{$I+}
 if IOResult<>0 then begin
  result:=0;
  exit;
 end;
 {$I-}fPosition:=FILEPOS(fFile);{$I+}
 if IOResult<>0 then begin
  result:=0;
  exit;
 end;
 result:=I;
end;

function TBeRoFileStream.write(const Buf;Count:longint):longint;
var I:longint;
begin
 {$I-}BLOCKWRITE(fFile,Buf,Count,I);{$I+}
 if IOResult<>0 then begin
  result:=0;
  exit;
 end;
 {$I-}fPosition:=FILEPOS(fFile);{$I+}
 if IOResult<>0 then begin
  result:=0;
  exit
 end;
 result:=I;
end;

function TBeRoFileStream.SeekEx(APosition:longint):longint;
begin
 if APosition<=Size then begin
  {$I-}System.SEEK(fFile,APosition);{$I+}
  if IOResult<>0 then begin
   result:=0;
   exit;
  end;
 end;
 {$I-}result:=FILEPOS(fFile);{$I+}
 if IOResult<>0 then begin
  result:=0;
 end;
end;

function TBeRoFileStream.Position:longint;
begin
 {$I-}result:=FILEPOS(fFile);{$I+}
 if IOResult<>0 then begin
  result:=0;
 end;
end;

function TBeRoFileStream.Size:longint;
begin
 {$I-}result:=FILESIZE(fFile);{$I+}
 if IOResult<>0 then begin
  result:=0;
 end;
end;

end.
