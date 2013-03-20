unit BeRoStringHashMap;
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

type PBeRoStringHashMapValue=^TBeRoStringHashMapValue;
     TBeRoStringHashMapValue={$ifdef fpc}
                               ptrint
                             {$else}
                              {$ifdef conditionalexpressions}
                               {$if CompilerVersion>=23.0}
                                NativeInt
                               {$else}
                                {$ifdef cpu64}
                                 int64
                                {$else}
                                 longint
                                {$endif}
                               {$ifend}
                              {$else}
                               {$ifdef cpu64}
                                int64
                               {$else}
                                longint
                               {$endif}
                              {$endif}
                             {$endif};

     PBeRoStringHashMapEntity=^TBeRoStringHashMapEntity;
     TBeRoStringHashMapEntity=record
      Key:ansistring;
      Value:TBeRoStringHashMapValue;
     end;

     TBeRoStringHashMapEntities=array of TBeRoStringHashMapEntity;

     TBeRoStringHashMapEntityIndices=array of longint;

     TBeRoStringHashMap=class
      private
       function FindCell(const Key:ansistring):longword;
       procedure Resize;
      protected
       function GetValue(const Key:ansistring):TBeRoStringHashMapValue;
       procedure SetValue(const Key:ansistring;const Value:TBeRoStringHashMapValue);
      public
       RealSize:longint;
       LogSize:longint;
       Size:longint;
       Entities:TBeRoStringHashMapEntities;
       EntityToCellIndex:TBeRoStringHashMapEntityIndices;
       CellToEntityIndex:TBeRoStringHashMapEntityIndices;
       constructor Create;
       destructor Destroy; override;
       procedure Clear;
       function Add(const Key:ansistring;Value:TBeRoStringHashMapValue):PBeRoStringHashMapEntity;
       function Get(const Key:ansistring;CreateIfNotExist:boolean=false):PBeRoStringHashMapEntity;
       function Delete(const Key:ansistring):boolean;
       property Values[const Key:ansistring]:TBeRoStringHashMapValue read GetValue write SetValue; default;
     end;

implementation

const CELL_EMPTY=-1;
      CELL_DELETED=-2;

      ENT_EMPTY=-1;
      ENT_DELETED=-2;

      MaxListSize=$07ffffff;

{$ifdef fpc}
 {$undef OldDelphi}
{$else}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=23.0}
   {$undef OldDelphi}
type qword=uint64;
     ptruint=NativeUInt;
     ptrint=NativeInt;
  {$else}
   {$define OldDelphi}
  {$ifend}
 {$else}
  {$define OldDelphi}
 {$endif}
{$endif}
{$ifdef OldDelphi}
type qword=int64;
{$ifdef cpu64}
     ptruint=qword;
     ptrint=int64;
{$else}
     ptruint=longword;
     ptrint=longint;
{$endif}
{$endif}

function HashString(const Str:ansistring):longword;
{$ifdef cpuarm}
var b:pansichar;
    Len,h,i:longword;
begin
 result:=2166136261;
 Len:=length(Str);
 h:=Len;
 if Len>0 then begin
  b:=pansichar(Str);
  while Len>3 do begin
   i:=longword(pointer(b)^);
   h:=(h xor i) xor $2e63823a;
   inc(h,(h shl 15) or (h shr (32-15)));
   dec(h,(h shl 9) or (h shr (32-9)));
   inc(h,(h shl 4) or (h shr (32-4)));
   dec(h,(h shl 1) or (h shr (32-1)));
   h:=h xor (h shl 2) or (h shr (32-2));
   result:=result xor i;
   inc(result,(result shl 1)+(result shl 4)+(result shl 7)+(result shl 8)+(result shl 24));
   inc(b,4);
   dec(Len,4);
  end;
  if Len>1 then begin
   i:=word(pointer(b)^);
   h:=(h xor i) xor $2e63823a;
   inc(h,(h shl 15) or (h shr (32-15)));
   dec(h,(h shl 9) or (h shr (32-9)));
   inc(h,(h shl 4) or (h shr (32-4)));
   dec(h,(h shl 1) or (h shr (32-1)));
   h:=h xor (h shl 2) or (h shr (32-2));
   result:=result xor i;
   inc(result,(result shl 1)+(result shl 4)+(result shl 7)+(result shl 8)+(result shl 24));
   inc(b,2);
   dec(Len,2);
  end;
  if Len>0 then begin
   i:=byte(b^);
   h:=(h xor i) xor $2e63823a;
   inc(h,(h shl 15) or (h shr (32-15)));
   dec(h,(h shl 9) or (h shr (32-9)));
   inc(h,(h shl 4) or (h shr (32-4)));
   dec(h,(h shl 1) or (h shr (32-1)));
   h:=h xor (h shl 2) or (h shr (32-2));
   result:=result xor i;
   inc(result,(result shl 1)+(result shl 4)+(result shl 7)+(result shl 8)+(result shl 24));
  end;
 end;
 result:=result xor h;
 if result=0 then begin
  result:=$ffffffff;
 end;
end;
{$else}
const m=longword($57559429);
      n=longword($5052acdb);
var b:pansichar;
    h,k,Len:longword;
    p:{$ifdef fpc}qword{$else}int64{$endif};
begin
 Len:=length(Str);
 h:=Len;
 k:=h+n+1;
 if Len>0 then begin
  b:=pansichar(Str);
  while Len>7 do begin
   begin
    p:=longword(pointer(b)^)*qword(n);
    h:=h xor longword(p and $ffffffff);
    k:=k xor longword(p shr 32);
    inc(b,4);
   end;
   begin
    p:=longword(pointer(b)^)*qword(m);
    k:=k xor longword(p and $ffffffff);
    h:=h xor longword(p shr 32);
    inc(b,4);
   end;
   dec(Len,8);
  end;
  if Len>3 then begin
   p:=longword(pointer(b)^)*qword(n);
   h:=h xor longword(p and $ffffffff);
   k:=k xor longword(p shr 32);
   inc(b,4);
   dec(Len,4);
  end;
  if Len>0 then begin
   if Len>1 then begin
    p:=word(pointer(b)^);
    inc(b,2);
    dec(Len,2);
   end else begin
    p:=0;
   end;
   if Len>0 then begin
    p:=p or (byte(b^) shl 16);
   end;
   p:=p*qword(m);
   k:=k xor longword(p and $ffffffff);
   h:=h xor longword(p shr 32);
  end;
 end;
 begin
  p:=(h xor (k+n))*qword(n);
  h:=h xor longword(p and $ffffffff);
  k:=k xor longword(p shr 32);
 end;
 result:=k xor h;
 if result=0 then begin
  result:=$ffffffff;
 end;
end;
{$endif}

constructor TBeRoStringHashMap.Create;
begin
 inherited Create;
 RealSize:=0;
 LogSize:=0;
 Size:=0;
 Entities:=nil;
 EntityToCellIndex:=nil;
 CellToEntityIndex:=nil;
 Resize;
end;

destructor TBeRoStringHashMap.Destroy;
begin
 Clear;
 SetLength(Entities,0);
 inherited Destroy;
end;

procedure TBeRoStringHashMap.Clear;
var Counter:longint;
begin
 for Counter:=0 to length(Entities)-1 do begin
  Entities[Counter].Key:='';
 end;
 RealSize:=0;
 LogSize:=0;
 Size:=0;
 SetLength(Entities,0);
 SetLength(EntityToCellIndex,0);
 SetLength(CellToEntityIndex,0);
 Resize;
end;

function TBeRoStringHashMap.FindCell(const Key:ansistring):longword;
var HashCode,Mask,Step:longword;
    Entity:longint;
begin
 HashCode:=HashString(Key);
 Mask:=(2 shl LogSize)-1;
 Step:=((HashCode shl 1)+1) and Mask;
 if LogSize<>0 then begin
  result:=HashCode shr (32-LogSize);
 end else begin
  result:=0;
 end;
 repeat
  Entity:=CellToEntityIndex[result];
  if (Entity=ENT_EMPTY) or ((Entity<>ENT_DELETED) and (Entities[Entity].Key=Key)) then begin
   exit;
  end;
  result:=(result+Step) and Mask;
 until false;
end;

procedure TBeRoStringHashMap.Resize;
var NewLogSize,NewSize,Cell,Entity,Counter:longint;
    OldEntities:TBeRoStringHashMapEntities;
    OldCellToEntityIndex:TBeRoStringHashMapEntityIndices;
    OldEntityToCellIndex:TBeRoStringHashMapEntityIndices;
begin
 NewLogSize:=0;
 NewSize:=RealSize;
 while NewSize<>0 do begin
  NewSize:=NewSize shr 1;
  inc(NewLogSize);
 end;
 if NewLogSize<1 then begin
  NewLogSize:=1;
 end;
 Size:=0;
 RealSize:=0;
 LogSize:=NewLogSize;
 OldEntities:=Entities;
 OldCellToEntityIndex:=CellToEntityIndex;
 OldEntityToCellIndex:=EntityToCellIndex;
 Entities:=nil;
 CellToEntityIndex:=nil;
 EntityToCellIndex:=nil;
 SetLength(Entities,2 shl LogSize);
 SetLength(CellToEntityIndex,2 shl LogSize);
 SetLength(EntityToCellIndex,2 shl LogSize);
 for Counter:=0 to length(CellToEntityIndex)-1 do begin
  CellToEntityIndex[Counter]:=ENT_EMPTY;
 end;
 for Counter:=0 to length(EntityToCellIndex)-1 do begin
  EntityToCellIndex[Counter]:=CELL_EMPTY;
 end;
 for Counter:=0 to length(OldEntityToCellIndex)-1 do begin
  Cell:=OldEntityToCellIndex[Counter];
  if Cell>=0 then begin
   Entity:=OldCellToEntityIndex[Cell];
   if Entity>=0 then begin
    Add(OldEntities[Counter].Key,OldEntities[Counter].Value);
   end;
  end;
 end;
end;

function TBeRoStringHashMap.Add(const Key:ansistring;Value:TBeRoStringHashMapValue):PBeRoStringHashMapEntity;
var Entity:longint;
    Cell:longword;
begin
 result:=nil;
 while RealSize>=(1 shl LogSize) do begin
  Resize;
 end;
 Cell:=FindCell(Key);
 Entity:=CellToEntityIndex[Cell];
 if Entity>=0 then begin
  result:=@Entities[Entity];
  result^.Key:=Key;
  result^.Value:=Value;
  exit;
 end;
 Entity:=Size;
 inc(Size);
 if Entity<(2 shl LogSize) then begin
  CellToEntityIndex[Cell]:=Entity;
  EntityToCellIndex[Entity]:=Cell;
  inc(RealSize);
  result:=@Entities[Entity];
  result^.Key:=Key;
  result^.Value:=Value;
 end;
end;

function TBeRoStringHashMap.Get(const Key:ansistring;CreateIfNotExist:boolean=false):PBeRoStringHashMapEntity;
var Entity:longint;
    Cell:longword;
begin
 result:=nil;
 Cell:=FindCell(Key);
 Entity:=CellToEntityIndex[Cell];
 if Entity>=0 then begin
  result:=@Entities[Entity];
 end else if CreateIfNotExist then begin
  result:=Add(Key,0);
 end;
end;

function TBeRoStringHashMap.Delete(const Key:ansistring):boolean;
var Entity:longint;
    Cell:longword;
begin
 result:=false;
 Cell:=FindCell(Key);
 Entity:=CellToEntityIndex[Cell];
 if Entity>=0 then begin
  Entities[Entity].Key:='';
  Entities[Entity].Value:=0;
  EntityToCellIndex[Entity]:=CELL_DELETED;
  CellToEntityIndex[Cell]:=ENT_DELETED;
  dec(RealSize);
  if RealSize<(1 shl LogSize) then begin
   Resize;
  end;
  result:=true;
 end;
end;

function TBeRoStringHashMap.GetValue(const Key:ansistring):TBeRoStringHashMapValue;
var Entity:longint;
    Cell:longword;
begin
 Cell:=FindCell(Key);
 Entity:=CellToEntityIndex[Cell];
 if Entity>=0 then begin
  result:=Entities[Entity].Value;
 end else begin
  result:=-1;
 end;
end;

procedure TBeRoStringHashMap.SetValue(const Key:ansistring;const Value:TBeRoStringHashMapValue);
begin
 Add(Key,Value);
end;

end.
