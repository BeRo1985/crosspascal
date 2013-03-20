unit CRC32;
{$i Compiler.inc}

interface

uses BERoUtils,BeRoStream;

type PCRC32Table=^TCRC32Table;
     TCRC32Table=array[0..255] of longword;

     TCRC32=class
      public
       Value:longword;
       Table:TCRC32Table;
       constructor Create;
       destructor Destroy; override;
       procedure Init;
       procedure UpdateByte(B:byte);
       procedure Update(const Data;Offset,Size:longword);
       procedure UpdateStream(Data:TBeRoStream);
       procedure UpdateString(Data:ansistring);
       function GetDigest:longword;
       function CalculateDigest(const Data;Offset,Size:longword):longword;
       function VerifyDigest(Digest:longword;const Data;Offset,Size:longword):boolean;
     end;

implementation

var CRCTable:TCRC32Table;

procedure CreateTable;
const GenerationValue=$edb88320;
var I,J,R:longword;
begin
 for I:=0 to 255 do begin
  R:=I;
  for J:=0 to 7 do begin
   if (R and 1)<>0 then begin
    R:=(R shr 1) xor GenerationValue;
   end else begin
    R:=R shr 1;
   end;
  end;
  CRCTable[I]:=R;
 end;
end;

constructor TCRC32.Create;
begin
 inherited Create;
 Table:=CRCTable;
 Value:=$ffffffff;
end;

destructor TCRC32.Destroy;
begin
 inherited Destroy;
end;

procedure TCRC32.Init;
begin
 Value:=$ffffffff;
end;

procedure TCRC32.UpdateByte(B:byte);
begin
 Value:=((Value shr 8) and $00ffffff) xor Table[(Value xor B) and $ff];
end;

procedure TCRC32.Update(const Data;Offset,Size:longword);
type pbyte=^byte;
var Counter:longword;
    B:pbyte;
begin
 B:=@Data;
 inc(B,Offset);
 for Counter:=1 to Size do begin
  UpdateByte(B^);
  inc(B);
 end;
end;

procedure TCRC32.UpdateStream(Data:TBeRoStream);
var Counter:longword;
begin
 Data.Seek(0);
 for Counter:=1 to Data.Size do UpdateByte(Data.ReadByte);
end;

procedure TCRC32.UpdateString(Data:ansistring);
var Counter:longword;
begin
 for Counter:=1 to length(Data) do UpdateByte(byte(Data[Counter]));
end;

function TCRC32.GetDigest:longword;
begin
 result:=Value xor $ffffffff;
end;

function TCRC32.CalculateDigest(const Data;Offset,Size:longword):longword;
var CRC:TCRC32;
begin
 CRC:=TCRC32.Create;
 CRC.Init;
 CRC.Update(Data,Offset,Size);
 result:=CRC.GetDigest;
 CRC.Destroy;
end;

function TCRC32.VerifyDigest(Digest:longword;const Data;Offset,Size:longword):boolean;
begin
 result:=CalculateDigest(Data,Offset,Size)=Digest;
end;

initialization
 CreateTable;
finalization
end.
