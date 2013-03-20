unit BeRoStringTree;
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

const MaxStringHashes=32;

type TBeRoStringTreeLink=longword;

     PBeRoStringTreeNode=^TBeRoStringTreeNode;
     TBeRoStringTreeNode=record
      TheChar:ansichar;
      Link:TBeRoStringTreeLink;
      LinkExist:boolean;
      Prevoius,Next,Up,Down:PBeRoStringTreeNode;
     end;

     TBeRoStringHashes=array[0..MaxStringHashes-1] of longword;
     TBeRoStringHashStrings=array[0..MaxStringHashes-1] of ansistring;
     TBeRoStringHashNodes=array[0..MaxStringHashes-1] of PBeRoStringTreeNode;

     TBeRoStringTree=class
      private
       Root:PBeRoStringTreeNode;
       Hashes:TBeRoStringHashes;
       HashStrings:TBeRoStringHashStrings;
       HashNodes:TBeRoStringHashNodes;
      public
       Hashing:boolean;
       constructor Create;
       destructor Destroy; override;
       procedure Clear;
       procedure Dump;
       function Add(Content:ansistring;Link:TBeRoStringTreeLink;Replace:boolean=false):boolean;
       function Delete(Content:ansistring):boolean;
       function Find(Content:ansistring;var Link:TBeRoStringTreeLink):boolean;
     end;

implementation

function HashString(S:ansistring):longword;
var Counter,StringLength:longint;
begin
 result:=0;
 StringLength:=length(S);
 for Counter:=1 to StringLength do begin
{$IFDEF CPU386}
  asm
   ROR dword PTR result,13
  end;
  inc(result,byte(S[Counter]));
{$ELSE}
  result:=((result shr 13) or (result shl (32-13)))+byte(S[Counter]);
{$ENDIF}
 end;
end;

function CreateStringTreeNode(AChar:ansichar):PBeRoStringTreeNode;
begin
 GETMEM(result,sizeof(TBeRoStringTreeNode));
 result^.TheChar:=AChar;
 result^.Link:=0;
 result^.LinkExist:=false;
 result^.Prevoius:=nil;
 result^.Next:=nil;
 result^.Up:=nil;
 result^.Down:=nil;
end;

procedure DestroyStringTreeNode(Node:PBeRoStringTreeNode);
begin
 if not assigned(Node) then exit;
 DestroyStringTreeNode(Node^.Next);
 DestroyStringTreeNode(Node^.Down);
 FREEMEM(Node);
end;

constructor TBeRoStringTree.Create;
begin
 inherited Create;
 Root:=nil;
 Clear;
end;

destructor TBeRoStringTree.Destroy;
begin
 Clear;
 inherited Destroy;
end;

procedure TBeRoStringTree.Clear;
var Counter:longint;
begin
 DestroyStringTreeNode(Root);
 Root:=nil;
 FILLCHAR(Hashes,sizeof(TBeRoStringHashes),#0);
 FILLCHAR(HashNodes,sizeof(TBeRoStringHashNodes),#0);
 for Counter:=0 to MaxStringHashes-1 do begin
  HashStrings[Counter]:='';
 end;
end;

procedure TBeRoStringTree.Dump;
var Ident:longint;
 procedure DumpNode(Node:PBeRoStringTreeNode);
 var SubNode:PBeRoStringTreeNode;
     IdentCounter,IdentOld:longint;
 begin
  for IdentCounter:=1 to Ident do write(' ');
  write(Node^.TheChar);
  IdentOld:=Ident;
  SubNode:=Node^.Next;
  while assigned(SubNode) do begin
   write(SubNode.TheChar);
   if not assigned(SubNode^.Next) then break;
   inc(Ident);
   SubNode:=SubNode^.Next;
  end;
  WRITELN;
  inc(Ident);
  while assigned(SubNode) and (SubNode<>Node) do begin
   if assigned(SubNode^.Down) then DumpNode(SubNode^.Down);
   SubNode:=SubNode^.Prevoius;
   dec(Ident);
  end;
  Ident:=IdentOld;
  if assigned(Node^.Down) then DumpNode(Node^.Down);
 end;
begin
 Ident:=0;
 DumpNode(Root);
end;

function TBeRoStringTree.Add(Content:ansistring;Link:TBeRoStringTreeLink;Replace:boolean=false):boolean;
var StringLength,Position,PositionCounter:longint;
    NewNode,LastNode,Node:PBeRoStringTreeNode;
    StringChar,NodeChar:ansichar;
    Hash,HashToCompare,HashCounter:longword;
begin
 result:=false;
 Hash:=0;
 StringLength:=length(Content);
 if StringLength>0 then begin
  if Hashing then begin
   Hash:=HashString(Content);
   for HashCounter:=0 to MaxStringHashes-1 do begin
    HashToCompare:=Hashes[HashCounter];
    if HashToCompare<>0 then begin
     if HashToCompare=Hash then begin
      if HashStrings[HashCounter]=Content then begin
       if assigned(HashNodes[HashCounter]) then begin
        LastNode:=HashNodes[HashCounter];
        if Replace or not LastNode^.LinkExist then begin
         LastNode^.Link:=Link;
         result:=true;
        end;
        exit;
       end;
      end;
     end;
    end else begin
     break;
    end;
   end;
  end;
  LastNode:=nil;
  Node:=Root;
  for Position:=1 to StringLength do begin
   StringChar:=Content[Position];
   if assigned(Node) then begin
    NodeChar:=Node^.TheChar;
    if NodeChar=StringChar then begin
     LastNode:=Node;
     Node:=Node^.Next;
   end else begin
     while (NodeChar<StringChar) and assigned(Node^.Down) do begin
      Node:=Node^.Down;
      NodeChar:=Node^.TheChar;
     end;
     if NodeChar=StringChar then begin
      LastNode:=Node;
      Node:=Node^.Next;
     end else begin
      NewNode:=CreateStringTreeNode(StringChar);
      if NodeChar<StringChar then begin
       NewNode^.Down:=Node^.Down;
       NewNode^.Up:=Node;
       if assigned(NewNode^.Down) then begin
        NewNode^.Down^.Up:=NewNode;
       end;
       NewNode^.Prevoius:=Node^.Prevoius;
       Node^.Down:=NewNode;
      end else if NodeChar>StringChar then begin
       NewNode^.Down:=Node;
       NewNode^.Up:=Node^.Up;
       if assigned(NewNode^.Up) then begin
        NewNode^.Up^.Down:=NewNode;
       end;
       NewNode^.Prevoius:=Node^.Prevoius;
       if not assigned(NewNode^.Up) then begin
        if assigned(NewNode^.Prevoius) then begin
         NewNode^.Prevoius^.Next:=NewNode;
        end else begin
         Root:=NewNode;
        end;
       end;
       Node^.Up:=NewNode;
      end;
      LastNode:=NewNode;
      Node:=LastNode^.Next;
     end;
    end;
   end else begin
    for PositionCounter:=Position to StringLength do begin
     NewNode:=CreateStringTreeNode(Content[PositionCounter]);
     if assigned(LastNode) then begin
      NewNode^.Prevoius:=LastNode;
      LastNode^.Next:=NewNode;
      LastNode:=LastNode^.Next;
     end else begin
      if not assigned(Root) then begin
       Root:=NewNode;
       LastNode:=Root;
      end;
     end;
    end;
    break;
   end;
  end;
  if assigned(LastNode) then begin
   if Replace or not LastNode^.LinkExist then begin
    if Hashing then begin
     for HashCounter:=0 to MaxStringHashes-2 do begin
      Hashes[HashCounter+1]:=Hashes[HashCounter];
      HashStrings[HashCounter+1]:=HashStrings[HashCounter];
      HashNodes[HashCounter+1]:=HashNodes[HashCounter];
     end;
     Hashes[0]:=Hash;
     HashStrings[0]:=Content;
     HashNodes[0]:=LastNode;
    end;
    LastNode^.Link:=Link;
    LastNode^.LinkExist:=true;
    result:=true;
   end;
  end;
 end;
end;

function TBeRoStringTree.Delete(Content:ansistring):boolean;
var StringLength,Position:longint;
    Node:PBeRoStringTreeNode;
    StringChar,NodeChar:ansichar;
    Hash,HashToCompare,HashCounter:longword;
begin
 result:=false;
 Hash:=0;
 StringLength:=length(Content);
 if StringLength>0 then begin
  if Hashing then begin
   Hash:=HashString(Content);
   for HashCounter:=0 to MaxStringHashes-1 do begin
    HashToCompare:=Hashes[HashCounter];
    if HashToCompare<>0 then begin
     if HashToCompare=Hash then begin
      if HashStrings[HashCounter]=Content then begin
       if assigned(HashNodes[HashCounter]) then begin
        HashNodes[HashCounter]^.LinkExist:=false;
        result:=true;
        exit;
       end;
      end;
     end;
    end else begin
     break;
    end;
   end;
  end;
  Node:=Root;
  for Position:=1 to StringLength do begin
   StringChar:=Content[Position];
   if assigned(Node) then begin
    NodeChar:=Node^.TheChar;
    while (NodeChar<>StringChar) and assigned(Node^.Down) do begin
     Node:=Node^.Down;
     NodeChar:=Node^.TheChar;
    end;
    if NodeChar=StringChar then begin
     if (Position=StringLength) and Node^.LinkExist then begin
      if Hashing then begin
       for HashCounter:=0 to MaxStringHashes-2 do begin
        Hashes[HashCounter+1]:=Hashes[HashCounter];
        HashStrings[HashCounter+1]:=HashStrings[HashCounter];
        HashNodes[HashCounter+1]:=HashNodes[HashCounter];
       end;
       Hashes[0]:=Hash;
       HashStrings[0]:=Content;
       HashNodes[0]:=Node;
      end;
      Node^.LinkExist:=false;
      result:=true;
      break;
     end;
     Node:=Node^.Next;
    end else begin
     break;
    end;
   end else begin
    break;
   end;
  end;
 end;
end;

function TBeRoStringTree.Find(Content:ansistring;var Link:TBeRoStringTreeLink):boolean;
var StringLength,Position:longint;
    Node:PBeRoStringTreeNode;
    StringChar,NodeChar:ansichar;
    Hash,HashToCompare,HashCounter:longword;
begin
 result:=false;
 Hash:=0;
 StringLength:=length(Content);
 if StringLength>0 then begin
  if Hashing then begin
   Hash:=HashString(Content);
   for HashCounter:=0 to MaxStringHashes-1 do begin
    HashToCompare:=Hashes[HashCounter];
    if HashToCompare<>0 then begin
     if HashToCompare=Hash then begin
      if HashStrings[HashCounter]=Content then begin
       if assigned(HashNodes[HashCounter]) then begin
        Link:=HashNodes[HashCounter]^.Link;
        result:=true;
        exit;
       end;
      end;
     end;
    end else begin
     break;
    end;
   end;
  end;
  Node:=Root;
  for Position:=1 to StringLength do begin
   StringChar:=Content[Position];
   if assigned(Node) then begin
    NodeChar:=Node^.TheChar;
    while (NodeChar<>StringChar) and assigned(Node^.Down) do begin
     Node:=Node^.Down;
     NodeChar:=Node^.TheChar;
    end;
    if NodeChar=StringChar then begin
     if (Position=StringLength) and Node^.LinkExist then begin
      if Hashing then begin
       for HashCounter:=0 to MaxStringHashes-2 do begin
        Hashes[HashCounter+1]:=Hashes[HashCounter];
        HashStrings[HashCounter+1]:=HashStrings[HashCounter];
        HashNodes[HashCounter+1]:=HashNodes[HashCounter];
       end;
       Hashes[0]:=Hash;
       HashStrings[0]:=Content;
       HashNodes[0]:=Node;
      end;
      Link:=Node^.Link;
      result:=true;
      exit;
     end;
     Node:=Node^.Next;
    end else begin
     break;
    end;
   end else begin
    break;
   end;
  end;
 end;
end;

end.
